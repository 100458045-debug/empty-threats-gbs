# =============================================================================
# _harvest_functions.R
#
# Layer A, level 2: download the project documents behind the IATI
# `document-link` URLs, so the Conditionality Stringency Index (CSI) can be
# built from their text.
#
# Level 1 (the IATI API pull) gives the URLs. This script fetches what is
# behind them.
#
# DESIGN NOTES
#
#   Resumability is the central requirement, not a nicety. AfDB's robots.txt
#   sets Crawl-delay: 10, so a few thousand PDFs is a job measured in hours.
#   It WILL be interrupted. Every outcome — success and permanent failure
#   alike — is written to a manifest as it happens, and a re-run skips
#   anything already resolved. Nothing is held in memory that matters.
#
#   Storage is content-addressed: the file lands at <sha1-of-url>.pdf. Two
#   activities citing the same document store one copy, and re-running never
#   duplicates.
#
#   Politeness is per-domain and enforced by wall clock. Do not parallelise
#   this. The crawl delay is the contract that keeps the harvest legitimate,
#   and an 11-hour polite job is cheaper than being blocked at hour three.
#
# USAGE
#   source("_harvest_functions.R")
#   links <- build_document_index()                      # A01/A04/A08 by default
#   harvest_documents(links, limit = 25)                 # smoke test first
#   harvest_documents(links)                             # then the full run
#   harvest_report()
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(httr2)
  library(rvest)
  library(digest)
  library(DBI)
  library(RSQLite)
})

# ---- paths ------------------------------------------------------------------

# Anchored on a file that exists only at the repository root. A looser anchor
# silently resolves to the parent folder and writes data/ outside the repo
# instead of into it.
PROJ_ROOT  <- rprojroot::find_root(rprojroot::has_file(".gitignore"))
DOC_DIR    <- file.path(PROJ_ROOT, "data", "raw", "documents")
INTERIM    <- file.path(PROJ_ROOT, "data", "interim")
MANIFEST   <- file.path(INTERIM, "document_manifest.csv")
DB_PATH    <- Sys.getenv("GBS_DB_PATH", file.path(PROJ_ROOT, "data", "raw", "gbs_analysis.sqlite"))  # 30GB source, not tracked in git; set GBS_DB_PATH or place the file here

dir.create(DOC_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(INTERIM, recursive = TRUE, showWarnings = FALSE)

# ---- politeness -------------------------------------------------------------

# Identify yourself. Polite scraping is not optional and an anonymous
# high-volume crawler is what gets IP-banned. Put a real contact here.
HARVEST_UA <- paste0(
  "UC3M-MSc-thesis-research/1.0 (GBS conditionality; ",
  "see repository for contact) httr2"
)

# Seconds to wait between hits, per domain.
#   afdb.org  : robots.txt Crawl-delay: 10 (verified 19 Aug 2026) — do not lower
#   others    : 2s is courteous and well under any published limit
CRAWL_DELAY <- c(
  # AfDB robots.txt declares Crawl-delay: 10 (verified 19 Aug 2026). Do not lower.
  "afdb.org"                = 10,
  "mapafrica.afdb.org"      = 10,
  "evrd.afdb.org"           = 10,
  # The rest: 2s is courteous and well inside anything these hosts publish.
  "documents.worldbank.org" = 2,
  "projects.worldbank.org"  = 2,
  "search.worldbank.org"    = 2,
  "worldbank.org"           = 2,
  "iadb.org"                = 2,
  "adb.org"                 = 2,
  "ec.europa.eu"            = 2,
  "international-partnerships.ec.europa.eu" = 2,
  "_default"                = 3
)

MAX_BYTES   <- 80 * 1024^2   # skip anything absurd; GBS docs are single-digit MB
REQ_TIMEOUT <- 120

.last_hit <- new.env(parent = emptyenv())

.delay_for <- function(domain) {
  # `[[` on a named vector errors when the name is absent; `[` returns NA, which
  # is what we want for the many domains not listed above.
  if (is.na(domain)) return(CRAWL_DELAY[["_default"]])
  d <- unname(CRAWL_DELAY[domain])
  if (is.na(d)) CRAWL_DELAY[["_default"]] else d
}

#' Block until this domain may be hit again. Wall-clock, per domain.
.throttle <- function(domain) {
  wait <- .delay_for(domain)
  last <- .last_hit[[domain]]
  if (!is.null(last)) {
    elapsed <- as.numeric(difftime(Sys.time(), last, units = "secs"))
    if (elapsed < wait) Sys.sleep(wait - elapsed)
  }
  assign(domain, Sys.time(), envir = .last_hit)
}

.domain_of <- function(url) {
  h <- tryCatch(httr2::url_parse(url)$hostname, error = function(e) NA_character_)
  if (is.na(h)) return(NA_character_)
  sub("^www\\.", "", tolower(h))
}

# ---- manifest ---------------------------------------------------------------

MANIFEST_COLS <- c(
  "iati_identifier", "url", "url_resolved", "domain", "status",
  "http_code", "content_type", "bytes", "sha1", "path", "ts", "note"
)

manifest_read <- function() {
  if (!file.exists(MANIFEST)) {
    return(tibble(!!!set_names(rep(list(character()), length(MANIFEST_COLS)),
                               MANIFEST_COLS)))
  }
  read_csv(MANIFEST, col_types = cols(.default = "c"))
}

manifest_append <- function(row) {
  new <- !file.exists(MANIFEST)
  write_csv(as_tibble(row), MANIFEST, append = !new, col_names = new)
  invisible(row)
}

# Statuses that must never be retried. Anything else (timeout, 5xx, connection
# reset) is transient and a re-run picks it up again.
TERMINAL <- c("ok", "cached", "not_pdf", "too_big", "http_404", "http_403", "unresolvable")

# ---- URL resolution, per domain ---------------------------------------------

#' AfDB wraps many PDFs in a pdf.js viewer. The real file sits URL-encoded in
#' the `file=` query parameter, so this is a string operation, not a browser
#' automation problem.
afdb_unwrap_pdf <- function(url) {
  if (!str_detect(url, "file=")) return(url)
  inner <- str_match(url, "[?&]file=([^&]+)")[, 2]
  if (is.na(inner)) return(url)
  utils::URLdecode(inner)
}

#' EC publishes a landing page, not a PDF. One HTML hop finds the real file.
#' Older /publications/ paths now redirect to /publications-library/ — follow
#' redirects or the link is lost.
ec_resolve_pdf <- function(url) {
  page <- tryCatch(
    request(url) |>
      req_user_agent(HARVEST_UA) |>
      req_timeout(REQ_TIMEOUT) |>
      req_perform(),
    error = function(e) NULL
  )
  if (is.null(page)) return(NA_character_)

  html <- tryCatch(resp_body_html(page), error = function(e) NULL)
  if (is.null(html)) return(NA_character_)

  href <- html |>
    html_elements("a") |>
    html_attr("href") |>
    discard(is.na) |>
    keep(~ str_detect(.x, "/document/download/|\\.pdf")) |>
    head(1)

  if (!length(href)) return(NA_character_)
  xml2::url_absolute(href, resp_url(page))
}

#' World Bank: public API, no key. IATI ids look like 44000-P114154; the Bank's
#' own project id is the P-number.
wb_resolve_pdf <- function(iati_identifier) {
  pid <- str_match(iati_identifier, "(P\\d{6})")[, 2]
  if (is.na(pid)) return(NA_character_)

  resp <- tryCatch(
    request("https://search.worldbank.org/api/v3/wds") |>
      req_url_query(format = "json", projectid = pid,
                    fl = "docna,docty,pdfurl", rows = 50) |>
      req_user_agent(HARVEST_UA) |>
      req_timeout(REQ_TIMEOUT) |>
      req_perform(),
    error = function(e) NULL
  )
  if (is.null(resp)) return(NA_character_)

  body <- resp_body_json(resp, simplifyVector = FALSE)
  docs <- body[["documents"]]
  if (is.null(docs)) return(NA_character_)

  urls <- map_chr(docs, ~ .x[["pdfurl"]] %||% NA_character_) |> discard(is.na)
  if (!length(urls)) NA_character_ else urls[[1]]
}

#' Dispatch to whichever resolver the domain needs.
resolve_url <- function(url, iati_identifier) {
  dom <- .domain_of(url)
  if (is.na(dom)) return(NA_character_)

  if (str_detect(dom, "afdb\\.org"))        return(afdb_unwrap_pdf(url))
  if (str_detect(dom, "worldbank\\.org"))   return(url)
  if (str_detect(dom, "ec\\.europa\\.eu")) {
    if (str_detect(url, "\\.pdf$")) return(url)
    .throttle(dom)
    return(ec_resolve_pdf(url))
  }
  url
}

# ---- build the input table --------------------------------------------------

#' Read activity -> document URL pairs from the analysis database.
#'
#' Categories are taken from `documentlink_category`, which is a separate
#' normalised table. This is the reason the frame was rebuilt from IATI Tables
#' rather than the flat export: there, repeated child elements are comma-joined
#' and URL i does not correspond to category i on 628 of 1,118 activities
#'. Here the relation is native and can be trusted.
#'
#' @param categories document category codes to keep. A01 appraisal, A04 legal
#'   agreement and A08 results framework are the ones carrying conditionality;
#'   NULL takes everything.
#' @param ids optional character vector of iatiidentifier to restrict to.
#' @param analytical_only keep only activities in the analytical sample
#'   (complete_flag = 1 and a computable gap).
build_document_index <- function(categories = c("A01", "A04", "A08"),
                                 ids = NULL,
                                 analytical_only = FALSE,
                                 db = DB_PATH) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  docs <- DBI::dbGetQuery(con, "
    SELECT d.iatiidentifier, d.url, d.format, d.category,
           a.donor, a.donor_ref, a.complete_flag, a.y1_gap_full
    FROM documents d
    JOIN analysis a ON a.iatiidentifier = d.iatiidentifier
    WHERE d.url IS NOT NULL")

  out <- as_tibble(docs)
  if (!is.null(categories))  out <- filter(out, category %in% categories)
  if (!is.null(ids))         out <- filter(out, iatiidentifier %in% ids)
  if (analytical_only)       out <- filter(out, complete_flag == 1, !is.na(y1_gap_full))

  out |>
    filter(str_detect(url, "^https?://")) |>
    # one row per URL: a URL can carry several categories, and we fetch the file once
    distinct(iatiidentifier, url, .keep_all = TRUE) |>
    mutate(domain = map_chr(url, .domain_of)) |>
    select(iati_identifier = iatiidentifier, donor, donor_ref, url, category, domain)
}

# ---- the fetch --------------------------------------------------------------

.fetch_one <- function(iati_identifier, url) {
  ts  <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  dom <- .domain_of(url)

  # Build the full row first, then overwrite the fields the caller supplies.
  # Passing defaults and overrides to tibble() in one call duplicates columns.
  row <- function(status, ..., resolved = NA_character_) {
    base <- list(
      iati_identifier = iati_identifier, url = url, url_resolved = resolved,
      domain = dom %||% NA_character_, status = status,
      http_code = NA_character_, content_type = NA_character_,
      bytes = NA_character_, sha1 = NA_character_, path = NA_character_,
      ts = ts, note = ""
    )
    over <- list(...)
    base[names(over)] <- over
    as_tibble(base)
  }

  if (is.na(dom)) return(manifest_append(row("unresolvable", note = "bad url")))

  resolved <- tryCatch(resolve_url(url, iati_identifier),
                       error = function(e) NA_character_)
  if (is.na(resolved) || !nzchar(resolved)) {
    return(manifest_append(row("unresolvable", note = "no pdf link found")))
  }

  key  <- digest(resolved, algo = "sha1")
  dest <- file.path(DOC_DIR, paste0(key, ".pdf"))
  if (file.exists(dest)) {
    return(manifest_append(row("cached", resolved = resolved,
                               sha1 = key, path = dest,
                               bytes = as.character(file.size(dest)))))
  }

  .throttle(.domain_of(resolved) %||% dom)

  resp <- tryCatch(
    request(resolved) |>
      req_user_agent(HARVEST_UA) |>
      req_timeout(REQ_TIMEOUT) |>
      req_retry(max_tries = 3, backoff = ~ 5 * .x) |>
      req_error(is_error = function(r) FALSE) |>   # inspect, don't throw
      req_perform(),
    error = function(e) NULL
  )

  if (is.null(resp)) {
    return(manifest_append(row("error", resolved = resolved,
                               note = "connection failed")))
  }

  code <- resp_status(resp)
  ctype <- tryCatch(resp_content_type(resp), error = function(e) NA_character_)

  if (code >= 400) {
    st <- if (code %in% c(404, 403)) paste0("http_", code) else "error"
    return(manifest_append(row(st, resolved = resolved,
                               http_code = as.character(code),
                               content_type = ctype %||% NA_character_)))
  }

  body <- tryCatch(resp_body_raw(resp), error = function(e) NULL)
  if (is.null(body) || !length(body)) {
    return(manifest_append(row("error", resolved = resolved,
                               http_code = as.character(code),
                               note = "empty body")))
  }

  # Trust the magic bytes over the declared content type; misconfigured servers
  # label PDFs as octet-stream and HTML error pages as application/pdf.
  is_pdf <- length(body) > 4 &&
    rawToChar(body[1:4]) == "%PDF"

  if (!is_pdf) {
    return(manifest_append(row("not_pdf", resolved = resolved,
                               http_code = as.character(code),
                               content_type = ctype %||% NA_character_,
                               bytes = as.character(length(body)))))
  }
  if (length(body) > MAX_BYTES) {
    return(manifest_append(row("too_big", resolved = resolved,
                               http_code = as.character(code),
                               bytes = as.character(length(body)))))
  }

  writeBin(body, dest)
  manifest_append(row("ok", resolved = resolved,
                      http_code = as.character(code),
                      content_type = ctype %||% NA_character_,
                      bytes = as.character(length(body)),
                      sha1 = key, path = dest))
}

#' Harvest, resumably.
#'
#' @param links output of build_document_index()
#' @param limit stop after N fetches. ALWAYS smoke-test with a small limit
#'   before committing to a multi-hour run.
#' @param retry_errors re-attempt rows that previously failed transiently.
harvest_documents <- function(links, limit = Inf, retry_errors = TRUE) {
  done <- manifest_read()

  if (nrow(done)) {
    skip <- done |>
      filter(status %in% TERMINAL | (!retry_errors)) |>
      pull(url) |>
      unique()
    before <- nrow(links)
    links <- filter(links, !url %in% skip)
    message(sprintf("manifest: %d already resolved, %d remaining (of %d)",
                    length(skip), nrow(links), before))
  }

  if (!nrow(links)) {
    message("nothing to do.")
    return(invisible(manifest_read()))
  }

  # Interleave domains so one slow host does not stall the whole queue and the
  # per-domain delays overlap productively.
  links <- links |> group_by(domain) |> mutate(.i = row_number()) |>
    ungroup() |> arrange(.i, domain) |> select(-.i)

  n <- min(nrow(links), limit)
  est <- sum(map_dbl(links$domain[seq_len(n)], .delay_for))
  message(sprintf("fetching %d documents; estimated floor %.1f hours of politeness delay",
                  n, est / 3600))

  pb <- txtProgressBar(min = 0, max = n, style = 3)
  for (i in seq_len(n)) {
    .fetch_one(links$iati_identifier[i], links$url[i])
    setTxtProgressBar(pb, i)
  }
  close(pb)

  message("done. harvest_report() for the summary.")
  invisible(manifest_read())
}

# ---- reporting --------------------------------------------------------------

harvest_report <- function() {
  m <- manifest_read()
  if (!nrow(m)) {
    message("manifest empty.")
    return(invisible(NULL))
  }

  cat("\n=== by status ===\n")
  m |> count(status, sort = TRUE) |> print(n = Inf)

  cat("\n=== by domain ===\n")
  # `ok` must not be reused as both the count and the base for the percentage:
  # summarise() would compute the mean of the sum it just created.
  m |>
    mutate(is_ok = status %in% c("ok", "cached")) |>
    group_by(domain) |>
    summarise(n = n(), n_ok = sum(is_ok), pct_ok = round(100 * mean(is_ok)),
              mb = round(sum(as.numeric(bytes), na.rm = TRUE) / 1024^2, 1),
              .groups = "drop") |>
    arrange(desc(n)) |> print(n = Inf)

  cat("\n=== coverage: activities with at least one PDF ===\n")
  m |>
    group_by(iati_identifier) |>
    summarise(got = any(status %in% c("ok", "cached")), .groups = "drop") |>
    summarise(activities = n(), with_pdf = sum(got),
              pct = round(100 * mean(got))) |> print()

  invisible(m)
}
