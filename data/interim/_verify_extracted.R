## -----------------------------------------------------------------------------
library(tidyverse)

read_gbs <- function(name, ...) read_csv(file.path("../data/interim", name), ...)


## -----------------------------------------------------------------------------
frame_raw <- read_gbs("gbs_frame.csv", col_types = cols(
  iatiidentifier = col_character(),
  reportingorg_ref = col_character(),
  reportingorg_type = col_character(),
  reportingorg_narrative = col_character(),
  activitystatus_code = col_character(),
  has_a01 = col_logical(),
  has_a02 = col_logical(),
  declared_activity_level = col_logical(),
  declared_transaction_level = col_logical(),
  .default = col_guess()
))

# org_type 15 ("other public sector") sits with the multilaterals: it covers
# entities like the Green Climate Fund, not bilateral government agencies
#.
x1_lookup <- c("10" = "Bilateral", "15" = "Multilateral", "40" = "Multilateral")

status_lookup <- c(
  "1" = "Pipeline/identification", "2" = "Implementation",
  "3" = "Finalisation", "4" = "Closed", "5" = "Cancelled"
)

frame <- frame_raw |>
  transmute(
    iatiidentifier,
    donor_ref = reportingorg_ref,
    donor     = reportingorg_narrative,
    org_type  = reportingorg_type,
    x1_donor_type = x1_lookup[org_type],
    a01 = as.integer(has_a01), a02 = as.integer(has_a02),
    # A02-only is "sector budget support"; A01-only "general"; both is rare
    # enough (17 activities corpus-wide) to keep as its own label rather than
    # force a choice.
    modality = case_when(
      a01 == 1 & a02 == 1 ~ "both",
      a02 == 1             ~ "A02_sector",
      a01 == 1             ~ "A01_general"
    ),
    declared_activity    = as.integer(declared_activity_level),
    declared_transaction = as.integer(declared_transaction_level),
    status_code = activitystatus_code,
    status_name = status_lookup[status_code]
  )

frame |> count(x1_donor_type, modality)


## -----------------------------------------------------------------------------
recipients <- read_gbs("gbs_recipients.csv", col_types = cols(
  iatiidentifier = col_character(), country = col_character(),
  percentage = col_double()
))

recipient_main <- recipients |>
  # A missing percentage cannot win a max() comparison; treating it as 0
  # rather than dropping the row keeps single-country activities in scope
  # even when the percentage element was left blank.
  mutate(percentage = replace_na(percentage, 0)) |>
  slice_max(percentage, n = 1, by = iatiidentifier, with_ties = FALSE) |>
  select(iatiidentifier, country)


## -----------------------------------------------------------------------------
dates <- read_gbs("gbs_dates.csv", col_types = cols(
  iatiidentifier = col_character(), date_type = col_character(),
  isodate = col_character()
)) |>
  filter(!is.na(date_type)) |>
  distinct(iatiidentifier, date_type, .keep_all = TRUE) |>
  pivot_wider(names_from = date_type, values_from = isodate)

# Early closure: the convergent-validity check for the financial gap.
# A positive value means the operation concluded before its announced date.
dates <- dates |>
  mutate(
    across(c(planned_start, actual_start, planned_end, actual_end), as.Date),
    y2_early_days = as.integer(planned_end - actual_end),
    planned_duration_days = as.integer(planned_end - planned_start)
  )


## -----------------------------------------------------------------------------
budget_tot <- read_gbs("gbs_budget.csv", col_types = cols(
  iatiidentifier = col_character(), budget_orig = col_double(),
  n_budget_lines = col_integer(), budget_ccy = col_character()
))


## -----------------------------------------------------------------------------
tx <- read_gbs("gbs_tx.csv", col_types = cols(
  iatiidentifier = col_character(), ttype = col_character(),
  year = col_character(), tdate = col_character(), usd = col_double()
))

panel_year <- tx |>
  summarise(
    commitment_usd   = sum(usd[ttype == "2"], na.rm = TRUE),
    disbursement_usd = sum(usd[ttype == "3"], na.rm = TRUE),
    n_disb_tx        = sum(ttype == "3"),
    .by = c(iatiidentifier, year)
  )

activity_tx <- panel_year |>
  summarise(
    commitment_usd   = sum(commitment_usd),
    disbursement_usd = sum(disbursement_usd),
    n_disb_years      = sum(disbursement_usd > 0),
    first_disb_year   = min(year[disbursement_usd > 0]),
    last_disb_year    = max(year[disbursement_usd > 0]),
    .by = iatiidentifier
  )

lifecycle <- read_gbs("gbs_lifecycle.csv", col_types = cols(
  iatiidentifier = col_character(), commitment_usd_full = col_double(),
  disbursement_usd_full = col_double(), n_disb_years_full = col_integer()
))


## -----------------------------------------------------------------------------
controls <- read_gbs("gbs_controls.csv", col_types = cols(
  iatiidentifier = col_character(), finance_type = col_character(),
  flow_type = col_character(), tied_status = col_character(),
  collaboration_type = col_character(), hierarchy = col_character()
))


## -----------------------------------------------------------------------------
analysis <- frame |>
  left_join(recipient_main |> rename(recipient_main = country), by = "iatiidentifier") |>
  left_join(activity_tx,  by = "iatiidentifier") |>
  left_join(dates,        by = "iatiidentifier") |>
  left_join(budget_tot,   by = "iatiidentifier") |>
  left_join(lifecycle,    by = "iatiidentifier") |>
  left_join(controls,     by = "iatiidentifier") |>
  mutate(
    cp_eligible   = as.integer(modality == "A01_general"),
    complete_flag = as.integer(status_code %in% c("3", "4")),
    y1_gap        = if_else(commitment_usd > 0, 1 - disbursement_usd / commitment_usd, NA_real_),
    y1_gap_full   = if_else(commitment_usd_full > 0,
                             1 - disbursement_usd_full / commitment_usd_full, NA_real_)
  )

nrow(analysis)
analysis |> filter(complete_flag == 1, !is.na(y1_gap_full)) |>
  summarise(n = n(), n_donors = n_distinct(donor_ref))


## -----------------------------------------------------------------------------
write_csv(analysis,       "../data/interim/analysis.csv")
write_csv(panel_year,     "../data/interim/panel_year.csv")
write_csv(recipient_main, "../data/interim/recipient_main.csv")

