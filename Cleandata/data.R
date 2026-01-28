library(tidyverse)
library(janitor)
library(forcats)

raw_startups <- read_csv("Cleandata/global_startup_success_dataset.csv", show_col_types = FALSE) |>
  clean_names()

glimpse(raw_startups)

pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

# Map columns that exist in YOUR dataset
col_status   <- pick_col(raw_startups, c("status", "startup_status", "company_status", "market_status"))
col_year     <- pick_col(raw_startups, c("founded_year", "year_founded", "founded", "founding_year", "start_year"))
col_funding  <- pick_col(raw_startups, c("funding_total_usd", "total_funding_usd", "funding_usd", "funding", "total_funding"))
col_emp      <- pick_col(raw_startups, c("employees", "employee_count", "num_employees"))
col_category <- pick_col(raw_startups, c("category", "category_list", "startup_category", "categories"))
col_country  <- pick_col(raw_startups, c("country", "country_code", "country_name", "location_country"))

# Industry-like column (optional)
col_industry <- pick_col(raw_startups, c("industry", "industry_type", "sector", "market"))
# If none exists, fall back to category later

startups_clean <- raw_startups |>
  mutate(
    status = if (!is.na(col_status)) tolower(as.character(.data[[col_status]])) else NA_character_,
    founded_year = if (!is.na(col_year)) as.integer(.data[[col_year]]) else NA_integer_,
    funding_total_usd = if (!is.na(col_funding)) suppressWarnings(as.numeric(.data[[col_funding]])) else NA_real_,
    employees = if (!is.na(col_emp)) suppressWarnings(as.numeric(.data[[col_emp]])) else NA_real_,
    category = if (!is.na(col_category)) as.character(.data[[col_category]]) else NA_character_,
    country  = if (!is.na(col_country)) as.character(.data[[col_country]]) else NA_character_,
    industry = if (!is.na(col_industry)) as.character(.data[[col_industry]]) else category
  ) |>
  mutate(
    success = if_else(!is.na(status) & status %in% c("ipo", "acquired"), 1L, 0L)
  ) |>
  filter(
    !is.na(founded_year),
    founded_year >= 1980,
    founded_year <= as.integer(format(Sys.Date(), "%Y"))
  ) |>
  mutate(
    category = fct_lump_n(as.factor(category), n = 15, other_level = "Other"),
    industry = fct_lump_n(as.factor(industry), n = 15, other_level = "Other"),
    country  = fct_lump_n(as.factor(country),  n = 15, other_level = "Other"),
    status   = as.factor(status),
    founded_decade = paste0(floor(founded_year / 10) * 10, "s"),
    log_funding = log1p(funding_total_usd),
    log_employees = log1p(employees)
  )

glimpse(startups_clean)


