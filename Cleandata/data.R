library(tidyverse)
library(janitor)
library(forcats)

raw_startups <- read_csv("Cleandata/global_startup_success_dataset.csv", show_col_types = FALSE) |>
  clean_names()

glimpse(raw_startups)

startups_clean <- raw_startups |>
  #we are making the yes or no consistent.
  mutate(
    ipo = str_to_lower(ipo),
    acquired = str_to_lower(acquired)
  ) |>
   mutate(
    success = if_else(ipo == "yes" | acquired == "yes", 1L, 0L) #l represents integer
  ) |> 
   filter(
    !is.na(founded_year),
    !is.na(country),
    !is.na(industry) #removing rows without these variables
  ) |>
  filter(
    founded_year >= 1980,
    founded_year <= as.integer(format(Sys.Date(), "%Y"))
  ) |>
   mutate(
    country = as.factor(country),
    industry = as.factor(industry),
    funding_stage = as.factor(funding_stage)
  ) |> #factor is easier to plot
   mutate(
    log_funding = log1p(total_funding_m),
    log_revenue = log1p(annual_revenue_m),
    log_employees = log1p(number_of_employees),
    log_followers = log1p(social_media_followers)
  ) |> #i tried plotting without log, didnt give good results, gemini helped a lot
  mutate(
    founded_decade = paste0(floor(founded_year / 10) * 10, "s")
  )

# 3) Quick check
glimpse(startups_clean)