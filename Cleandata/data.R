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
    founded_decade = paste0(floor(founded_year / 10) * 10, "s") #results come in eg, 2000s, 2020s
  )

# 3) Quick check
glimpse(startups_clean)

#plots

startups_clean |>
  mutate(
    ipo = factor(ipo, levels = c("no", "yes")),
    acquired = factor(acquired, levels = c("no", "yes"))
  ) |>
  pivot_longer(c(ipo, acquired), names_to = "outcome", values_to = "value") |>
  ggplot(aes(x = value)) +
  geom_bar() +
  facet_wrap(~ outcome) +
  theme_minimal() +
  labs(title = "IPO and Acquisition Counts", x = NULL, y = "Count")

#plot2
startups_clean |>
  group_by(industry) |>
  summarise(
    n = n(),
    success_rate = mean(success),
  ) |>
  filter(n >= 20) |>
  arrange(desc(success_rate)) |>
  slice_head(n = 15) |>
  ggplot(aes(x = reorder(industry, success_rate), y = success_rate)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(title = "Success Rate by Industry (Top 15)", x = NULL, y = "Success Rate")

#plot3
startups_clean |>
  group_by(country) |>
  summarise(
    n = n(),
    success_rate = mean(success),
  ) |>
  filter(n >= 20) |>
  arrange(desc(success_rate)) |>
  slice_head(n = 15) |>
  ggplot(aes(x = reorder(country, success_rate), y = success_rate)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(title = "Success Rate by Country (Top 15)", x = NULL, y = "Success Rate")

#plot4
startups_clean |>
  group_by(funding_stage) |>
  summarise(
    n = n(),  # not applicable. but i just wanted to see it. All of the companies have already succeded based on my defination of success
    success_rate = mean(success),
  ) |>
  filter(n >= 20) |>
  ggplot(aes(x = reorder(funding_stage, success_rate), y = success_rate)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(title = "Success Rate by Funding Stage", x = NULL, y = "Success Rate")

#plot5
startups_clean |>
  group_by(founded_year) |>
  summarise(
    n = n(),
    success_rate = mean(success),
  ) |>
  filter(n >= 20) |>
  ggplot(aes(x = founded_year, y = success_rate)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Success Rate Over Time (Founded Year)", x = "Founded Year", y = "Success Rate")


#plot6
startups_clean |>
  ggplot(aes(x = log_funding, y = success)) +
  geom_jitter(height = 0.05, alpha = 0.2) +
  theme_minimal() +
  labs(title = "Funding vs Success", x = "log(1 + total funding in millions)", y = "Success (0/1)")

#plot7


startups_clean |>
  ggplot(aes(x = log_employees, y = success)) +
  geom_jitter(height = 0.05, alpha = 0.2) +
  theme_minimal() +
  labs(title = "Employees vs Success", x = "log(1 + employees)", y = "Success (0/1)")


#plot8

startups_clean |>
  ggplot(aes(x = log_followers, y = success)) +
  geom_jitter(height = 0.05, alpha = 0.2) +
  theme_minimal() +
  labs(title = "Social Media Followers vs Success", x = "log(1 + followers)", y = "Success (0/1)")


#SHINY

library(shiny)
library(bslib)
library(shinycssloaders)

ui <- fluidPage
  titlePanel("Startup Success Dashboard"),

