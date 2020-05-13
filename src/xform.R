suppressPackageStartupMessages(library(tidyverse))

urls <- list(
  cases = "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv",
  deaths = "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv"
)

dc <- suppressMessages(readr::read_csv(urls$cases))
dd <- suppressMessages(readr::read_csv(urls$deaths))

# need custom parser for date format
fix_date <- function(dt) {
  as.Date(sapply(strsplit(dt, "/"), function(x) {
    x[3] <- paste0("20", x[3])
    paste(sprintf("%02d", as.integer(x[c(3, 1, 2)])), collapse = "-")
  }))
}

# pivot from wide to long, fix date and names
process <- function(x, name) {
  rnm <- function(x)
    gsub("\\/", "_", tolower(x))

  x %>%
    # filter(countyFIPS >= 1000) %>%
    tidyr::pivot_longer(
      cols = ends_with("20"),
      names_to = "date",
      values_to = name) %>%
    dplyr::rename_all(rnm) %>%
    dplyr::mutate(
      date = fix_date(date),
      admin0_code = "US",
      admin2_code = sprintf("%05d", countyfips),
      admin1_code = sprintf("%02d", statefips)) %>%
      select(one_of(
        c("admin0_code", "admin1_code", "admin2_code", "date", name)))
}

dc2 <- process(dc, "cases")
dd2 <- process(dd, "deaths")

tmp <- left_join(dc2, dd2, by = c("admin0_code", "admin1_code",
  "admin2_code", "date"))

state <- tmp %>%
  group_by(admin0_code, admin1_code, date) %>%
  summarise(
    cases = sum(cases, na.rm = TRUE),
    deaths = sum(deaths, na.rm = TRUE)) %>%
  group_by(admin1_code) %>%
  mutate(all_zero = all(cases == 0)) %>%
  filter(!all_zero) %>%
  mutate(min_zero_date = min(date[cases > 0])) %>%
  filter(date >= min_zero_date) %>%
  dplyr::select(-all_zero, -min_zero_date)

county <- tmp %>%
  filter(substr(admin2_code, 1, 2) != "00") %>%
  group_by(admin2_code) %>%
  mutate(all_zero = all(cases == 0)) %>%
  filter(!all_zero) %>%
  mutate(min_zero_date = min(date[cases > 0])) %>%
  filter(date >= min_zero_date) %>%
  dplyr::select(-all_zero, -min_zero_date)

readr::write_csv(county, "output/admin2_US.csv")
readr::write_csv(state, "output/admin1_US.csv")
