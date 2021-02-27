suppressPackageStartupMessages(library(tidyverse))

dir.create("output", showWarning = FALSE)
dir.create("output/admin0", showWarning = FALSE)
dir.create("output/admin1", showWarning = FALSE)
dir.create("output/admin2", showWarning = FALSE)

urls <- list(
  cases = "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv",
  deaths = "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv"
)

dc <- suppressMessages(readr::read_csv(urls$cases))
dd <- suppressMessages(readr::read_csv(urls$deaths))

# pivot from wide to long, fix date and names
process <- function(x, name) {
  rnm <- function(x)
    gsub("\\/", "_", tolower(x))

  x %>%
    # filter(countyFIPS >= 1000) %>%
    tidyr::pivot_longer(
      cols = starts_with(c("2020", "2021")),
      names_to = "date",
      values_to = name) %>%
    dplyr::rename_all(rnm) %>%
    dplyr::mutate(
      date = as.Date(date),
      admin0_code = "US",
      admin2_code = sprintf("%05d", countyfips),
      admin1_code = statefips) %>%
    dplyr::select(dplyr::one_of(
      c("admin0_code", "admin1_code", "admin2_code", "date", name)))
}

dc2 <- process(dc, "cases")
dd2 <- process(dd, "deaths")

message("Most recent date: ", max(dc2$date))

tmp <- dplyr::left_join(dc2, dd2, by = c("admin0_code", "admin1_code",
  "admin2_code", "date"))

state <- tmp %>%
  dplyr::group_by(admin0_code, admin1_code, date) %>%
  dplyr::summarise(
    cases = sum(cases, na.rm = TRUE),
    deaths = sum(deaths, na.rm = TRUE)) %>%
  dplyr::group_by(admin1_code) %>%
  dplyr::mutate(all_zero = all(cases == 0)) %>%
  dplyr::filter(!all_zero) %>%
  dplyr::mutate(min_zero_date = min(date[cases > 0])) %>%
  dplyr::filter(date >= min_zero_date) %>%
  dplyr::select(-all_zero, -min_zero_date)

county <- tmp %>%
  dplyr::filter(substr(admin2_code, 1, 2) != "00") %>%
  dplyr::group_by(admin2_code) %>%
  dplyr::mutate(all_zero = all(cases == 0)) %>%
  dplyr::filter(!all_zero) %>%
  dplyr::mutate(min_zero_date = min(date[cases > 0])) %>%
  dplyr::filter(date >= min_zero_date) %>%
  dplyr::select(-all_zero, -min_zero_date)

country <- tmp %>%
  dplyr::group_by(admin0_code, date) %>%
  dplyr::summarise(
    cases = sum(cases, na.rm = TRUE),
    deaths = sum(deaths, na.rm = TRUE))

readr::write_csv(county, "output/admin2/US.csv")
readr::write_csv(state, "output/admin1/US.csv")
readr::write_csv(country, "output/admin0/US.csv")
