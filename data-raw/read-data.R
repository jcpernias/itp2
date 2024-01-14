library(tidyverse)
library(glue)

## Utility functions

data_file_path <- function(x) {
  file.path("./data-raw", x)
}


## Read data files

variables <- read_csv(data_file_path("variables.csv"), col_types = "ccc")
absent <- read_csv(data_file_path("absent.csv"), col_types = "ci")

read_ecv_file <- function(type, year, variables, absent) {
  file_name <- data_file_path(glue("esudb{year}{type}.csv.gz"))
  file_vars <- variables |> filter(file == .env$type)
  var_list <- as.list(file_vars$type) |> setNames(file_vars$name)
  var_list$.default = "-"
  excl_vars <- absent |> filter(year == as.integer(.env$year) + 2000) |>
    pull(var)
  exclude <- excl_vars %in% excl_vars
  read_csv(file_name, col_types = var_list[!exclude])
}


ecv_files <- expand_grid(type = c("d", "h", "p", "r"),
                         year = c("05", "11", "19"))

pwalk(ecv_files, \(type, year) {
  assign(glue("ecv{year}{type}"),
         read_ecv_file(type, year, variables, absent),
         envir = .GlobalEnv)
})


## Create factors

factors_db <- read_csv(data_file_path("factors.csv"), col_types = "cc")

make_factor_table <- function(factor_name, factors_db) {
  factor_type <- factors_db |>
    filter(factor == factor_name) |>
    pull(type)
  read_csv(data_file_path(glue("{factor_name}.csv")),
           col_types = glue("c{factor_type}"))
}

make_factor <- function(x, factor_table) {
  f <- as_tibble_col(x) |>
    left_join(factor_table, join_by(value == ecv)) |>
    pull(levels)
  factor(f, levels = factor_table$levels)
}


fin_hardship_tbl <- make_factor_table("fin_hardship", factors_db)

f <- make_factor(ecv05p |> pull(PM100), fin_hardship_tbl)

summary(f)
