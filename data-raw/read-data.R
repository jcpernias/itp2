library(tidyverse)
library(glue)

variables <- read_csv("./data-raw/variables.csv", col_types = "ccc")
absent <- read_csv("./data-raw/absent.csv", col_types = "ci")

ecv_files <- expand_grid(type = c("d", "h", "p", "r"),
                         year = c("05", "11", "19"))

file_names <-  unlist(pmap(ecv_files, \(type, year) {
  file.path("./data-raw", glue("esudb{year}{type}.csv.gz"))
}))

## TODO: handle absent argument∫
read_ecv_file <- function(type, year, variables, absent) {
  file_name <- file.path("./data-raw",
                         glue("esudb{year}{type}.csv.gz"))
  file_vars <- variables |> filter(file == .env$type)
  var_list <- as.list(file_vars$type) |> setNames(file_vars$name)
  var_list$.default = "-"
  excl_vars <- absent |> filter(year == as.integer(.env$year) + 2000) |>
    pull(var)
  exclude <- excl_vars %in% excl_vars
  read_csv(file_name, col_types = var_list[!exclude])
}

db <- read_ecv_file("d", "05", variables, absent)

db <- read_ecv_file("p", "05", variables, absent)

absent |> filter(year == 2005) |> pull(var)
db |> colnames() %in% (absent |> filter(year == 2005) |> pull(var))
