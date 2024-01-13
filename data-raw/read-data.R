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
  read_csv(file_name, col_types = var_list)
}

db <- read_ecv_file("d", "05", variables, absent)
