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

## Fix PE040 in 2019 database
##
ecv19p$PE040 <- as.integer(ecv19p$PE040)

## Join databases
##
ecv_all_d <- bind_rows(ecv05d, ecv11d, ecv19d)
ecv_all_h <- bind_rows(ecv05h, ecv11h, ecv19h)
ecv_all_r <- bind_rows(ecv05r, ecv11r, ecv19r)
ecv_all_p <- bind_rows(ecv05p, ecv11p, ecv19p)


ecv_hh <- left_join(ecv_all_d, ecv_all_h,
                    by = join_by(DB030 == HB030, DB010 == HB010))
ecv_pp <- left_join(ecv_all_r, ecv_all_p,
                    by = join_by(RB030 == PB030, RB010 == PB010))


## Final databases
##

fin_hardship_tbl <- make_factor_table("fin_hardship", factors_db)
region_tbl <- make_factor_table("region", factors_db)
urb_tbl <- make_factor_table("urb", factors_db)
type_hh_tbl <- make_factor_table("type_hh", factors_db)



households <- ecv_hh |>
  transmute(id_hh = as.integer((DB010 - 2000) * 10000000 + DB030),
            ecv_year = factor(DB010, levels = c(2005, 2011, 2019)),
            region = make_factor(DB040, region_tbl),
            urb = make_factor(DB100, urb_tbl),
            size_hh = HX040,
            cunits = HX240,
            type_hh = make_factor(HX060, type_hh_tbl),
            ydisp_hh = vhRentaa,
            pov_hh = vhPobreza == 1,
            depriv_hh = vhMATDEP == 1)
