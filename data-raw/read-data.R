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
  factor(f, levels = unique(factor_table$levels))
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

factors_db <- read_csv(data_file_path("factors.csv"), col_types = "cc")
fin_hardship_tbl <- make_factor_table("fin_hardship", factors_db)
region_tbl <- make_factor_table("region", factors_db)
urb_tbl <- make_factor_table("urb", factors_db)
type_hh_tbl <- make_factor_table("type_hh", factors_db)
sex_tbl <- make_factor_table("sex", factors_db)
nation_tbl <- make_factor_table("nation", factors_db)
well_being_tbl <- make_factor_table("well_being", factors_db)
fin_hardship_tbl <- make_factor_table("fin_hardship", factors_db)


households <- ecv_hh |>
  transmute(id_hh = as.integer((DB010 - 2000) * 10000000 + DB030),
            ecv_year = factor(DB010, levels = c(2005, 2011, 2019)),
            region = make_factor(DB040, region_tbl),
            urb = make_factor(DB100, urb_tbl),
            size_hh = as.integer(HX040),
            cunits = HX240,
            type_hh = make_factor(HX060, type_hh_tbl),
            ydisp_hh = vhRentaa,
            pov_hh = vhPobreza == 1,
            depriv_hh = vhMATDEP == 1)

individuals <- ecv_pp |>
  transmute(id_hh = as.integer((RB010 - 2000) * 10000000 + trunc(RB030 / 100)),
            id_p = as.integer((RB010 - 2000) * 10000000 + RB030),
            ecv_year = factor(RB010, levels = c(2005, 2011, 2019)),
            birth_year = as.integer(RB080),
            age = as.integer(RB010 - RB080 - 1),
            sex = make_factor(RB090, sex_tbl),
            absent = RB200 == 2,
            lowjob = vrLOWJOB == 1,
            eu2020 = vrEU2020 == 1,
            country_birth = make_factor(PB210, nation_tbl),
            nationality = make_factor(PB220A, nation_tbl),
            educ_year = as.integer(PE030),
            educ_none = PE040_F == 2 | PE040 == 0,
            educ_prim = PE040 %in% c(0, 1, 100),
            educ_sec = PE040 %in% c(2, 3, 200, 300, 344, 353, 354),
            educ_sup = PE040 %in% c(4, 5, 400, 450, 500),
            work_age = PL190,
            work_years = PL200,
            full_time = PL031 %in% c(1, 3) | PL030 == 1,
            part_time = PL031 %in% c(2, 4) | PL030 == 2,
            self = PL040 %in% c(1, 2),
            worker = PL040 == 3,
            unempl = PL031 == 5 | PL030 == 3,
            inactive = PL031 %in% 6:11 | PL030 %in% 4:9,
            has_worked = PL015 == 1,
  )

pov_transition <- ecv_pp |>
  transmute(id_p = as.integer((RB010 - 2000) * 10000000 + RB030),
            ecv_year = factor(RB010, levels = c(2005, 2011, 2019)),
            age = as.integer(RB010 - RB080 - 1),
            institution = case_when(ecv_year == 2019 ~ PT220 == 2,
                                    ecv_year == 2011 ~ PT010 == 5,
                                    ecv_year == 2005 ~ PM010 == 7),
            adults = as.integer(PT020),
            children = as.integer(PT030),
            siblings = as.integer(PM035),
            nworking = as.integer(PT040),
            fabsent = case_when(ecv_year == 2019 ~ PT240 != 1,
                                ecv_year == 2011 ~ !(PT010 %in% c(1, 2)),
                                ecv_year == 2005 ~ !(PM010 %in% c(1, 3, 5))),
            fcountry = make_factor(PT060, nation_tbl),
            fnation = make_factor(PT070, nation_tbl),
            mabsent = case_when(ecv_year == 2019 ~ PT230 != 1,
                                ecv_year == 2011 ~ !(PT010 %in% c(1, 3)),
                                ecv_year == 2005 ~ !(PM010 %in% c(1, 2, 4))),
            mcountry = make_factor(PT090, nation_tbl),
            mnation = make_factor(PT100, nation_tbl),
            feduc =
              factor(case_when(PT110 <= 1 | PM040 <= 2 ~ "Low",
                               PT110 == 2 | PM040 == 3 ~ "Med",
                               PT110 == 3 | PM040 >= 3 ~ "High"),
                     levels = c("Low", "Med", "High")),
            meduc =
              factor(case_when(PT120 <= 1 | PM050 <= 2 ~ "Low",
                               PT120 == 2 | PM050 == 3 ~ "Med",
                               PT120 == 3 | PM050 >= 3 ~ "High"),
                     levels = c("Low", "Med", "High")),
            fself = case_when(ecv_year == 2019 ~ PT130 == 3,
                              ecv_year == 2011 ~ PT130 == 2,
                              ecv_year == 2005 ~ PM060 %in% c(2, 3)),
            funempl = case_when(ecv_year == 2019 ~ PT130 == 4,
                                ecv_year == 2011 ~ PT130 == 3,
                                ecv_year == 2005 ~ PM060 == 4),
            finactive = case_when(ecv_year == 2019 ~ PT130 %in% 6:8,
                                  ecv_year == 2011 ~ PT130 %in% 5:6,
                                  ecv_year == 2005 ~ PM060 %in% 5:7),
            mself = case_when(ecv_year == 2019 ~ PT160 == 3,
                              ecv_year == 2011 ~ PT160 == 2,
                              ecv_year == 2005 ~ PM080 %in% c(2, 3)),
            munempl = case_when(ecv_year == 2019 ~ PT160 == 4,
                                ecv_year == 2011 ~ PT160 == 3,
                                ecv_year == 2005 ~ PM080 == 4),
            minactive = case_when(ecv_year == 2019 ~ PT160 %in% 6:8,
                                  ecv_year == 2011 ~ PT160 %in% 5:6,
                                  ecv_year == 2005 ~ PM080 %in% 5:7),
            well_being = make_factor(PT190, well_being_tbl),
            fin_hardship = make_factor(PM100, fin_hardship_tbl))

save(households, file = "./data/households.RData", compress = "xz")
save(individuals, file = "./data/individuals.RData", compress = "xz")
save(pov_transition, file = "./data/pov_transition.RData", compress = "xz")
