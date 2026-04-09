load("data-raw/mw_level_0.rda")
load("data-raw/mw_level_1.rda")
load("data-raw/mw_level_2.rda")
load("data-raw/mw_level_3.rda")
load("data-raw/major_lakes.rda")
load("data-raw/malawi_data.rda")


usethis::use_data(
  mw_level_0, mw_level_1, mw_level_2, mw_level_3,
  major_lakes, malawi_data,
  overwrite = TRUE
)
