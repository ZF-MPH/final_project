# Final Project in EDUC 642
library(here)
library(tidyverse)
library(janitor)
library(rio)

demographics <- import(here("data", "Teen-Demographic-SPSS(1) copy.zip"))
PA <- import(here("data", "Teen-PA-SPSS(1) copy.zip"))

as_tibble(demographics)
as_tibble(PA)

convert(here("data", demographics.zip)),
  (here("data", demographics.sav))
export(demographics, here("data", "Teen-Demographic-SPSS(1) copy.sav"))
export(PA, here("data", "Teen-PA-SPSS(1) copy.sav"))
