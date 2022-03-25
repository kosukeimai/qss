##
## File: UNpop.R
## Author: Kosuke Imai and Nora Webb Williams
## The code loads the UN population data, adds a variable,
## and saves the data as a STATA file
##

## Load the necessary packages
library(haven)
library(tidyverse)
library(qss)

## Load the UN pop data
data(UNpop, package = "qss")

## Replace the raw population with the population in millions
UNpop <- UNpop %>%
  mutate(world.pop = world.pop / 1000 )

## Save the data as a .dta file
write_dta(UNpop, path = "UNpop.dta")