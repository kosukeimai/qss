##
## File: UNpop.R
## Author: Kosuke Imai
## The code loads the UN population data and saves it as a STATA file
##
library(foreign)
UNpop <- read.csv("UNpop.csv")
UNpop$world.pop <- UNpop$world.pop / 1000 # population in millions 
write.dta(UNpop, file = "UNpop.dta")
