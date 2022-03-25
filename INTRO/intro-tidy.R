## ---------------------------------------------------------
5 + 3


## ---------------------------------------------------------
5 - 3
5 / 3
5 ^ 3
5 * (10 - 3)
sqrt(4)


## ---- eval=FALSE------------------------------------------
## ## This is the start of an R Script
## ## The heading provides some information about the file
## 
## ## File name: testing_arithm.R
## ## Author: Kosuke Imai and Nora Webb Williams
## ## Purpose: Practicing basic math commands and commenting in R
## ##
## 
## 5 - 3 # What is 5 minus three?
## 5 / 3
## 5 ^ 3
## 5 * (10 - 3) # A bit more complex
## sqrt(4) # This function will take the square root of a number


## ----eval=FALSE-------------------------------------------
## install.packages("devtools") # install the package
## library(devtools) # load the package
## ## install a package from github
## devtools::install_github("kosukeimai/qss-package", build_vignettes = TRUE)
## ## You may need to allow R to update/install additional packages


## ---------------------------------------------------------
## load the qss package
library("qss")


## ---- message=FALSE, warning=FALSE------------------------
library(tidyverse) #if this command does not work, remember to install the package


## ---------------------------------------------------------
result <- 5 + 3
result
print(result)


## ---------------------------------------------------------
result <- 5 - 3
result


## ----eval=TRUE, error=TRUE--------------------------------
Result


## ---------------------------------------------------------
kosuke <- "instructor"
kosuke


## ---------------------------------------------------------
kosuke <- "instructor and author"
kosuke


## ---------------------------------------------------------
Result <- "5"
Result


## ----error=TRUE-------------------------------------------
Result / 3
sqrt(Result)


## ---------------------------------------------------------
result
class(result)
Result
class(Result)
class(sqrt)


## ---------------------------------------------------------
world.pop <- c(2525779, 3026003, 3691173, 4449049, 5320817, 6127700, 6916183)
world.pop




## ---------------------------------------------------------
pop.first <- c(2525779, 3026003, 3691173)
pop.second <- c(4449049, 5320817, 6127700, 6916183)
pop.all <- c(pop.first, pop.second)
pop.all


## ---------------------------------------------------------
world.pop[2]
world.pop[c(2, 4)]
world.pop[c(4, 2)]
world.pop[-3]


## ---------------------------------------------------------
pop.million <- world.pop / 1000
pop.million


## ---------------------------------------------------------
pop.rate <- world.pop / world.pop[1]
pop.rate


## ---------------------------------------------------------
pop.increase <- world.pop[-1] - world.pop[-7]
percent.increase <- (pop.increase / world.pop[-7]) * 100
percent.increase


## ---------------------------------------------------------
percent.increase[c(1, 2)] <- c(20, 22)
percent.increase


## ---------------------------------------------------------
length(world.pop)
min(world.pop)
max(world.pop)
range(world.pop)
mean(world.pop)
sum(world.pop) / length(world.pop)


## ---------------------------------------------------------
year <- seq(from = 1950, to = 2010, by = 10)
year


## ---------------------------------------------------------
seq(to = 2010, by = 10, from = 1950)


## ---------------------------------------------------------
seq(from = 2010, to = 1950, by = -10)
2008:2012
2012:2008


## ---------------------------------------------------------
names(world.pop)
names(world.pop) <- year
names(world.pop)
world.pop


## ----eval = FALSE-----------------------------------------
## myfunction <- function(input1, input2, ..., inputN) {
## 
##     DEFINE 'output' USING INPUTS
## 
##     return(output)
## }


## ---------------------------------------------------------
my.summary <- function(x){ # function takes one input, x
  s.out <- sum(x)
  l.out <- length(x)
  m.out <- s.out / l.out
  out <- c(s.out, l.out, m.out) # define the output
  names(out) <- c("sum", "length", "mean") # add labels
  return(out) # end function by calling output
}
z <- 1:10 # z is a vector from 1 to 10
my.summary(z) # run my.summary function on z
my.summary(world.pop) # run my.summary function on world.pop


## ---- eval=FALSE------------------------------------------
## getwd() # Check what your current working directory is
## setwd("qss/INTRO") # Set your working directory with a path
## getwd() # Check that you changed your working directory


## ---- eval=FALSE------------------------------------------
## ## If your working directory is where the .csv file is stored
## UNpop <- read_csv("UNpop.csv")
## class(UNpop) # What type of object is UNpop?


## ---- eval= FALSE-----------------------------------------
## load("UNpop.RData")


## ----message=FALSE, warning=FALSE-------------------------
## Specifying a relative path to find and read in UNpop.csv
## Will overwrite previously loaded UNpop object
UNpop <- read_csv("INTRO/UNpop.csv")
class(UNpop) # what type of object is UNpop?


## ---------------------------------------------------------
## Load the package
library(qss)
## Load the UN pop data
## Will overwrite previously loaded UNpop object
data(UNpop, package = "qss")


## ---------------------------------------------------------
names(UNpop)
nrow(UNpop)
ncol(UNpop)
dim(UNpop)


## ---------------------------------------------------------
UNpop$world.pop


## ---------------------------------------------------------
## subset all rows for the column called "world.pop" from the UNpop data
UNpop[, "world.pop"]
## subset the first three rows (and all columns)
UNpop[c(1, 2, 3),]
## subset the first three rows of the "year" column
UNpop[1:3, "year"]


## ---------------------------------------------------------
## Subset the first three rows of UNpop with tidyverse
slice(UNpop, n = 1:3)


## ---------------------------------------------------------
## Extract/subset the world.pop variable (column)
select(UNpop, world.pop)


## ---------------------------------------------------------
## Base R subset the first three rows of the year variable
UNpop[1:3, "year"]
## or in tidyverse, combining slice() and select()
select(slice(UNpop, 1:3), year)


## ---------------------------------------------------------
UNpop %>% # take the UNpop data we have loaded, and then...
  slice(1:3) %>% # subset the first three rows, and then...
  select(year) # subset the year column


## ---------------------------------------------------------
UNpop %>%
  slice(seq(1, n(), by = 2)) %>% # using a sequence from 1 to n()
  select(world.pop)


## ---------------------------------------------------------
UNpop %>%
  filter(row_number() %% 2 == 1) %>%
  select(world.pop)


## ---------------------------------------------------------
pop.1970 <- UNpop %>% # take the UNpop data and then....
  filter(year == 1970) %>% # subset rows where the year variable is equal to 1970
  select(world.pop) %>% # subset just the world.pop column
  pull() # return a vector, not a tibble

## Print the vector to the console to see it
print(pop.1970)


## ---------------------------------------------------------
UNpop.mill <- UNpop %>% # create a new tibble from UNpop
  mutate(world.pop.mill = world.pop / 1000) %>% # create a new variable, world.pop.mill
  select(-world.pop) # drop the original world.pop column


## ---------------------------------------------------------
## Adding a nonsense variable to the UNpop.mill data
UNpop.mill <- UNpop.mill %>%
  mutate(nonsense.var = world.pop.mill / year)


## ---------------------------------------------------------
## Adding a variable with if_else
UNpop.mill <- UNpop.mill %>%
  mutate(after.1980 = if_else(year >= 1980, 1, 0))


## ---------------------------------------------------------
## Creating a vector of the years of interest
specific.years <- c(1950, 1980, 2000)

## Adding a variable with if_else and %in%
UNpop.mill <- UNpop.mill %>%
  mutate(year.of.interest = if_else(year %in% specific.years, 1, 0))


## ---------------------------------------------------------
summary(UNpop.mill)
mean(UNpop.mill$world.pop.mill)


## ---------------------------------------------------------
## Add a row where values for all columns is NA
UNpop.mill.wNAs <- UNpop.mill %>%
  add_row(year = NA, world.pop.mill = NA,
          nonsense.var = NA, after.1980 = NA,
          year.of.interest = NA)
## Take the mean of world.pop.mill (returns NA)
mean(UNpop.mill.wNAs$world.pop.mill)
## Take the mean of world.pop.mill (ignores the NA)
mean(UNpop.mill.wNAs$world.pop.mill, na.rm = TRUE)


## ---------------------------------------------------------
UNpop.mill %>%
  summarize(mean.pop = mean(world.pop.mill),
            median.pop = median(world.pop.mill))


## ---- message = FALSE, warning=FALSE----------------------
UNpop.mill %>%
  group_by(after.1980) %>% # create subset group for each value of after.1980
  summarize(mean.pop = mean(world.pop.mill)) # calculate mean for each group


## ---- eval=FALSE------------------------------------------
## save.image("qss/INTRO/Chapter1.RData")


## ---- eval=FALSE------------------------------------------
## save(UNpop, file = "Chapter1.RData")
## save(world.pop, year, file = "qss/INTRO/Chapter1.RData")


## ---- eval=FALSE------------------------------------------
## load("Chapter1.RData")


## ---- eval=FALSE------------------------------------------
## write_csv(UNpop, path = "INTRO/UNpop.csv")


## ---- eval=FALSE, tidy=TRUE-------------------------------
## ## install packages -- note the syntax for multiple packages at once
## install.packages(c("foreign", "haven", "rio"))


## ---- warning=FALSE, eval = FALSE-------------------------
## library("foreign") # load package
## library("haven")
## library("rio")


## ---- eval=FALSE------------------------------------------
## read.dta("UNpop.dta")
## read.spss("UNpop.sav")


## ----eval=FALSE-------------------------------------------
## UNpop_dta_url <- "https://github.com/kosukeimai/qss/raw/master/INTRO/UNpop.dta"
## 
## UNpop <- read_dta(UNpop_dta_url)
## 
## ## reading in with import; note that each UNpop <- will override the prior object
## UNpop <- import("https://github.com/kosukeimai/qss/raw/master/INTRO/UNpop.csv")
## 
## UNpop <- import("https://github.com/kosukeimai/qss/raw/master/INTRO/UNpop.RData")
## 
## UNpop <- import("https://github.com/kosukeimai/qss/raw/master/INTRO/UNpop.dta")


## ---- eval=FALSE------------------------------------------
## write.dta(UNpop, file = "UNpop.dta")
## write_dta(UNpop, "UNpop.dta")


## ---- eval=FALSE------------------------------------------
## ##
## ## File: UNpop.R
## ## Author: Kosuke Imai and Nora Webb Williams
## ## The code loads the UN population data, adds a variable,
## ## and saves the data as a STATA file
## ##
## 
## ## Load the necessary packages
## library(haven)
## library(tidyverse)
## library(qss)
## 
## ## Load the UN pop data
## data(UNpop, package = "qss")
## 
## ## Replace the raw population with the population in millions
## UNpop <- UNpop %>%
##   mutate(world.pop = world.pop / 1000 )
## 
## ## Save the data as a .dta file
## write_dta(UNpop, path = "UNpop.dta")


## ---- eval=FALSE------------------------------------------
## source("UNpop.R")

