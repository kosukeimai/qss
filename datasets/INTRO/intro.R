## ---- eval = FALSE-------------------------------------------------------
## install.packages("swirl") # install the package
## library(swirl) # load the package
## install_course_github("kosukeimai", "qss-swirl") # install the course
## library(swirl)
## swirl()

## ------------------------------------------------------------------------
5 + 3
5 - 3
5 / 3
5 ^ 3
5 * (10 - 3)
sqrt(4)

## ------------------------------------------------------------------------
result <- 5 + 3
result
print(result)
result <- 5 - 3
result

kosuke <- "instructor"
kosuke
kosuke <- "instructor and author"
kosuke

Result <- "5"
Result
result

class(result)
Result
class(Result)
class(sqrt)

## ------------------------------------------------------------------------
world.pop <- c(2525779, 3026003, 3691173, 4449049, 5320817, 6127700, 6916183)
world.pop

pop.first <- c(2525779, 3026003, 3691173)
pop.second <- c(4449049, 5320817, 6127700, 6916183)
pop.all <- c(pop.first, pop.second)
pop.all

world.pop[2]
world.pop[c(2, 4)] 
world.pop[c(4, 2)] 
world.pop[-3]

pop.million <- world.pop / 1000
pop.million

pop.rate <- world.pop / world.pop[1]
pop.rate

pop.increase <- world.pop[-1] - world.pop[-7]
percent.increase <- (pop.increase / world.pop[-7]) * 100
percent.increase
percent.increase[c(1, 2)] <- c(20, 22)
percent.increase

## ------------------------------------------------------------------------
length(world.pop)  
min(world.pop)     
max(world.pop)     
range(world.pop)   
mean(world.pop)    
sum(world.pop) / length(world.pop) 

year <- seq(from = 1950, to = 2010, by = 10)
year

seq(to = 2010, by = 10, from = 1950)

seq(from = 2010, to = 1950, by = -10)
2008:2012
2012:2008

names(world.pop) 
names(world.pop) <- year
names(world.pop)
world.pop

## myfunction <- function(input1, input2, ..., inputN) {
## 
##     DEFINE `output' USING INPUTS
## 
##     return(output)
## }

my.summary <- function(x){ # function takes one input
  s.out <- sum(x)
  l.out <- length(x)
  m.out <- s.out / l.out
  out <- c(s.out, l.out, m.out) # define the output
  names(out) <- c("sum", "length", "mean") # add labels
  return(out) # end function by calling output
}
z <- 1:10
my.summary(z)
my.summary(world.pop)

## ---- eval = FALSE-------------------------------------------------------
## ## setwd("qss/INTRO")
## ## getwd()

## ------------------------------------------------------------------------
UNpop <- read.csv("UNpop.csv") 
class(UNpop)

load("UNpop.RData") 

names(UNpop)
nrow(UNpop)
ncol(UNpop)
dim(UNpop)
summary(UNpop)

UNpop$world.pop

UNpop[, "world.pop"] # extract the column called "world.pop"
UNpop[c(1, 2, 3),]   # extract the first three rows (and all columns)
UNpop[1:3, "year"]   # extract the first three rows of the "year" column

## take elements 1, 3, 5, ... of the "world.pop" variable
UNpop$world.pop[seq(from = 1, to = nrow(UNpop), by = 2)]

world.pop <- c(UNpop$world.pop, NA)
world.pop
mean(world.pop)
mean(world.pop, na.rm = TRUE)

## ---- eval = FALSE-------------------------------------------------------
## ## save.image("qss/INTRO/Chapter1.RData")
## 
## ## save(UNpop, file = "Chapter1.RData")
## ## save(world.pop, year, file = "qss/INTRO/Chapter1.RData")
## 
## ## write.csv(UNpop, file = "UNpop.csv")
## 
## ## load("Chapter1.RData")

## ---- eval = FALSE-------------------------------------------------------
## install.packages("foreign") # install package
## library("foreign") # load package
## 
## read.dta("UNpop.dta")
## read.spss("UNpop.sav")
## 
## write.dta(UNpop, file = "UNpop.dta")

## ---- eval = FALSE-------------------------------------------------------
## source("UNpop.R")
## 
## ##
## ## File: UNpop.R
## ## Author: Kosuke Imai
## ## The code loads the UN population data and saves it as a STATA file
## ##
## 
## library(foreign)
## UNpop <- read.csv("UNpop.csv")
## UNpop$world.pop <- UNpop$world.pop / 1000  # population in millions
## write.dta(UNpop, file = "UNpop.dta")
## 
## library(lintr)
## lint("UNpop.R")

