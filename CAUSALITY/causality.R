## ------------------------------------------------------------------------
resume <- read.csv("resume.csv")

dim(resume)
head(resume)
summary(resume)

race.call.tab <- table(race = resume$race, call = resume$call) 
race.call.tab

addmargins(race.call.tab)

## overall callback rate: total callbacks divided by the sample size
sum(race.call.tab[, 2]) / nrow(resume)

## callback rates for each race
race.call.tab[1, 2] / sum(race.call.tab[1, ]) # black
race.call.tab[2, 2] / sum(race.call.tab[2, ]) # white

race.call.tab[1, ]  # the first row
race.call.tab[, 2]  # the second column

mean(resume$call)

## ------------------------------------------------------------------------
class(TRUE)

as.integer(TRUE)
as.integer(FALSE)

x <- c(TRUE, FALSE, TRUE) # a vector with logical values

mean(x) # proportion of TRUEs
sum(x) # number of TRUEs

FALSE & TRUE
TRUE & TRUE
TRUE | FALSE
FALSE | FALSE
TRUE & FALSE & TRUE

(TRUE | FALSE) & FALSE # the parentheses evaluate to TRUE
TRUE | (FALSE & FALSE) # the parentheses evaluate to FALSE

TF1 <- c(TRUE, FALSE, FALSE)
TF2 <- c(TRUE, FALSE, TRUE)
TF1 | TF2
TF1 & TF2

## ------------------------------------------------------------------------
4 > 3

"Hello" == "hello"  # R is case-sensitive
"Hello" != "hello"

x <- c(3, 2, 1, -2, -1)
x >= 2
x != 1

## logical conjunction of two vectors with logical values
(x > 0) & (x <= 2)
## logical disjunction of two vectors with logical values
(x > 2) | (x <= -1)

x.int <- (x > 0) & (x <= 2) # logical vector
x.int

mean(x.int) # proportion of TRUEs
sum(x.int)  # number of TRUEs

## ------------------------------------------------------------------------
## callback rate for black-sounding names
mean(resume$call[resume$race == "black"]) 

## race of first 5 observations
resume$race[1:5]  

## comparison of first 5 observations
(resume$race == "black")[1:5] 

dim(resume) # dimension of original data frame

## subset blacks only
resumeB <- resume[resume$race == "black", ] 
dim(resumeB) # this data.frame has fewer rows than the original data.frame
mean(resumeB$call) # callback rate for blacks

## keep "call" and "firstname" variables 
## also keep observations with black female-sounding names
resumeBf <- subset(resume, select = c("call", "firstname"),
                   subset = (race == "black" & sex == "female"))
head(resumeBf)

## ## an alternative syntax with the same results
## resumeBf <- resume[resume$race == "black" & resume$sex == "female",
##                    c("call", "firstname")]
## black male
resumeBm <- subset(resume, subset = (race == "black") & (sex == "male"))
## white female
resumeWf <- subset(resume, subset = (race == "white") & (sex == "female"))
## white male
resumeWm <- subset(resume, subset = (race == "white") & (sex == "male"))
## racial gaps
mean(resumeWf$call) - mean(resumeBf$call) # among females
mean(resumeWm$call) - mean(resumeBm$call) # among males

## ------------------------------------------------------------------------
resume$BlackFemale <- ifelse(resume$race == "black" & 
                                 resume$sex == "female", 1, 0)
table(race = resume$race, sex = resume$sex,
      BlackFemale = resume$BlackFemale)

## ------------------------------------------------------------------------
resume$type <- NA
resume$type[resume$race == "black" & resume$sex == "female"] <- "BlackFemale"
resume$type[resume$race == "black" & resume$sex == "male"] <- "BlackMale"
resume$type[resume$race == "white" & resume$sex == "female"] <- "WhiteFemale"
resume$type[resume$race == "white" & resume$sex == "male"] <- "WhiteMale"

## check object class
class(resume$type)

## coerce new character variable into a factor variable
resume$type <- as.factor(resume$type)
## list all levels of a factor variable
levels(resume$type)

## obtain the number of observations for each level
table(resume$type)
tapply(resume$call, resume$type, mean)

## turn first name into a factor variable 
resume$firstname <- as.factor(resume$firstname)
## compute callback rate for each first name
callback.name <- tapply(resume$call, resume$firstname, mean)
## sort the result in the increasing order
sort(callback.name)

## ------------------------------------------------------------------------
resume[1, ]

## ------------------------------------------------------------------------
social <- read.csv("social.csv") # load the data

summary(social) # summarize the data

## turnout for each group
tapply(social$primary2006, social$messages, mean)

## turnout for control group
mean(social$primary2006[social$messages == "Control"])

## subtract control group turnout from each group
tapply(social$primary2006, social$messages, mean) -
    mean(social$primary2006[social$messages == "Control"])

social$age <- 2006 - social$yearofbirth # create age variable
tapply(social$age, social$messages, mean)
tapply(social$primary2004, social$messages, mean) 
tapply(social$hhsize, social$messages, mean) 

## ------------------------------------------------------------------------
minwage <- read.csv("minwage.csv") # load the data

dim(minwage) # dimension of data
summary(minwage) # summary of data

## subsetting the data into two states
minwageNJ <- subset(minwage, subset = (location != "PA"))
minwagePA <- subset(minwage, subset = (location == "PA"))

## proportion of restaurants whose wage is less than $5.05
mean(minwageNJ$wageBefore < 5.05) # NJ before 
mean(minwageNJ$wageAfter < 5.05)  # NJ after 
mean(minwagePA$wageBefore < 5.05) # PA before 
mean(minwagePA$wageAfter < 5.05)  # PA after 

## create a variable for proportion of full-time employees in NJ and PA
minwageNJ$fullPropAfter <- minwageNJ$fullAfter / 
    (minwageNJ$fullAfter + minwageNJ$partAfter)
minwagePA$fullPropAfter <- minwagePA$fullAfter / 
    (minwagePA$fullAfter + minwagePA$partAfter)

## compute the difference in means
mean(minwageNJ$fullPropAfter) - mean(minwagePA$fullPropAfter)

## ------------------------------------------------------------------------
prop.table(table(minwageNJ$chain))
prop.table(table(minwagePA$chain))

## subset Burger King only
minwageNJ.bk <- subset(minwageNJ, subset = (chain == "burgerking"))
minwagePA.bk <- subset(minwagePA, subset = (chain == "burgerking"))

## comparison of full-time employment rates
mean(minwageNJ.bk$fullPropAfter) - mean(minwagePA.bk$fullPropAfter)

minwageNJ.bk.subset <- 
    subset(minwageNJ.bk, subset = ((location != "shoreNJ") & 
                                       (location != "centralNJ")))

mean(minwageNJ.bk.subset$fullPropAfter) - mean(minwagePA.bk$fullPropAfter)

## ------------------------------------------------------------------------
## full-time employment proportion in the previous period for NJ
minwageNJ$fullPropBefore <- minwageNJ$fullBefore / 
    (minwageNJ$fullBefore + minwageNJ$partBefore)

## mean difference between before and after the minimum wage increase
NJdiff <- mean(minwageNJ$fullPropAfter) - mean(minwageNJ$fullPropBefore)
NJdiff

## full-time employment proportion in the previous period for PA
minwagePA$fullPropBefore <- minwagePA$fullBefore / 
    (minwagePA$fullBefore + minwagePA$partBefore)
## mean difference between before and after for PA
PAdiff <- mean(minwagePA$fullPropAfter) - mean(minwagePA$fullPropBefore)
## difference-in-differences
NJdiff - PAdiff

## full-time employment proportion in the previous period for PA
minwagePA$fullPropBefore <- minwagePA$fullBefore / 
    (minwagePA$fullBefore + minwagePA$partBefore)
## mean difference between before and after for PA
PAdiff <- mean(minwagePA$fullPropAfter) - mean(minwagePA$fullPropBefore)
## difference-in-differences
NJdiff - PAdiff

## ------------------------------------------------------------------------
## cross-section comparison between NJ and PA
median(minwageNJ$fullPropAfter) - median(minwagePA$fullPropAfter)
## before and after comparison
NJdiff.med <- median(minwageNJ$fullPropAfter) - 
    median(minwageNJ$fullPropBefore)
NJdiff.med
## median difference-in-differences
PAdiff.med <- median(minwagePA$fullPropAfter) - 
    median(minwagePA$fullPropBefore)
NJdiff.med - PAdiff.med

## summary shows quartiles as well as minimum, maximum, and mean
summary(minwageNJ$wageBefore)
summary(minwageNJ$wageAfter)
## interquartile range 
IQR(minwageNJ$wageBefore)
IQR(minwageNJ$wageAfter)

## deciles (10 groups)
quantile(minwageNJ$wageBefore, probs = seq(from = 0, to = 1, by = 0.1))
quantile(minwageNJ$wageAfter, probs = seq(from = 0, to = 1, by = 0.1))

## ------------------------------------------------------------------------
sqrt(mean((minwageNJ$fullPropAfter - minwageNJ$fullPropBefore)^2))
mean(minwageNJ$fullPropAfter - minwageNJ$fullPropBefore)

## standard deviation
sd(minwageNJ$fullPropBefore)
sd(minwageNJ$fullPropAfter)
## variance
var(minwageNJ$fullPropBefore)
var(minwageNJ$fullPropAfter)

