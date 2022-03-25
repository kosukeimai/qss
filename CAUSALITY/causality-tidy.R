## ---- message=FALSE---------------------------------------
## Load required packages
library(tidyverse)
library(qss)

## Load in the data from QSS package
data(resume, package = "qss")

## ---------------------------------------------------------
## Or read in a saved CSV
resume <- read_csv("CAUSALITY/resume.csv")


## ---- message=FALSE---------------------------------------
resume <- read_csv("CAUSALITY/resume.csv",
                   col_types = cols(
                     firstname = col_character(),
                     sex = col_character(),
                     race = col_character(),
                     call = col_number()))


## ---------------------------------------------------------
dim(resume)


## ---------------------------------------------------------
head(resume)
tail(resume)
glimpse(resume)


## ---------------------------------------------------------
summary(resume)


## ---- message=FALSE---------------------------------------
race.call.summary <- resume %>%
  group_by(race, call) %>% # create for each race and callback status
  count() 

race.call.summary


## ---- message=FALSE---------------------------------------
race.call.tab <- race.call.summary %>%
  pivot_wider(names_from = call, # reshape the data
              values_from = n)

race.call.tab


## ---------------------------------------------------------
race.call.tab.names <- race.call.tab %>% 
    rename(no_callback = "0",
         callback = "1")

race.call.tab.names


## ---------------------------------------------------------
race.call.tab.names <- race.call.tab.names %>% 
  mutate(total_resumes = no_callback + callback,
         callback_prop = callback / total_resumes)

race.call.tab.names


## ---------------------------------------------------------
overall_callback <- resume %>% 
  summarize(total_callback_rate = sum(call) / n())

overall_callback


## ---------------------------------------------------------
overall_callback <- resume %>% 
  summarize(total_callback_rate = mean(call))

overall_callback


## ---- message=FALSE---------------------------------------
callback_by_race <- resume %>% 
  group_by(race) %>% 
  summarize(callback_rate = mean(call))

callback_by_race


## ---------------------------------------------------------
class(TRUE)


## ---------------------------------------------------------
as.integer(TRUE)
as.integer(FALSE)


## ---------------------------------------------------------
x <- c(TRUE, FALSE, TRUE) # a vector with logical values
mean(x) # proportion of TRUEs
sum(x) # number of TRUEs




## ---------------------------------------------------------
FALSE & TRUE
TRUE & TRUE


## ---------------------------------------------------------
TRUE | FALSE
FALSE | FALSE


## ---------------------------------------------------------
TRUE & FALSE & TRUE


## ---------------------------------------------------------
(TRUE | FALSE) & FALSE # the parentheses evaluate to TRUE
TRUE | (FALSE & FALSE) # the parentheses evaluate to FALSE


## ---------------------------------------------------------
TF1 <- c(TRUE, FALSE, FALSE)
TF2 <- c(TRUE, FALSE, TRUE)
TF1 | TF2
TF1 & TF2


## ---------------------------------------------------------
4 > 3
"Hello" == "hello"  # R is case-sensitive
"Hello" != "hello"


## ---------------------------------------------------------
x <- c(3, 2, 1, -2, -1)
x >= 2
x != 1


## ---------------------------------------------------------
## logical conjunction of two vectors with logical values
(x > 0) & (x <= 2)
## logical disjunction of two vectors with logical values
(x > 2) | (x <= -1)


## ---------------------------------------------------------
x.int <- (x > 0) & (x <= 2) # logical vector
x.int
mean(x.int) # proportion of TRUEs
sum(x.int)  # number of TRUEs


## ---------------------------------------------------------
## callback rate for black-sounding names
resume %>% 
  filter(race == "black") %>% # only keep observations where "race" is black
  summarize(mean(call)) # take the average of call


## ---------------------------------------------------------
## Subset the data with black names
resumeB <- filter(resume, race == "black")
## Calculate the mean callback rate
summarize(resumeB, mean(call))
## with $ operator to run mean() on the call column
mean(resumeB$call)


## ---------------------------------------------------------
resumeBf <- filter(resume, race == "black" & sex == "female")


## ---------------------------------------------------------
## callback rate for black female names
Bf_callback <- filter(resume, race == "black" & sex == "female") %>%
  summarize(callback_rate = mean(call)) %>% 
  pull()

## print the value to the console
print(Bf_callback)

## callback rate for black male names
Bm_callback <- filter(resume, race == "black" & sex == "male") %>%
  summarize(callback_rate = mean(call)) %>% 
  pull()

## print the value to the console
print(Bm_callback)

## callback rate for white female names
Wf_callback <- filter(resume, race == "white" & sex == "female") %>%
  summarize(callback_rate = mean(call)) %>% 
  pull()

## print the value to the console
print(Wf_callback)

## callback rate for white male names
Wm_callback <- filter(resume, race == "white" & sex == "male") %>%
  summarize(callback_rate = mean(call)) %>% 
  pull()

## print the value to the console
print(Wm_callback)

## difference between white women and black women
Wf_callback - Bf_callback

## difference between white men and black men
Wm_callback - Bm_callback


## ---- message=FALSE---------------------------------------
racial_gaps_by_sex <- resume %>% 
  group_by(race, sex) %>% # using two variables to group the data
  summarize(callback = mean(call)) %>% # the callback rate for each group
  pivot_wider(names_from = race, # reshaping the data
              values_from = callback) %>% 
  mutate(race_gap = white - black)

print(racial_gaps_by_sex)


## ---- message=FALSE---------------------------------------
## what happens in this portion of the code?
resume %>% 
  group_by(race, sex) %>% 
  summarize(callback = mean(call))

## What happens after we add the pivot_wider()? 
resume %>% 
  group_by(race, sex) %>% 
  summarize(callback = mean(call)) %>% 
  pivot_wider(names_from = race,
              values_from = callback)

## And so on


## ---------------------------------------------------------
resume <- resume %>% 
  ## create a new variable that is 1 if the resume is black and female, 
  ## 0 otherwise
  mutate(BlackFemale = if_else(race == "black" & 
                               sex == "female", 1, 0))


## ---------------------------------------------------------
# Rows in the resumeBf data
nrow(resumeBf)
# Is that equal to sum of BlackFemale?
nrow(resumeBf) == resume %>% summarize(bf = sum(BlackFemale))


## ---------------------------------------------------------
resume_fact <- resume %>% 
  mutate(type = if_else(race == "black" & sex == "female", "BlackFemale", ""),
         type = if_else(race == "black" & sex == "male", "BlackMale", type),
         type = if_else(race == "white" & sex == "female", "WhiteFemale", type),
         type = if_else(race == "white" & sex == "male", "WhiteMemale", type))

head(resume_fact)


## ---------------------------------------------------------
resume <- resume %>% 
  ## add a categorical variable for race/gender type
  mutate(type = case_when(race == "black" & sex == "female" ~ "BlackFemale",
                          race == "white" & sex == "female" ~ "WhiteFemale",
                          race == "black" & sex == "male" ~ "BlackMale",
                          race == "white" & sex == "male" ~ "WhiteMale",
                          TRUE ~ "other"
))

head(resume)

## Did any observations receive the "other" value for type?
filter(resume, type == "other")


## ---------------------------------------------------------
## check object class
class(resume$type)

## coerce the character variable into a factor variable
resume <- resume %>% 
  mutate(type = as.factor(type))

## look at the levels of the factor
levels(resume$type)


## ---- message=FALSE---------------------------------------
firstname_callback <- resume %>% 
  group_by(firstname) %>% 
  select(firstname, call) %>% 
  summarize(callback = mean(call))

head(firstname_callback)


## ---------------------------------------------------------
slice(resume, 1)


## ---- label='naming-shaming', echo = FALSE, out.width='75%', fig.align = 'center', fig.cap= '(ref:name-shame-cap)', fig.scap="Naming-and-shaming get-out-the-vote message"----
knitr::include_graphics("CAUSALITY/naming-shaming.pdf")


## ---------------------------------------------------------
## Load in the data from QSS package
data(resume, package = "qss")

## Or from a csv
social <- read_csv("CAUSALITY/social.csv", 
                   col_types = cols(sex = col_character(),
                                     yearofbirth = col_double(),
                                     primary2004 = col_double(),
                                     messages = col_character(),
                                     primary2006 = col_double(),
                                     hhsize = col_double())) 
summary(social) # summarize the data


## ---- message=FALSE---------------------------------------
## Average turnout by treatment message
turnout_by_message <- social %>%
  group_by(messages) %>%
  summarize(turnout = mean(primary2006))

turnout_by_message

## Differences between treatment(s) and control means
turnout_diffs <- turnout_by_message %>% 
  pivot_wider(names_from = messages,
              values_from = turnout) %>% 
  mutate(diff_Civic_Duty = `Civic Duty` - Control,
         diff_Hawthorne = Hawthorne - Control,
         diff_Neighbors = Neighbors - Control) %>% 
  select(diff_Civic_Duty, diff_Hawthorne, diff_Neighbors)

turnout_diffs


## ---- message=FALSE---------------------------------------
social %>% 
  mutate(age = 2006 - yearofbirth) %>% 
  group_by(messages) %>% 
  summarize(age_avg = mean(age),
            primary2004_avg = mean(primary2004),
            hhsize_avg = mean(hhsize))


## ---- message=FALSE---------------------------------------
minwage <- read_csv("CAUSALITY/minwage.csv") # load the data
## or 
data(minwage, package = "qss")
dim(minwage) # dimension of data
glimpse(minwage)
summary(minwage) # summary of data


## ---- message=FALSE---------------------------------------
## Add a 'state' variable
minwage <- minwage %>% 
  mutate(state = if_else(location == "PA", "PA", "NJ"))

## Create the 'new_wage' object
new_wage <- 5.05

## Calculate the proportions above and below the new wage by state
state_props <- minwage %>% 
  mutate(above_min_before = if_else(wageBefore >= new_wage, 1, 0),
         above_min_after = if_else(wageAfter >= new_wage, 1, 0)) %>% 
  group_by(state) %>% 
  summarize(prop_before = mean(above_min_before),
            prop_after = mean(above_min_after))

state_props


## ---- message=FALSE---------------------------------------
## First create new variables to calculate the 
## proportion of full-time employees
minwage <- minwage %>%
  mutate(totalAfter = fullAfter + partAfter,
        fullPropAfter = fullAfter / totalAfter)

## Then calculate the average proportion of full-time workers by state 
full_prop_by_state <- minwage %>%
  group_by(state) %>%
  summarize(fullPropAfter = mean(fullPropAfter))

## To calculate the difference between states, we use pivot_wider()
## and mutate()
pivot_wider(full_prop_by_state, 
            names_from = state, values_from = fullPropAfter) %>%
  mutate(diff = NJ - PA)


## ---- message = FALSE-------------------------------------
chains_by_state <- minwage %>%
  group_by(state) %>%
  count(chain) %>%
  mutate(prop = n / sum(n)) %>% 
  pivot_wider(-n, # this drops the 'n' variable prior to pivoting 
              names_from = state,
              values_from = prop)

chains_by_state


## ----message=FALSE, warning=FALSE-------------------------
full_prop_by_state_chain <- minwage %>%
  group_by(state, chain) %>%
  summarize(fullPropAfter = mean(fullPropAfter)) %>% 
  pivot_wider(names_from = state,
              values_from = fullPropAfter) %>% 
  mutate(diff = NJ - PA)

full_prop_by_state_chain


## ---- message = FALSE-------------------------------------
prop_by_state_chain_location_subset <- minwage %>%
  filter(!location %in% c("shoreNJ", "centralNJ")) %>% 
  group_by(state, chain) %>%
  summarize(fullPropAfter = mean(fullPropAfter)) %>% 
  pivot_wider(names_from = state,
              values_from = fullPropAfter) %>% 
  mutate(diff = NJ - PA)

prop_by_state_chain_location_subset


## ---------------------------------------------------------
## First, create a variable for the full-time 
## proportion prior to the change
minwage <- minwage %>%
  mutate(totalBefore = fullBefore + partBefore,
         fullPropBefore = fullBefore / totalBefore)

## Then look at the differences in average proportion of full-time
## before and after (in NJ only)
minwage %>%
  filter(state == "NJ") %>%
  summarize(diff = mean(fullPropAfter) - mean(fullPropBefore))


## ----DiD-plot, dev='pdf', fig.show='hide', echo=FALSE-----
minwagePA <- filter(minwage, state == "PA")
minwageNJ <- filter(minwage, state == "NJ")

x <- seq(from = 0.2, to = 0.8, length = 10)
#y0 <- seq(from = 0.1, to = 0.4, length = 10)  
y0 <- seq(from = mean(minwagePA$fullPropBefore), 
          to = mean(minwagePA$fullPropAfter), length = 10)  
#y1 <- seq(from = 0.3, to = 0.8, length = 10)
y1 <- seq(from = mean(minwageNJ$fullPropBefore), 
          to = mean(minwageNJ$fullPropAfter), length = 10)
est <- mean(minwageNJ$fullPropBefore) - 
    mean(minwagePA$fullPropBefore)

baseplot <- function() {
    plot(x, y0, type = "l", xlim = c(0, 1.2), ylim = c(0.24, 0.36), col = "red", lwd = 1.5,
         xlab = "", ylab = "Average proportion of full-time employees", xaxt = "n")
    points(x[c(1, 10)], y0[c(1, 10)], pch = 1, col = "red")
    lines(x, y1, lwd = 1.5, col = "black")
    points(x[1], y1[1], pch = 19, col = "black")
    points(x[10], y1[10], pch = 19, col = "black")
    text(lab = "treatment group\n (New Jersey)", 0.8, 0.335, col = "black")
    text(lab = "control group\n (Pennsylvania)", 0.2, 0.3225, col = "red")
    mtext("Before", side = 1, at = 0.2, line = 1)
    mtext("After", side = 1, at = 0.8, line = 1)
}

## DiD
baseplot()
lines(x, y0+est, lwd = 1.5, lty = "dashed", col = "black")
points(x[10], y0[10]+est, pch = 17, col = "black")
text(lab = "counterfactual\n (New Jersey)", 0.8, 0.2475, col = "black")

# function to draw curly braces
# x, y position where to put the braces
# range is the length of the brace
# position: 1 vertical, 2 horizontal
# direction: 1 left/down, 2 right/up
# depth controls width of the shape

CurlyBraces <- function(x0, x1, y0, y1, pos = 1, direction = 1, depth = 1, color = "black") {

    a=c(1,2,3,48,50)    # set flexion point for spline
    b=c(0,.2,.28,.7,.8) # set depth for spline flexion point

    curve = spline(a, b, n = 50, method = "natural")$y * depth

    curve = c(curve,rev(curve))

    if (pos == 1){
        a_sequence = seq(x0,x1,length=100)
        b_sequence = seq(y0,y1,length=100)  
    }
    if (pos == 2){
        b_sequence = seq(x0,x1,length=100)
        a_sequence = seq(y0,y1,length=100)      
    }

    # direction
    if(direction==1)
        a_sequence = a_sequence+curve
    if(direction==2)
        a_sequence = a_sequence-curve

    # pos
    if(pos==1)
        lines(a_sequence,b_sequence, lwd=1.5, xpd=NA, col = color) # vertical
    if(pos==2)
        lines(b_sequence,a_sequence, lwd=1.5, xpd=NA, col = color) # horizontal

}

CurlyBraces(x0=0.82, x1=0.82, y0=y0[10]+est, y1=y1[10], pos = 1, direction = 1,
            depth=0.05, color = "blue")
text(1.05, 0.29, "average\n causal effect\n estimate")




## ---- message=FALSE---------------------------------------
## DiD estimate
minwage %>%
  group_by(state) %>%
  ## difference before and after
  summarize(diff = mean(fullPropAfter) - mean(fullPropBefore)) %>%
  pivot_wider(names_from = state, values_from = diff) %>%
  ## difference in differece bwetween states
  mutate(diff_in_diff = NJ - PA)


## ---- message=FALSE---------------------------------------
## median difference-in-differences
minwage %>%
  group_by(state) %>%
  summarize(diff = median(fullPropAfter) - median(fullPropBefore)) %>%
  pivot_wider(names_from = state, values_from = diff) %>%
  mutate(diff_in_diff = NJ - PA)


## ---------------------------------------------------------
## summary() shows quartiles for the two wages variables
## as well as minimum, maximum, and mean
minwage %>%
  filter(state == "NJ") %>% # just look at NJ
  select(wageBefore, wageAfter) %>%
  summary()

## The interquartile range
minwage %>%
  filter(state == "NJ") %>%
  select(wageBefore, wageAfter) %>%
  summarize(wageBeforeIQR = IQR(wageBefore),
            wageAfterIQR = IQR(wageAfter))


## ---------------------------------------------------------
## Create an object for the quantiles we want (deciles)
decile_probs <- seq(from = 0, to = 1, by = 0.1)
## Save deciles as characters
decile_names <- as.character(decile_probs)

## Generate the deciles for wage before and after
minwage %>% 
  filter(state == "NJ") %>%
  select(wageBefore, wageAfter) %>% 
  summarize(wageBeforeDecile = quantile(wageBefore, probs = decile_probs),
            wageAfterDecile = quantile(wageAfter, probs = decile_probs),
            decile = decile_names) 


## ---------------------------------------------------------
## Calculate the RMS of the change in full-time 
## employment proportion in NJ
minwageNJ %>% 
  mutate(fullPropChange = fullPropAfter - fullPropBefore,
         sqfullPropChange = fullPropChange^2) %>% 
  summarize(rms = sqrt(mean(sqfullPropChange)))

## Compare to the mean
minwageNJ %>% 
  mutate(fullPropChange = fullPropAfter - fullPropBefore) %>% 
  summarize(mean = mean(fullPropChange))


## ---- include=FALSE---------------------------------------
## Original, before corrections
minwage %>%
  group_by(state) %>%
  summarize_at(vars(wageAfter, wageBefore), 
               .funs = list(sd, var))

minwage %>%
  group_by(state) %>%
  summarize_at(vars(wageAfter, wageBefore), 
               .funs = list(stdv = sd, 
                            variance = var))


## ---------------------------------------------------------
minwage %>%
  group_by(state) %>%
  summarize_at(vars(fullPropBefore, fullPropAfter), 
               .funs = list(sd, var))

minwage %>%
  group_by(state) %>%
  summarize_at(vars(fullPropBefore, fullPropAfter), 
               .funs = list(stdv = sd, 
                            variance = var))




## ---- label = 'leader', echo = FALSE, warning=FALSE-------
data.frame("Name" = c("country", "year", "leadername", "age", "politybefore", "polityafter", "civilwarbefore", "civilwarafter", "interwarbefore", "interwarafter", "result"), 
           "Description" =  c("country",
                              "year",
                              "name of leader who was targeted", 
                              "age of the targeted leader",
                              "average polity score during the three-year period prior to the attempt",
                              "average polity score during the three-year period after the attempt", 
                              "\\rexpr{1} if the country was in civil war during the three-year period prior to the attempt, \\rexpr{0} otherwise",
                              "\\rexpr{1} if the country was in civil war during the three-year period after the attempt, \\rexpr{0} otherwise",
                              "\\rexpr{1} if the country was in international war during the three-year period prior to the attempt, \\rexpr{0} otherwise",
                              "\\rexpr{1} if the country was in international war during the three-year period after the attempt, \\rexpr{0} otherwise",
                              "result of the assassination attempt")) %>% 
  mutate(Name = kableExtra::cell_spec(Name, monospace = TRUE)) %>%
  knitr::kable(escape = FALSE, booktabs = TRUE, linesep = "", col.names = kableExtra::linebreak(c("\\textit{Variable}", "\\textit{Description}")), format.args = list(big.mark = ","), caption = 'Leader Assassination Data.') %>%
  kableExtra::kable_styling() %>% 
  kableExtra::column_spec(2, width = "30em")

