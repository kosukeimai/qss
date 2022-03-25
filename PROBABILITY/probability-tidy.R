## ----message=FALSE, warning =FALSE------------------------
library(tidyverse)
## write the birthday function
birthday <- function(k) {
  logdenom <- k * log(365) + lfactorial(365 - k)
  lognumer <- lfactorial(365)
  pr <- 1 -   exp(lognumer - logdenom)
  pr
}

## create a tibble with the k and pr per k
bday <- tibble(k = 1:50, pr = birthday(k))

## plot the data
ggplot(bday, aes(x = k, y = pr)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(str_c("Probability that at least two",
                           "people have the same birthday", sep = "\n"),
                     limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  labs(x = "Number of people")


## ---------------------------------------------------------
## setting seed for replication
set.seed(4444)
k <- 23 # number of people
sims <- 1000 # number of simulations
event <- 0 # counter
for (i in 1:sims) {
    days <- sample(1:365, k, replace = TRUE)
    days.unique <- unique(days) # unique birthdays
    ## if there are duplicates, the number of unique birthdays
    ## will be less than the number of birthdays, which is `k'
    if (length(days.unique) < k) {
        event <- event + 1
    }
}
## fraction of trials where at least two bdays are the same
answer <- event / sims
answer


## ---- echo=FALSE, results='hide'--------------------------
k <- 23 # number of people
sims <- 100000 # number of simulations
event <- 0 # counter
for (i in 1:sims) {
    ## sample k possibly non-unique birthdays
    days <- sample(1:365, k, replace = TRUE)
    days.unique <- unique(days) # unique birthdays
    ## if less than k unique days, at least two have the same bday
    if (length(days.unique) < k) {
        event <- event + 1  # add one to the counter
    }
}
## fraction of trials where at least two bdays are the same
answer <- event / sims
answer




## ---------------------------------------------------------
choose(84, 6)


## ---------------------------------------------------------
data(FLVoters, package = "qss")
## how many observations?
dim(FLVoters)
## what do the data look like?
glimpse(FLVoters)
## removing observations with missing values
FLVoters <- FLVoters %>%
  na.omit()
## how many observations remain?
dim(FLVoters)


## ---------------------------------------------------------
margin_race <-
  FLVoters %>%
  count(race) %>%
  mutate(prop = n / sum(n))
margin_race


## ---------------------------------------------------------
margin_gender <-
  FLVoters %>%
  count(gender) %>%
  mutate(prop = n / sum(n))
margin_gender


## ---------------------------------------------------------
## Conditional probability, among women
margin_race_f <- FLVoters %>%
  filter(gender == "f") %>%
  count(race) %>%
  mutate(prop = n / sum(n))
margin_race_f

## Conditional probability, among men
margin_race_m <- FLVoters %>%
  filter(gender == "m") %>%
  count(race) %>%
  mutate(prop = n / sum(n))
margin_race_m


## ---------------------------------------------------------
joint_p <-
  FLVoters %>%
  count(gender, race) %>%
  mutate(prop = n / sum(n))
joint_p


## ---------------------------------------------------------
## gender to columns, with proportion as value
joint_p_wider <- joint_p %>% 
  select(-n) %>%
  pivot_wider(names_from = gender, 
              values_from = prop) %>% 
  mutate(total_prop = f + m)
  


## ---------------------------------------------------------
## race to columns, with proportion as value
joint_p_wider <- joint_p %>% 
  select(-n) %>%
  pivot_wider(names_from = race, 
              values_from = prop) %>% 
  mutate(total_prop = rowSums(across(where(is.numeric))))

joint_p_wider

## alternative
joint_p %>%
  group_by(gender) %>%
  summarize(prop = sum(prop))


## ---------------------------------------------------------
## adding the age_group variable
FLVoters <- FLVoters %>%
  mutate(age_group = cut(age, breaks = c(0, 20, 40, 60, Inf), 
                         right = TRUE,
                         labels = c("<= 20", "20-40", "40-60", "> 60")))


## ---------------------------------------------------------
## joint probability table
joint3 <-
  FLVoters %>%
  count(race, age_group, gender) %>%
  mutate(prop = n / sum(n))

head(joint3)


## ---------------------------------------------------------
## calculate marginal probability of age groups
margin_age <-
  FLVoters %>%
  count(age_group) %>%
  mutate(margin_age = n / sum(n)) %>% 
  select(-n)
margin_age

## merge this with the joint probability table
## and add conditional prob
joint3 <- left_join(joint3, margin_age, 
                    by = "age_group") %>% 
  mutate(con_prob_age = prop / margin_age)

## conditional probability of black female given 
## above 60 years old

filter(joint3, race == "black", gender == "f", age_group == "> 60") %>%
  select(race, age_group, gender, con_prob_age)


## ---------------------------------------------------------
## joint probability by age and gender
joint2 <- FLVoters %>%
  count(age_group, gender) %>%
  ungroup() %>%
  mutate(prob_age_gender = n / sum(n)) %>% 
  select(-n)
joint2

## merge to the 3 way joint probability
## calculate conditional prob of race given age and gender
joint3 <- left_join(joint3, joint2, 
                    by = c("age_group", "gender")) %>% 
  mutate(con_prob_race = prop / prob_age_gender)

## Conditional prob of black given female and above 60
filter(joint3, gender == "f", age_group == "> 60", race == "black") %>% 
  select(con_prob_race)




## ---------------------------------------------------------
## adjust the marginal probability data: select/rename needed variables
margin_race <- select(margin_race, race, prob_race = prop)
margin_gender <- select(margin_gender, gender, prob_gender = prop)

## combine the marginal probabilities and link to joint probabilities
race_gender_indep <- tidyr::crossing(margin_race, margin_gender) %>% 
  left_join(select(joint_p, gender, race, prob_joint = prop),
            by = c("gender", "race")) %>%
  ## calculate the independent prob comparison
  mutate(prob_indp = prob_race * prob_gender)

race_gender_indep

## plot it, just for female
ggplot(filter(race_gender_indep, gender == "f"),
       aes(x = prob_indp, y = prob_joint)) +
  geom_abline(intercept = 0, slope = 1, color = "black", size = 0.5) +
  geom_point(shape = 1) +
  coord_fixed() +
  labs(x = expression(P("race") * P("gender")),
       y = expression(P("race and gender")))




## ----fig.show='hold', out.width='45%'---------------------
## adjust the marginal probability data: select/rename needed variables
margin_age <- select(margin_age, age_group, prob_age = margin_age)

## combine the marginal probabilities and link to joint probabilities
joint_indep <-
  tidyr::crossing(margin_age, margin_gender, margin_race) %>%
  mutate(indep_prob = prob_race * prob_age * prob_gender) %>%
  left_join(select(joint3, race, age_group, gender, prob_joint = prop),
            by = c("gender", "age_group", "race"))

## Plot joint versus independent prob
ggplot(filter(joint_indep, age_group == "> 60", gender == "f"), 
       aes(x = prob_joint, y = indep_prob)) +
  geom_abline(intercept = 0, slope = 1, color = "black", size = 0.5) +
  geom_point(shape = 1) +
  coord_fixed() +
  labs(x = "P(race and above 60 and female)",
       y = "P(race) * P(above 60) * P(female)",
       title = "Joint independence") 

## conditional prob of race and age given gender
cond_prob_gender <- left_join(select(joint3, race, age_group, gender, 
                                joint_prob = prop),
                         margin_gender,
            by = c("gender")) %>%
  mutate(cond_prob_gender = joint_prob / prob_gender)

## conditional prob of race given gender
cond_prob_race_gender <- left_join(select(joint_p, race, gender, 
                                     prob_race_gender = prop),
                              margin_gender,
            by = "gender") %>%
  mutate(cond_prob_race_gender = prob_race_gender / prob_gender) %>% 
  select(race, gender, cond_prob_race_gender)

## conditional prob of age given gender
cond_prob_age_gender <- left_join(select(joint2, age_group, 
                                         gender, prob_age_gender),
                                  margin_gender,
            by = "gender") %>%
  mutate(cond_prob_age_gender = prob_age_gender / prob_gender) %>% 
  select(age_group, gender, cond_prob_age_gender)

# independent probability of race and age
indep_cond_gender <- full_join(cond_prob_race_gender, cond_prob_age_gender,
            by = "gender") %>%
  mutate(indep_prob = cond_prob_race_gender * cond_prob_age_gender)

## Merge the independent probability with the conditional probability
master <- left_join(cond_prob_gender, indep_cond_gender, 
                   by = c("gender", "age_group", "race")) 

## plotting just for women over 60
ggplot( filter(master, age_group == "> 60", gender == "f"),
  aes(x = cond_prob_gender, y = indep_prob)) +
  geom_abline(intercept = 0, slope = 1, color = "black", size = 0.5) +
  geom_point(shape = 1) +
  coord_fixed() +
  labs(x = "P(race and above 60 | female)",
       y = "P(race | female) * P(above 60 | female)",
       title = "Marginal independence")



## ---------------------------------------------------------
## create a function that simulates the game
sims <- 1000
doors <- c("goat", "goat", "car")
result.switch <- result.noswitch <- rep(NA, sims)

for (i in 1:sims) {
    ## randomly choose the initial door
    first <- sample(1:3, size = 1)
    result.noswitch[i] <- doors[first]
    remain <- doors[-first] # remaining two doors
    ## Monty chooses one door with a goat
    if (doors[first] == "car") # two goats left
        monty <- sample(1:2, size = 1)
    else # one goat and one car left
        monty <- (1:2)[remain == "goat"]
    result.switch[i] <- remain[-monty]
}

mean(result.noswitch == "car")
mean(result.switch == "car")




## ---------------------------------------------------------
data("cnames", package = "qss")
glimpse(cnames)


## ---------------------------------------------------------
## Finding the most likely race per name
most_likely_race <- cnames %>%
  select(-count) %>%
  ## reshape to longer
  pivot_longer(cols = -surname, 
               names_to = "race_pred", 
               values_to = "highest_pct") %>% 
  # remove pct prefix from variable names
  mutate(race_pred = str_replace(race_pred, "pct", "")) %>%
  ## group by surname
  group_by(surname) %>%
  ## select row per name with the largest percentage
  ## by keeping the first instance after arranging by percentage
  filter(row_number(desc(highest_pct)) == 1) %>%
  # Ungroup to avoid errors later
  ungroup()

## merging back with the original data
cnames <- full_join(cnames, most_likely_race, by = "surname")


## ---------------------------------------------------------
## Size of the voter file
dim(FLVoters)
## Merge with the census data
FLVotersJoin <- FLVoters %>%
  inner_join(cnames, by = "surname")
## Size after matching (smaller, some names not matched)
dim(FLVotersJoin)

glimpse(FLVotersJoin)


## ---------------------------------------------------------
## which values for race and race_pred?
unique(FLVotersJoin$race)
unique(FLVotersJoin$race_pred)

## Recoding so the fields match
FLVotersJoin <- FLVotersJoin %>% 
  mutate(race = recode(race, "native" = "other"),
         race_pred = recode(race_pred, 
                            "api" = "asian", 
                            "others" = "other"))

## check that the recoding worked
unique(FLVotersJoin$race)
unique(FLVotersJoin$race_pred)


## ---------------------------------------------------------
race_tp <- FLVotersJoin %>%
  group_by(race) %>%
  summarize(tp = mean(race == race_pred)) %>%
  arrange(desc(tp))

race_tp


## ---------------------------------------------------------
race_fp <- FLVotersJoin %>%
  group_by(race_pred) %>%
  summarize(fp = mean(race != race_pred)) %>%
  arrange(desc(fp))

race_fp



## ----message = FALSE, warning=FALSE-----------------------
data("FLCensus", package = "qss")

## recode the race variable
FLCensus <- FLCensus %>% 
    rename("asian" = "api",
           "other" = "others")
  
## probability of race in Florida
race_prop <- FLCensus %>%
  select(total.pop, white, black, asian, hispanic, other) %>%
  pivot_longer(cols = - total.pop, 
               names_to = "race", 
               values_to = "pct") %>%
  group_by(race) %>%
  summarize(race_prop = weighted.mean(pct, w = total.pop)) %>%
  arrange(desc(race_prop))
race_prop


## ---------------------------------------------------------
## merge the race probability with the existing data
FLVotersJoin <- left_join(FLVotersJoin, race_prop, by = "race")

## Calculate prob of surname given race
## total number of individuals
total.count <- sum(cnames$count)

## have to reshape the names data to longer format, similar to above
cnames_reshape <- cnames %>%
  ## drop unneeded columns
  select(-race_pred, -highest_pct) %>% 
  ## reshape to longer
  pivot_longer(cols = starts_with("pct"), 
               names_to = "race", 
               values_to = "pct") %>% 
  ## recode to match race names, and go to proportion
  mutate(race = str_replace(race, "pct", ""),
         race = recode(race, "api" = "asian", 
                            "others" = "other"),
         race_surname = pct/100) %>% 
  select(-pct) %>% 
  ## merge the statewide race proportions, P(race)
  left_join(race_prop, by = "race") %>% 
  ## calculate the quantity of interest
  ## P(surname | race) = P(race | surname) * P(surname) / P(race)
  mutate(prob_surname_race = race_surname * (count/total.count) / race_prop) %>%
  rename("posib_race" = race)


## ---- warning=FALSE, message=FALSE------------------------
## reshape the FL census data so we have race as a variable, then pct of pop
merge_temp <- FLCensus %>% 
  pivot_longer(cols = c(white, black, hispanic, asian, other),
               names_to = "pop_race",
               values_to = "pop_pct") %>% 
  inner_join(FLVoters, by = c("county", "VTD")) %>% 
  inner_join(cnames_reshape, by = c("surname", "pop_race" = "posib_race")) %>% 
  mutate(race_residence = prob_surname_race * pop_pct)

## then calculate the summation
name_residence <- merge_temp %>% 
  group_by(surname, county, VTD) %>% 
  summarize(name_residence = sum(race_residence))

## now we have all quantities of interest, can merge all together
## and calculate the predicted race
FLVoters_full <- merge_temp %>% 
  inner_join(name_residence, by = c("surname", "county", "VTD")) %>% 
  mutate(pred_race = prob_surname_race * pop_pct / name_residence) %>% 
  select(surname, pop_race, race, pred_race, county, VTD)


## ---------------------------------------------------------
## now filter to save the highest predicted race, 
## and see if it matches actual race
FL_updated <- FLVoters_full %>% 
  ungroup() %>% 
  group_by(surname, county, VTD) %>%
  ## select row per name with the largest percentage
  ## by keeping the first instance after arranging by percentage
  filter(row_number(desc(pred_race)) == 1) %>%
  # Ungroup to avoid errors later
  ungroup()

## calculate the new true positive rate
race_tp_new <- FL_updated %>%
  group_by(race) %>%
  summarize(tp = mean(race == pop_race)) %>%
  arrange(desc(tp))

race_tp_new


## ---------------------------------------------------------
## proportion of blacks among those with surname "White"
filter(cnames, surname == "WHITE") %>% 
  select(pctblack) %>% 
  pull()

## Predicted probability of being black given residence location
filter(FLVoters_full, surname == "WHITE", pop_race == "black") %>%
  select(pred_race) %>% 
  summary()


## ---------------------------------------------------------
## the new false positive rate
race_fp_new <- FL_updated %>%
  group_by(pop_race) %>%
  summarize(fp = mean(race != pop_race)) %>%
  arrange(desc(fp))

race_fp_new


## ----bernoulli, echo=FALSE, out.width='7.5cm', fig.show='hold', fig.cap="The Probability Mass and Cumulative Distribution Functions for a Bernoulli Random Variable. The probability of success is $0.25$. The open and solid circles represent the exclusion and inclusion of the corresponding points, respectively.", fig.scap="Bernoulli distribution"----
par(cex = 1.5)
x <- 0:1
barplot(dbinom(x, size = 1, prob = 0.25), ylim = c(0, 1), names.arg = x, xlab = "x",
        ylab = "f(x)", main = "Probability mass function")
x <- -1:2
pb <- pbinom(x, size = 1, prob = 0.25)
plot(x[1:2], rep(pb[1], 2), ylim = c(0, 1), type = "s", xlim = c(-1, 2), xlab = "x",
     ylab = "F(x)", main = "Cumulative distribution function")
for (i in 2:(length(x)-1)) {
    lines(x[i:(i+1)], rep(pb[i], 2))
}
points(x[2:(length(x)-1)], pb[2:(length(x)-1)], pch = 19)
points(x[2:(length(x)-1)], pb[1:(length(x)-2)])


## ----uniform, echo=FALSE, out.width='7.5cm', fig.show='hold', fig.cap="The Probability Density and Cumulative Distribution Functions for a Uniform Random Variable. The interval is set to $[0, 1]$. The open and solid circles represent the exclusion and inclusion of the corresponding points, respectively.", fig.scap="Uniform distribution"----
par(cex = 1.5)
x <- seq(from = -1, to = 2, by = 0.01)
plot(x[x >=0 & x <= 1], dunif(x[x >=0 & x <= 1], min = 0, max = 1),
     xlab = "x", ylab = "f(x)", type = "l", xlim = c(-1, 2), ylim =
         c(0, 1),  main = "Probability density function")
lines(x[x < 0], dunif(x[x < 0], min = 0, max = 1))
lines(x[x > 1], dunif(x[x > 1], min = 0, max = 1))
points(0, 0)
points(1, 0)
points(0, 1, pch = 19)
points(1, 1, pch = 19)
x <- seq(from = -1, to = 2, by = 0.01)
plot(x, punif(x, min = 0, max = 1), type = "l", ylab = "F(x)",
     main = "Cumulative distribution function")


## ---------------------------------------------------------
## uniform PDF: x = 0.5, interval = [0, 1]
dunif(0.5, min = 0, max = 1)
## uniform CDF: x = 1, interval = [-2, 2]
punif(1, min = -2, max = 2)


## ---------------------------------------------------------
sims <- 1000
p <- 0.5 # success probabilities
x <- runif(sims, min = 0, max = 1) # uniform [0, 1]
head(x)
y <- as.integer(x <= p) # Bernoulli; turn TRUE/FALSE to 1/0
head(y)
mean(y) # close to success probability p, proportion of 1's vs. 0's


## ----binomial, echo=FALSE, out.width='7.5cm', fig.show='hold', fig.cap="The Probability Mass and Cumulative Distribution Functions for a binomial Random Variable. The success probability is $0.5$ and the total number of trials is $3$. The open and solid circles represent the exclusion and inclusion of the corresponding points, respectively.", fig.scap="Binomial distribution"----
par(cex = 1.5)
x <- 0:3
barplot(dbinom(x, size = 3, prob = 0.5), ylim = c(0, 0.4), names.arg = x, xlab = "x",
        ylab = "Density", main = "Probability mass function")
x <- -1:4
pb <- pbinom(x, size = 3, prob = 0.5)
plot(x[1:2], rep(pb[1], 2), ylim = c(0, 1), type = "s", xlim = c(-1, 4), xlab = "x",
     ylab = "Probability", main = "Cumulative distribution function")
for (i in 2:(length(x)-1)) {
    lines(x[i:(i+1)], rep(pb[i], 2))
}
points(x[2:(length(x)-1)], pb[2:(length(x)-1)], pch = 19)
points(x[2:(length(x)-1)], pb[1:(length(x)-2)])


## ---------------------------------------------------------
## PMF when x = 2, n = 3, p = 0.5
dbinom(2, size = 3, prob = 0.5)


## ---------------------------------------------------------
## CDF when x = 1, n = 3, p = 0.5
pbinom(1, size = 3, prob = 0.5)


## ---------------------------------------------------------
## number of voters who turn out
voters <- c(1000, 10000, 100000)
dbinom(voters / 2, size = voters, prob = 0.5)


## ----normal, echo=FALSE, out.width='7.5cm', fig.show='hold', fig.cap="The Probability Density and Cumulative Distribution Functions of the Normal Distribution.", fig.scap="The normal distribution"----

par(cex = 1.5)
x <- seq(from = -7, to = 7, by = 0.01)
lwd <- 1.5
plot(x, dnorm(x), xlab = "x", ylab = "Density", type = "l",
     main = "Probability density function", ylim = c(0, 0.9))
lines(x, dnorm(x, sd = 2), col = "red", lwd = lwd)
lines(x, dnorm(x, mean = 1, sd = 0.5), col = "blue", lwd = lwd)
text(3, 0.7, "mean = 1\n s.d. = 0.5", col = "blue")
text(-2, 0.4, "mean = 0\n s.d. = 1")
text(-4.5, 0.15, "mean = 0\n s.d. = 2", col = "red")
plot(x, pnorm(x), xlab = "x", ylab = "Probability", type = "l",
     main = "Cumulative distribution function", lwd = lwd)
lines(x, pnorm(x, sd = 2), col = "red", lwd = lwd)
lines(x, pnorm(x, mean = 1, sd = 0.5), col = "blue", lwd = lwd)
text(2.5, 0.2, "mean = 1\n s.d. = 0.5", col = "blue")
text(-1, 0.8, "mean = 0\n s.d. = 1")
text(-4, 0.2, "mean = 0\n s.d. = 2", col = "red")


## ----normalcdf, echo=FALSE, fig.align='center', fig.cap="The Area under the Probability Density Function Curve of the Normal Distribution. The red area can be computed as the difference of the cumulative distribution function (CDF) evaluated at $k$ and $-k$ (i.e., the red and blue areas minus the blue area).", fig.scap="The area of the normal distribution"----
par(cex = 1.5)
x <- seq(from = -4, to = 4, by = 0.01)
k <- 1
lwd <- 1.5
plot(x, dnorm(x), xlab = "", ylab = "Density", type = "l", axes = FALSE,
     main = "", ylim = c(0, 0.5))
polygon(x = c(rep(-k, 2),
            seq(from = -k, to = k, by = 0.01),
            rep(k, 2)),
        y = c(0, dnorm(-k),
            dnorm(seq(from = -k, to = k, by = 0.01)),
            dnorm(k), 0), density = 100, col = "red")
polygon(x = c(rep(-k, 2),
            seq(from = -k, to = -4, by = -0.01),
            c(-4, -k)),
        y = c(0, dnorm(-k),
            dnorm(seq(from = -k, to = -4, by = -0.01)),
            c(0, 0)), density = 100, col = "blue")
abline(v = 0)
axis(side = 1, at = c(-k, 0, k),
     labels = c(expression(-k), 0, expression(k)))
axis(2)


## ---------------------------------------------------------
## plus minus one standard deviation from the mean
pnorm(1) - pnorm(-1)
## plus minus two standard deviations from the mean
pnorm(2) - pnorm(-2)


## ---------------------------------------------------------
mu <- 5
sigma <- 2
## plus minus one standard deviation from the mean
pnorm(mu + sigma, mean = 5, sd = 2) - pnorm(mu - sigma, mean = 5, sd = 2)
## plus minus two standard deviations from the mean
pnorm(mu + 2*sigma, mean = 5, sd = 2) - pnorm(mu - 2*sigma, mean = 5, sd = 2)


## ---- echo=FALSE, message = FALSE, warning=FALSE, results = FALSE----
## You will need to read in the data from Chapter 4
## which is saved as a csv file
pres <- read_csv("PROBABILITY/pres.csv")

## and then re-estimate the regression, fit1
## regression without an intercept; estimated slope is identical
fit1 <- lm(Obama2012.z ~ -1 + Obama2008.z, data = pres)
fit1


## ---------------------------------------------------------
## see the page reference above
## `Obama2012.z' is Obama's 2012 standardized vote share
## `Obama2008.z' is Obama's 2008 standardized vote share
fit1


## ---- out.width="45%", fig.show='hold'--------------------
par(cex = 1.5)
e <- resid(fit1)
## z-score of residuals
e.zscore <- scale(e)
## alternatively we can divide residuals by their standard deviation
e.zscore <- e / sd(e)
hist(e.zscore, freq = FALSE, ylim = c(0, 0.4),
     xlab = "Standardized residuals",
     main = "Distribution of standardized residuals")
x <- seq(from = -3, to = 3, by = 0.01)
lines(x, dnorm(x)) # overlay the normal density
qqnorm(e.zscore, xlim = c(-3, 3), ylim = c(-3, 3)) # quantile-quantile plot
abline(0, 1) # 45 degree line


## ---------------------------------------------------------
e.sd <- sd(e)
e.sd


## ---------------------------------------------------------
CA.2008 <- filter(pres, state == "CA") %>% 
  select(Obama2008.z) %>% 
  pull()
CA.2008
CA.mean2012 <- coef(fit1) * CA.2008
CA.mean2012
## area to the right; greater than CA.2008
pnorm(CA.2008, mean = CA.mean2012, sd = e.sd, lower.tail = FALSE)


## ---------------------------------------------------------
TX.2008 <- filter(pres, state == "TX") %>% 
  select(Obama2008.z) %>% 
  pull()
TX.mean2012 <- coef(fit1) * TX.2008
TX.mean2012
pnorm(TX.2008, mean = TX.mean2012, sd = e.sd, lower.tail = FALSE)


## ---------------------------------------------------------
## theoretical variance: p was set to 0.5 earlier
p * (1-p)
## sample variance using `y' generated earlier
var(y)


## ----message = FALSE--------------------------------------
# load the data
data("pres08", package = "qss")
# Add variable for Obama vote share
pres08 <- pres08 %>%
  mutate(p = Obama / (Obama + McCain))


## ---------------------------------------------------------
sim_election <- function(.id, df, n_draws = 1000) {
  # For each state randomly sample
  mutate(df, draws = rbinom(n(), n_draws, p)) %>%
  filter(draws > (n_draws / 2)) %>%
  summarize(EV = sum(EV),
            .id = .id)
}


## ---- cache = TRUE----------------------------------------
sims <- 10000
sim_results <- map_df(seq_len(sims), 
                      ~ sim_election(.x, pres08, n_draws = 1000))


## ---------------------------------------------------------
ggplot(sim_results, aes(x = EV, y = ..density..)) +
  geom_histogram(binwidth = 12) +
  geom_vline(xintercept = 364, color = "blue", size = 0.5) +
  labs(x = "Obama’s Electoral College votes", y = "Density")


## ---------------------------------------------------------
sim_results %>%
  select(EV) %>%
  summarize_all(list(mean = mean, 
                     median = median,
                     var = var, 
                     sd = sd))


## ---------------------------------------------------------
## probability of binomial random variable taking greater than n/2 votes
n_draws <- 1000

pres08 %>%
  mutate(pb = pbinom(n_draws / 2, size = n_draws, prob = p,
                     lower.tail  = FALSE)) %>%
  summarize(mean = sum(pb * EV))



## ---------------------------------------------------------
var_sd <- pres08 %>% 
  mutate(pb = pbinom(n_draws / 2, size = n_draws, prob = p,
                     lower.tail  = FALSE)) %>%
  summarize(V = sum(pb * (1 - pb) * EV ^ 2),
            sd = sqrt(V))


## ---------------------------------------------------------
sims <- 1000
p <- 0.2
size <- 10
## Putting the results into a tibble
lln_binom <- tibble(
  n = seq_len(sims),
  x = rbinom(sims, prob = p, size = size),
  mean = cumsum(x) / n,
  distrib = str_c("Binomial(", size, ", ", p, ")"))

## look at the last few rows
tail(lln_binom)


## ---------------------------------------------------------
lln_unif <-
 tibble(n = seq_len(sims),
        x = runif(sims),
        mean = cumsum(x) / n,
        distrib = str_c("Uniform(0, 1)"))

tail(lln_unif)




## ----out.width="45%", fig.show='hold'---------------------
## Binomial
ggplot(data = lln_binom) +
  geom_line(aes(x = n, y = mean)) +
  geom_hline(yintercept =  2, lty = "dashed") + # expectation
  labs(x = "Sample size", y = "Sample Mean",
       title = "Binomial (10, 0.2)")
## Uniform
ggplot(data = lln_unif) +
  geom_line(aes(x = n, y = mean)) +
  geom_hline(yintercept =  0.5, lty = "dashed") + # expectation
  labs(x = "Sample size", y = "Sample Mean",
       title = "Uniform (0, 1)")








## ---- fig.show='hold', out.width="45%"--------------------
sims <- 1000
n.samp <- 1000
z.binom <- z.unif <- rep(NA, sims)
for (i in 1:sims) {
    x <- rbinom(n.samp, p = 0.2, size = 10)
    z.binom[i] <- (mean(x) - 2) / sqrt(1.6 / n.samp)
    x <- runif(n.samp, min = 0, max = 1)
    z.unif[i] <- (mean(x) - 0.5) / sqrt(1 / (12 * n.samp))
}
## bind the results together
results <- tibble(z.binom = z.binom,
                  z.unif = z.unif,
                  n.samp = seq(1:n.samp))

## plot the results
## binomial
ggplot(results) +
  geom_histogram(aes(x = z.binom, y = ..density..),
                 bins = 20) +
  stat_function(fun = dnorm, color = "blue") +
  labs(x = "z-score",
       y = "Density",
       title = "Binomial (0.2, 10)")

## uniform
ggplot(results) +
  geom_histogram(aes(x = z.unif, y = ..density..),
                 bins = 20) +
  stat_function(fun = dnorm, color = "blue") +
  labs(x = "z-score",
       y = "Density",
       title = "Uniform (0, 1)")


## ---- fig.show='hold', out.width="45%", echo = FALSE------
sims <- 100
n.samp <- 100
z.binom <- z.unif <- rep(NA, sims)
for (i in 1:sims) {
    x <- rbinom(n.samp, p = 0.2, size = 10)
    z.binom[i] <- (mean(x) - 2) / sqrt(1.6 / n.samp)
    x <- runif(n.samp, min = 0, max = 1)
    z.unif[i] <- (mean(x) - 0.5) / sqrt(1 / (12 * n.samp))
}
## bind the results together
results <- tibble(z.binom = z.binom,
                  z.unif = z.unif,
                  n.samp = seq(1:n.samp))

## plot the results
## binomial
ggplot(results) +
  geom_histogram(aes(x = z.binom, y = ..density..),
                 bins = 20) +
  stat_function(fun = dnorm, color = "blue") +
  labs(x = "z-score",
       y = "Density",
       title = "Binomial (0.2, 10)")

## uniform
ggplot(results) +
  geom_histogram(aes(x = z.unif, y = ..density..),
                 bins = 20) +
  stat_function(fun = dnorm, color = "blue") +
  labs(x = "z-score",
       y = "Density",
       title = "Uniform (0, 1)")

