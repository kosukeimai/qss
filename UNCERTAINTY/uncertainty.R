## ------------------------------------------------------------------------
## simulation parameters
n <- 100 # sample size
mu0 <- 0 # mean of Y_i(0)
sd0 <- 1 # standard deviation of Y_i(0)
mu1 <- 1 # mean of Y_i(1)
sd1 <- 1 # standard deviation of Y_i(1)

## generate a sample
Y0 <- rnorm(n, mean = mu0, sd = sd0)
Y1 <- rnorm(n, mean = mu1, sd = sd1)
tau <- Y1 - Y0 # individual treatment effect
## true value of the sample average treatment effect
SATE <- mean(tau)
SATE

## repeatedly conduct randomized controlled trials
sims <- 5000 # repeat 5,000 times, we could do more
diff.means <- rep(NA, sims)  # container

for (i in 1:sims) {
    ## randomize the treatment by sampling of a vector of 0's and 1's
    treat <- sample(c(rep(1, n / 2), rep(0, n / 2)), size = n, replace = FALSE)
    ## difference-in-means
    diff.means[i] <- mean(Y1[treat == 1]) - mean(Y0[treat == 0])
}

## estimation error for SATE
est.error <- diff.means - SATE
summary(est.error)

## PATE simulation
PATE <- mu1 - mu0
diff.means <- rep(NA, sims)  
for (i in 1:sims) {
    ## generate a sample for each simulation: this used to be outside of loop
    Y0 <- rnorm(n, mean = mu0, sd = sd0)
    Y1 <- rnorm(n, mean = mu1, sd = sd1)
    treat <- sample(c(rep(1, n / 2), rep(0, n / 2)), size = n, replace = FALSE)
    diff.means[i] <- mean(Y1[treat == 1]) - mean(Y0[treat == 0])
}

## estimation error for PATE
est.error <- diff.means - PATE

## unbiased 
summary(est.error) 

## ------------------------------------------------------------------------
hist(diff.means, freq = FALSE, xlab = "Difference-in-means estimator", 
     main = "Sampling distribution")
abline(v = SATE, col = "red") # true value of SATE
text(0.6, 2.4, "true SATE", col = "red")

sd(diff.means)
sqrt(mean((diff.means - SATE)^2))

## PATE simulation with standard error
sims <- 5000 
diff.means <- se <- rep(NA, sims)  # container for standard error added
for (i in 1:sims) {
    ## generate a sample
    Y0 <- rnorm(n, mean = mu0, sd = sd0)
    Y1 <- rnorm(n, mean = mu1, sd = sd1)
    ## randomize the treatment by sampling of a vector of 0's and 1's
    treat <- sample(c(rep(1, n / 2), rep(0, n / 2)), size = n, replace = FALSE)
    diff.means[i] <- mean(Y1[treat == 1]) - mean(Y0[treat == 0])
    ## standard error
    se[i] <- sqrt(var(Y1[treat == 1]) / (n / 2) + var(Y0[treat == 0]) / (n / 2)) 
}

## standard deviation of difference-in-means
sd(diff.means)
## mean of standard errors
mean(se) 

## ------------------------------------------------------------------------
n <- 1000 # sample size
x.bar <- 0.6 # point estimate
s.e. <- sqrt(x.bar * (1 - x.bar) / n) # standard error
## 99% confidence intervals
c(x.bar - qnorm(0.995) * s.e., x.bar + qnorm(0.995) * s.e.)
## 95% confidence intervals
c(x.bar - qnorm(0.975) * s.e., x.bar + qnorm(0.975) * s.e.)
## 90% confidence intervals
c(x.bar - qnorm(0.95) * s.e., x.bar + qnorm(0.95) * s.e.)

## empty container matrices for 2 sets of confidence intervals
ci95 <- ci90 <- matrix(NA, ncol = 2, nrow = sims)
## 95 percent confidence intervals
ci95[, 1] <- diff.means - qnorm(0.975) * se # lower limit
ci95[, 2] <- diff.means + qnorm(0.975) * se # upper limit

## 90 percent confidence intervals
ci90[, 1] <- diff.means - qnorm(0.95) * se # lower limit
ci90[, 2] <- diff.means + qnorm(0.95) * se # upper limit

## coverage rate for 95% confidence interval
mean(ci95[, 1] <= 1 & ci95[, 2] >= 1)
## coverage rate for 90% confidence interval
mean(ci90[, 1] <= 1 & ci90[, 2] >= 1)

p <- 0.6 # true parameter value
n <- c(50, 100, 1000) # 3 sample sizes to be examined
alpha <- 0.05 
sims <- 5000 # number of simulations
results <- rep(NA, length(n)) # a container for results

## loop for different sample sizes
for (i in 1:length(n)) { 
    ci.results <- rep(NA, sims) # a container for whether CI includes truth
    ## loop for repeated hypothetical survey sampling
    for (j in 1:sims) {
        data <- rbinom(n[i], size = 1, prob = p) # simple random sampling
        x.bar <- mean(data) # sample proportion as an estimate
        s.e. <- sqrt(x.bar * (1 - x.bar) / n[i]) # standard errors
        ci.lower <- x.bar - qnorm(1 - alpha/2) * s.e.
        ci.upper <- x.bar + qnorm(1 - alpha/2) * s.e.
        ci.results[j] <- (p >= ci.lower) & (p <= ci.upper) 
    }
    ## proportion of CIs that contain the true value
    results[i] <- mean(ci.results)
}
results

## ------------------------------------------------------------------------
MoE <- c(0.01, 0.03, 0.05)  # the desired margin of error
p <- seq(from = 0.01, to = 0.99, by = 0.01)
n <- 1.96^2 * p * (1 - p) / MoE[1]^2
plot(p, n, ylim = c(-1000, 11000), xlab = "Population proportion", 
     ylab = "Sample size", type = "l")
lines(p, 1.96^2 * p * (1 - p) / MoE[2]^2, lty = "dashed")
lines(p, 1.96^2 * p * (1 - p) / MoE[3]^2, lty = "dotted")
text(0.4, 10000, "margin of error = 0.01")
text(0.4, 1800, "margin of error = 0.03") 
text(0.6, -200, "margin of error = 0.05") 

## election and polling results, by state
pres08 <- read.csv("pres08.csv")
polls08 <- read.csv("polls08.csv")

## convert to a Date object
polls08$middate <- as.Date(polls08$middate)
## number of days to the election day
polls08$DaysToElection <- as.Date("2008-11-04") - polls08$middate

## create a matrix place holder
poll.pred <- matrix(NA, nrow = 51, ncol = 3)

## state names which the loop will iterate through
st.names <- unique(pres08$state)
## add labels for easy interpretation later on
row.names(poll.pred) <- as.character(st.names)

## loop across 50 states plus DC
for (i in 1:51){
    ## subset the ith state
    state.data <- subset(polls08, subset = (state == st.names[i]))
    ## subset the latest polls within the state
    latest <- state.data$DaysToElection == min(state.data$DaysToElection) 
    ## compute the mean of latest polls and store it
    poll.pred[i, 1] <- mean(state.data$Obama[latest]) / 100
}

## upper and lower confidence limits 
n <- 1000 # sample size
alpha <- 0.05
s.e. <- sqrt(poll.pred[, 1] * (1 - poll.pred[, 1]) / n) # standard error
poll.pred[, 2] <- poll.pred[, 1] - qnorm(1 - alpha/2) * s.e.
poll.pred[, 3] <- poll.pred[, 1] + qnorm(1 - alpha/2) * s.e.

alpha <- 0.05
plot(pres08$Obama / 100, poll.pred[, 1], xlim = c(0, 1), ylim = c(0, 1),
     xlab = "Obama's vote share", ylab = "Poll prediction")
abline(0, 1)

## adding 95% confidence intervals for each state
for (i in 1:51) {
    lines(rep(pres08$Obama[i] / 100, 2), c(poll.pred[i, 2], poll.pred[i, 3]))
}
## proportion of confidence intervals that contain the election day outcome
mean((poll.pred[, 2] <= pres08$Obama / 100) & 
         (poll.pred[, 3] >= pres08$Obama / 100))

## bias
bias <- mean(poll.pred[, 1] - pres08$Obama / 100)
bias

## bias corrected estimate
poll.bias <- poll.pred[, 1] - bias
## bias-corrected standard error 
s.e.bias <- sqrt(poll.bias * (1 - poll.bias) / n)
## bias-corrected 95% confidence interval
ci.bias.lower <- poll.bias - qnorm(1 - alpha / 2) * s.e.bias
ci.bias.upper <- poll.bias + qnorm(1 - alpha / 2) * s.e.bias

## proportion of bias-corrected CIs that contain the election day outcome
mean((ci.bias.lower <= pres08$Obama / 100) & 
         (ci.bias.upper >= pres08$Obama / 100))

## ------------------------------------------------------------------------
## read in data
STAR <- read.csv("STAR.csv", head = TRUE)
hist(STAR$g4reading[STAR$classtype == 1], freq = FALSE, xlim = c(500, 900),
     ylim = c(0, 0.01), main = "Small class",
     xlab = "Fourth-grade reading test score")
abline(v = mean(STAR$g4reading[STAR$classtype == 1], na.rm = TRUE), 
       col = "red")

hist(STAR$g4reading[STAR$classtype == 2], freq = FALSE, xlim = c(500, 900),
     ylim = c(0, 0.01), main = "Regular class",
     xlab = "Fourth-grade reading test score")
abline(v = mean(STAR$g4reading[STAR$classtype == 2], na.rm = TRUE), 
       col = "red")

## estimate and standard error for small class
n.small <- sum(STAR$classtype == 1 & !is.na(STAR$g4reading))
est.small <- mean(STAR$g4reading[STAR$classtype == 1], na.rm = TRUE)
se.small <- sd(STAR$g4reading[STAR$classtype == 1], na.rm = TRUE) / 
    sqrt(n.small)
est.small
se.small

## estimate and standard error for regular class
n.regular <- sum(STAR$classtype == 2 & !is.na(STAR$classtype) & 
                     !is.na(STAR$g4reading))
est.regular <- mean(STAR$g4reading[STAR$classtype == 2], na.rm = TRUE)
se.regular <- sd(STAR$g4reading[STAR$classtype == 2], na.rm = TRUE) / 
    sqrt(n.regular)
est.regular
se.regular

alpha <- 0.05
## 95% confidence intervals for small class
ci.small <- c(est.small - qnorm(1 - alpha / 2) * se.small, 
              est.small + qnorm(1 - alpha / 2) * se.small)
ci.small

## 95% confidence intervals for regular class
ci.regular <- c(est.regular - qnorm(1 - alpha / 2) * se.regular,
                est.regular + qnorm(1 - alpha / 2) * se.regular)
ci.regular

## difference-in-means estimator
ate.est <- est.small - est.regular
ate.est

## standard error and 95% confidence interval
ate.se <- sqrt(se.small^2 + se.regular^2)
ate.se

ate.ci <- c(ate.est - qnorm(1 - alpha / 2) * ate.se,
            ate.est + qnorm(1 - alpha / 2) * ate.se)
ate.ci

## ------------------------------------------------------------------------
## 95% CI for small class  
c(est.small - qt(0.975, df = n.small - 1) * se.small, 
  est.small + qt(0.975, df = n.small - 1) * se.small)

## 95% CI based on the central limit theorem
ci.small

## 95% CI for regular class
c(est.regular - qt(0.975, df = n.regular - 1) * se.regular, 
  est.regular + qt(0.975, df = n.regular - 1) * se.regular)

## 95% CI based on the central limit theorem
ci.regular

t.ci <- t.test(STAR$g4reading[STAR$classtype == 1], 
               STAR$g4reading[STAR$classtype == 2])
t.ci

## ------------------------------------------------------------------------
## truth: enumerate the number of assignment combinations
true <- c(choose(4, 0) * choose(4, 4), 
          choose(4, 1) * choose(4, 3), 
          choose(4, 2) * choose(4, 2), 
          choose(4, 3) * choose(4, 1), 
          choose(4, 4) * choose(4, 0))
true

## compute probability: divide it by the total number of events
true <- true / sum(true) 
## number of correctly classified cups as labels
names(true) <- c(0, 2, 4, 6, 8)
true

## simulations
sims <- 1000
## lady's guess: M stands for `Milk first,' T stands for `Tea first'
guess <- c("M", "T", "T", "M", "M", "T", "T", "M") 
correct <- rep(NA, sims) # place holder for number of correct guesses
for (i in 1:sims) {
    ## randomize which cups get Milk/Tea first
    cups <- sample(c(rep("T", 4), rep("M", 4)), replace = FALSE)
    correct[i] <- sum(guess == cups) # number of correct guesses
}
## estimated probability for each number of correct guesses
prop.table(table(correct)) 
## comparison with analytical answers; the differences are small
prop.table(table(correct)) - true

## ------------------------------------------------------------------------
## all correct
x <- matrix(c(4, 0, 0, 4), byrow = TRUE, ncol = 2, nrow = 2)
## six correct
y <- matrix(c(3, 1, 1, 3), byrow = TRUE, ncol = 2, nrow = 2)

## `M' milk first, `T' tea first
rownames(x) <- colnames(x) <- rownames(y) <- colnames(y) <- c("M", "T")
x
y

## one-sided test for 8 correct guesses
fisher.test(x, alternative = "greater") 

## two-sided test for 6 correct guesses
fisher.test(y) 

### Section 7.2.3: One-Sample Tests

n <- 1018
x.bar <- 550 / n
se <- sqrt(0.5 * 0.5 / n) # standard deviation of sampling distribution

## upper red area in the figure
upper <- pnorm(x.bar, mean = 0.5, sd = se, lower.tail = FALSE)  

## lower red area in the figure; identical to the upper area
lower <- pnorm(0.5 - (x.bar - 0.5), mean = 0.5, sd = se)  

## two-side p-value
upper + lower

2 * upper

## one-sided p-value
upper

z.score <- (x.bar - 0.5) / se
z.score

pnorm(z.score, lower.tail = FALSE) # one-sided p-value
2 * pnorm(z.score, lower.tail = FALSE) # two-sided p-value

## 99% confidence interval contains 0.5
c(x.bar - qnorm(0.995) * se, x.bar + qnorm(0.995) * se)

## 95% confidence interval does not contain 0.5
c(x.bar - qnorm(0.975) * se, x.bar + qnorm(0.975) * se)

## no continuity correction to get the same p-value as above
prop.test(550, n = n, p = 0.5, correct = FALSE)

## with continuity correction
prop.test(550, n = n, p = 0.5)
prop.test(550, n = n, p = 0.5, conf.level = 0.99)

## two-sided one-sample t-test
t.test(STAR$g4reading, mu = 710)

## ------------------------------------------------------------------------
## one-sided p-value
pnorm(-abs(ate.est), mean = 0, sd = ate.se)
## two-sided p-value
2 * pnorm(-abs(ate.est), mean = 0, sd = ate.se)

## testing the null of zero average treatment effect
t.test(STAR$g4reading[STAR$classtype == 1], 
       STAR$g4reading[STAR$classtype == 2])

resume <- read.csv("resume.csv")

## organize the data in tables
x <- table(resume$race, resume$call)
x

## one-sided test 
prop.test(x, alternative = "greater")

## sample size
n0 <- sum(resume$race == "black")
n1 <- sum(resume$race == "white")

## sample proportions
p <- mean(resume$call) # overall
p0 <- mean(resume$call[resume$race == "black"]) # black
p1 <- mean(resume$call[resume$race == "white"]) # white

## point estimate
est <- p1 - p0
est

## standard error
se <- sqrt(p * (1 - p) * (1 / n0 + 1 / n1))
se

## z-statistic
zstat <- est / se
zstat

## one-sided p-value
pnorm(-abs(zstat))

prop.test(x, alternative = "greater", correct = FALSE) 

## ------------------------------------------------------------------------
## set the parameters
n <- 250
p.star <- 0.48 # data generating process
p <- 0.5 # null value
alpha <- 0.05

## critical value
cr.value <- qnorm(1 - alpha / 2) 

## standard errors under the hypothetical data generating process
se.star <- sqrt(p.star * (1 - p.star) / n)

## standard error under the null
se <- sqrt(p * (1 - p) / n)  

## power
pnorm(p - cr.value * se, mean = p.star, sd = se.star) + 
    pnorm(p + cr.value * se, mean = p.star, sd = se.star, lower.tail = FALSE)

## parameters
n1 <- 500
n0 <- 500
p1.star <- 0.05
p0.star <- 0.1

## overall call back rate as a weighted average
p <- (n1 * p1.star + n0 * p0.star) / (n1 + n0) 
## standard error under the null
se <- sqrt(p * (1 - p) * (1 / n1 + 1 / n0)) 
## standard error under the hypothetical data generating process
se.star <- sqrt(p1.star * (1 - p1.star) / n1 + p0.star * (1 - p0.star) / n0)

pnorm(-cr.value * se, mean = p1.star - p0.star, sd = se.star) + 
    pnorm(cr.value * se, mean = p1.star - p0.star, sd = se.star, 
          lower.tail = FALSE)

power.prop.test(n = 500, p1 = 0.05, p2 = 0.1, sig.level = 0.05)
power.prop.test(p1 = 0.05, p2 = 0.1, sig.level = 0.05, power = 0.9)
power.t.test(n = 100, delta = 0.25, sd = 1, type = "one.sample")
power.t.test(power = 0.9, delta = 0.25, sd = 1, type = "one.sample")
power.t.test(delta = 0.25, sd = 1, type = "two.sample", 
             alternative = "one.sided", power = 0.9)

## ------------------------------------------------------------------------
minwage <- read.csv("minwage.csv")

## compute proportion of full employment before minimum wage increase
minwage$fullPropBefore <- minwage$fullBefore /
    (minwage$fullBefore + minwage$partBefore)

## same thing after minimum wage increase
minwage$fullPropAfter <- minwage$fullAfter /
    (minwage$fullAfter + minwage$partAfter)

## an indicator for NJ: 1 if it's located in NJ and 0 if in PA
minwage$NJ <- ifelse(minwage$location == "PA", 0, 1)

fit.minwage <- lm(fullPropAfter ~ -1 + NJ + fullPropBefore + 
                      wageBefore + chain, data = minwage)

## regression result
fit.minwage

fit.minwage1 <- lm(fullPropAfter ~ NJ + fullPropBefore + 
                       wageBefore + chain, data = minwage)
fit.minwage1

predict(fit.minwage, newdata = minwage[1, ])
predict(fit.minwage1, newdata = minwage[1, ])

## ------------------------------------------------------------------------
women <- read.csv("women.csv")
fit.women <- lm(water ~ reserved, data = women)
summary(fit.women)

confint(fit.women) # 95% confidence intervals

summary(fit.minwage)

## confidence interval just for the `NJ' variable
confint(fit.minwage)["NJ", ] 

## ------------------------------------------------------------------------
## load the data and subset them into two parties
MPs <- read.csv("MPs.csv")
MPs.labour <- subset(MPs, subset = (party == "labour")) 
MPs.tory <- subset(MPs, subset = (party == "tory"))

## two regressions for labour: negative and positive margin
labour.fit1 <- lm(ln.net ~ margin,
                  data = MPs.labour[MPs.labour$margin < 0, ])
labour.fit2 <- lm(ln.net ~ margin,
                  data = MPs.labour[MPs.labour$margin > 0, ]) 

## two regressions for tory: negative and positive margin
tory.fit1 <- lm(ln.net ~ margin, data = MPs.tory[MPs.tory$margin < 0, ])
tory.fit2 <- lm(ln.net ~ margin, data = MPs.tory[MPs.tory$margin > 0, ])

## tory party: prediction at the threshold
tory.y0 <- predict(tory.fit1, interval = "confidence", 
                   newdata = data.frame(margin = 0))
tory.y0

tory.y1 <- predict(tory.fit2, interval = "confidence", 
                   newdata = data.frame(margin = 0))
tory.y1

## range of predictors; min to 0 and 0 to max
y1.range <- seq(from = 0, to = min(MPs.tory$margin), by = -0.01)
y2.range <- seq(from = 0, to = max(MPs.tory$margin), by = 0.01)

## prediction using all the values
tory.y0 <- predict(tory.fit1, interval = "confidence", 
                   newdata = data.frame(margin = y1.range))
tory.y1 <- predict(tory.fit2, interval = "confidence", 
                   newdata = data.frame(margin = y2.range))

## plotting the first regression with losers
plot(y1.range, tory.y0[, "fit"], type = "l", xlim = c(-0.5, 0.5),
     ylim = c(10, 15), xlab = "Margin of victory", ylab = "log net wealth")
abline(v = 0, lty = "dotted")
lines(y1.range, tory.y0[, "lwr"], lty = "dashed") # lower CI
lines(y1.range, tory.y0[, "upr"], lty = "dashed") # upper CI

## plotting the second regression with winners
lines(y2.range, tory.y1[, "fit"], lty = "solid")  # point estimates
lines(y2.range, tory.y1[, "lwr"], lty = "dashed") # lower CI 
lines(y2.range, tory.y1[, "upr"], lty = "dashed") # upper CI

## re-compute the predicted value and return standard errors
tory.y0 <- predict(tory.fit1, interval = "confidence", se.fit = TRUE, 
                   newdata = data.frame(margin = 0))
tory.y0

tory.y1 <- predict(tory.fit2, interval = "confidence", se.fit = TRUE,
                   newdata = data.frame(margin = 0))

## s.e. of the intercept is the same as s.e. of the predicted value
summary(tory.fit1)

## standard error
se.diff <- sqrt(tory.y0$se.fit^2 + tory.y1$se.fit^2)
se.diff

## point estimate
diff.est <- tory.y1$fit[1, "fit"] - tory.y0$fit[1, "fit"]
diff.est

## confidence interval
CI <- c(diff.est - se.diff * qnorm(0.975), diff.est + se.diff * qnorm(0.975))
CI

## hypothesis test
z.score <- diff.est / se.diff
p.value <- 2 * pnorm(abs(z.score), lower.tail = FALSE) # two-sided p-value
p.value

