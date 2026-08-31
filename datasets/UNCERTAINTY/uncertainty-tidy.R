## ----echo = FALSE, warning=FALSE, message=FALSE-----------
library(tidyverse)
set.seed(1234)


## ---------------------------------------------------------
## simulation parameters
n <- 100 # sample size
mu0 <- 0 # mean of Y_i(0) [not treated]
sd0 <- 1 # standard deviation of Y_i(0)
mu1 <- 1 # mean of Y_i(1) [treated]
sd1 <- 1 # standard deviation of Y_i(1)

## generate a sample as a tibble
smpl <- tibble(id = seq_len(n),
               # Y if not treated
               Y0 = rnorm(n, mean = mu0, sd = sd0),
               # Y if treated
               Y1 = rnorm(n, mean = mu1, sd = sd1),
               # individual treatment effect
               tau = Y1 - Y0)

## true value of the sample average treatment effect
SATE <- smpl %>% select(tau) %>% summarize(SATE = mean(tau)) %>% pull()
SATE


## ----message=FALSE----------------------------------------
sim_treat <- function(smpl) {
  n <- nrow(smpl)
  # indexes of obs receiving treatment (randomly assign to half)
  idx <- sample(seq_len(n), floor(nrow(smpl) / 2), replace = FALSE)
  # "treat" variable is 1 for those receiving treatment, else 0
  smpl[["treat"]] <- as.integer(seq_len(nrow(smpl)) %in% idx)
  smpl %>%
    # what outcome we observe for unit based on treatment assignment
    mutate(Y_obs = if_else(treat == 1, Y1, Y0)) %>%
    group_by(treat) %>%
    summarize(Y_obs = mean(Y_obs)) %>%
    pivot_wider(names_from = treat, 
                values_from = Y_obs) %>%
    rename(Y1_mean = `1`, Y0_mean = `0`) %>%
    mutate(diff_mean = Y1_mean - Y0_mean,
           est_error = diff_mean - SATE)
}
## show the results of the function on the data
## values will differ each time it is run
sim_treat(smpl)


## ---- message=FALSE, cache=TRUE---------------------------
## number of simulations
sims <- 500
## run the created function sims times
sate_sims <- map_df(seq_len(sims), ~ sim_treat(smpl))
## what is the distribution of the error? 
summary(sate_sims$est_error)


## ---- message=FALSE, warning=FALSE, cache=TRUE------------
PATE <- mu1 - mu0
PATE
## Update the function for PATE instead of SATE
sim_pate <- function(n, mu0, mu1, sd0, sd1) {
  smpl <- tibble(Y0 = rnorm(n, mean = mu0, sd = sd0),
                 Y1 = rnorm(n, mean = mu1, sd = sd1),
                 tau = Y1 - Y0)
  # indexes of obs receiving treatment
  idx <- sample(seq_len(n), floor(nrow(smpl) / 2), replace = FALSE)
  # treat variable are those receiving treatment, else 0
  smpl[["treat"]] <- as.integer(seq_len(nrow(smpl)) %in% idx)
  smpl %>%
    mutate(Y_obs = if_else(treat == 1L, Y1, Y0)) %>%
    group_by(treat) %>%
    summarize(Y_obs = mean(Y_obs)) %>%
    pivot_wider(names_from = treat, 
                values_from = Y_obs) %>%
    rename(Y1_mean = `1`, Y0_mean = `0`) %>%
    mutate(diff_mean = Y1_mean - Y0_mean,
           est_error = diff_mean - PATE)
}
## number of simulations
sims <- 500
## run the created function sims times
## input values are defined above
pate_sims <-  map_df(seq_len(sims), ~ sim_pate(n, mu0, mu1, sd0, sd1))
## what is the distribution of the error? 
summary(pate_sims$est_error)




## ---------------------------------------------------------
ggplot(pate_sims, aes(x = diff_mean, y = ..density..)) +
  geom_histogram(binwidth = 0.1) +
  geom_vline(xintercept = PATE, color = "black", size = 0.5) +
  ggtitle("Sampling distribution") +
  labs(x = "Difference-in-means estimator", y = "Density")


## ---------------------------------------------------------
## the standard deviation of the difference in means
pate_sims %>% 
  select(diff_mean) %>% 
  summarize(sd = sd(diff_mean))


## ---------------------------------------------------------
RMSE <- pate_sims %>% 
  summarize(rmse = sqrt(mean(est_error)^2))
RMSE


## ----message=FALSE, warning=FALSE-------------------------
## PATE simulation with standard error
sim_pate_se <- function(n, mu0, mu1, sd0, sd1) {
  # PATE - difference in means
  PATE <- mu1 - mu0
  # sample
  smpl <- tibble(Y0 = rnorm(n, mean = mu0, sd = sd0),
                 Y1 = rnorm(n, mean = mu1, sd = sd1),
                 tau = Y1 - Y0)
  # indexes of obs receiving treatment
  idx <- sample(seq_len(n), floor(nrow(smpl) / 2), replace = FALSE)
  # treat variable are those receiving treatment, else 0
  smpl[["treat"]] <- as.integer(seq_len(nrow(smpl)) %in% idx)
  # sample
  smpl %>%
    mutate(Y_obs = if_else(treat == 1, Y1, Y0)) %>%
    group_by(treat) %>%
    summarize(mean = mean(Y_obs),
              var = var(Y_obs),
              nobs = n()) %>%
    summarize(diff_mean = diff(mean),
              se = sqrt(sum(var / nobs)),
              est_error = diff_mean - PATE)
}
## test a single simulation
sim_pate_se(n, mu0, mu1, sd0, sd1)
## run 500 times
sims <- 500
pate_sims_se <- map_df(seq_len(sims), ~ sim_pate_se(n, mu0, mu1, sd0, sd1))

## standard deviation of difference-in-means
## and mean of standard errors
sd_se <- pate_sims_se %>% 
  summarize(sd = sd(diff_mean),
            mean_se = mean(se))
sd_se


## ----criticalvals, echo=FALSE, fig.cap='(ref:critical-cap)', fig.scap = "Critical value based on the standard normal distribution"----
par(cex = 1.5) 
x <- seq(from = -4, to = 4, by = 0.01)
alpha <- 0.05
lwd <- 1.5
plot(x, dnorm(x), xlab = "", ylab = "Density", type = "l", axes = FALSE,
     main = "", ylim = c(0, 0.5))
polygon(x = c(rep(qnorm(alpha/2), 2), 
            seq(from = qnorm(alpha/2), to = qnorm(1-alpha/2), by = 0.01),
            rep(qnorm(1-alpha/2), 2)),
        y = c(0, dnorm(qnorm(alpha/2)), 
            dnorm(seq(from = qnorm(alpha/2), to = qnorm(1-alpha/2), by = 0.01)),
            dnorm(qnorm(1-alpha/2)), 0), density = 100, col = "red")
abline(v = 0)
axis(side = 1, at = c(qnorm(alpha/2), 0, qnorm(1-alpha/2)), 
     labels = c(expression(-z[alpha/2]), 0, expression(z[alpha/2])))
axis(2)


## ---------------------------------------------------------
## set the sample size
n <- 1000
## set the point estimate
x_bar <- 0.6
## calculate the standard error
se <- sqrt(x_bar * (1 - x_bar) / n)
## set the desired Confidence levels
levels <- c(0.99, 0.95, 0.90)
## build a tibble to calculate the ci at each level
tibble(level = levels) %>%
  mutate(
    ci_lower = x_bar - qnorm(1 - (1 - level) / 2) * se,
    ci_upper = x_bar + qnorm(1 - (1 - level) / 2) * se
  )



## ---------------------------------------------------------
## initial confidence level
level <- 0.95
## CI at that level for the PATE simulations with standard errors
pate_sims_ci <- pate_sims_se %>%
  mutate(ci_lower = diff_mean - qnorm(1 - (1 - level) / 2) * se,
         ci_upper = diff_mean + qnorm(1 - (1 - level) / 2) * se,
         includes_pate = PATE > ci_lower & PATE < ci_upper)
## view a subset of the CIs
glimpse(pate_sims_ci)

## compute the rate of PATE coverage 
pate_sims_ci %>% 
  summarize(coverage = mean(includes_pate))


## ---------------------------------------------------------
pate_sims_coverage <- function(.data, level = 0.95) {
  mutate(.data,
         ci_lower = diff_mean - qnorm(1 - (1 - level) / 2) * se,
         ci_upper = diff_mean + qnorm(1 - (1 - level) / 2) * se,
         includes_pate = PATE > ci_lower & PATE < ci_upper) %>%
    summarize(coverage = mean(includes_pate))
}

pate_sims_coverage(pate_sims_se, level = 0.95)
pate_sims_coverage(pate_sims_se, level = 0.99)
pate_sims_coverage(pate_sims_se, level = 0.90)



## ---------------------------------------------------------
## Function to test if CI contains true parameter value
binom_ci_contains <- function(n, p, alpha = 0.05) {
  x <- rbinom(n, size = 1, prob = p)
  x_bar <- mean(x)
  se <- sqrt(x_bar * (1 - x_bar) / n)
  ci_lower <- x_bar - qnorm(1 - alpha / 2) * se
  ci_upper <- x_bar + qnorm(1 - alpha / 2) * se
  (ci_lower <= p) & (p <= ci_upper)
}
## Demonstrate the function
p <- 0.6 # true parameter value
n <- 10
binom_ci_contains(n = n, p = p, alpha = 0.05)


## ---------------------------------------------------------
## Show coverage by taking the average of the logical result for each sim
mean(map_lgl(seq_len(sims), ~ binom_ci_contains(n, p)))


## ---------------------------------------------------------
## Function to calculate CI coverage while varying number of simulations
binom_ci_coverage <- function(n, p, sims) {
  mean(map_lgl(seq_len(sims), ~ binom_ci_contains(n, p)))
}
## Apply the function to a range of simulations values
tibble(n = c(10, 100, 1000)) %>%
  mutate(coverage = map_dbl(n, binom_ci_coverage, 
                            p = p, 
                            sims = sims))


## ---------------------------------------------------------
## First, write a function to find population proportion give MoE
moe_pop_prop <- function(MoE) {
  tibble(p = seq(from = 0.01, to = 0.99, by = 0.01),
         n = 1.96 ^ 2 * p * (1 - p) / MoE ^ 2,
         MoE = MoE)
}
glimpse(moe_pop_prop(0.01))

## Then use map_df to call the function for a range of MoEs
MoE <- c(0.01, 0.03, 0.05)
props <- map_df(MoE, moe_pop_prop)

## plot the results
ggplot(props, aes(x = p, y = n, color = factor(MoE))) +
  geom_line(aes(linetype = factor(MoE))) +
  labs(color = "Margin of error",
       linetype = "Margin of error",
       x = "Population proportion",
       y = "Sample size") +
  theme(legend.position = "bottom")


## ---- message=FALSE, warning=FALSE------------------------
## load required library
library(lubridate)
## load final vote shares
data("pres08", package = "qss")
## load polling data
data("polls08", package = "qss")
## set the election date
ELECTION_DATE <- ymd(20081104)
## Add days to election variable
polls08 <- polls08 %>%
  mutate(DaysToElection = as.integer(ELECTION_DATE - middate))
## Calculate mean of latest polls by state
poll_pred <- polls08 %>%
  group_by(state) %>%
  # latest polls in the state
  filter(DaysToElection == min(DaysToElection)) %>%
  # take mean of latest polls and convert from 0-100 to 0-1
  summarize(Obama = mean(Obama) / 100)
## Add confidence intervals
## sample size (assumed)
sample_size <- 1000
# confidence level
alpha <- 0.05
## Add the CIs and se
poll_pred <- poll_pred %>%
  mutate(se = sqrt(Obama * (1 - Obama) / sample_size),
         ci_lwr = Obama + qnorm(alpha / 2) * se,
         ci_upr = Obama + qnorm(1 - alpha / 2) * se)
## Add actual outcome
## And check if coverage includes the actual result
poll_pred <-left_join(poll_pred,
            select(pres08, state, actual = Obama),
            by = "state") %>%
  mutate(actual = actual / 100,
         covers = (ci_lwr <= actual) & (actual <= ci_upr))
## Check the results
glimpse(poll_pred)


## ---------------------------------------------------------
## Plot the results
ggplot(poll_pred, aes(x = actual, y = Obama)) +
  geom_abline(intercept = 0, slope = 1, color = "black", size = 0.5) +
  geom_pointrange(aes(ymin = ci_lwr, ymax = ci_upr),
                  shape = 1) +
  scale_y_continuous("Poll prediction", limits = c(0, 1)) +
  scale_x_continuous("Obama's vote share", limits = c(0, 1)) +
  scale_color_discrete("CI includes result?") +
  coord_fixed()


## ---------------------------------------------------------
poll_pred %>%
  summarize(mean(covers))


## ---------------------------------------------------------
poll_pred <- poll_pred %>%
  # calculate the bias
  mutate(bias = Obama - actual) %>%
  # bias corrected prediction, se, and CI
  mutate(Obama_bc = Obama - mean(bias),
         se_bc = sqrt(Obama_bc * (1 - Obama_bc) / sample_size),
         ci_lwr_bc = Obama_bc + qnorm(alpha / 2) * se_bc,
         ci_upr_bc = Obama_bc + qnorm(1 - alpha / 2) * se_bc,
         covers_bc = (ci_lwr_bc <= actual) & (actual <= ci_upr_bc))
## Updated coverage rate
poll_pred %>%
  summarize(mean(covers_bc))


## ----message=FALSE----------------------------------------
## load the data
data("STAR", package = "qss")
## Add meaningful labels to the classtype variable:
STAR <- STAR %>%
  mutate(classtype = factor(classtype,
                            labels = c("Small class", "Regular class",
                                       "Regular class with aide")))
## Summarize scores by classroom type:
classtype_means <- STAR %>%
  group_by(classtype) %>%
  summarize(g4reading = mean(g4reading, na.rm = TRUE))

## Plot the distribution of scores by classroom type for two classroom types
classtypes_used <- c("Small class", "Regular class")
ggplot(filter(STAR,
              classtype %in% classtypes_used,
              !is.na(g4reading)),
       aes(x = g4reading, y = ..density..)) +
  geom_histogram(binwidth = 20) +
  geom_vline(data = filter(classtype_means, classtype %in% classtypes_used),
             mapping = aes(xintercept = g4reading),
             color = "blue", size = 0.5) +
  facet_grid(classtype ~ .) +
  labs(x = "Fourth grade reading score", y = "Density")


## ---------------------------------------------------------
## alpha for 95% confidence
alpha <- 0.05
## calculate the mean, se, and CIs
star_estimates <- STAR %>%
  filter(!is.na(g4reading),
         classtype %in% classtypes_used) %>%
  group_by(classtype) %>%
  summarize(n = n(),
            est = mean(g4reading),
            se = sd(g4reading) / sqrt(n)) %>%
  mutate(lwr = est + qnorm(alpha / 2) * se,
         upr = est + qnorm(1 - alpha / 2) * se)

star_estimates


## ---------------------------------------------------------
## difference-in-means estimator
star_ate <- star_estimates %>%
  # ensure that it is ordered small then regular
  arrange(desc(classtype)) %>%
  summarize(
    se = sqrt(sum(se ^ 2)),
    est = diff(est)
  ) %>%
  mutate(ci_lwr = est + qnorm(alpha / 2) * se,
         ci_up = est + qnorm(1 - alpha / 2) * se)

star_ate


## ----echo = FALSE, out.width='7.5cm', out.height='7.5cm', fig.show = 'hold'----
par(cex = 1.5)
x <- seq(from = -6, to = 6, by = 0.01)
plot(x, dnorm(x), xlab = "", ylab = "Density", type = "l", lwd = 2.5)
lines(x, dt(x, df = 2), lty = "dotted", col = "red", lwd = 2.5)
lines(x, dt(x, df = 10), lty = "dashed", col = "blue", lwd = 2.5)
lines(x, dt(x, df = 50), lty = "dotdash", col = "darkgreen", lwd = 2.5)
legend("topright", legend = c("normal", "df = 2", "df = 10", "df = 50"), 
       bty = "n", lty = c("solid", "dotted", "dashed", "dotdash"), 
       col = c("black", "red", "blue", "darkgreen"), lwd = 2.5)
y <- seq(from = 0.01, to = 0.99, by = 0.01)
plot(qnorm(y), qnorm(y), type = "l", lwd = 2.5, ylim = c(-4, 4),
     xlab = "Quantile of the standard normal distribution",
     ylab = "Quantile of the t-distribution")
lines(qnorm(y), qt(y, df = 2), lty = "dotted", col = "red", lwd = 2.5)
lines(qnorm(y), qt(y, df = 10), lty = "dashed", col = "blue", lwd = 2.5)
lines(qnorm(y), qt(y, df = 50), lty = "dotdash", col = "darkgreen", lwd = 2.5)
legend("bottomright", legend = c("normal", "df = 2", "df = 10", "df = 50"), 
       bty = "n", lty = c("solid", "dotted", "dashed", "dotdash"), 
       col = c("black", "red", "blue", "darkgreen"), lwd = 2.5)


## ---------------------------------------------------------
## alpha for 95% confidence
alpha <- 0.05
## calculate the mean, se, and CIs
star_estimates_t <- STAR %>%
  filter(!is.na(g4reading),
         classtype %in% classtypes_used) %>%
  group_by(classtype) %>%
  summarize(n = n(),
            est = mean(g4reading),
            se = sd(g4reading) / sqrt(n)) %>%
  mutate(lwr = est + qt(alpha / 2, df = n - 1) * se,
         upr = est + qt(1 - alpha / 2, df = n - 1) * se)

star_estimates_t
## compare to original
star_estimates


## ---------------------------------------------------------
## compare reading scores between small and regular classes
reading_small <-  filter(STAR, classtype == "Small class")$g4reading
reading_reg <- filter(STAR, classtype == "Regular class")$g4reading

t_ci <- t.test(reading_small, reading_reg)
       
t_ci


## ----ladytea, echo = FALSE, fig.cap="Sampling Distribution for the Lady Tasting Tea Experiment. The barplot shows the distribution of the number of correctly classified cups.", fig.scap="Sampling distribution for the tea-tasting experiment"----
par(cex = 1.5)
barplot(c(1, 16, 36, 16, 1) / 70, names.arg = c(0, 2, 4, 6, 8),
        xlab = "Number of correct guesses",
        ylab = "Probability", ylim = c(0, 0.6))


## ---------------------------------------------------------
## Number of cups of tea
cups <- 4
## Number guessed correctly
k <- c(0, seq_len(cups))
## Calculate probability correct
true <-  tibble(correct = k * 2,
                n = choose(cups, k) * choose(cups, cups - k)) %>%
  mutate(prob = n / sum(n))
true


## ---- cache=TRUE------------------------------------------
## Number of simulations
sims <- 1000
## The lady's guess (fixed); M for milk first; T for tea first
guess <- tibble(guess = c("M", "T", "T", "M", "M", "T", "T", "M"))

## A function to randomize the tea and calculate correct guesses
randomize_tea <- function(df) {
  # randomize the order of teas
  assignment <- sample_frac(df, 1) %>%
    rename(actual = guess)
  bind_cols(df, assignment) %>%
    summarize(correct = sum(guess == actual))
}

## Run the function 1000 times
approx <-
  map_df(seq_len(sims), ~ randomize_tea(guess)) %>%
  count(correct) %>%
  mutate(prob = n / sum(n))

## Then merge with the analytical solution
results <- approx %>% 
  select(correct, prob_sim = prob) %>% 
  left_join(select(true, correct, prob_exact = prob),
          by = "correct") %>%
  mutate(diff = prob_sim - prob_exact)

results


## ---------------------------------------------------------
## all correct
x <- matrix(c(4, 0, 0, 4), byrow = TRUE, ncol = 2, nrow = 2)
## six correct
y <- matrix(c(3, 1, 1, 3), byrow = TRUE, ncol = 2, nrow = 2)
## `M' milk first, `T' tea first
rownames(x) <- colnames(x) <- rownames(y) <- colnames(y) <- c("M", "T")
x
y


## ---------------------------------------------------------
## one-sided test for 8 correct guesses
fisher.test(x, alternative = "greater") 
## two-sided test for 6 correct guesses
fisher.test(y)


## ----pvalue, echo = FALSE, out.width='7.5cm', out.height='7.5cm', fig.show = 'hold', fig.align = "center", fig.cap="One-sided and Two-sided $p$-values. The density curve represents the reference distribution under the null hypothesis that the population proportion is 0.5. The observed value is indicated by the solid vertical line. The two-sided $p$-value equals the sum of the two red shaded areas under the curve whereas the one-sided $p$-value is equal to the one of the two red areas under the curve (depending on the alternative hypothesis).", fig.scap="One-sided and two-sided $p$-values"----
par(cex = 1.5) 
n <- 1018
x <- seq(from = 0.4, to = 0.6, by = 0.001)
alpha <- 0.05
lwd <- 1.5
se <- 0.5 / sqrt(n)
plot(x, dnorm(x, mean = 0.5, sd = se),
     xlab = "Sample proportion", ylab = "Density", type = "l", 
     main = "", xlim = c(0.4, 0.6))
abline(v = 0.54)
abline(v = 0.5, lty = "dotted")
abline(v = 0.46, lty = "dashed")
text(0.57, 17, "observed\n value")
polygon(x = c(rep(0.4, 2), 
            seq(from = 0.4, to = 0.46, by = 0.001),
            rep(0.46, 2)),
        y = c(0, dnorm(0.4, mean = 0.5, sd = se), 
            dnorm(seq(from = 0.4, to = 0.46, by = 0.001),
                  mean = 0.5, sd = se), 
            dnorm(0.46, mean = 0.5, sd = se), 0), 
        density = 100, col = "red")
polygon(x = c(rep(0.6, 2), 
            seq(from = 0.6, to = 0.54, by = -0.001),
            rep(0.54, 2)),
        y = c(0, dnorm(0.6, mean = 0.5, sd = se), 
            dnorm(seq(from = 0.6, to = 0.54, by = -0.001),
                  mean = 0.5, sd = se), 
            dnorm(0.54, mean = 0.5, sd = se), 0), 
        density = 100, col = "red")


## ---------------------------------------------------------
n <- 1018
x.bar <- 550 / n
se <- sqrt(0.5 * 0.5 / n) # standard deviation of sampling distribution
## upper red area in the figure
upper <- pnorm(x.bar, mean = 0.5, sd = se, lower.tail = FALSE)  
## lower red area in the figure; identical to the upper area
lower <- pnorm(0.5 - (x.bar - 0.5), mean = 0.5, sd = se)  
## two-side p-value
upper + lower


## ---------------------------------------------------------
2 * upper


## ---------------------------------------------------------
## one-sided p-value
upper


## ---------------------------------------------------------
z.score <- (x.bar - 0.5) / se
z.score
pnorm(z.score, lower.tail = FALSE) # one-sided p-value
2 * pnorm(z.score, lower.tail = FALSE) # two-sided p-value


## ---------------------------------------------------------
## 99% confidence interval contains 0.5
c(x.bar - qnorm(0.995) * se, x.bar + qnorm(0.995) * se)
## 95% confidence interval does not contain 0.5
c(x.bar - qnorm(0.975) * se, x.bar + qnorm(0.975) * se)


## ---------------------------------------------------------
## no continuity correction to get the same p-value as above
prop.test(550, n = n, p = 0.5, correct = FALSE)
## with continuity correction
prop.test(550, n = n, p = 0.5)


## ---------------------------------------------------------
prop.test(550, n = n, p = 0.5, conf.level = 0.99)


## ---------------------------------------------------------
# two-sided one-sample t-test
t.test(STAR$g4reading, mu = 710)


## ---------------------------------------------------------
star_ate %>%
  mutate(p_value_1sided = pnorm(-abs(est),
                                mean = 0, sd = se),
         p_value_2sided = 2 * pnorm(-abs(est), mean = 0,
                                    sd = se))



## ---------------------------------------------------------
## testing the null of zero average treatment effect
reading_small <- filter(STAR, classtype == "Small class")$g4reading
reading_reg <- filter(STAR, classtype == "Regular class")$g4reading
## t-test
t.test(reading_small,
       reading_reg)


## ---------------------------------------------------------
## load the data
data("resume", package = "qss")
## reshape the data
x <- resume %>%
  count(race, call) %>%
  pivot_wider(names_from = call, values_from = n) %>%
  ungroup()
x
## run the test on the relevant columns
prop.test(as.matrix(select(x, -race)), alternative = "greater")


## ---------------------------------------------------------
## sample size
n0 <- sum(resume$race == "black")
n1 <- sum(resume$race == "white")

## sample proportions
p <- mean(resume$call) # overall
p0 <- mean(filter(resume, race == "black")$call)
p1 <- mean(filter(resume, race == "white")$call)

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


## ---------------------------------------------------------
prop.test(as.matrix(select(x, -race)), alternative = "greater", correct = FALSE)


## ----journalpvalue, echo = FALSE, out.width='7.5cm', fig.align='center', out.height='7.5cm', fig.show = 'hold', fig.cap="The Distribution of $p$-values for Hypothesis Tests Published in Two Leading Political Science Journals.", fig.scap="The distribution of $p$-values in published studies"----
par(cex = 1.5)
onetail <- read.csv("UNCERTAINTY/onetailed.csv")
twotail <- read.csv("UNCERTAINTY/twotailed.csv")
pvalue.one <- 1 - pnorm(onetail$Z)
pvalue.two <- 2 * (1 - pnorm(abs(twotail$Z)))
pvalue <- c(pvalue.one, pvalue.two)
hist(pvalue, breaks = 20, freq = FALSE, xlab = "p-value",
     main = "")
abline(v = 0.05, col = "blue")


## ----poweranalysis, echo = FALSE, fig.show='hold', out.width = "45%", fig.cap='(ref:poweranalysis-cap)', fig.scap="Illustration of power analysis"----
par(cex = 1.5)    
alpha <- 0.05
n <- 250
p <- 0.48
se <- sqrt(p * (1-p) / n)
p0 <- 0.5
se0 <- sqrt(p0 * (1-p0) / n)
lower <- 0.35
upper <- 0.65
cr.value <- qnorm(1 - alpha / 2)
x <- seq(from = lower, to = upper, by = 0.001)  
plot(x, dnorm(x, mean = p, sd = se), type = "l",
     xlab = "Proportion, p*", ylab = "Density", col = "red", lwd = 2)
x1 <- seq(from = lower, to = p0 - cr.value * se0, by = 0.001)
polygon(x = c(x1, rep(p0 - cr.value * se0, 2), lower), 
        y = c(dnorm(x1, mean = p, sd = se), 
            dnorm(p0 - cr.value * se0, mean = p, sd = se),
            rep(0, 2)), 
        density = 100, col = "red")
x2 <- seq(from = upper, to = p0 + cr.value * se0, by = -0.001)
polygon(x = c(x2, rep(p0 + cr.value * se0, 2), upper), 
        y = c(dnorm(x2, mean = p, sd = se), 
            dnorm(p0 + cr.value * se0, mean = p, sd = se),
            rep(0, 2)), 
        density = 100, col = "red")
lines(x, dnorm(x, mean = p0, sd = se0), lwd = 2)
abline(v = p0 + cr.value * se0, lty = "longdash")
abline(v = p0 - cr.value * se0, lty = "longdash")
abline(v = p0, lty = "dotted")
p <- seq(from = 0.35, to = 0.65, by = 0.001)
plot(p, pnorm(p0 - cr.value * sqrt(p0 * (1 - p0) / n), 
              mean = p, sd = sqrt(p * (1 - p) / n)) + 
                  pnorm(p0 + cr.value * sqrt(p0 * (1 - p0) / n), 
                        mean = p, sd = sqrt(p * (1 - p) / n), 
                        lower.tail = FALSE), type = "l", ylab = "Power",
     ylim = c(0, 1), xlab = "Proportion, p*")
n <- 500
lines(p, pnorm(p0 - cr.value * sqrt(p0 * (1 - p0) / n), 
              mean = p, sd = sqrt(p * (1 - p) / n)) + 
                  pnorm(p0 + cr.value * sqrt(p0 * (1 - p0) / n), 
                        mean = p, sd = sqrt(p * (1 - p) / n), lower.tail = FALSE), 
      col = "red")
n <- 1000
lines(p, pnorm(p0 - cr.value * sqrt(p0 * (1 - p0) / n), 
              mean = p, sd = sqrt(p * (1 - p) / n)) + 
                  pnorm(p0 + cr.value * sqrt(p0 * (1 - p0) / n), 
                        mean = p, sd = sqrt(p * (1 - p) / n), lower.tail = FALSE), 
      col = "blue")
abline(h = 0.05, lty = "longdash")
abline(v = 0.5, lty = "dotted")
legend(x = 0.55, y = 0.6, legend = c("n = 250", "n = 500", "n = 1000"), col = c("black", "red", "blue"), 
       lty = "solid", bty = "n")


## ---------------------------------------------------------
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


## ---------------------------------------------------------
## parameters
n1 <- 500
n0 <- 500
p1.star <- 0.05
p0.star <- 0.1


## ---------------------------------------------------------
## overall call back rate as a weighted average
p <- (n1 * p1.star + n0 * p0.star) / (n1 + n0) 
## standard error under the null
se <- sqrt(p * (1 - p) * (1 / n1 + 1 / n0)) 
## standard error under the hypothetical data generating process
se.star <- sqrt(p1.star * (1 - p1.star) / n1 
                + p0.star * (1 - p0.star) / n0)


## ---------------------------------------------------------
pnorm(-cr.value * se, mean = p1.star - p0.star, sd = se.star) +
  pnorm(cr.value * se, mean = p1.star - p0.star, sd = se.star, 
          lower.tail = FALSE)


## ---------------------------------------------------------
power.prop.test(n = 500, p1 = 0.05, p2 = 0.1, sig.level = 0.05)


## ---------------------------------------------------------
power.prop.test(p1 = 0.05, p2 = 0.1, sig.level = 0.05, power = 0.9)


## ---------------------------------------------------------
power.t.test(n = 100, delta = 0.25, sd = 1, type = "one.sample")


## ---------------------------------------------------------
power.t.test(power = 0.9, delta = 0.25, sd = 1, type = "one.sample")


## ---------------------------------------------------------
power.t.test(delta = 0.25, sd = 1, type = "two.sample", 
             alternative = "one.sided", power = 0.9)


## ---------------------------------------------------------
## load the data
data("minwage", package = "qss")
## compute proportion of full employment before minimum wage increase
## same thing after minimum wage increase
## an indicator for NJ: 1 if it's located in NJ and 0 if in PA
minwage <- 
  mutate(minwage,
         fullPropBefore = fullBefore / (fullBefore + partBefore),
         fullPropAfter = fullAfter / (fullAfter + partAfter),
         NJ = if_else(location == "PA", 0 , 1))


## ---------------------------------------------------------
fit_minwage <- lm(fullPropAfter ~ -1 + NJ + fullPropBefore +
                    wageBefore + chain, data = minwage)
## regression result
fit_minwage


## ---------------------------------------------------------
## with intercept
fit_minwage1 <- lm(fullPropAfter ~ NJ + fullPropBefore +
                     wageBefore + chain, data = minwage)
fit_minwage1


## ---- message=FALSE, warning=FALSE------------------------
## load the required package
library(modelr)
## Generate prediction from the first model
pred_1 <-minwage %>% 
  slice(1) %>% 
  add_predictions(fit_minwage) %>%
  select(pred) %>% 
  mutate(model = "fit_minwage")
## Generate prediction from the second model
## then add predictions from first to compare
pred_compare <-minwage %>% 
  slice(1) %>% 
  add_predictions(fit_minwage1) %>%
  select(pred) %>% 
  mutate(model = "fit_minwage1") %>% 
  bind_rows(pred_1)

pred_compare  


## ----message=FALSE, warning=FALSE-------------------------
library(broom)
## load the data
data("women", package = "qss")
## fit the model
fit_women <- lm(water ~ reserved, data = women)
## view the coefficients 
summary(fit_women)
tidy(fit_women)


## ---------------------------------------------------------
## display confidence intervals
tidy(fit_women, conf.int = TRUE)


## ---------------------------------------------------------
summary(fit_minwage)
tidy(fit_minwage, conf.int = TRUE)


## ---------------------------------------------------------
## load the data and subset them into two parties
data("MPs", package = "qss")
MPs_labour <- filter(MPs, party == "labour")
MPs_tory <- filter(MPs, party == "tory")

## two regressions for labour: negative and positive margin
labour_fit1 <- lm(ln.net ~ margin, data = filter(MPs_labour, margin < 0))
labour_fit2 <- lm(ln.net ~ margin, data = filter(MPs_labour, margin > 0))

## two regressions for tory: negative and positive margin
tory_fit1 <- lm(ln.net ~ margin, data = filter(MPs_tory, margin < 0))
tory_fit2 <- lm(ln.net ~ margin, data = filter(MPs_tory, margin > 0))


## ---------------------------------------------------------
## tory party: prediction at the threshold
tory_y0 <- augment(tory_fit1, newdata = tibble(margin = 0), 
          interval = "confidence",
          conf.level = 0.95)

tory_y0
tory_y1 <- augment(tory_fit2, newdata = tibble(margin = 0), 
                   interval = "confidence")
tory_y1


## ---------------------------------------------------------
## create data with the ranges of "margin" less than zero
y1_range <- data_grid(filter(MPs_tory, margin <= 0), margin)
## add predictions and CIs
tory_y0 <- augment(tory_fit1, newdata = y1_range, 
                   interval = "confidence")
## create the data with the ranges of "margin" greater than zero
y2_range <- data_grid(filter(MPs_tory, margin >= 0), margin)
## add predictions and CIs
tory_y1 <- augment(tory_fit2, newdata = y2_range, 
                   interval = "confidence")




## ---------------------------------------------------------
ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  # plot losers
  geom_ribbon(aes(x = margin, ymin = .lower, ymax = .upper),
              data = tory_y0, alpha = 0.3) +
  geom_line(aes(x = margin, y = .fitted), data = tory_y0) +
  # plot winners
  geom_ribbon(aes(x = margin, ymin = .lower, ymax = .upper),
              data = tory_y1, alpha = 0.3) +
  geom_line(aes(x = margin, y = .fitted), data = tory_y1) +
  xlim(-.5,.25) +
  labs(x = "Margin of victory", y = "log net wealth")


## ---------------------------------------------------------
## predictions at threshold with SEs
tory_y0 <- augment(tory_fit1, newdata = tibble(margin = 0), 
          interval = "confidence",
          se_fit = TRUE)

tory_y0
tory_y1 <- augment(tory_fit2, newdata = tibble(margin = 0), 
                   interval = "confidence",
                   se_fit = TRUE)
tory_y1


## ---------------------------------------------------------
summary(tory_fit2)


## ---------------------------------------------------------
# standard error
se_diff <- sqrt(tory_y0$.se.fit ^ 2 + tory_y1$.se.fit ^ 2)
se_diff
# point estimate
diff_est <- tory_y1$.fitted - tory_y0$.fitted
diff_est
# confidence interval
CI <- c(diff_est - se_diff * qnorm(0.975),
        diff_est + se_diff * qnorm(0.975))
CI
# hypothesis test
z_score <- diff_est / se_diff
# two sided p value
p_value <- 2 * pnorm(abs(z_score), lower.tail = FALSE)
p_value

