## ------------------------------------------------------------------------
values <- c(2, 4, 6)
n <- length(values) # number of elements in `values'
results <- rep(NA, n) # empty container vector for storing the results

## loop counter `i' will take values on 1, 2, ..., n in that order
for (i in 1:n) {
    ## store the result of multiplication as the ith element of
    ## `results' vector
    results[i] <- values[i] * 2
    cat(values[i], "times 2 is equal to", results[i], "\n")
}
results

## check if the code runs when i = 1
i <- 1
x <- values[i] * 2
cat(values[i], "times 2 is equal to", x, "\n")

## ------------------------------------------------------------------------
## define the operation to be executed
operation <- "add"
if (operation == "add") {
    cat("I will perform addition 4 + 4\n")
    4 + 4
}
if (operation == "multiply") {
    cat("I will perform multiplication 4 * 4\n")
    4 * 4
}

## Note that `operation' is redefined
operation <- "multiply"
if (operation == "add") {
    cat("I will perform addition 4 + 4")
    4 + 4
} else {
    cat("I will perform multiplication 4 * 4")
    4 * 4
}

## Note that `operation' is redefined
operation <- "subtract"
if (operation == "add") {
    cat("I will perform addition 4 + 4\n")
    4 + 4
} else if (operation == "multiply") {
    cat("I will perform multiplication 4 * 4\n")
    4 * 4
} else {
    cat("`", operation, "' is invalid. Use either `add' or `multiply'.\n",
        sep = "")
}

values <- 1:5
n <-  length(values)
results <- rep(NA, n)
for (i in 1:n) {
    ## x and r get overwritten in each iteration
    x <- values[i]
    r <- x %% 2  # remainder when divided by 2 to check whether even or odd
    if (r == 0) { # remainder is zero
        cat(x, "is even and I will perform addition",
            x, "+", x, "\n")
        results[i] <- x + x
    } else { # remainder is not zero
        cat(x, "is odd and I will perform multiplication",
            x, "*", x, "\n")
        results[i] <- x * x
    }
}
results

## ------------------------------------------------------------------------
## load election results, by state
pres08 <- read.csv("pres08.csv")
## load polling data
polls08 <- read.csv("polls08.csv")
## compute Obama's margin
polls08$margin <- polls08$Obama - polls08$McCain
pres08$margin <- pres08$Obama - pres08$McCain

x <- as.Date("2008-11-04")
y <- as.Date("2008/9/1")
x - y # number of days between 2008/9/1 and 11/4

## convert to a Date object
polls08$middate <- as.Date(polls08$middate)

## computer the number of days to the election day
polls08$DaysToElection <- as.Date("2008-11-04") - polls08$middate
poll.pred <- rep(NA, 51) # initialize a vector place holder

## extract unique state names which the loop will iterate through
st.names <- unique(polls08$state)

## add state names as labels for easy interpretation later on
names(poll.pred) <- as.character(st.names)

## loop across 50 states plus DC
for (i in 1:51){
    ## subset the ith state
    state.data <- subset(polls08, subset = (state == st.names[i]))
    ## further subset the latest polls within the state
    latest <- subset(state.data, DaysToElection == min(DaysToElection))
    ## compute the mean of latest polls and store it
    poll.pred[i] <- mean(latest$margin)
}

## error of latest polls
errors <- pres08$margin - poll.pred
names(errors) <- st.names # add state names
mean(errors) # mean prediction error

sqrt(mean(errors^2))

## histogram
hist(errors, freq = FALSE, ylim = c(0, 0.08),
     main = "Poll prediction error",
     xlab = "Error in predicted margin for Obama (percentage points)")
## add mean
abline(v = mean(errors), lty = "dashed", col = "red")
text(x = -7, y = 0.07, "average error", col = "red")

## type = "n" generates "empty" plot
plot(poll.pred, pres08$margin, type = "n", main = "", xlab = "Poll results",
     xlim = c(-40, 90), ylim = c(-40, 90), ylab = "Actual election results")
## add state abbreviations
text(x = poll.pred, y = pres08$margin, labels = pres08$state, col = "blue")
## lines
abline(a = 0, b = 1, lty = "dashed") # 45 degree line
abline(v = 0)  # vertical line at 0
abline(h = 0)  # horizontal line at 0

## which state polls called wrong?
pres08$state[sign(poll.pred) != sign(pres08$margin)]
## what was the actual margin for these states?
pres08$margin[sign(poll.pred) != sign(pres08$margin)]

## actual results: total number of electoral votes won by Obama
sum(pres08$EV[pres08$margin > 0])
## poll prediction
sum(pres08$EV[poll.pred > 0])

## load the data
pollsUS08 <- read.csv("pollsUS08.csv")

## compute number of days to the election as before
pollsUS08$middate <- as.Date(pollsUS08$middate)
pollsUS08$DaysToElection <- as.Date("2008-11-04") - pollsUS08$middate

## empty vectors to store predictions
Obama.pred <- McCain.pred <- rep(NA, 90)

for (i in 1:90) {
    ## take all polls conducted within the past 7 days
    week.data <- subset(pollsUS08, subset = ((DaysToElection <= (90 - i + 7))
                                  & (DaysToElection > (90 - i))))
    ## compute support for each candidate using the average
    Obama.pred[i] <- mean(week.data$Obama)
    McCain.pred[i] <- mean(week.data$McCain)
}

## plot going from 90 days to 1 day before the election
plot(90:1, Obama.pred, type = "b", xlim = c(90, 0), ylim = c(40, 60),
     col = "blue", xlab = "Days to the election",
     ylab = "Support for candidate (percentage points)")
## `type = "b"' gives plot that includes both points and lines
lines(90:1, McCain.pred, type = "b", col = "red")

## actual election results: pch = 19 gives solid circles
points(0, 52.93, pch = 19, col = "blue")
points(0, 45.65, pch = 19, col = "red")

## line indicating the election day
abline(v = 0)

## labeling candidates
text(80, 48, "Obama", col = "blue")
text(80, 41, "McCain", col = "red")

## ------------------------------------------------------------------------
## load the data
face <- read.csv("face.csv")
## two-party vote share for Democrats and Republicans
face$d.share <- face$d.votes / (face$d.votes + face$r.votes)
face$r.share <- face$r.votes / (face$d.votes + face$r.votes)
face$diff.share <- face$d.share - face$r.share

plot(face$d.comp, face$diff.share, pch = 16,
     col = ifelse(face$w.party == "R", "red", "blue"),
     xlim = c(0, 1), ylim = c(-1, 1),
     xlab = "Competence scores for Democrats",
     ylab = "Democratic margin in vote share",
     main = "Facial competence and vote share")

## ------------------------------------------------------------------------
cor(face$d.comp, face$diff.share)

## ------------------------------------------------------------------------
fit <- lm(diff.share ~ d.comp, data = face) # fit the model
fit

## lm(face$diff.share ~ face$d.comp)
coef(fit)  # get estimated coefficients
head(fitted(fit))  # get fitted or predicted values

plot(face$d.comp, face$diff.share, xlim = c(0, 1.05), ylim = c(-1,1),
     xlab = "Competence scores for Democrats",
     ylab = "Democratic margin in vote share",
     main = "Facial competence and vote share")
abline(fit) # add regression line
abline(v = 0, lty = "dashed")

epsilon.hat <- resid(fit)  # residuals
sqrt(mean(epsilon.hat^2))  # RMSE

## ------------------------------------------------------------------------
pres12 <- read.csv("pres12.csv")  # load 2012 data
## quick look at two data sets
head(pres08)
head(pres12)

## merge two data frames
pres <- merge(pres08, pres12, by = "state")

## summarize the merged data frame
summary(pres)

## change the variable name for illustration
names(pres12)[1] <- "state.abb"

## merging data sets using the variables of different names
pres <- merge(pres08, pres12, by.x = "state", by.y = "state.abb")
summary(pres)

## cbinding two data frames
pres1 <- cbind(pres08, pres12)
## this shows all variables are kept
summary(pres1)

## DC and DE are flipped in this alternative approach
pres1[8:9, ]

## merge() does not have this problem
pres[8:9, ]

pres$Obama2008.z <- scale(pres$Obama.x)
pres$Obama2012.z <- scale(pres$Obama.y)

## intercept is estimated essentially zero
fit1 <- lm(Obama2012.z ~ Obama2008.z, data = pres)
fit1

## regression without an intercept; estimated slope is identical
fit1 <- lm(Obama2012.z ~ -1 + Obama2008.z, data = pres)
fit1

plot(pres$Obama2008.z, pres$Obama2012.z, xlim = c(-4, 4), ylim = c(-4, 4),
     xlab = "Obama's standardized vote share in 2008",
     ylab = "Obama's standardized vote share in 2012")
abline(fit1) # draw a regression line

## bottom quartile
mean((pres$Obama2012.z >
          pres$Obama2008.z)[pres$Obama2008.z
                            <= quantile(pres$Obama2008.z, 0.25)])

## top quartile
mean((pres$Obama2012.z >
          pres$Obama2008.z)[pres$Obama2008.z
                            >= quantile(pres$Obama2008.z, 0.75)])

## ------------------------------------------------------------------------
florida <- read.csv("florida.csv")

## regress Buchanan's 2000 votes on Perot's 1996 votes
fit2 <- lm(Buchanan00 ~ Perot96, data = florida)
fit2

## compute TSS (total sum of squares) and SSR (sum of squared residuals)
TSS2 <- sum((florida$Buchanan00 - mean(florida$Buchanan00))^2)
SSR2 <- sum(resid(fit2)^2)

## Coefficient of determination
(TSS2 - SSR2) / TSS2

R2 <- function(fit) {
    resid <- resid(fit) # residuals
    y <- fitted(fit) + resid # outcome variable
    TSS <- sum((y - mean(y))^2)
    SSR <- sum(resid^2)
    R2 <- (TSS - SSR) / TSS
    return(R2)
}
R2(fit2)

## built-in R function
summary(fit2)$r.squared
R2(fit1)

plot(fitted(fit2), resid(fit2), xlim = c(0, 1500), ylim = c(-750, 2500),
     xlab = "Fitted values", ylab = "Residuals")
abline(h = 0)

florida$county[resid(fit2) == max(resid(fit2))]

## data without Palm Beach
florida.pb <- subset(florida, subset = (county != "PalmBeach"))

fit3 <- lm(Buchanan00 ~ Perot96, data = florida.pb)
fit3

## R^2 or coefficient of determination
R2(fit3)

## residual plot
plot(fitted(fit3), resid(fit3), xlim = c(0, 1500), ylim = c(-750, 2500),
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residual plot without Palm Beach")
abline(h = 0) # horizontal line at 0
plot(florida$Perot96, florida$Buchanan00, xlab = "Perot's votes in 1996",
     ylab = "Buchanan's votes in 2000")
abline(fit2, lty = "dashed") # regression with Palm Beach
abline(fit3) # regression without Palm Beach
text(30000, 3250, "Palm Beach")
text(30000, 1500, "regression\n with Palm Beach")
text(30000, 400, "regression\n without Palm Beach")

## ------------------------------------------------------------------------
women <- read.csv("women.csv")

## proportion of female politicians in reserved GP vs. unreserved GP
mean(women$female[women$reserved == 1])
mean(women$female[women$reserved == 0])

## drinking-water facilities
mean(women$water[women$reserved == 1]) -
    mean(women$water[women$reserved == 0])

## irrigation facilities
mean(women$irrigation[women$reserved == 1]) -
    mean(women$irrigation[women$reserved == 0])

lm(water ~ reserved, data = women)
lm(irrigation ~ reserved, data = women)

## ------------------------------------------------------------------------
social <- read.csv("social.csv")
levels(social$messages) # base level is `Civic'

fit <- lm(primary2008 ~ messages, data = social)
fit

## ## create indicator variables
## social$Control <- ifelse(social$messages == "Control", 1, 0)
## social$Hawthorne <- ifelse(social$messages == "Hawthorne", 1, 0)
## social$Neighbors <- ifelse(social$messages == "Neighbors", 1, 0)
## ## fit the same regression as above by directly using indicator variables
## lm(primary2008 ~ Control + Hawthorne + Neighbors, data = social)

## create a data frame with unique values of `messages'
unique.messages <- data.frame(messages = unique(social$messages))
unique.messages

## make prediction for each observation from this new data frame
predict(fit, newdata = unique.messages)

## sample average
tapply(social$primary2008, social$messages, mean)

## linear regression without intercept
fit.noint <- lm(primary2008 ~ -1 + messages, data = social)
fit.noint

## estimated average effect of `Neighbors' condition
coef(fit)["messagesNeighbors"] - coef(fit)["messagesControl"]

## difference in means
mean(social$primary2008[social$messages == "Neighbors"]) -
    mean(social$primary2008[social$messages == "Control"])

## adjusted Rsquare
adjR2 <- function(fit) {
    resid <- resid(fit) # residuals
    y <- fitted(fit) + resid # outcome
    n <- length(y)
    TSS.adj <- sum((y - mean(y))^2) / (n - 1)
    SSR.adj <- sum(resid^2) / (n - length(coef(fit)))
    R2.adj <- 1 - SSR.adj / TSS.adj
    return(R2.adj)
}
adjR2(fit)
R2(fit) # unadjusted Rsquare calculation

summary(fit)$adj.r.squared

## ------------------------------------------------------------------------
## average treatment effect (ate) among those who voted in 2004 primary
social.voter <- subset(social, primary2004 == 1)

ate.voter <-
    mean(social.voter$primary2008[social.voter$messages == "Neighbors"]) -
        mean(social.voter$primary2008[social.voter$messages == "Control"])
ate.voter

## average effect among those who did not vote
social.nonvoter <- subset(social, primary2004 == 0)

ate.nonvoter <-
    mean(social.nonvoter$primary2008[social.nonvoter$messages == "Neighbors"]) -
        mean(social.nonvoter$primary2008[social.nonvoter$messages == "Control"])
ate.nonvoter

## difference
ate.voter - ate.nonvoter

## subset neighbors and control groups
social.neighbor <- subset(social, (messages == "Control") |
                              (messages == "Neighbors"))

## standard way to generate main and interaction effects
fit.int <- lm(primary2008 ~ primary2004 + messages + primary2004:messages,
              data = social.neighbor)
fit.int

## lm(primary2008 ~ primary2004 * messages, data = social.neighbor)

social.neighbor$age <- 2008 - social.neighbor$yearofbirth
summary(social.neighbor$age)

fit.age <- lm(primary2008 ~ age * messages, data = social.neighbor)
fit.age

## age = 25, 45, 65, 85 in Neighbors group
age.neighbor <- data.frame(age = seq(from = 25, to = 85, by = 20),
                           messages = "Neighbors")

## age = 25, 45, 65, 85 in Control group
age.control <- data.frame(age = seq(from = 25, to = 85, by = 20),
                          messages = "Control")

## average treatment effect for age = 25, 45, 65, 85
ate.age <- predict(fit.age, newdata = age.neighbor) -
    predict(fit.age, newdata = age.control)

ate.age

fit.age2 <- lm(primary2008 ~ age + I(age^2) + messages + age:messages +
                 I(age^2):messages, data = social.neighbor)

fit.age2

## predicted turnout rate under the ``Neighbors'' treatment condition
yT.hat <- predict(fit.age2,
                  newdata = data.frame(age = 25:85, messages = "Neighbors"))

## predicted turnout rate under the control condition
yC.hat <- predict(fit.age2,
                  newdata = data.frame(age = 25:85, messages = "Control"))

## plotting the predicted turnout rate under each condition
plot(x = 25:85, y = yT.hat, type = "l", xlim = c(20, 90), ylim = c(0, 0.5),
     xlab = "Age", ylab = "Predicted turnout rate")
lines(x = 25:85, y = yC.hat, lty = "dashed")
text(40, 0.45, "Neighbors condition")
text(45, 0.15, "Control condition")

## plotting the average treatment effect as a function of age
plot(x = 25:85, y = yT.hat - yC.hat, type = "l", xlim = c(20, 90),
     ylim = c(0, 0.1), xlab = "Age",
     ylab = "Estimated average treatment effect")

## ------------------------------------------------------------------------
## load the data and subset them into two parties
MPs <- read.csv("MPs.csv")
MPs.labour <- subset(MPs, subset = (party == "labour"))
MPs.tory <- subset(MPs, subset = (party == "tory"))

## two regressions for Labour: negative and positive margin
labour.fit1 <- lm(ln.net ~ margin,
                  data = MPs.labour[MPs.labour$margin < 0, ])
labour.fit2 <- lm(ln.net ~ margin,
                  data = MPs.labour[MPs.labour$margin > 0, ])

## two regressions for Tory: negative and positive margin
tory.fit1 <- lm(ln.net ~ margin, data = MPs.tory[MPs.tory$margin < 0, ])
tory.fit2 <- lm(ln.net ~ margin, data = MPs.tory[MPs.tory$margin > 0, ])

## Labour: range of predictions
y1l.range <- c(min(MPs.labour$margin), 0) # min to 0
y2l.range <- c(0, max(MPs.labour$margin)) # 0 to max

## prediction
y1.labour <- predict(labour.fit1, newdata = data.frame(margin = y1l.range))
y2.labour <- predict(labour.fit2, newdata = data.frame(margin = y2l.range))

## Tory: range of predictions
y1t.range <- c(min(MPs.tory$margin), 0) # min to 0
y2t.range <- c(0, max(MPs.tory$margin)) # 0 to max

## predict outcome
y1.tory <- predict(tory.fit1, newdata = data.frame(margin = y1t.range))
y2.tory <- predict(tory.fit2, newdata = data.frame(margin = y2t.range))

## scatterplot with regression lines for labour
plot(MPs.labour$margin, MPs.labour$ln.net, main = "Labour",
     xlim = c(-0.5, 0.5), ylim = c(6, 18), xlab = "Margin of victory",
     ylab = "log net wealth at death")
abline(v = 0, lty = "dashed")

## add regression lines
lines(y1l.range, y1.labour, col = "red")
lines(y2l.range, y2.labour, col = "red")

## scatterplot with regression lines for tory
plot(MPs.tory$margin, MPs.tory$ln.net, main = "Tory", xlim = c(-0.5, 0.5),
     ylim = c(6, 18), xlab = "Margin of victory",
     ylab = "log net wealth at death")
abline(v = 0, lty = "dashed")

## add regression lines
lines(y1t.range, y1.tory, col = "red")
lines(y2t.range, y2.tory, col = "red")

## average net wealth for Tory MP
tory.MP <- exp(y2.tory[1])
tory.MP

## average net wealth for Tory non-MP
tory.nonMP <- exp(y1.tory[2])
tory.nonMP

## causal effect in pounds
tory.MP - tory.nonMP

## two regressions for Tory: negative and positive margin
tory.fit3 <- lm(margin.pre ~ margin, data = MPs.tory[MPs.tory$margin < 0, ])
tory.fit4 <- lm(margin.pre ~ margin, data = MPs.tory[MPs.tory$margin > 0, ])
## the difference between two intercepts is the estimated effect
coef(tory.fit4)[1] - coef(tory.fit3)[1]

