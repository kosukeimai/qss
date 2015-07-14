# Code placed in this file fill be executed every time the
# lesson is started. Any variables created here will show up in
# the user's working directory and thus be accessible to them
# throughout the lesson.

## Create dummy data
x1 <- rnorm(100, mean = 5, sd = 1)
x2 <- rnorm(100, mean = 10, sd = 1)
y <- 2*x1 + 3 * x2 + rnorm(100,mean = 0, sd = 0.5)
df <- data.frame(y, x1, x2)
