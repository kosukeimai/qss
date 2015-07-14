# Code placed in this file fill be executed every time the
# lesson is started. Any variables created here will show up in
# the user's working directory and thus be accessible to them
# throughout the lesson.

# UNpop <- read.csv("UNpop.csv", head = T)
# world.pop <- UNpop$world.pop

year <- seq(from = 1950, to = 2010, by = 10)
world.pop <- c(2525779, 3026003, 3691173, 4449049, 5320817, 6127700, 6916183)
UNpop <- as.data.frame(cbind(world.pop, year))

x <- sample(1:100, 10)
