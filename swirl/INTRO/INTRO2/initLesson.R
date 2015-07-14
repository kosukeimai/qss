# Code placed in this file fill be executed every time the
# lesson is started. Any variables created here will show up in
# the user's working directory and thus be accessible to them
# throughout the lesson.

year <- seq(from = 1950, to = 2010, by = 10)
world.pop <- c(2525779, 3026003, 3691173, 4449049, 5320817, 6127700, 6916183)
UNpop <- as.data.frame(cbind(world.pop, year))

x <- sample(1:100, 10)

result <- 8 - 2

# Make path to lesson directory
lesson_dir <- file.path(path.package("swirl"), "Courses",
                        "INTRO", "INTRO2")

# Make path to data and let user call read.csv(data_path)
data_path <- file.path(lesson_dir, "data.csv")


