# Code placed in this file fill be executed every time the
# lesson is started. Any variables created here will show up in
# the user's working directory and thus be accessible to them
# throughout the lesson.


# Make path to lesson directory
lesson_dir <- file.path(path.package("socraticswirl"), "Courses", "DISCOVERY", "DISCOVERY2")

# Make path to data and let user call read.csv(data_path)
data_path <- file.path(lesson_dir, "florentine.csv")

# Load data into a variable for the user
florence <- read.csv(data_path, stringsAsFactors=FALSE, row.names = "FAMILY")
florence <- as.matrix(florence)

install.packages("igraph")
library(igraph)
