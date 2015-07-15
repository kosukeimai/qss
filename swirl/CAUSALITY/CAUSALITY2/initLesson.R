# Code placed in this file fill be executed every time the
# lesson is started. Any variables created here will show up in
# the user's working directory and thus be accessible to them
# throughout the lesson.


# Make path to lesson directory
lesson_dir <- file.path(path.package("socraticswirl"), "Courses",
                        "CAUSALITY", "CAUSALITY2")

# Make path to data and let user call read.csv(data_path)
data_path <- file.path(lesson_dir, "resume.csv")
data_path2 <- file.path(lesson_dir, "social.csv")

# Load data into a variable for the user
resume <- read.csv(data_path, stringsAsFactors=FALSE)
social <- read.csv(data_path2, stringsAsFactors=FALSE)
