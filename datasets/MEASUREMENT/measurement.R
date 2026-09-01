## ------------------------------------------------------------------------
## load data
afghan <- read.csv("afghan.csv")

## summarize variables of interest
summary(afghan$age)
summary(afghan$educ.years)
summary(afghan$employed)
summary(afghan$income)

prop.table(table(ISAF = afghan$violent.exp.ISAF, 
                 Taliban = afghan$violent.exp.taliban))

## ------------------------------------------------------------------------
## print income data for first 10 respondents
head(afghan$income, n = 10)

## indicate whether respondents' income is missing
head(is.na(afghan$income), n = 10)
sum(is.na(afghan$income))  # count of missing values
mean(is.na(afghan$income)) # proportion missing

x <- c(1, 2, 3, NA)
mean(x) 
mean(x, na.rm = TRUE)

prop.table(table(ISAF = afghan$violent.exp.ISAF, 
                 Taliban = afghan$violent.exp.taliban, exclude = NULL))

afghan.sub <- na.omit(afghan)  # listwise deletion
nrow(afghan.sub)               
length(na.omit(afghan$income)) 

## ------------------------------------------------------------------------
## a vector of proportions to plot
ISAF.ptable <- prop.table(table(ISAF = afghan$violent.exp.ISAF, 
                                exclude = NULL))
ISAF.ptable 

## make barplots by specifying a certain range for y-axis
barplot(ISAF.ptable,
        names.arg = c("No harm", "Harm", "Nonresponse"), 
        main = "Civilian victimization by the ISAF",
        xlab = "Response category",
        ylab = "Proportion of the respondents", ylim = c(0, 0.7))

## repeat the same for the victimization by Taliban
Taliban.ptable <- prop.table(table(Taliban = afghan$violent.exp.taliban, 
                         exclude = NULL)) 
barplot(Taliban.ptable,
        names.arg = c("No harm", "Harm", "Nonresponse"), 
        main = "Civilian victimization by the Taliban",
        xlab = "Response category",
        ylab = "Proportion of the respondents", ylim = c(0, 0.7)) 

## ------------------------------------------------------------------------
hist(afghan$age, freq = FALSE, ylim = c(0, 0.04), xlab = "Age",
     main = "Distribution of respondent's age")

## histogram of education.  use `breaks' to choose bins 
hist(afghan$educ.years, freq = FALSE, 
     breaks = seq(from = -0.5, to = 18.5, by = 1),
     xlab = "Years of education", 
     main = "Distribution of respondent's education")

## add a text label at (x, y) = (3, 0.5)
text(x = 3, y = 0.5, "median")

## add a vertical line representing median
abline(v = median(afghan$educ.years))

## adding a vertical line representing median
lines(x = rep(median(afghan$educ.years), 2), y = c(0, 0.5))

### Section 3.3.3: Box Plot
boxplot(educ.years ~ province, data = afghan, 
        main = "Education by province", ylab = "Years of education")

tapply(afghan$violent.exp.taliban, afghan$province, mean, na.rm = TRUE)
tapply(afghan$violent.exp.ISAF, afghan$province, mean, na.rm = TRUE)

## ---- eval = FALSE-------------------------------------------------------
## ## Saving or Printing a Graph
## 
## pdf(file = "educ.pdf", height = 5, width = 5)
## boxplot(educ.years ~ province, data = afghan,
##          main = "Education by Province", ylab = "Years of education")
## dev.off()
## 
## pdf(file = "hist.pdf", height = 4, width = 8)
## ## for simplicity omit the texts and lines from the earlier example
## hist(afghan$age, freq = FALSE,
##       xlab = "Age", ylim = c(0, 0.04),
##       main = "Distribution of Respondent's Age")
## hist(afghan$educ.years, freq = FALSE,
##       breaks = seq(from = -0.5, to = 18.5, by = 1),
##       xlab = "Years of education", xlim = c(0, 20),
##       main = "Distribution of Respondent's Education")
## dev.off()

## ------------------------------------------------------------------------
## load village data
afghan.village <- read.csv("afghan-village.csv")

## boxplots for altitude 
boxplot(altitude ~ village.surveyed, data = afghan.village,
        ylab = "Altitude (meter)", names = c("Nonsampled", "Sampled"))

## boxplots for log population
boxplot(log(population) ~ village.surveyed, data = afghan.village,
        ylab = "log population", names = c("Nonsampled", "Sampled"))

## ------------------------------------------------------------------------
tapply(is.na(afghan$violent.exp.taliban), afghan$province, mean)
tapply(is.na(afghan$violent.exp.ISAF), afghan$province, mean)

mean(afghan$list.response[afghan$list.group == "ISAF"]) - 
    mean(afghan$list.response[afghan$list.group == "control"]) 

table(response = afghan$list.response, group = afghan$list.group)

## ------------------------------------------------------------------------
congress <- read.csv("congress.csv")

## subset the data by party
rep <- subset(congress, subset = (party == "Republican"))
dem <- congress[congress$party == "Democrat", ] # another way to subset

## 80th and 112th congress
rep80 <- subset(rep, subset = (congress == 80))
dem80 <- subset(dem, subset = (congress == 80))
rep112 <- subset(rep, subset = (congress == 112))
dem112 <- subset(dem, subset = (congress == 112))

## preparing the labels and axis limits to avoid repetition
xlab <- "Economic liberalism/conservatism"
ylab <- "Racial liberalism/conservatism"
lim <- c(-1.5, 1.5)

## scatterplot for the 80th Congress 
plot(dem80$dwnom1, dem80$dwnom2, pch = 16, col = "blue", 
     xlim = lim, ylim = lim, xlab = xlab, ylab = ylab,
     main = "80th Congress") # democrats
points(rep80$dwnom1, rep80$dwnom2, pch = 17, col = "red") # republicans
text(-0.75, 1, "Democrats")
text(1, -1, "Republicans")

## scatterplot for the 112th Congress
plot(dem112$dwnom1, dem112$dwnom2, pch = 16, col = "blue", 
     xlim = lim, ylim = lim, xlab = xlab, ylab = ylab,
     main = "112th Congress")
points(rep112$dwnom1, rep112$dwnom2, pch = 17, col = "red")

## party median for each congress
dem.median <- tapply(dem$dwnom1, dem$congress, median)
rep.median <- tapply(rep$dwnom1, rep$congress, median)

## Democrats
plot(names(dem.median), dem.median, col = "blue", type = "l",
     xlim = c(80, 115), ylim = c(-1, 1), xlab = "Congress",
     ylab = "DW-NOMINATE score (1st dimension)")
## add Republicans
lines(names(rep.median), rep.median, col = "red")
text(110, -0.6, "Democratic\n Party")
text(110, 0.85, "Republican\n Party")

## ------------------------------------------------------------------------
## Gini coefficient data
gini <- read.csv("USGini.csv")

## time-series plot for partisan difference
plot(seq(from = 1947.5, to = 2011.5, by = 2), 
     rep.median - dem.median, xlab = "Year",
     ylab = "Republican median -\n Democratic median",
     main = "Political polarization")
## time-series plot for Gini coefficient
plot(gini$year, gini$gini, ylim = c(0.35, 0.45), xlab = "Year", 
     ylab = "Gini coefficient", main = "Income inequality")

cor(gini$gini[seq(from = 2, to = nrow(gini), by = 2)], 
    rep.median - dem.median)

## ------------------------------------------------------------------------
hist(dem112$dwnom2, freq = FALSE, main = "Democrats", 
     xlim = c(-1.5, 1.5), ylim = c(0, 1.75), 
     xlab = "Racial liberalism/conservatism dimension") 
hist(rep112$dwnom2, freq = FALSE, main = "Republicans",
     xlim = c(-1.5, 1.5), ylim = c(0, 1.75), 
     xlab = "Racial liberalism/conservatism dimension") 

qqplot(dem112$dwnom2, rep112$dwnom2, xlab = "Democrats", 
       ylab = "Republicans", xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5), 
       main = "Racial liberalism/conservatism dimension") 
abline(0, 1) # 45 degree line

## ------------------------------------------------------------------------
## 3x4 matrix filled by row; first argument take actual entries
x <- matrix(1:12, nrow = 3, ncol = 4, byrow = TRUE)
rownames(x) <- c("a", "b", "c")
colnames(x) <- c("d", "e", "f", "g")
dim(x) # dimension
x

## data frame can take different data types
y <- data.frame(y1 = as.factor(c("a", "b", "c")), y2 = c(0.1, 0.2, 0.3))
class(y$y1)
class(y$y2)

## as.matrix() converts both variables to character
z <- as.matrix(y)
z

## column sums
colSums(x)  
## row means
rowMeans(x) 
## column sums
apply(x, 2, sum) 
## row means
apply(x, 1, mean)
## standard deviation for each row
apply(x, 1, sd)

## ------------------------------------------------------------------------
## create a list
x <- list(y1 = 1:10, y2 = c("hi", "hello", "hey"),
          y3 = data.frame(z1 = 1:3, z2 = c("good", "bad", "ugly"))) 
## 3 ways of extracting elements from a list 
x$y1 # first element
x[[2]] # second element
x[["y3"]] # third element

## ------------------------------------------------------------------------
names(x)  # names of all elements 
length(x) # number of elements

dwnom80 <- cbind(congress$dwnom1[congress$congress == 80], 
                 congress$dwnom2[congress$congress == 80])
dwnom112 <- cbind(congress$dwnom1[congress$congress == 112], 
                 congress$dwnom2[congress$congress == 112])

## kmeans with 2 clusters
k80two.out <- kmeans(dwnom80, centers = 2, nstart = 5)
k112two.out <- kmeans(dwnom112, centers = 2, nstart = 5)

## elements of a list
names(k80two.out)

## final centroids
k80two.out$centers
k112two.out$centers

## number of observations for each cluster by party
table(party = congress$party[congress$congress == 80], 
      cluster = k80two.out$cluster)
table(party = congress$party[congress$congress == 112], 
      cluster = k112two.out$cluster)

## kmeans with 4 clusters
k80four.out <- kmeans(dwnom80, centers = 4, nstart = 5)
k112four.out <- kmeans(dwnom112, centers = 4, nstart = 5)

## plotting the results using the labels and limits defined earlier
plot(dwnom80, col = k80four.out$cluster + 1, xlab = xlab, ylab = ylab,
     xlim = lim, ylim = lim, main = "80th Congress")

## plotting the centroids
points(k80four.out$centers, pch = 8, cex = 2)

## 112th congress
plot(dwnom112, col = k112four.out$cluster + 1, xlab = xlab, ylab = ylab,
     xlim = lim, ylim = lim, main = "112th Congress")
points(k112four.out$centers, pch = 8, cex = 2)

palette()

