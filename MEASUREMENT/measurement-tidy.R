## ---- echo=FALSE------------------------------------------
library(qss)
library(ggplot2)
## Load in the data from QSS package
data(afghan, package = "qss")


## ---- message=FALSE, warning=FALSE------------------------
library(tidyverse)
library(qss)

## Load in the data from QSS package
data(afghan, package = "qss")

## Summarize the main variables
afghan %>%
  select(age, educ.years, employed, income) %>%
  summary()


## ---------------------------------------------------------
count(afghan, income)


## ---------------------------------------------------------
unique(afghan$income)


## ---------------------------------------------------------
## What proportion of respondents were harmed by 
## ISAF and/or Taliban?
harm_props <- afghan %>%
  group_by(violent.exp.ISAF, violent.exp.taliban) %>%
  count() %>%
  ungroup() %>%
  mutate(prop = n / sum(n))

harm_props

## without ungroup(), commenting out the line
afghan %>%
  group_by(violent.exp.ISAF, violent.exp.taliban) %>%
  count() %>%
##  ungroup() %>%
  mutate(prop = n / sum(n))


## ---------------------------------------------------------
## What proportion of respondents were harmed by ISAF?
ISAF_harm_prop <- harm_props %>% 
  filter(violent.exp.ISAF == 1) %>% 
  summarize(harm_prop = sum(prop)) %>% 
  pull()

ISAF_harm_prop

## What proportion of respondents were harmed by Taliban?
talib_harm_prop <- harm_props %>% 
  filter(violent.exp.taliban == 1) %>% 
  summarize(harm_prop = sum(prop)) %>% 
  pull()

talib_harm_prop

## What proportion of respondents were harmed by both?
both_harm_prop <- harm_props %>% 
  filter(violent.exp.taliban == 1 & 
           violent.exp.ISAF == 1) %>% 
  summarize(harm_prop = sum(prop)) %>% 
  pull()

both_harm_prop


## ---------------------------------------------------------
## print income data for first 10 respondents
afghan %>% 
  select(income) %>% 
  slice(1:10)

## What is.na() returns for these observations
afghan %>% 
  select(income) %>% 
  slice(1:10) %>% 
  is.na()


## ---------------------------------------------------------
## What number and proportion of responses are missing for income?
summarize(afghan,
          n_missing = sum(is.na(income)),
          p_missing = mean(is.na(income)))


## ---------------------------------------------------------
x <- c(1, 2, 3, NA)
mean(x) 
mean(x, na.rm = TRUE)


## ---------------------------------------------------------
## Table for non-missing values of ISAF and Taliban
afghan %>%
  filter(!is.na(violent.exp.ISAF), !is.na(violent.exp.taliban)) %>%
  group_by(violent.exp.ISAF, violent.exp.taliban) %>%
  count() %>%
  ungroup() %>%
  mutate(prop = n / sum(n)) %>% 
  arrange(prop) #compare to arrange(desc(prop))


## ---------------------------------------------------------
## Reminder of what harm_props is
harm_props

## What proportion of observations are missing for either 
## ISAF or Taliban harm?
missing_prop <- harm_props %>% 
  filter(is.na(violent.exp.ISAF) | is.na(violent.exp.taliban)) %>% 
  ungroup() %>%
  summarize(missing_prop = sum(prop)) %>% 
  pull()

missing_prop


## ---------------------------------------------------------
afghan.sub <- na.omit(afghan)  # listwise deletion
nrow(afghan.sub) 
afghan.sub.2 <- drop_na(afghan) # equivalent with drop_na()
nrow(afghan.sub.2)

## compare to the dimensions if we only delete missing for income
## instead of full listwise deletion
afghan %>% 
  drop_na(income) %>%  
  nrow()




## ---- label='harm-barplot', out.width='45%', fig.show = 'hold', fig.align = 'center'----
## First plot
## Bar plot with ggplot
ggplot(data = afghan, # Tell R what data to use
       aes(x = as.factor(violent.exp.ISAF))) + # specify the x-axis
  geom_bar(aes(y = ..prop.., ## add a bar plot layer
           group = 1)) +
  scale_x_discrete(labels = c('No Harm','Harm','Nonresponse')) +
  ylab("Proportion of Respondents") + # Add a label to y-axis
  xlab("Response Category") + # Add a label to the x-axis
  ggtitle("Civilian Victimization by the ISAF") # Add a title

## Second plot
## Bar plot with ggplot
ggplot(data = afghan,
       aes(x = as.factor(violent.exp.taliban))) + 
  geom_bar(aes(y = ..prop.., 
           group = 1)) +
  scale_x_discrete(labels = c('No Harm','Harm','Nonresponse')) +
  ylab("Proportion of Respondents") + # Add a label to y-axis
  xlab("Response Category") + # Add a label to the x-axis
  ggtitle("Civilian Victimization by the Taliban") # Add a title




## ---------------------------------------------------------
## reshape data longer
afghan_reshape <- afghan %>% 
  pivot_longer(violent.exp.ISAF:violent.exp.taliban,
               names_to = "harming_group",
               values_to = "harm")

## Bar plot with both harm indicators together 
ggplot(data = afghan_reshape, # what's different here?
       aes(x = as.factor(harm))) +  
  geom_bar(aes(y = ..prop.., # what's different here?
           fill = harming_group,
           group = harming_group),
           position = "dodge") +
  scale_x_discrete(labels = c('No Harm','Harm','Nonresponse')) +
  scale_fill_discrete(name = "Harming Group", labels = c("ISAF", "Taliban")) +
  ylab("Proportion of Respondents") + 
  xlab("Response Category") + 
  ggtitle("Civilian Victimization")




## ----hist_age---------------------------------------------
ggplot(afghan, aes(x = age)) + # the data and initial aes()
  geom_histogram(aes(y = ..density..), # histogram, additional aes()
                 binwidth = 5, # how wide for each bin
                 boundary = 0) + # bin position
  scale_x_continuous(breaks = seq(20, 80, by = 10)) +
  labs(title = "Distribution of respondent's age",
       y = "Density", x = "Age") +
  theme_classic()


## ---------------------------------------------------------
ggplot(afghan, aes(x = educ.years, y = ..density..)) +
  geom_histogram(binwidth = 1, center = 0) +
  geom_vline(xintercept = median(afghan$educ.years)) +
  annotate(geom = "text", x = median(afghan$educ.years),
           y = 0.4, 
           label = "median", 
           hjust = -0.1) +
  labs(title = "Distribution of respondent's education",
       x = "Years of education",
       y = "Density")


## ----eval=FALSE-------------------------------------------
## ## The code for adding curly braces and text is omitted
## ggplot(afghan, aes(y = age)) +
##   geom_boxplot() +
##   labs(y = "Age", x = "", title = "Distribution of Age")


## ----echo=FALSE-------------------------------------------
CurlyBraces <- function(x0, x1, y0, y1, pos = 1, direction = 1, depth = 1, color = "black") {

    a=c(1,2,3,48,50)    # set flexion point for spline
    b=c(0,.2,.28,.7,.8) # set depth for spline flexion point

    curve = spline(a, b, n = 50, method = "natural")$y * depth

    curve = c(curve,rev(curve))

    if (pos == 1){
        a_sequence = seq(x0,x1,length=100)
        b_sequence = seq(y0,y1,length=100)  
    }
    if (pos == 2){
        b_sequence = seq(x0,x1,length=100)
        a_sequence = seq(y0,y1,length=100)      
    }

    # direction
    if(direction==1)
        a_sequence = a_sequence+curve
    if(direction==2)
        a_sequence = a_sequence-curve

    # pos
    if(pos==1)
        lines(a_sequence,b_sequence, lwd=1.5, xpd=NA, col = color) # vertical
    if(pos==2)
        lines(b_sequence,a_sequence, lwd=1.5, xpd=NA, col = color) # horizontal

}


## ---- echo=FALSE, warning=FALSE---------------------------
# with assist from https://stackoverflow.com/questions/35633239/add-curly-braces-to-ggplot2-and-then-use-ggsave
library(pBrackets)
bracketsGrob <- function(...){
l <- list(...)
e <- new.env()
e$l <- l
  grid:::recordGrob(  {
    do.call(grid.brackets, l)
  }, e)
}

b1 <- bracketsGrob(.75, .26, .75, .14, h=0.06, lwd=1.5, col="blue")
b2 <- bracketsGrob(.75, .77, .75, .26, h=0.06, lwd=1.5, col="blue")

ggplot(afghan, aes(y = age)) +
  geom_boxplot() +
  labs(y = "Age", x = "", title = "Distribution of Age") +
  xlim(-.75, .75) +
  ggplot2::annotate("text", x = -.55, y = 40, label = "upper quartile", size = 14/.pt) +
  ggplot2::annotate("text", x = -.55, y = 23, label = "lower quartile", size = 14/.pt) +
  ggplot2::annotate("text", x = -.55, y = 31, label = "median", size = 14/.pt) +
  annotation_custom(b1) +
  annotation_custom(b2) +
  ggplot2::annotate("text", x = .48, y = 49, label = "1.5 x IQR", 
           hjust = -.15,
           color = "blue", size = 14/.pt) +
  ggplot2::annotate("text", x = .49, y = 26, label = "IQR", 
           hjust = -.25,
           color = "blue", size = 14/.pt) +
  theme_classic(base_size = 12) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


## ---------------------------------------------------------
ggplot(afghan, aes(y = educ.years, x = province)) +
  geom_boxplot() +
  labs(y = "Year of Education", x = "Province", title = "Education by province")


## ----warning=FALSE----------------------------------------
afghan %>%
  group_by(province) %>%
  summarize(violent.exp.taliban =
              mean(violent.exp.taliban, na.rm = TRUE),
            violent.exp.ISAF =
              mean(violent.exp.ISAF, na.rm = TRUE))


## ----eval = FALSE-----------------------------------------
## ## Save the last figure as a pdf in the results_figures directory
## ggsave("results_figures/education_by_province.pdf")


## ----eval=FALSE-------------------------------------------
## library(gridExtra)
## 
## ## The age histogram
## age_hist <- ggplot(afghan, aes(x = age)) +
##   geom_histogram(aes(y = ..density..),
##                  binwidth = 5,
##                  boundary = 0) +
##   scale_x_continuous(breaks = seq(20, 80, by = 10)) +
##   labs(title = "Distribution of \nrespondent's age",
##        y = "Age", x = "Density")
## 
## ## The education histogram
## educ_hist <- ggplot(afghan, aes(x = educ.years, y = ..density..)) +
##   geom_histogram(binwidth = 1, center = 0) +
##   geom_vline(xintercept = median(afghan$educ.years)) +
##   annotate(geom = "text", x = median(afghan$educ.years),
##            y = 0.4,
##            label = "median",
##            hjust = -0.1) +
##   labs(title = "Distribution of \nrespondent's education",
##        x = "Years of education",
##        y = "Density")
## 
## ## Put the plots side-by-side
## grid.arrange(age_hist, educ_hist, ncol = 2)




## ---- label='loge', echo = FALSE, out.width='90%', fig.align = 'center', fig.show="hold", fig.cap= '(ref:loge-cap)', fig.scap="The natural logarithm"----
data("afghan.village", package = "qss")
par(cex = 1.5)
m <- matrix(c(0, 2, 1, 2, 1, 3, 0, 3), nrow = 2, ncol = 4)
##set up the plot
layout(m)
x <- seq(from = -5, to = 5, by = 0.1)
y <- exp(x)
plot(x ~ y, type = "l", xlab = "x", 
     ylab = "log x", cex = 1.5)
#par(mfrow = c(1,2), cex = 1.5)
hist(afghan.village$population / 1000, freq = FALSE, nclass = 10,
     xlab = "Population\n (thousands)", main = "", ylim = c(0, 0.4))
hist(log(afghan.village$population), freq = FALSE, 
     xlab = "Log Population", main = "", ylim = c(0, 0.4))




## ---- out.width='45%', fig.align = 'center', fig.show="hold"----
## Altitude box plot by sampled or not
ggplot(afghan.village, aes(x = as.factor(village.surveyed),
                           y = altitude)) +
  geom_boxplot() +
  scale_x_discrete(labels = c('Nonsampled','Sampled')) +
  labs(y = "Altitude (meters)", x = "")

## Log population box plot by sampled or not
ggplot(afghan.village, aes(x = as.factor(village.surveyed),
                           y = log(population))) +
  geom_boxplot() +
  scale_x_discrete(labels = c('Nonsampled','Sampled')) +
  labs(y = "log population", x = "")




## ---------------------------------------------------------
## Non-response rates on harm questions by province
afghan %>%
  group_by(province) %>%
  summarize(ISAF = mean(is.na(violent.exp.ISAF)),
            taliban = mean(is.na(violent.exp.taliban)))


## ---------------------------------------------------------
## Difference in mean item count between treatment/control
afghan %>% 
  filter(list.group %in% c("ISAF", "control")) %>% 
  group_by(list.group) %>% 
  summarize(avg_list_response = mean(list.response)) %>% 
  pivot_wider(names_from = list.group,
         values_from = avg_list_response) %>% 
  mutate(list_response_diff = ISAF - control)


## ---------------------------------------------------------
afghan %>%
  group_by(list.response, list.group) %>%
  count() %>% 
  pivot_wider(names_from = list.group, 
              values_from = n)


## ---- label='ideal', echo = FALSE, out.width='10cm', out.height='10cm', fig.align = 'center', fig.cap= '(ref:ideal-cap)', fig.scap="Spatial voting model"----
plot(0.5, 0.5, xlim = c(-1, 1), ylim = c(-1,1), axes = FALSE, 
     xlab = "", ylab = "", type = "p", pch = 16, cex = 2)
arrows(y0 = 0, y1 = 0, x0 = -1, x1 = 1, code = 3, length = 0.15)
arrows(y0 = -1, y1 = 1, x0 = 0, x1 = 0, code = 3, length = 0.15)
text(0.5, 0.6, "status quo")
points(-0.35, -0.75, pch = 17, cex = 2)
text(-0.35, -0.9, "proposal")
points(0.3, -0.1, pch = 4, cex = 2)
text(0.4, -0.2, "ideal point")
lines(c(0.3, 0.5), c(-0.1, 0.5), lty = "dashed")
lines(c(0.3, -0.35), c(-0.1, -0.75), lty = "dotted")
text(.8, 0.15, "economic\n conservatism")
text(-.75, -0.15, "economic\n liberalism")
text(-.2, 0.9, "racial\n conservatism")
text(.25, -0.9, "racial\n liberalism")






## ---- warning=FALSE, message=FALSE------------------------
## Necessary packages and data
library(gridExtra)
data("congress", package = "qss")

## 80th congress
plot_80 <- ggplot(data = filter(congress, congress == 80),
       aes(x = dwnom1, y = dwnom2)) +
  geom_point(aes(shape = party, color = party), 
             show.legend = FALSE) +
  scale_color_manual(values = c(Democrat = "blue",
                                 Republican = "red",
                                 Other = "green")) +
  scale_shape_manual(values = c(Democrat = "square",
                                 Republican = "triangle",
                                 Other = "circle")) +
  scale_y_continuous("Racial liberalism/conservatism",
                     limits = c(-1.5, 1.5)) +
  scale_x_continuous("Economic\n liberalism/conservatism",
                     limits = c(-1.5, 1.5)) +
  ggtitle("80th Congress") +
  coord_fixed()

## 112th congress
plot_112 <- ggplot(data = filter(congress, congress == 112),
       aes(x = dwnom1, y = dwnom2)) +
  geom_point(aes(shape = party, color = party), 
             show.legend = FALSE) +
  scale_color_manual(values = c(Democrat = "blue",
                                 Republican = "red",
                                 Other = "green")) +
    scale_shape_manual(values = c(Democrat = "square",
                                 Republican = "triangle",
                                 Other = "circle")) +
  scale_y_continuous("Racial liberalism/conservatism",
                     limits = c(-1.5, 1.5)) +
  scale_x_continuous("Economic\n liberalism/conservatism",
                     limits = c(-1.5, 1.5)) +
  ggtitle("112th Congress") +
  coord_fixed()

## Put the plots side-by-side
grid.arrange(plot_80, plot_112, ncol = 2)


## ---- warning=FALSE, message=FALSE------------------------
## median DWnom1 scores
median_dw1 <- congress %>%
  filter(party %in% c("Republican", "Democrat")) %>% 
  group_by(party, congress) %>% 
  summarize(median_dw1 = median(dwnom1))




## ---------------------------------------------------------
ggplot(data = median_dw1,
       aes(x = congress, y = median_dw1,
             color = party)) +
  geom_line() +
  labs(y = "DW-NOMINATE score (1st Dimension)", x = "Congress",
       color = "Party")






## ---- fig.show="hold", out.width="45%"--------------------
## First, reshape the median data and calculate partisan difference
polarization <- median_dw1 %>% 
  pivot_wider(names_from = party,
              values_from = median_dw1) %>% 
  mutate(polarization = Republican - Democrat)

## Plot polarization over time (by congress)
ggplot(polarization, aes(x = congress, y = polarization)) +
  geom_point() +
  labs(x = "Congress", y = "Republican median -\n Democratic median") +
  ggtitle("Political polarization")

## Read in the Gini data
data("USGini", package = "qss")

## Plot US Gini over time (by year)
ggplot(USGini, aes(x = year, y = gini)) +
  geom_point() +
  labs(x = "Year", y = "Gini coefficient") +
  ggtitle("Income inequality")




## ---------------------------------------------------------
## Every second year Gini
gini_2yr <- USGini %>% 
  filter(row_number() %% 2 == 0) %>% 
  select(gini) %>% 
  pull()

## Pull out the polarization score
pol_annual <- polarization %>% 
  select(polarization) %>% 
  pull()

## The correlation
cor(gini_2yr, pol_annual)


## ---------------------------------------------------------
congress %>%
  filter(congress == 112, party %in% c("Republican", "Democrat")) %>%
  ggplot(aes(x = dwnom2, y = ..density..)) +
  geom_histogram(binwidth = .2) +
  facet_grid(party ~ .) +
  labs(x = "Racial liberalism/conservatism dimension",
       y = "Density")


## ---- message=FALSE, warning=FALSE, fig.align = 'center'----
quantile_probs <- seq(from = 0, to = 1, by = 0.01)
quantile_names <- as.character(quantile_probs)

## The quantile data
quantiles <- congress %>% 
  filter(congress == 112) %>% 
  group_by(party) %>% 
  summarize(dwnom_quantile = quantile(dwnom2, probs = quantile_probs),
            quantile = quantile_names) %>% 
  pivot_wider(names_from = party,
              values_from = dwnom_quantile)
## plot it
ggplot(data = quantiles,
       aes(x = Democrat,
           y = Republican)) +
  geom_point(shape = 1) +
  ylim(-1.5, 1.5) +
  xlim(-1.5, 1.5) +
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Racial liberalism/conservatism \ndimension") +
  coord_fixed()


## ----eval=FALSE-------------------------------------------
## ## x-axis
## dem112 <- filter(congress, party == "Democrat", congress == 112)
## ## y-axis
## rep112 <- filter(congress, party == "Republican", congress == 112)
## 
## ## Q-Q plot
## qqplot(x = dem112$dwnom2,
##        y = rep112$dwnom2,
##        xlab = "Democrats",
##        ylab = "Republicans",
##        xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5),
##        main = "Racial liberalism/conservatism dimension")


## ---------------------------------------------------------
## 3x4 matrix filled by row; first argument take actual entries
x <- matrix(1:12, nrow = 3, ncol = 4, byrow = TRUE)
rownames(x) <- c("a", "b", "c")
colnames(x) <- c("d", "e", "f", "g")
dim(x) # dimension
x


## ---------------------------------------------------------
## data frame can take different data types
y <- data.frame(y1 = as.factor(c("a", "b", "c")), y2 = c(0.1, 0.2, 0.3))
class(y$y1)
class(y$y2)
## as.matrix() converts both variables to character
z <- as.matrix(y)
z


## ---------------------------------------------------------
## column sums
colSums(x)  
## row means
rowMeans(x) 


## ---------------------------------------------------------
## create a list
x <- list(y1 = 1:10, y2 = c("hi", "hello", "hey"),
          y3 = data.frame(z1 = 1:3, z2 = c("good", "bad", "ugly"))) 
## 3 ways of extracting elements from a list 
x$y1 # first element
x[[2]] # second element
x[["y3"]] # third element


## ---------------------------------------------------------
names(x) # names of all elements 
length(x) # number of elements


## ---------------------------------------------------------
## 80th congress, k = 2
k80two.out <- congress %>% 
  filter(congress == 80) %>% 
  select(dwnom1, dwnom2) %>% 
  kmeans(centers = 2, nstart = 5)

## 112th congress, k = 2
k112two.out <- congress %>% 
  filter(congress == 112) %>% 
  select(dwnom1, dwnom2) %>% 
  kmeans(centers = 2, nstart = 5)


## ---------------------------------------------------------
## elements of a k-means list object
names(k80two.out)


## ---- warning=FALSE, message=FALSE------------------------
## final centroids
k80two.out$centers
k112two.out$centers

# load needed library
library(tidymodels) # or library(broom)

## tidy() output
k80two.clusters <- tidy(k80two.out)
k80two.clusters

k112two.clusters <- tidy(k112two.out)
k112two.clusters


## ---------------------------------------------------------
## Members per cluster, 80th
congress80 <-
  congress %>%
  filter(congress == 80) %>%
  mutate(cluster2 = k80two.out$cluster) %>% 
  group_by(party, cluster2) %>%
  count() %>% 
  pivot_wider(names_from = cluster2,
              values_from = n)

## Members per cluster, 112th
congress112 <-
  congress %>%
  filter(congress == 112) %>%
  mutate(cluster2 = k112two.out$cluster) %>% 
  group_by(party, cluster2) %>%
  count() %>% 
  pivot_wider(names_from = cluster2,
              values_from = n)


## ---------------------------------------------------------
## 80th congress, k = 4
k80four.out <- congress %>% 
  filter(congress == 80) %>% 
  select(dwnom1, dwnom2) %>% 
  kmeans(centers = 4, nstart = 5)

## 112th congress, k = 4
k112four.out <- congress %>% 
  filter(congress == 112) %>% 
  select(dwnom1, dwnom2) %>% 
  kmeans(centers = 4, nstart = 5)




## ---- fig.show="hold", out.width='45%', fig.align = 'center'----
## plot the 80th congress
## prepare the data
congress80 <- filter(congress, congress == 80) %>%
  mutate(cluster4 = factor(k80four.out$cluster))

## prepare the centroids
k80four.clusters <- tidy(k80four.out)

## Plot it
ggplot() +
  geom_point(data = congress80,
             aes(x = dwnom1, 
                 y = dwnom2, 
                 color = cluster4)) +
  geom_point(data = k80four.clusters,
             mapping = aes(x = dwnom1, y = dwnom2), 
             size = 3,
             shape = 8) +
  ylim(-1.5, 1.5) +
  xlim(-1.5, 1.5) +
  coord_fixed() +
  theme(legend.position = "none")

## plot the 112th congress
## prepare the data
congress112 <- filter(congress, congress == 112) %>%
  mutate(cluster4 = factor(k112four.out$cluster))

## prepare the centroids
k112four.clusters <- tidy(k112four.out)

## Plot it
ggplot() +
  geom_point(data = congress112,
             aes(x = dwnom1, 
                 y = dwnom2, 
                 color = cluster4)) +
  geom_point(data = k112four.clusters,
             mapping = aes(x = dwnom1, y = dwnom2), 
             size = 3,
             shape = 8) +
  ylim(-1.5, 1.5) +
  xlim(-1.5, 1.5) +
  coord_fixed() +
  theme(legend.position = "none")


