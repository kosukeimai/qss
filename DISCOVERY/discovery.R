##### Chapter 5: Discovery

#### Section 5.1: Textual Data

### Section 5.1.1: The Disputed Authorship of 'The Federalist Papers'

## load two required libraries
library(tm, SnowballC)

## load the raw corpus
corpus.raw <- Corpus(DirSource(directory = "federalist", pattern = "fp")) 
corpus.raw

## make lower case
corpus.prep <- tm_map(corpus.raw, content_transformer(tolower)) 
## remove white space
corpus.prep <- tm_map(corpus.prep, stripWhitespace) 
## remove punctuation 
corpus.prep <- tm_map(corpus.prep, removePunctuation)

## remove numbers
corpus.prep <- tm_map(corpus.prep, removeNumbers) 

head(stopwords("english"))

## remove stop words 
corpus <- tm_map(corpus.prep, removeWords, stopwords("english")) 

## finally stem remaining words
corpus <- tm_map(corpus, stemDocument) 

## the output is truncated here to save space
content(corpus[[10]]) # Essay No. 10

### Section 5.1.2: Document-Term Matrix

dtm <- DocumentTermMatrix(corpus)
dtm

inspect(dtm[1:5, 1:8])

dtm.mat <- as.matrix(dtm)

### Section 5.1.3: Topic Discovery

library(wordcloud)

wordcloud(colnames(dtm.mat), dtm.mat[12, ], max.words = 20)  # essay No. 12
wordcloud(colnames(dtm.mat), dtm.mat[24, ], max.words = 20)  # essay No. 24

stemCompletion(c("revenu", "commerc", "peac", "army"), corpus.prep)

dtm.tfidf <- weightTfIdf(dtm) # tf-idf calculation

dtm.tfidf.mat <- as.matrix(dtm.tfidf)  # convert to matrix

## 10 most important words for Paper No. 12
head(sort(dtm.tfidf.mat[12, ], decreasing = TRUE), n = 10)

## 10 most important words for Paper No. 24
head(sort(dtm.tfidf.mat[24, ], decreasing = TRUE), n = 10)

k <- 4  # number of clusters
## subset The Federalist papers written by Hamilton
hamilton <- c(1, 6:9, 11:13, 15:17, 21:36, 59:61, 65:85)
dtm.tfidf.hamilton <- dtm.tfidf.mat[hamilton, ]

## run k-means
km.out <- kmeans(dtm.tfidf.hamilton, centers = k)
km.out$iter # check the convergence; number of iterations may vary

## label each centroid with the corresponding term
colnames(km.out$centers) <- colnames(dtm.tfidf.hamilton)

for (i in 1:k) { # loop for each cluster
    cat("CLUSTER", i, "\n")
    cat("Top 10 words:\n") # 10 most important terms at the centroid
    print(head(sort(km.out$centers[i, ], decreasing = TRUE), n = 10))
    cat("\n")
    cat("Federalist Papers classified: \n") # extract essays classified
    print(rownames(dtm.tfidf.hamilton)[km.out$cluster == i])
    cat("\n")
}

### Section 5.1.4: Authorship Prediction

## document-term matrix converted to matrix for manipulation 
dtm1 <- as.matrix(DocumentTermMatrix(corpus.prep)) 
tfm <- dtm1 / rowSums(dtm1) * 1000 # term frequency per 1000 words

## words of interest
words <- c("although", "always", "commonly", "consequently",
           "considerable", "enough", "there", "upon", "while", "whilst")

## select only these words
tfm <- tfm[, words]

## essays written by Madison: `hamilton' defined earlier
madison <- c(10, 14, 37:48, 58)

## average among Hamilton/Madison essays
tfm.ave <- rbind(colSums(tfm[hamilton, ]) / length(hamilton), 
                 colSums(tfm[madison, ]) / length(madison))
tfm.ave

author <- rep(NA, nrow(dtm1)) # a vector with missing values
author[hamilton] <- 1  # 1 if Hamilton
author[madison] <- -1  # -1 if Madison

## data frame for regression
author.data <- data.frame(author = author[c(hamilton, madison)], 
                          tfm[c(hamilton, madison), ])

hm.fit <- lm(author ~ upon + there + consequently + whilst, 
             data = author.data)
hm.fit

hm.fitted <- fitted(hm.fit) # fitted values
sd(hm.fitted)  

### Section 5.1.5: Cross-Validation

## proportion of correctly classified essays by Hamilton
mean(hm.fitted[author.data$author == 1] > 0)

## proportion of correctly classified essays by Madison
mean(hm.fitted[author.data$author == -1] < 0)

n <- nrow(author.data)
hm.classify <- rep(NA, n) # a container vector with missing values 

for (i in 1:n) {
    ## fit the model to the data after removing the ith observation
    sub.fit <- lm(author ~ upon + there + consequently + whilst, 
                  data = author.data[-i, ]) # exclude ith row
    ## predict the authorship for the ith observation
    hm.classify[i] <- predict(sub.fit, newdata = author.data[i, ])
}

## proportion of correctly classified essays by Hamilton
mean(hm.classify[author.data$author == 1] > 0)

## proportion of correctly classified essays by Madison
mean(hm.classify[author.data$author == -1] < 0)

disputed <- c(49, 50:57, 62, 63) # 11 essays with disputed authorship
tf.disputed <- as.data.frame(tfm[disputed, ])

## prediction of disputed authorship
pred <- predict(hm.fit, newdata = tf.disputed)
pred # predicted values

par(cex = 1.25)
## fitted values for essays authored by Hamilton; red squares
plot(hamilton, hm.fitted[author.data$author == 1], pch = 15, 
     xlim = c(1, 85), ylim  = c(-2, 2), col = "red", 
     xlab = "Federalist Papers", ylab = "Predicted values")
abline(h = 0, lty = "dashed")

## essays authored by Madison; blue circles
points(madison, hm.fitted[author.data$author == -1], 
       pch = 16, col = "blue")

## disputed authorship; black triangles
points(disputed, pred, pch = 17) 

#### Section 5.2: Network Data

### Section 5.2.1: Marriage Network in Renaissance Florence

## the first column "FAMILY" of the CSV file represents row names
florence <- read.csv("florentine.csv", row.names = "FAMILY")
florence <- as.matrix(florence) # coerce into a matrix

## print out the adjacency (sub)matrix for the first 5 families
florence[1:5, 1:5]
rowSums(florence)

### Section 5.2.2: Undirected Graph and Centrality Measures

par(cex = 1.25)

library("igraph")  # load the package

florence <- graph.adjacency(florence, mode = "undirected", diag = FALSE)

plot(florence) # plot the graph
degree(florence)
closeness(florence)

1 / (closeness(florence) * 15)  

betweenness(florence)

par(cex = 1.25) 
plot(florence, vertex.size = closeness(florence) * 1000, 
     main = "Closeness")

plot(florence, vertex.size = betweenness(florence), 
     main = "Betweenness")

### Section 5.2.3: Twitter-Following Network

twitter <- read.csv("twitter-following.csv", stringsAsFactors = FALSE)
senator <- read.csv("twitter-senator.csv", stringsAsFactors = FALSE)

n <- nrow(senator) # number of senators

## initialize adjacency matrix
twitter.adj <- matrix(0, nrow = n, ncol = n)

## assign screen names to rows and columns
colnames(twitter.adj) <- rownames(twitter.adj) <- senator$screen_name

## change `0' to `1' when edge goes from node `i' to node `j'
for (i in 1:nrow(twitter)) {
    twitter.adj[twitter$following[i], twitter$followed[i]] <- 1
}

twitter.adj <- graph.adjacency(twitter.adj, mode = "directed", diag = FALSE)

### Section 5.2.4: Directed Graph and Centrality

senator$indegree <- degree(twitter.adj, mode = "in")
senator$outdegree <- degree(twitter.adj, mode = "out") 

in.order <- order(senator$indegree, decreasing = TRUE)
out.order <- order(senator$outdegree, decreasing = TRUE)

## 3 greatest indegree
senator[in.order[1:3], ]

## 3 greatest outdegree
senator[out.order[1:3], ]

n <- nrow(senator)
## color: Democrats = `blue', Republicans = `red', Independent = `black'
col <- rep("red", n)
col[senator$party == "D"] <- "blue"
col[senator$party == "I"] <- "black"

## pch: Democrats = circle, Republicans = diamond, Independent = cross
pch <- rep(16, n)
pch[senator$party == "D"] <- 17
pch[senator$party == "I"] <- 4

par(cex = 1.25)
## plot for comparing two closeness measures (incoming vs. outgoing)
plot(closeness(twitter.adj, mode = "in"), 
     closeness(twitter.adj, mode = "out"), pch = pch, col = col, 
     main = "Closeness", xlab = "Incoming path", ylab = "Outgoing path")

## plot for comparing directed and undirected betweenness
plot(betweenness(twitter.adj, directed = TRUE), 
     betweenness(twitter.adj, directed = FALSE), pch = pch, col = col,
     main = "Betweenness", xlab = "Directed", ylab = "Undirected")

senator$pagerank <- page.rank(twitter.adj)$vector

par(cex = 1.25)
## `col' parameter is defined earlier
plot(twitter.adj, vertex.size = senator$pagerank * 1000, 
     vertex.color = col, vertex.label = NA, 
     edge.arrow.size = 0.1, edge.width = 0.5)

PageRank <- function(n, A, d, pr) { # function takes 4 inputs
    deg <- degree(A, mode = "out") # outdegree calculation 
    for (j in 1:n) {
        pr[j] <- (1 - d) / n +  d * sum(A[ ,j] * pr / deg)
    }
    return(pr)
}

nodes <- 4

## adjacency matrix with arbitrary values
adj <- matrix(c(0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0), 
              ncol = nodes, nrow = nodes, byrow = TRUE)
adj

adj <- graph.adjacency(adj)  # turn it into an igraph object

d <- 0.85  # typical choice of constant
pr <- rep(1 / nodes, nodes) # starting values

## maximum absolute difference; use a value greater than threshold
diff <- 100

## while loop with 0.001 being the threshold
while (diff > 0.001) {
    pr.pre <- pr # save the previous iteration
    pr <- PageRank(n = nodes, A = adj, d = d, pr = pr)
    diff <- max(abs(pr - pr.pre))
}

pr

#### Section 5.3: Spatial Data

### Section 5.3.1: The 1854 Cholera Outbreak in Action

### Section 5.3.2: Spatial Data in R

library(maps)
data(us.cities)
head(us.cities)

par(cex = 1.25)

map(database = "usa") 
capitals <- subset(us.cities, capital == 2) # subset state capitals

## add points proportional to population using latitude and longitude
points(x = capitals$long, y = capitals$lat,
       cex = capitals$pop / 500000, pch = 19) 
title("US state capitals") # add a title

par(cex = 1.25)
map(database = "state", regions = "California")

cal.cities <- subset(us.cities, subset = (country.etc == "CA"))
sind <- order(cal.cities$pop, decreasing = TRUE) # order by population
top7 <- sind[1:7] # seven cities with largest population

map(database = "state", regions = "California")

points(x = cal.cities$long[top7], y = cal.cities$lat[top7], pch = 19)

## add a constant to latitude to avoid overlapping with circles
text(x = cal.cities$long[top7] + 2.25, y = cal.cities$lat[top7],
     label = cal.cities$name[top7])
title("Largest cities of California")

usa <- map(database = "usa", plot = FALSE) # save map
names(usa)  # list elements

length(usa$x) 
head(cbind(usa$x, usa$y)) # first five coordinates of a polygon

### Section 5.3.3: Colors in R

allcolors <- colors()

head(allcolors)   # some colors
length(allcolors) # number of color names

red <- rgb(red = 1, green = 0, blue = 0) # red
green <- rgb(red = 0, green = 1, blue = 0) # green
blue <- rgb(red = 0, green = 0, blue = 1) # blue
c(red, green, blue) # results

black <- rgb(red = 0, green = 0, blue = 0) # black
white <- rgb(red = 1, green = 1, blue = 1) # white
c(black, white) # results

rgb(red = c(0.5, 1), green = c(0, 1), blue = c(0.5, 0))

## semi-transparent blue
blue.trans <- rgb(red = 0, green = 0, blue = 1, alpha = 0.5) 

## semi-transparent black
black.trans <- rgb(red = 0, green = 0, blue = 0, alpha = 0.5) 

par(cex = 1.25)

## completely colored dots; difficult to distinguish
plot(x = c(1, 1), y = c(1, 1.2), xlim = c(0.5, 4.5), ylim = c(0.5, 4.5), 
     pch = 16, cex = 5, ann = FALSE, col = black)
points(x = c(3, 3), y = c(3, 3.2), pch = 16, cex = 5, col = blue)

## semi-transparent; easy to distinguish
points(x = c(2, 2), y = c(2, 2.2), pch = 16, cex = 5, col = black.trans) 
points(x = c(4, 4), y = c(4, 4.2), pch = 16, cex = 5, col = blue.trans) 

### Section 5.3.4: US Presidential Elections

pres08 <- read.csv("pres08.csv")
## two-party vote share
pres08$Dem <- pres08$Obama / (pres08$Obama + pres08$McCain)
pres08$Rep <- pres08$McCain / (pres08$Obama + pres08$McCain)

## color for California
cal.color <- rgb(red = pres08$Rep[pres08$state == "CA"], 
                 blue = pres08$Dem[pres08$state == "CA"], 
                 green = 0)

par(cex = 1.25)
## California as a blue state
map(database = "state", regions = "California", col = "blue", 
    fill = TRUE)

## California as a purple state
map(database = "state", regions = "California", col = cal.color,
    fill = TRUE)

par(cex = 1.25)
## America as red and blue states
map(database = "state") # create a map
for (i in 1:nrow(pres08)) {
    if ((pres08$state[i] != "HI") & (pres08$state[i] != "AK") &
        (pres08$state[i] != "DC")) {
        map(database = "state", regions = pres08$state.name[i], 
            col = ifelse(pres08$Rep[i] > pres08$Dem[i], "red", "blue"),
            fill = TRUE, add = TRUE)
    }
}

## America as purple states
map(database = "state") # create a map
for (i in 1:nrow(pres08)) {
    if ((pres08$state[i] != "HI") & (pres08$state[i] != "AK") &
        (pres08$state[i] != "DC")) {
        map(database = "state", regions = pres08$state.name[i], 
            col = rgb(red = pres08$Rep[i], blue = pres08$Dem[i],
                green = 0), fill = TRUE, add = TRUE)
    }
}

### Section 5.3.5: Expansion of Walmart

walmart <- read.csv("walmart.csv")

## red = WalMartStore, green = SuperCenter, blue = DistributionCenter
walmart$storecolors <- NA # create an empty vector

walmart$storecolors[walmart$type == "Wal-MartStore"] <- 
    rgb(red = 1, green = 0, blue = 0, alpha = 1/3)
walmart$storecolors[walmart$type == "SuperCenter"] <-
    rgb(red = 0, green = 1, blue = 0, alpha = 1/3)
walmart$storecolors[walmart$type == "DistributionCenter"] <-
    rgb(red = 0, green = 0, blue = 1, alpha = 1/3)

## larger circles for DistributionCenter
walmart$storesize <- ifelse(walmart$type == "DistributionCenter", 1, 0.5)

par(cex = 1.25)
## map with legend
map(database = "state")

points(walmart$long, walmart$lat, col = walmart$storecolors, 
       pch = 19, cex = walmart$storesize)

legend(x = -120, y = 32, bty = "n", 
       legend = c("Wal-Mart", "Supercenter", "Distrib. Center"),
       col = c("red", "green", "blue"), pch = 19, # solid circles
       pt.cex = c(0.5, 0.5, 1)) # size of circles

### Section 5.3.6: Animation in R

walmart.map <- function(data, date) {
    walmart <- subset(data, subset = (opendate <= date))
    map(database = "state")
    points(walmart$long, walmart$lat, col = walmart$storecolors, 
           pch = 19, cex = walmart$storesize)
}

par(cex = 1.25)

walmart$opendate <- as.Date(walmart$opendate)

walmart.map(walmart, as.Date("1974-12-31"))
title("1975")

walmart.map(walmart, as.Date("1984-12-31"))
title("1985")

walmart.map(walmart, as.Date("1994-12-31"))
title("1995")

walmart.map(walmart, as.Date("2004-12-31"))
title("2005")

n <- 25 # number of maps to animate
dates <- seq(from = min(walmart$opendate), 
             to = max(walmart$opendate), length.out = n)
## library("animation")
## saveHTML({
##     for (i in 1:length(dates)) {
##         walmart.map(walmart, dates[i])
##         title(dates[i])
##     }
## }, title = "Expansion of Walmart", htmlfile = "walmart.html",
##          outdir = getwd(), autobrowse = FALSE)

#### 5.4: Summary