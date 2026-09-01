## ---- message=FALSE, warning=FALSE------------------------
## load the packages
library("tm")
library("SnowballC")
library("stringr")
library("tidytext")
library("tidyverse")


## ---------------------------------------------------------
## the location of files from qss package
DIR_SOURCE <- system.file("extdata/federalist", package = "qss")
## loading the corpus
corpus_raw <- VCorpus(DirSource(directory = DIR_SOURCE, pattern = "fp"))
## the corpus object
corpus_raw


## ---------------------------------------------------------
## A tidy version of the corpus
corpus_tidy <- tidy(corpus_raw) %>%
  select(id, text) %>%
  mutate(new_id = as.integer(str_sub(id, start = 3, end = 4)))

glimpse(corpus_tidy)


## ---- warning=FALSE, message=FALSE------------------------
tokens_raw <- corpus_tidy %>%
  ## tokenizes into words
  unnest_tokens(word, text, to_lower = TRUE) %>%
  ## stem the words
  mutate(stem = wordStem(word)) %>%
  ## remove any numbers in the strings
  mutate(word = str_replace_all(word, "\\d+", "")) %>%
  ## drop any empty strings
  filter(word != "")

## look at the result
glimpse(tokens_raw)


## ---------------------------------------------------------
## load standard stop words
data("stop_words", package = "tidytext")
glimpse(stop_words)

## remove stopwords
tokens <- tokens_raw %>%
  anti_join(stop_words, by = "word")




## ---- output.lines=1:3------------------------------------
## the output is truncated here to save space
content(corpus_raw[[10]]) # Essay No. 10


## ---------------------------------------------------------
tokens_counts <- count(tokens, new_id, stem)
head(tokens_counts)


## ---------------------------------------------------------
dtm <- cast_dtm(tokens_counts,
                document = new_id,
                term = stem,
                value = n)
dtm


## ---------------------------------------------------------
inspect(dtm[1:5, 1:8])


## ---------------------------------------------------------
dtm.mat <- as.matrix(dtm)


## ---- fig.show="hold", out.width="45%", warning = FALSE, message=FALSE----
library(wordcloud)
par(mar = c(0, 0, 0, 0))
## wordcloud for document 12
doc_12 <- filter(tokens_counts, new_id == 12)

wordcloud(words = doc_12$stem, freq = doc_12$n,
          max.words = 20)

## wordcloud for document 24
doc_24 <- filter(tokens_counts, new_id == 24)

wordcloud(words = doc_24$stem, freq = doc_24$n,
          max.words = 20)


## ---------------------------------------------------------
stemCompletion(c("revenu", "commerc", "peac", "armi"), corpus_raw)


## ---------------------------------------------------------
tokens_counts <- bind_tf_idf(tokens_counts,
                             term = stem,
                             document = new_id,
                             n = n)
head(tokens_counts)


## ---------------------------------------------------------
dtm_tfidf <- weightTfIdf(dtm)
dtm_tfidf


## ---------------------------------------------------------
## Top words for document 12
tokens_counts %>%
  filter(new_id == 12) %>%
  slice_max(tf_idf, n = 10)

## Top words for document 24
tokens_counts %>%
  filter(new_id == 24) %>%
  slice_max(tf_idf, n = 10)


## ---------------------------------------------------------
k <- 5 # number of clusters
## subset The Federalist papers written by Hamilton
hamilton <- c(1, 6:9, 11:13, 15:17, 21:36, 59:61, 65:85)
hamilton_docs <- filter(tokens_counts,
                        new_id %in% hamilton)

## convert into a document term matrix
## then calculate tf_idf
hamilton_dtm <- cast_dtm(hamilton_docs,
                document = new_id,
                term = stem,
                value = n) %>%
  weightTfIdf()

## check the output
hamilton_dtm

## run k-means, with a set seed for replication
set.seed(1234)
km.out <- kmeans(hamilton_dtm, centers = k)
km.out$iter # check the convergence; number of iterations may vary


## ---------------------------------------------------------
## How many documents per cluster?
table(km.out$cluster)

## label each centroid with the corresponding term
colnames(km.out$centers) <- colnames(hamilton_dtm)

for (i in 1:k) { # loop for each cluster
    print(str_c("CLUSTER ", i))
    print("Top 10 words: ")
    ## create a tibble of the cluster words
    ## print 10 most important terms
    cluster_centers <- enframe(km.out$centers[i, ]) %>%
     slice_max(value, n = 10)
    print(cluster_centers)
    print("Federalist Papers classified:") # extract essays classified
    ## create a tibble of cluster assignments
    cluster_docs <- enframe(km.out$cluster, "document", "cluster") %>%
      filter(cluster == i)
    print(as.vector(cluster_docs$document))
    cat("\n")
}


## ---- warning=FALSE, message=FALSE------------------------
## essays written by Madison, Jay, or joint:
## "hamilton" defined earlier
madison <- c(10, 14, 37:48, 58)
jay <- c(2:5, 64)
joint <- c(18:20)

## the specific words of interest
STYLE_WORDS <- c("although", "always", "commonly", "consequently",
                 "considerable", "enough", "there", "upon", 
                 "while", "whilst")

## add a variable for the author
tokens_raw <- tokens_raw %>%
  mutate(author = case_when(new_id %in% hamilton ~ "Hamilton",
                            new_id %in% madison ~ "Madison",
                            new_id %in% jay ~ "Jay",
                            new_id %in% joint ~ "Joint",
                            TRUE ~ "Disputed"))

## Average word use per thousand words by author
tfm <- tokens_raw %>%
  group_by(author, word) %>%
  ## total term use per author
  summarize(n = n()) %>%
  ungroup() %>%
  group_by(author) %>%
  ## average term use by author per 1000 words
  mutate(tf_thou = n / sum(n) * 1000) %>%
  ## just the words of interest
  filter(word %in% STYLE_WORDS) %>%
  ## drop n for pivoting
  select(-n) %>%
  ## reshape
  pivot_wider(names_from = word,
              values_from = tf_thou) %>%
  mutate_at(vars(always:consequently), replace_na, 0)

tfm


## ---- message=FALSE, warning=FALSE------------------------
## Create new data set for regression
## Average word use per thousand words by author per document
reg_data <- tokens_raw %>%
  group_by(author, new_id, word) %>%
  ## total term use per author-document
  summarize(n = n()) %>%
  ## average term use by author per 1000 words per document
  mutate(tf_thou = n / sum(n) * 1000) %>%
  ## just the words of interest
  filter(word %in% STYLE_WORDS) %>%
  ## create the outcome variable
  mutate(author_outcome = case_when(author == "Hamilton" ~ 1,
                                    author == "Madison" ~ -1,
                                    TRUE ~ NA_real_)) %>%
  ## drop n to reshape
  select(-n) %>%
  pivot_wider(names_from = word,
              values_from = tf_thou) %>%
  mutate_at(vars(always:`while`), replace_na, 0) %>%
  ungroup()


## ---------------------------------------------------------
hm.fit <- lm(author_outcome ~ upon + there + consequently + whilst,
             data = reg_data)
hm.fit


## ---------------------------------------------------------
hm.fitted <- fitted(hm.fit) # fitted values
sd(hm.fitted)


## ---------------------------------------------------------
library(modelr)
## add author predictions
author_data <- reg_data %>%
  add_predictions(hm.fit) %>%
  mutate(pred_author = if_else(pred >= 0, "Hamilton", "Madison"))

## correct predictions rate
author_data %>%
  filter(!is.na(author_outcome)) %>%
  group_by(author) %>%
  summarize(`Proportion Correct` = mean(author == pred_author))


## ---- message=FALSE, warning=FALSE------------------------
ham_mad <- filter(reg_data, !is.na(author_outcome))
n <- nrow(ham_mad)
hm.classify <- as.vector(rep(NA, n), mode = "list") # a container list
for (i in 1:n) {
  ## fit the model to the data after removing the ith observation
    sub.fit <- lm(author_outcome ~ upon + there +
                    consequently + whilst,
                  data = ham_mad[-i, ]) # exclude ith row
    ## predict the authorship for the ith observation
    hm.classify[[i]] <- slice(ham_mad, i) %>% add_predictions(sub.fit)
}
## create output table, calculate prediction rate
bind_rows(hm.classify) %>%
  mutate(pred_author = if_else(pred >= 0, "Hamilton", "Madison")) %>%
  group_by(author) %>%
  summarize(`Proportion Correct` = mean(author == pred_author))




## ---------------------------------------------------------
plot_data <- filter(author_data, !(author %in% c("Jay", "Joint")))

ggplot(plot_data,
       aes(x = new_id, y = pred,
           color = author, shape = author)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point() +
  scale_y_continuous(breaks = seq(10, 80, by = 10),
                     minor_breaks = seq(5, 80, by = 5)) +
  scale_color_manual(values = c("Madison" = "blue",
                                "Hamilton" = "red",
                                "Disputed" = "black")) +
  scale_shape_manual(values = c("Madison" = 16, "Hamilton" = 15,
                                 "Disputed" = 17)) +
  labs(color = "Author", shape = "Author",
       x = "Federalist Papers", y = "Predicted values")


## ---------------------------------------------------------
## load from QSS
data("florentine", package = "qss")
## print out the adjacency (sub)matrix for the first 5 families
florentine[1:5, 1:5]


## ---- message=FALSE, warning=FALSE------------------------
## Count ties for each family
florentine %>%
  group_by(FAMILY) %>%
  rowwise() %>%
  summarize(connections = sum(c_across(ACCIAIUOL:TORNABUON)))


## ----warning=FALSE, message=FALSE-------------------------
library("igraph")  # load the package

## Convert column to rownames, treat as matrix
florence <- florentine %>%
  column_to_rownames(var = "FAMILY") %>%
  as.matrix()

## Convert adjacency matrix to graph object
florence <- graph.adjacency(florence, mode = "undirected", diag = FALSE)


## ----eval=FALSE-------------------------------------------
## plot(florence) # plot the graph (default visualization)


## ----echo=FALSE, warning=FALSE, message=FALSE-------------
## better visual for the plot
par(mar = c(0, 0, 0, 0))
plot(florence, edge.arrow.size=.5, vertex.color="light blue", vertex.size=15,
     vertex.frame.color="gray", vertex.label.color="black",
     vertex.label.cex=0.8, vertex.label.dist=2, edge.curved=0.2)


## ---- warning=FALSE---------------------------------------
degree(florence)


## ---- warning=FALSE---------------------------------------
closeness(florence)


## ---- warning=FALSE---------------------------------------
1 / (closeness(florence) * 15)


## ---------------------------------------------------------
betweenness(florence)


## ----echo = 2:4, eval = FALSE-----------------------------
## par(cex = 1.25)
## plot(florence, vertex.size = closeness(florence) * 1000,
##      main = "Closeness")
## plot(florence, vertex.size = betweenness(florence),
##      main = "Betweenness")




## ----echo = FALSE, fig.show='hold', out.width='45%', warning=FALSE----
par(mar = c(.5, .5, 1, 1))
## better visuals for the plot
plot(florence, vertex.size = closeness(florence) * 1000,
     main = "Closeness", edge.arrow.size=.5, vertex.color="light blue",
     vertex.size=15, vertex.frame.color="gray", vertex.label.color="black",
     vertex.label.cex=0.8, vertex.label.dist=2, edge.curved=0.2)

plot(florence, vertex.size = betweenness(florence),
     main = "Betweenness", edge.arrow.size=.5, vertex.color="light blue",
     vertex.size=15, vertex.frame.color="gray", vertex.label.color="black",
     vertex.label.cex=0.8, vertex.label.dist=2, edge.curved=0.2)






## ---------------------------------------------------------
## load the data
data("twitter.following", package = "qss")
data("twitter.senator", package = "qss")
## rename to be shorter
follow <- twitter.following
senator <- twitter.senator
## examine
head(follow)
head(senator)


## ---------------------------------------------------------
twitter_adj <- graph_from_edgelist(as.matrix(follow),
                                   directed = TRUE)


## ---- warning=FALSE---------------------------------------
senator <- mutate(senator,
         indegree = degree(twitter_adj, mode = "in"),
         outdegree = degree(twitter_adj, mode = "out"))


## ---- warning=FALSE---------------------------------------
## with slice and arrange
arrange(senator, desc(indegree)) %>%
  slice(1:3) %>%
  select(name, party, state, indegree, outdegree)

## with slice_max
slice_max(senator, order_by = indegree, n = 3) %>%
  arrange(desc(indegree)) %>%
  select(name, party, state, indegree, outdegree)




## ---- fig.show='hold', out.width="45%", warning=FALSE-----
# Define scales to reuse for the plots
scale_color_parties <- scale_color_manual("Party",
                                           values = c(R = "red",
                                                       D = "blue",
                                                       I = "green"),
                                          labels = c(R = "Republican",
                                                     D= "Democrat",
                                                     I = "Independent"))
scale_shape_parties <- scale_shape_manual("Party",
                                          values = c(R = 16,
                                                     D = 17,
                                                     I = 4),
                                          labels = c(R = "Republican",
                                                     D= "Democrat",
                                                     I = "Independent"))
## Calculate closeness measures and graph
senator %>%
  mutate(closeness_in = closeness(twitter_adj,
                                          mode = "in"),
         closeness_out = closeness(twitter_adj,
                                           mode = "out")) %>%
  ggplot(aes(x = closeness_in, y = closeness_out,
             color = party, shape = party)) +
  geom_point() +
  scale_color_parties +
  scale_shape_parties +
  labs(main = "Closeness", x = "Incoming path",
       y = "Outgoing path") +
  theme_classic(base_size = 22) +
  theme(legend.position = "none")

## Calculate betweenness measures and graph
senator %>%
  mutate(betweenness_dir = betweenness(twitter_adj,
                                               directed = TRUE),
         betweenness_undir = betweenness(twitter_adj,
                                                 directed = FALSE)) %>%
  ggplot(aes(x = betweenness_dir,
             y = betweenness_undir, color = party,
             shape = party)) +
  geom_point() +
  scale_color_parties +
  scale_shape_parties +
  labs(main = "Betweenness", x = "Directed", y = "Undirected") +
  theme_classic(base_size = 22)



## ----cache=TRUE-------------------------------------------
## Calculate the pagerank
senator <- mutate(senator,
                  page_rank = page_rank(twitter_adj)[["vector"]])

## Create igraph object
net <- graph_from_data_frame(d = follow,
                             vertices = senator,
                             directed=T)
## View the new object
net

## look at some network edges, nodes, and node (vertex) attributes
head(E(net)) ## E() for edges
head(V(net)) ## V() for vertex
head(V(net)$party)


## ----eval = FALSE-----------------------------------------
## ## Code will not run as-is
## ## Adding hypothetical weights to edges
## E(net)$weight <- hypothetical_weights_vector
## 


## ---------------------------------------------------------
## Vector of colors of the nodes based on party
col <- senator %>%
  mutate(col = case_when(party == "R" ~ "red",
                         party == "D" ~ "blue",
                         TRUE ~ "black")) %>%
  select(col) %>% pull()

## plot the new object
## with node size based on page_rank
plot(net, vertex.size = V(net)$page_rank*1000,
     vertex.label = NA, vertex.color = col,
     edge.arrow.size = 0.1,
     edge.width = 0.5)


## ---------------------------------------------------------
PageRank <- function(n, A, d, pr) { # function takes 4 inputs
    deg <- degree(A, mode = "out") # outdegree calculation
    for (j in 1:n) {
        pr[j] <- (1 - d) / n +  d * sum(A[ ,j] * pr / deg)
    }
    return(pr)
}


## ---- eval = FALSE----------------------------------------
## while (condition) {
## 
##     LOOP CONTENTS HERE
## 
## }


## ---------------------------------------------------------
nodes <- 4
## adjacency matrix with arbitrary values
adj <- matrix(c(0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0),
              ncol = nodes, nrow = nodes, byrow = TRUE)
adj
adj <- graph.adjacency(adj)  # turn it into an igraph object


## ---------------------------------------------------------
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






## ---------------------------------------------------------
data("us.cities", package = "maps")
glimpse(us.cities)


## ---------------------------------------------------------
## Filter just the continental US capitals
capitals <- filter(us.cities,
                   capital == 2,
                   !country.etc %in% c("HI", "AK"))

## Convert the USA map data from maps package to a dataframe
usa_map <- map_data("usa")

## Plot the map and capitals
ggplot() +
  geom_map(map = usa_map) +
  borders(database = "usa") +
  geom_point(aes(x = long, y = lat, size = pop),
             data = capitals) +
  # scale size area ensures: 0 = no area
  scale_size_area() +
  coord_quickmap() +
  theme_void(base_size = 12) +
  labs(x = "", y = "",
       size = "Population")


## ---------------------------------------------------------
## Save the top 7 CA cities by population 
cal_cities <- filter(us.cities, country.etc == "CA") %>%
  slice_max(pop, n = 7)

## Map the cities
ggplot() +
  borders(database = "state", regions = "California") +
  geom_point(aes(x = long, y = lat), data = cal_cities) +
  geom_text(aes(x = long, y = lat, label = name),
            position = position_jitter(width = 0.5, height = 0.5),
            data = cal_cities) +
  coord_quickmap() +
  theme_void() +
  labs(x = "", y = "")


## ---------------------------------------------------------
head(usa_map)
dim(usa_map)


## ---------------------------------------------------------
allcolors <- colors()
head(allcolors)   # some colors
length(allcolors) # number of color names


## ---------------------------------------------------------
red <- rgb(red = 1, green = 0, blue = 0) # red
green <- rgb(red = 0, green = 1, blue = 0) # green
blue <- rgb(red = 0, green = 0, blue = 1) # blue
c(red, green, blue) # results


## ---------------------------------------------------------
black <- rgb(red = 0, green = 0, blue = 0) # black
white <- rgb(red = 1, green = 1, blue = 1) # white
c(black, white) # results


## ---------------------------------------------------------
rgb(red = c(0.5, 1), green = c(0, 1), blue = c(0.5, 0))


## ---------------------------------------------------------
## semi-transparent blue
blue.trans <- rgb(red = 0, green = 0, blue = 1, alpha = 0.5)
## semi-transparent black
black.trans <- rgb(red = 0, green = 0, blue = 0, alpha = 0.5)


## ---------------------------------------------------------
## Sample data with color and alpha column
sample_data <- tibble(x = rep(1:4, each = 2),
              y = x + rep(c(0, 0.2), times = 2),
              color = rep(c("#000000", "#0000FF"), each = 4),
              alpha = c(1, 1, 0.5, 0.5, 1, 1, 0.5, 0.5))

## plot it
ggplot(aes(x = x, y = y, color = color, alpha = alpha),
       data = sample_data) +
  geom_point(size = 15) +
  scale_color_identity() +
  scale_alpha_identity() 


## ----message=FALSE, fig.show='hold', out.width="45%"------
## Load the data
data("pres08", package = "qss")
## Calculate vote-share
pres08 <- pres08 %>%
  mutate(Dem = Obama / (Obama + McCain),
         Rep = McCain / (Obama + McCain))

## Set the purple shade
cal_color <- filter(pres08, state == "CA") %>%
  mutate(purple_shade = rgb(red = Rep,
                            green = 0,
                            blue = Dem)) %>%
  select(purple_shade) %>% pull()

## Plot California as blue
ggplot() +
  borders(database = "state", regions = "California", fill = "blue") +
  coord_quickmap() +
  theme_void()

## Plot California as purple shade
ggplot() +
  borders(database = "state", regions = "California", fill = cal_color) +
  coord_quickmap() +
  theme_void()


## ---------------------------------------------------------
## prepare the presidential data for merge
## by changing case of the state variable
## and removing unneeded states (plus DC)
pres08 <- mutate(pres08, state = str_to_lower(state.name)) %>%
  filter(!(state %in% c("hawaii",
                         "d.c.",
                         "alaska")))

## take the states map data, remove DC
states <- map_data("state") %>%
  filter(!(region %in% c("hawaii",
                         "district of columbia",
                         "alaska"))) %>%
  ## merge with the presidential data
  full_join(pres08, by = c("region" = "state")) %>%
  ## create a party winner variable
  ## and a shade of winning variable
  mutate(party = if_else(Dem > Rep, "Dem", "Rep"),
         purple_shade = rgb(red = Rep,
                            green = 0,
                            blue = Dem))
## Check the data
glimpse(states)


## ----fig.show='hold', out.width="45%"---------------------
### Plot with red/blue
ggplot(states) +
  geom_polygon(aes(group = group, x = long, y = lat,
                   fill = party)) +
  borders(database = "state") +
  coord_quickmap() +
  scale_fill_manual(values = c("Rep" = "red", "Dem" = "blue"), 
                    guide = "none") +
  theme_void() +
  labs(x = "", y = "")

## Plot with shading
ggplot(states) +
  geom_polygon(aes(group = group, x = long, y = lat,
                   fill = purple_shade)) +
  borders(database = "state") +
  scale_fill_identity() +
  coord_quickmap() +
  theme_void() +
  labs(x = "", y = "")




## ---------------------------------------------------------
## Load the data
data("walmart", package = "qss")

## add a "size" column for larger points for Distribution Centers
## Then recode the "type" levels
walmart <- walmart %>%
  mutate(size = if_else(type == "DistributionCenter", 2, 1),
         type = recode(type, "DistributionCenter" = "Distribution \ncenter",
                       "SuperCenter" = "Supercenter",
                       "Wal-MartStore" = "Walmart"))

## Map it
ggplot() +
  borders(database = "state") +
  geom_point(aes(x = long, y = lat, color = type, size = size),
             data = walmart,
             alpha = 1 / 3) +
  coord_quickmap() +
  scale_size_identity() +
  theme_void(base_size = 12) + # remove all extra formatting
  labs(color = "Type") # change the label for the legend


## ---------------------------------------------------------
walmart.map <- function(data, date) {
    temp <- filter(data, opendate <= date) %>%
    mutate(size = if_else(type == "DistributionCenter", 2, 1))
    ggplot() +
    borders(database = "state") +
    geom_point(aes(x = long, y = lat, color = type, size = size),
             data = temp,
             alpha = 1 / 3) +
    coord_quickmap() +
    scale_size_identity() +
    theme_void(base_size = 12) +
    labs(color = "Type") +
    ggtitle(date)
}


## ---- echo = 2:20, out.width='7cm', out.height='7cm', fig.show='hold'----
par(cex = 1.25)
walmart.map(walmart, as.Date("1974-12-31"))
walmart.map(walmart, as.Date("1984-12-31"))
walmart.map(walmart, as.Date("1994-12-31"))
walmart.map(walmart, as.Date("2004-12-31"))


## ----eval=FALSE-------------------------------------------
## library(gganimate)
## library(lubridate)
## 
## ## Round down to year from opendate
## walmart <- walmart %>%
##   mutate(year = floor_date(opendate, unit = "year"))
## 
## ## Create the animation
## walmart_animated <-
##   ggplot() +
##     borders(database = "state") +
##     geom_point(aes(x = long, y = lat,
##                    color = type),
##                data = walmart) +
##     coord_quickmap() +
##     theme_void() +
##     transition_states(states = year,
##                     transition_length = 0,
##                     state_length = 1) +
##   shadow_mark()
## 
## ## show the animation
## walmart_animated
## 
## ## save the animation
## anim_save("DISCOVERY/walmart.gif")




## ---------------------------------------------------------
cosine <- function(a, b) {
    ## t() transposes a matrix ensuring that vector `a' is multiplied 
    ## by each row of matrix `b'
    numer <- apply(a * t(b), 2, sum) 
    denom <- sqrt(sum(a^2)) * sqrt(apply(b^2, 1, sum))
    return(numer / denom)
}

