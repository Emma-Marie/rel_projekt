#### workshop 2th october ###

setwd("/Users/Emma-Marie/Documents/AU/ka_2.semester/rel_projekt/rel_projekt") # set working directory

#install.packages("psych") # load package - only needed the first time installing this package
library("psych") # open package
#install.packages("devtools")
library("devtools")
#install_github('alastair-JL/AnthroTools')
library(AnthroTools)
#install.packages("readxl") # used to read excel files into R
library(readxl) 

# function for loading excel data and turning it into a data frame
df_function <- function(sheetname) { # create a function with the name df_function
  xlsx_file <- read_excel("data_hans.xlsx", sheet = sheetname, na = "NA") # load the excel sheet 
  as.data.frame(xlsx_file) # turn into data frame
}

# load excel (Hans' data)
d <- df_function("RELSPIR")
d <- na.omit(d) # remone NA values

# create free-list table
dh <- FreeListTable(d, CODE = "CODE", Salience = "Salience", Subj = "PARTID", Order = "order", tableType = "HIGHEST_RANK") # tableType can be changed (see pp 48)
dh$Subject <- NULL # delete ID's column, so it isn't interpreted as a variable by R

d.dist <- dist(t(dh)) # Euclidian distance, tell R that you are working with a distance matrix

# turn distances into spatial coordinates
dcoord <- cmdscale(d.dist, k = 2) # k is number of dimensions.

# plot the spatial distances MY WAY (CHAT GPT)
par(mar =c(5, 5, 1, 1)) # alter margins of plot
plot(dcoord, xlab = "Dimension 1", ylab = "Dimension 2", type = "n") # type = "n" removes the dots
text(dcoord[, 1], dcoord[, 2], labels = colnames(dh), cex = 0.8) # add column names as labels. "cex" adjusts the size of the text

# plot the spatial distances --> BENS WAY (THE BOOK)
dim1 <- dcoord[,1]
dim2 <- dcoord[,2]
plot(dim1, dim2, main = NA, type = "n")
text(dim1, dim2, labels = rownames(dcoord), cex = .9)

# plot cluster Dendrogram
par(mar =c(0, 0, 2, 0)) # alter margins of plot
plot(hclust(d.dist))

### co-occurrence Matrix ###
 # load fruitlist data from AnthroTools
data("FruitList")
dat <- FruitList

# create matrix
labs <- c("Subj", "CODE") 
d <- dat[labs]
d.tab <- table(d[1:2]) # make a table
d.bin <- apply(d.tab, 2, function(x) ifelse (x > 0, 1, 0)) # recode
d.corr <- crossprod(d.bin) # corespondance matrix
diag(d.corr) <- 0 # make the diagonals 0s

# network of free-list co-occurences
library(igraph)#install.packages("igraph")
network <- graph_from_adjacency_matrix(d.corr, mode = "undirected")
plot(network)

### calculating clusters ###
Cluster.fun <- function(var){
  varnums <- as.numeric(as.factor(var))
  y <- rle(varnums)
  n1 <- sum(y$lengths[y$values == 1])
  n2 <- sum(y$lengths[y$values == 2])
  r1 <- length(y$lengths[y$values ==1])
  r2 <- length(y$lengths[y$values ==2])
  C1 <- (n1-r1)/(n1-1)
  C2 <- (n2-r2)/(n2-1)
  N <- length(var)
  R <- length(y$lengths)
  K <- length(unique(var))
  Cprime <- (N-R)/(N-K)
return(data.frame(C1 = C1, C2 = C2),
       Cprime = Cprime)
}

lapply(d, Cluster.fun) # doesn't work...

### Triad Test Workflow ###

# create data
RVS <- c( "Ben", "Uffe", "Jesper", "Lene", "Jorn", "Marianne Q.-F.", "Thomas", "Martin", "Mark") # items

tlist <- data.frame(t(combn(RVS, 3))) # creates a set of all combinations (triads)
tlist$NUM <- seq(1, nrow(tlist), 1) # assign numberic identifyer to each triad
tlist <- tlist[, c(4, 1, 2, 3)] # arrange columns in intuitive way
tlist$rand <- sample(1:nrow(tlist), nrow(tlist), replace = F) # randomizes the order of traids
tlist <- tlist[order(tlist$rand),] # sort
write.csv(tlist, "tlist.csv", row.names = F) # save as .csv


d.diff <- triad.test(tlist) # ERROR
#d <- 1-as.dist(d)
#plot(hclust(d))

