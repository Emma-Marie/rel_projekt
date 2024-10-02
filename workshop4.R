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

# co-occurrence Matrix on fruitlist data from AnthroTools
data("FruitList")
dat <- FruitList

labs <- c("Subj", "CODE") 
d <- dat[labs]
d.tab <- table(d[1:2]) # make a table
d.bin <- apply(d.tab, 2, function(x) ifelse (x > 0, 1, 0)) # recode
d.corr <- crossprod(d.bin) # corespondance matrix
diag(d.corr) <- 0 # make the diagonals 0s

#install.packages("igraph")
library(igraph)
network <- graph_from_adjacency_matrix(d.corr, mode = "undirected")
plot(network)
