#### Lesson 3th of october - Structure analysis ####

setwd("/Users/Emma-Marie/Documents/AU/ka_2.semester/rel_projekt/rel_projekt") # set working directory

#install.packages("psych") # load package - only needed the first time installing this package
library("psych") # open package
library("devtools")
library(AnthroTools)
library(readxl) 
library(igraph)

# Load Hans' data from last class
#xlsx_file <- read_excel("data_hans.xlsx", sheet = "RELSPIR", na = "NA") # load the excel sheet 
#d <- as.data.frame(xlsx_file) # turn into data frame
#dc <- CleanFreeList(d, deleteDoubleCode = T, ejectBadSubj = T, Order = "order", Subj = "PARTID") # clean

# load my data
MENTSUND <- read.delim("data/FL_23_MENTSUND.txt") 
d <- na.omit(MENTSUND) # remove NA values
dc <- CleanFreeList(d, deleteDoubleCode = T, ejectBadSubj = T, Order = "order", Subj = "PARTID") # BE CAREFULL WITH CLEANING LIKE THIS, you don't know what is lost

# Turn into Euclidian distance matrix
dtab <- FreeListTable(dc, CODE = "CODE", Order = "order", Subj = "PARTID", tableType = "HIGHEST_RANK")
dtab$Subject <- NULL # delete ID's column, so it isn't interpreted as a variable by R
ddist <- dist(t(dtab)) # the t is transposing the direction of the data frame
# dendogram
plot(hclust(ddist))

# multi dimensional scaling
sundcmd <- cmdscale(ddist, eig = TRUE, k = 2)
x <- sundcmd$points[,1]
y <- sundcmd$points[,2]

plot(y ~ x, type = "n",
     xlab = NA, 
     ylab = NA,
     main = NA,
     xlim = c(-1,1), ylim = c(-1,1)) # defining the dimensions of the graph (how long the two axes are) --> NB this is "zoomed in"
  text(y ~ x, labels = rownames(sundcmd$points)) # write the names instead of dots

# conceptional network
d.bin <- apply(dtab, 2, 
               function (x) ifelse (x > 0, 1, 0)) # we are only interested in wether or not something are listed, so if it is listed more than 0, it is turned into a 1 (even if it's mentioned 10 times)
d.corr <- crossprod(d.bin)
diag(d.corr) <- 0

par(mar = c(0, 0, 0, 0), mfrow = c(1, 1)) # margins are turned to 0
network1 <- graph_from_adjacency_matrix(d.corr, mode = "undirected") # creating the newtork, so all the lines in the plot
plot(network1, vertex.color = "pink",
     vertex.label.color = "black")


