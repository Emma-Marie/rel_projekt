#########################################################
################### ASSIGNMENT 3 #######################
#########################################################

## DESCRIPTION:
# STRUCTURE ANALYSIS: Using the data from last year’s course, do some structure analyses. 
# Make some conceptual distance plots, make a conceptual network plot.
#########################################################

### Prelims

setwd("/Users/Emma-Marie/Documents/AU/ka_2.semester/rel_projekt/rel_projekt") # set working directory

#install.packages("psych") # load package - only needed the first time installing this package
library("psych") # open package
#install.packages("devtools")
library("devtools")
#install_github('alastair-JL/AnthroTools')
library(AnthroTools)
#install.packages("readxl") # used to read excel files into R
library(readxl) 

# load data (which I saved as .txt in assignment 1, and therefore the loaded data is a data.frame)
RelSpir <- read.delim("data/FL_23_RELSPIR.txt") 
RelSpir <- na.omit(RelSpir) # remove NA values

# turn into table - OBS: tableType can be changed (see pp 48)
rs_table <- FreeListTable(RelSpir, CODE = "CODE", Salience = "Salience", Subj = "PARTID", Order = "order", tableType = "HIGHEST_RANK") 
rs_table$Subject <- NULL # delete ID's column, so it isn't interpreted as a variable by R

# Sort by salience and keep the top 10 items (I do this because otherwise the plots are soooo messy!)
#top_items <- head(sort(colSums(rs_table), decreasing = TRUE), 10)
#top_table <- rs_table[, names(top_items)]  # Select only top 10 items

# Distance matrix
rs.dist <- dist(t(rs_table)) # Euclidian distance
EuDistRS <- cmdscale(rs.dist, k = 2) # k is number of dimensions.

# plot the distances spatially in coordinate system
par(mar =c(4, 4, 1, 1)) # set plot margins
plot(EuDistRS, xlab = "Dimension 1", ylab = "Dimension 2", type = "n") # type = "n" removes the dots
text(EuDistRS[, 1], EuDistRS[, 2], labels = colnames(rs_table), cex = 0.8)# add column names as labels. "cex" adjusts the size of the text

# Plot the distances --> a dendrogram
hc <- hclust(rs.dist) # hierarchical clustering -> needed to make the CODE item names be on the dendogram and not the row names (participant names)
par(mar =c(0, 2, 2, 0)) # set plot margins
plot(hc, labels = colnames(rs_table)) # set column names (which is the CODE item names) as the words in the dendogram

#### co-occurrence Matrix and conceptual network plot ###

# create matrix
labs <- c("PARTID", "CODE") 
d <- RelSpir[labs]
d.tab <- table(d[1:2]) # make a table of frequencies (we are not interested in the order in which items were listed)
d.bin <- apply(d.tab, 2, function(x) ifelse (x > 0, 1, 0)) # recode all frequencies <1 to 1 (relevant when same participant lists the same item more than once)

d.corr <- crossprod(d.bin)
diag(d.corr) <- 0

# network of free-list co-occurences
library(igraph)#install.packages("igraph")
network <- graph_from_adjacency_matrix(d.corr, mode = "undirected")
plot(network)# Add labels

#### do the same but with only top 30 items (only if too many items)
# Identify top 30 most salient items (otherwise plot is tooooo messy)
#item_sums <- colSums(d.bin)  # Sum occurrences for each item
#top_items <- names(sort(item_sums, decreasing = TRUE)[1:30]) # Select top 30 items

# Filter the matrix to include only the top 30 items
#d.bin_top <- d.bin[, top_items]  # Binary matrix with top 30 items
#d.corr_top <- crossprod(d.bin_top)  # create a co-occurence matrix
#diag(d.corr_top) <- 0  # Make the diagonals 0s
#network <- graph_from_adjacency_matrix(d.corr_top, mode = "undirected")
#plot(network, vertex.label = top_items)# Add labels for the top 30 items

################# chapter 4 #####################
# load data (from last year)
d <- read.delim("data/FL_23_RELSPIR.txt")

# create presence/absence table (participant-by-item matrix)
M <- FreeListTable(d, CODE = "CODE", Salience = "Salience", Subj = "PARTID", tableType = "PRESENCE")

View(M) # checking table --> the Subjects has its own column
M$Subject <- NULL # delete subjects column, so it isn't interpreted as a variable by R


### Conceptual overlap across domains --> Jaccard's similarity index ###
# define jaccard function
jaccard <- function(var1, var2){
  var1 <- as.numeric(var1) # I have added this, because of error
  var2 <- as.numeric(var2) # I have added this, because of error
  sums <- rowSums(cbind(var1, var2), na.rm = T)
  similarity <- length(sums[sums==2])
  total <- length(sums[sums==1]) + similarity
  similarity/total
}

# define jaccmat function, which is used to run jaccard() over the presence/absence table using a for loop
jaccmat <- function(M){
  newmat <- matrix(NA, ncol = ncol(M), nrow = ncol(M)) # create empty square matrix with same number of rows and columns as M
  for (i in seq_along(M)){ # apply jaqqard() to all combinations of variables in M
    for (j in seq_along(M)){
      newmat[i, j] <- jaccard(M[, i], M[, j])
      colnames(newmat) <- colnames(M)
      rownames(newmat) <- colnames(M)
    }
  }
  newmat <- round(newmat, 2) # round the numbers to two decimals (my addition)
  return(data.frame(newmat)) # return a data frame og the Jaccard matrix
}

# run function on data
jacM <- jaccmat(M)
View(jacM)

### Intragroup sharing and variation --> CSD (Cognitive sharing and organization) ###

CSD <- function(MM, TT, N){ # create function that produces data frame including DSC and Q calculations
  numerator <- MM - TT
  CC <- MM/N
  CSD <- numerator/(MM - MM * N)
  Q <- CSD * CC
  return(data.frame(CSD = CSD, Q = Q, C = CC))
}

# extract necessary values from free-list data
data <- d
MM <- length(table(data$CODE)) # word bank size
TT <- sum(table(data$CODE)) # includes repeats
N <- length(unique(data$PARTID)) # sample size

# run function
CSD(MM, TT, N)



