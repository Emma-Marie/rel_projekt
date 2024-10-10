#####################################################
########### Assignment 4 ############################
#####################################################
#### Using the new data you collected, apply #######
#### what you’ve learned in the previous ###########
#### assignments to some of the new data. ##########
####################################################

# prelims
setwd("/Users/Emma-Marie/Documents/AU/ka_2.semester/rel_projekt/rel_projekt") # set working directory

library("psych") # open package
library("devtools")
library(AnthroTools)
library(readxl) 
library(igraph)

# load excel data
d <- as.data.frame(read_excel("data/DATA_24_cleaning.xlsx", 
                               sheet = "RELIGIOSITET"))

### FREQUENCY PLOT
FL.bin <- FreeListTable(d, CODE = "CODE",
                        Order = "order",
                        Subj = "PARTID",
                        tableType = "PRESENCE")
SUM <- colSums(FL.bin[,2:ncol(FL.bin)]) # take the sum of each column (frequency of participants who listed each item)
FREQ <- data.frame(SUM) # turn into data frame
newdata <- FREQ[order(-FREQ$SUM),,drop = F] # sort the sums from most to least frequent
barplot(newdata$SUM, names.arg = rownames(newdata), las = 2, ylab = "Frequency of words", main = "Frequency analysis 'religiøsitet'", col = "pink")

### ITEM SALIENCE
item_saliens <- function(dat){
  FL.S <- CalculateSalience(dat,
                            CODE = "CODE",
                            Salience = "Salience", # adds column with salience score to the data
                            Subj = "PARTID",
                            Order = "order")
  # Subsetting (to retrieve only a subset of the data) data and removing missing cases (NAs)
  labs <- c("PARTID", "order", "CODE", "Salience") # create vector of variable names
  FL.sub <- FL.S[labs] # subsetting the variables
  FL.c <- FL.sub[complete.cases(FL.sub), ] # takes only ROWS (hence the placement of the comma AFTER "complete.cases(FL.sub)") without NAs
  return(FL.c) # return the FL.c value to be used when calculating the cultural salience later
}

d_sal <- item_saliens(d)

### CULTURAL SALIENCE
cul_sal <- function(dat){
  FL.c <- item_saliens(dat) # use the item_salience function from before
  SAL.tab <- SalienceByCode(FL.c,
                            CODE = "CODE",
                            Salience = "Salience",
                            Subj = "PARTID",
                            #GROUPING = "Grouping", # allows us to calculate salience across multiple groups --> DOESN't WORK WITH THIS LINE UNCOMENTET!
                            dealWithDoubles = "MAX") # handles when on person list the same item several times, and only takes the highest salient value of the item
  SAL.tab[order(-SAL.tab$SmithsS),, drop = F] # sorting the SmithsS values in the table --> but not necessary (FLowerPlot sorts as well)
  return(SAL.tab) # return to use in flower plot function
}

d_culsal <- cul_sal(d)

### FLOWER PLOT
flo_plot <- function(dat, item){
  SAL.tab <- cul_sal(dat) # use cul_sal function on data
  originalName <- deparse(substitute(dat))# Collect the original name
  assign(originalName, dat) # assign name to data
  FlowerPlot(SAL.tab, item)
}

flo_plot(d, "rel")

### DISTANCE MATRIX
# turn into table
d <- na.omit(d) # remove NA values
dc <- CleanFreeList(d, deleteDoubleCode = T, ejectBadSubj = T, # clean bad doubles and PARTIDs
                    Order = "order", Subj = "PARTID")
d_table <- FreeListTable(dc, CODE = "CODE", Salience = "Salience", Subj = "PARTID", Order = "order", tableType = "HIGHEST_RANK") 
d_table$Subject <- NULL # delete ID's column, so it isn't interpreted as a variable by R

# Euclidian distance
d.dist <- dist(t(d_table)) 
EuDistRS <- cmdscale(d.dist, k = 2) # k is number of dimensions.

# plot the distances spatially in coordinate system
par(mar =c(4, 4, 1, 1)) # set plot margins
plot(EuDistRS, xlab = "Dimension 1", ylab = "Dimension 2", type = "n") # type = "n" removes the dots
text(EuDistRS[, 1], EuDistRS[, 2], labels = colnames(rs_table), cex = 0.8)# add column names as labels. "cex" adjusts the size of the text

### DENDOGRAM
hc <- hclust(d.dist) # hierarchical clustering -> needed to make the CODE item names be on the dendogram and not the row names (participant names)
par(mar =c(0, 2, 2, 0)) # set plot margins
plot(hc, labels = colnames(d_table)) # set column names (which is the CODE item names) as the words in the dendogram

#### CO-OCCURENCE MATRIX AND CONCEPTUAL NETWORK PLOT ####

# create matrix
labs <- c("PARTID", "CODE") 
d <- d[labs]
d.tab <- table(d[1:2]) # make a table of frequencies (we are not interested in the order in which items were listed)
d.bin <- apply(d.tab, 2, function(x) ifelse (x > 0, 1, 0)) # recode all frequencies <1 to 1 (relevant when same participant lists the same item more than once)
d.corr <- crossprod(d.bin)
diag(d.corr) <- 0

# network of free-list co-occurences
library(igraph)#install.packages("igraph")
network <- graph_from_adjacency_matrix(d.corr, mode = "undirected")
plot(network)# Add labels

### JACCARD'S SIMILARITY INDEX 

# create presence/absence table (participant-by-item matrix)
M <- FreeListTable(d, CODE = "CODE", Salience = "Salience", Subj = "PARTID", tableType = "PRESENCE")
M$Subject <- NULL # delete subjects column, so it isn't interpreted as a variable by R

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

#### CSD (cognitive sharing and organization)

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
