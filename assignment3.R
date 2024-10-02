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
