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

# calculate item salience (from assignment 2)
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

rs_sal <- item_saliens(RelSpir)
View(rs_sal)

# turn into table
rs_table <- FreeListTable(rs_sal, CODE = "CODE", Salience = "Salience", Subj = "PARTID", tableType = "PRESENCE") # tableType can be changed (see pp 48)
View(rs_table)

### multi-dimensional scaling (MIDS) with free-list dara


