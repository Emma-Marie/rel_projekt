######## ASSIGNMENT 1 ########

### Description: CODE, LOAD, AND SAVE FREE-LIST DATA: Using data from last year’s course, practice coding, loading, and saving free-list data.

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

### Read free-list data
excel_sheets("data/Projekt_FL_23.xlsx") # see name of the sheets in the excel file
FL_relspir <- read_excel("data/Projekt_FL_23.xlsx", na = "NA") # load excel file
FL_relspir <- as.data.frame("") # turn data int data frame

### Save data

write.table(FL, file = "data/FL.txt", sep = "\t", row.names = TRUE, col.names = NA) # saves in tab-delimited format (.txt)
#write.table(FL, file = "data/FL.csv", sep = ";", row.names = TRUE, col.names = NA) # saves in .csv

### Reshape data to long format
unique(FL$item) # see all unique values in the column "item" - there is 319!

FLlong <- reshape(data, varying = c("item", "CODE"), timevar = "order", idvar = "PARTID", direction = "long", sep = "")

