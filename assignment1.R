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


# load all the individual sheets in the excel file and turn into data.frame
df_function <- function(sheetname) { # create a function with the name df_function
  xlsx_file <- read_excel("data/Projekt_FL_23.xlsx", sheet = sheetname, na = "NA") # load the excel sheet 
  as.data.frame(xlsx_file) # turn into data frame
}

# run function on all the sheets
FL_23_RELSPIR <- df_function("RELSPIR")
FL_23_MENTSUND <- df_function("MENTSUND")
FL_23_GUD <- df_function("GUD")
FL_23_KIRKE <- df_function("KIRKE")
FL_23_KVINDE <- df_function("KVINDE")
FL_23_KRISTEN <- df_function("KRISTEN")
FL_23_MUSLIM <- df_function("MUSLIM")
FL_23_VACCINE <- df_function("VACCINE")

#remove column 5-16 from FL_23_RELSPIR - the other data frames looks fine
View(FL_23_RELSPIR)
FL_23_RELSPIR <- subset(FL_23_RELSPIR, select = -c(5:16)) # drop column 5 to 16, which contains only NAs and one "whut"

### save excel data as .txt
write.table(FL_23_RELSPIR, file = "data/FL_23_RELSPIR.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(FL_23_MENTSUND, file = "data/FL_23_MENTSUND.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(FL_23_GUD, file = "data/FL_23_GUD.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(FL_23_KIRKE, file = "data/FL_23_KIRKE.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(FL_23_KVINDE, file = "data/FL_23_KVINDE.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(FL_23_KRISTEN, file = "data/FL_23_KRISTEN.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(FL_23_MUSLIM, file = "data/FL_23_MUSLIM.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(FL_23_VACCINE, file = "data/FL_23_VACCINE.txt", sep = "\t", row.names = TRUE, col.names = NA)

# save as .csv
#write.table(df_name, file = "data/FL_23_RELSPIR.csv", sep = "\t", row.names = TRUE, col.names = NA)

########### Code the data! #############

## Merge data sets

### Reshape data to long format - just for fun :-)
unique(FL_23_RELSPIR$CODE) # see all unique values in the column "item" - there is 205!

RELSPIR_wide <- reshape(FL_23_RELSPIR,
                  timevar = "order", 
                  idvar = "PARTID", 
                  v.names = "CODE",
                  direction = "wide")
View(RELSPIR_wide)

RELSPIR_long <- reshape(RELSPIR_wide,
                        varying = c("CODE.1", "CODE.2", "CODE.3", "CODE.4", "CODE.5"),
                        timevar = "order", 
                        idvar = "PARTID", 
                        direction = "long")
View(RELSPIR_long)

### Merging data
FL1 <- read.delim("data/FL_23_RELSPIR.txt")
FL2 <- read.delim("data/FL_23_MENTSUND.txt")
FL3 <- read.delim("data/FL_23_GUD.txt")
FL4 <- read.delim("data/FL_23_KIRKE.txt")
FL5 <- read.delim("data/FL_23_KVINDE.txt")
FL6 <- read.delim("data/FL_23_KRISTEN.txt")
FL7 <- read.delim("data/FL_23_MUSLIM.txt")
FL8 <- read.delim("data/FL_23_VACCINE.txt")

mr1 <- merge(FL1, FL2, by = c("PARTID", "order"),
             all.x = TRUE, all.y = TRUE)
mr2 <- merge(mr1, FL3, by = c("PARTID", "order"),
             all.x = TRUE, all.y = TRUE)
#mr3 <- merge(mr2, FL4, by = c("PARTID", "order"),
             all.x = TRUE, all.y = TRUE)
#mr4 <- merge(mr3, FL5, by = c("PARTID", "order"),
             all.x = TRUE, all.y = TRUE)
#mr5 <- merge(mr4, FL6, by = c("PARTID", "order"),
             all.x = TRUE, all.y = TRUE)
#mr6 <- merge(mr5, FL7, by = c("PARTID", "order"),
             all.x = TRUE, all.y = TRUE)
#mr7 <- merge(mr6, FL8, by = c("PARTID", "order"),
             all.x = TRUE, all.y = TRUE)

# inspect the merged data frame
View(mr2)

 # remove column 3 and 6 because they are unnecessary
mr2_clean <- subset(mr2, select = -c(3, 6, 9))
View(mr2_clean)
