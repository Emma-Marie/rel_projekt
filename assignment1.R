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

#remove column 5-16 from FL_23_RELSPIR, which contains only NAs and one "whut" - the other data frames looks fine
FL_23_RELSPIR <- subset(FL_23_RELSPIR, select = -c(5:16))
# drop row 6, because it contains a 6th freelist word from one participant, and that wasn't part of the freelist task
FL_23_RELSPIR <- FL_23_RELSPIR[-c(6), ]

### save excel data as .txt

save_df <- function(dat){
  # Collect the original name
  originalName <- deparse(substitute(dat))
  # assign name to data
  assign(originalName, dat)
  # save data frame with the right name
  write.table(dat, file = paste0("data/",originalName, ".txt"), sep = "\t", row.names = TRUE, col.names = NA)
  # Uncomment the line below to save as .csv file
  #write.table(dat, file = paste0("data/",originalName, ".csv"), sep = "\t", row.names = TRUE, col.names = NA)
}

# use function to save data frames as .txt
save_df(FL_23_RELSPIR)
save_df(FL_23_MENTSUND)
save_df(FL_23_GUD)
save_df(FL_23_KIRKE)
save_df(FL_23_KVINDE)
save_df(FL_23_KRISTEN)
save_df(FL_23_MUSLIM)
save_df(FL_23_VACCINE)

########### Code the data! #############

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

### Clean data

# remove column 3 and 6 because they are unnecessary - ONLY do this if it isn't removed already!!!
mr2 <- subset(mr2, select = -c(3, 6, 9))
View(test_mr2)

# Use AnthroTools to scan and correct mundane mistakes in data
#mr2_clean <- CleanFreeList(mr2) # DOESN'T WORK!!

#########################################################

### Frequency distribution plot

# create a participant-by-item presence matrix
mr2.bin <- FreeListTable(mr2, CODE = "CODE",
                         Order = "order",
                         Subj = "PARTID",
                         tableType = "PRESENCE")



