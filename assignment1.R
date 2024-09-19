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

# load all the individual sheets in the excel file
FL_23_RELSPIR <- read_excel("data/Projekt_FL_23.xlsx", sheet = "RELSPIR", na = "NA")
View(FL_23_RELSPIR)
FL_23_RELSPIR <- as.data.frame("FL_23_RELSPIR") # turn data int data frame
View(FL_23_RELSPIR) # WHY ISNT IT A DATA FRAME???
str(FL_23_RELSPIR)

df_function <- function(sheetname) { # create a function with the name df_function
  read_excel("data/Projekt_FL_23.xlsx", sheet = sheetname, na = "NA")
}

# run function on all the sheets
FL_23_RELSPIR <- df_function("RELSPIR")
FL_23_MENTSUND <- df_function("MENTSUND")



FL_23_GUD <- read_excel("data/Projekt_FL_23.xlsx", sheet = "GUD", na = "NA")
View(FL_23_GUD)

FL_23_KIRKE <- read_excel("data/Projekt_FL_23.xlsx", sheet = "KIRKE", na = "NA")
View(FL_23_KIRKE)

FL_23_KVINDE <- read_excel("data/Projekt_FL_23.xlsx", sheet = "KVINDE", na = "NA")
View(FL_23_KVINDE)

FL_23_KRISTEN <- read_excel("data/Projekt_FL_23.xlsx", sheet = "KRISTEN", na = "NA")
View(FL_23_KRISTEN)

FL_23_MUSLIM <- read_excel("data/Projekt_FL_23.xlsx", sheet = "MUSLIM", na = "NA")
View(FL_23_MUSLIM)

FL_23_VACCINE <- read_excel("data/Projekt_FL_23.xlsx", sheet = "VACCINE", na = "NA")
View(FL_23_VACCINE)


### Save data

save_txt <- function(df_name) { # create a function to save data frame as ,txt
  write.table(df_name, file = "data/FL_23_RELSPIR.txt", sep = "\t", row.names = TRUE, col.names = NA)
}


save_csv <- function(df_name) { # create a function to save data frame as .csv file
  write.table(df_name, file = "data/FL_23_RELSPIR.csv", sep = "\t", row.names = TRUE, col.names = NA)
}

save_csv(FL_23_RELSPIR)


# run function on data frames
save_txt(FL_23_RELSPIR)


########### Code the data! #############

## Merge data sets

### Reshape data to long format
unique(FL$item) # see all unique values in the column "item" - there is 319!

FLlong <- reshape(data, varying = c("item", "CODE"), timevar = "order", idvar = "PARTID", direction = "long", sep = "")

