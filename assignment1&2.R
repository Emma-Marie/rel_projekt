#########################################################
################### ASSIGNMENT 1 #######################
#########################################################

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

# OBS: my data file has a "_" more in the name than the one Ben sent us (so it's the same data, but with different name)
excel_sheets("data/Projekt_FL_23_.xlsx") # see name of the sheets in the excel file

# load all the individual sheets in the excel file and turn into data.frame
df_function <- function(sheetname) { # create a function with the name df_function
  xlsx_file <- read_excel("data/Projekt_FL_23_.xlsx", sheet = sheetname, na = "NA") # load the excel sheet 
  as.data.frame(xlsx_file) # turn into data frame
}

FL_23_RELSPIR <- df_function("RELSPIR")
FL_23_MENTSUND <- df_function("MENTSUND")
FL_23_GUD <- df_function("GUD")
#check file
View(FL_23_RELSPIR)
#remove column 5-16 from FL_23_RELSPIR, which contains only NAs and one "whut" - the other data frames looks fine
FL_23_RELSPIR <- subset(FL_23_RELSPIR, select = -c(5:16))

### save excel data as .txt

save_df <- function(dat){
  # Collect the original name
  originalName <- deparse(substitute(dat))
  # assign name to data
  assign(originalName, dat)
  # save data frame with the right name
  write.table(dat, file = paste0("data/",originalName, ".txt"), sep = "\t", row.names = TRUE, col.names = NA)
  #write.table(dat, file = paste0("data/",originalName, ".csv"), sep = "\t", row.names = TRUE, col.names = NA) # save as .csv file
}

# use function to save data frames as .txt
save_df(FL_23_RELSPIR)
save_df(FL_23_MENTSUND)
save_df(FL_23_GUD)

########### Code the data! #############

### Reshape data to long format - just for fun :-)
unique(FL_23_RELSPIR$CODE) # see all unique values in the column "item" - there is 46!

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
#FL4 <- read.delim("data/FL_23_KIRKE.txt")
#FL5 <- read.delim("data/FL_23_KVINDE.txt")
#FL6 <- read.delim("data/FL_23_KRISTEN.txt")
#FL7 <- read.delim("data/FL_23_MUSLIM.txt")
#FL8 <- read.delim("data/FL_23_VACCINE.txt")

merge_1 <- merge(FL1, FL2, by = c("PARTID", "order"),
             all.x = TRUE, all.y = TRUE)
merge_2 <- merge(merge_1, FL3, by = c("PARTID", "order"),
             all.x = TRUE, all.y = TRUE)
#merge_3 <- merge(merge_2, FL4, by = c("PARTID", "order"),
#             all.x = TRUE, all.y = TRUE)
#merge_4 <- merge(merge_3, FL5, by = c("PARTID", "order"),
#             all.x = TRUE, all.y = TRUE)
#merge_5 <- merge(merge_4, FL6, by = c("PARTID", "order"),
#             all.x = TRUE, all.y = TRUE)
#merge_6 <- merge(merge_5, FL7, by = c("PARTID", "order"),
#             all.x = TRUE, all.y = TRUE)
#merge_7 <- merge(merge_6, FL8, by = c("PARTID", "order"),
#             all.x = TRUE, all.y = TRUE)

# Clean: remove column 3 and 6 because they are unnecessary - ONLY do this if it isn't removed already!!!
merge_2 <- subset(merge_2, select = -c(3, 6, 9))
View(merge_2)

# Use AnthroTools to scan and correct mundane mistakes in data
#mr2_clean <- CleanFreeList(mr2) # DOESN'T WORK!!

#########################################################
### Frequency distribution plot

# create a participant-by-item presence matrix for the data set
freq_plot <- function(dat){
  FL.bin <- FreeListTable(dat, CODE = "CODE",
                          Order = "order",
                          Subj = "PARTID",
                          tableType = "PRESENCE")
  SUM <- colSums(FL.bin[,2:ncol(FL.bin)]) # take the sum of each column (frequency of participants who listed each item)
  FREQ <- data.frame(SUM) # turn into data frame
  newdata <- FREQ[order(-FREQ$SUM),,drop = F] # sort the sums from most to least frequent
  barplot(newdata$SUM, names.arg = rownames(newdata), las = 2, ylab = "Frequency of words", main = "Frequency analysis 'religion and spirituality'", col = "pink")
  
  # save plot as png
  originalName <- deparse(substitute(dat))# Collect the original name
  assign(originalName, dat) # assign name to data
  png(file = paste0("fig_output/",originalName, "_freqplot.png"),
      width=600, height=350)
  
  # create the frequency bar plot
  FL.bin <- FreeListTable(dat, CODE = "CODE",
                          Order = "order",
                          Subj = "PARTID",
                          tableType = "PRESENCE")
  SUM <- colSums(FL.bin[,2:ncol(FL.bin)]) # take the sum of each column (frequency of participants who listed each item)
  FREQ <- data.frame(SUM) # turn into data frame
  newdata <- FREQ[order(-FREQ$SUM),,drop = F] # sort the sums from most to least frequent
  barplot(newdata$SUM, names.arg = rownames(newdata), las = 2, ylab = "Frequency of words", main = "Frequency analysis", col = "pink")
  
  dev.off() # the plot is done and saved after this line
}

# making plots of the data
freq_plot(FL_23_RELSPIR)
freq_plot(FL_23_MENTSUND)
freq_plot(FL_23_GUD)

# how to plot the proportions of each item instead (just another way of making the bar plot)
n <- length(unique(FL.bin$Subject))
barplot(newdata$SUM/n, names.arg = rownames(newdata), las = 2, ylim = c(0, 0.5))

#########################################################
################### ASSIGNMENT 2 #######################
#########################################################

### Item salience ###

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

RELSPIR_sal <- item_saliens(FL_23_RELSPIR)

##### Cultural salience/Smiths S #####

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

RELSPIR_culsal <- cul_sal(FL_23_RELSPIR)

# create flower plot
flo_plot <- function(dat, item){
  SAL.tab <- cul_sal(dat) # use cul_sal function on data
  originalName <- deparse(substitute(dat))# Collect the original name
  assign(originalName, dat) # assign name to data
  FlowerPlot(SAL.tab, item)
}

flo_plot(FL_23_RELSPIR, "rel/spir")
flo_plot(FL_23_MENTSUND, "mentsund")
flo_plot(FL_23_GUD, "gud")


