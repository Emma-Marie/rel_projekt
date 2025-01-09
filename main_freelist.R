##############################################
############### Main free-list #############
#############################################

# Prelims
setwd("/Users/Emma-Marie/Documents/AU/ka_2.semester/rel_projekt/rel_projekt") # set working directory

library(readxl) 
library(AnthroTools)
library(igraph) # used for conceptual network plot

# load all the individual sheets in the excel file and turn into data.frame
df_function <- function(sheetname) { # create a function with the name df_function
  xlsx_file <- read_excel("data/DATA_24_cleaning.xlsx", sheet = sheetname, na = "NA") # load the excel sheet 
  as.data.frame(xlsx_file) # turn into data frame
}

# load FL data
FL_REL <- df_function("RELIGIOSITET")
FL_SPIR <- df_function("SPIRITUALITET")

### save excel data as .txt
save_df <- function(dat){
  # Collect the original name
  originalName <- deparse(substitute(dat))
  # assign name to data
  assign(originalName, dat)
  # save data frame with the right name
  write.table(dat, file = paste0("data/",originalName, ".txt"), sep = "\t", row.names = TRUE, col.names = NA)
}

# use function to save data frames as .txt
save_df(FL_REL)
save_df(FL_SPIR)

######## Merge data ##############################
# load cleaned survey data
surv_dat <- read.delim("data/dat_survey_cleaned.txt")
surv_dat[1] <- NULL # remove first column with row number

# Merge survey and FL data by "PARTID"
merge_rel <- merge(surv_dat, FL_REL, by = "PARTID",
                   all.x = TRUE, all.y = TRUE)
merge_spir <- merge(surv_dat, FL_SPIR, by = "PARTID",
                    all.x = TRUE, all.y = TRUE)

# divide data into young and "old"
rel_y <- subset(merge_rel, UNG == 0)
rel_o <- subset(merge_rel, UNG == 1)
spir_y <- subset(merge_spir, UNG == 0)
spir_o <- subset(merge_spir, UNG == 1)

# check length of data sets to get feeling of the volume of free list data
table(surv_dat$UNG) # 119 of total participants are 0 and 102 are 1

length(rel_y$UNG) # 274 rows
length(rel_o$UNG) # 146
length(spir_y$UNG) # 271
length(spir_o$UNG) # 142
# NB: Data set for younger are much larger than  data set for older, even though the difference 
# in the number of participants in the two groups don't differ that much.

############# Frequency plot ###########################

# create a participant-by-item presence matrix for the data set
freq_plot <- function(dat, item){
  
  # save plot as pdf
  pdf(file = paste0("fig_output/", item, "_freqplot.pdf"),width=4, height=3)
  
  # create the frequency bar plot
  FL.bin <- FreeListTable(dat, CODE = "CODE",
                          Order = "order",
                          Subj = "PARTID",
                          tableType = "PRESENCE")
  SUM <- colSums(FL.bin[,2:ncol(FL.bin)]) # take the sum of each column (frequency of participants who listed each item)
  FREQ <- data.frame(SUM) # turn into data frame
  newdata <- FREQ[order(-FREQ$SUM),,drop = F] # sort the sums from most to least frequent
  barplot(newdata$SUM, names.arg = rownames(newdata), las = 2, ylab = "Frequency of words", main = paste0("Frequency analysis: ", item), col = "darkseagreen")
  
  dev.off() # the plot is done and saved after this line
}

# plot and save
freq_plot(FL_REL, "religion")
freq_plot(FL_SPIR, "spirituality")

######## Flower plot ########

# function to calculate item salience
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

# function to calculate cultural salience
cul_sal <- function(dat){
  FL.c <- item_saliens(dat) # use the item_salience function from before
  SAL.tab <- SalienceByCode(FL.c,
                            CODE = "CODE",
                            Salience = "Salience",
                            Subj = "PARTID",
                            #GROUPING
                            dealWithDoubles = "MAX") # handles when on person list the same item several times, and only takes the highest salient value of the item
  SAL.tab[order(-SAL.tab$SmithsS),, drop = F] # sorting the SmithsS values in the table --> but not necessary (FLowerPlot sorts as well)
  return(SAL.tab) # return to use in flower plot function
}

# function to create flower plot
flo_plot <- function(dat, item){
  # save plot as pdf
  pdf(file = paste0("fig_output/flowerplot_",item,".pdf"),width=4, height=3)
  
  # cultural salience
  SAL.tab <- cul_sal(dat) # use cul_sal function on data
  
  # plot
  par(mar = c(0, 0, 0, 0), mfrow = c(1, 1), cex = 0.8)
  FlowerPlot(SAL.tab, item)
  dev.off()
}

# plot and save
flo_plot(rel_y, "rel young") # religiosity, < 28 years
flo_plot(rel_o, "rel old") # religiosity, > 28 years
flo_plot(spir_y, "spir young") # spirituality, < 28 years
flo_plot(spir_o, "spir old") # spirituality, > 28 years


####### Cluster dendogram #######

# Create function to create and save cluster dendogram
clust_dendo <- function(dat, item){
  
  # run item salience function
  dat_sal <- item_saliens(dat)
  
  # clean bad doubles and PARTIDs
  FL.c <- CleanFreeList(dat_sal, deleteDoubleCode = T, ejectBadSubj = T, 
                            Order = "order", Subj = "PARTID")
  # turn into table
  FL_table <- FreeListTable(FL.c, CODE = "CODE", Salience = "Salience", Subj = "PARTID", Order = "order", tableType = "HIGHEST_RANK") 
  FL_table$Subject <- NULL # delete ID's column, so it isn't interpreted as a variable by R
  
  # Euclidian distance
  d.dist_FL <- dist(t(FL_table)) 
  
  # hierarchical clustering
  hc_FL <- hclust(d.dist_FL) # hierarchical clustering: to make the CODE item names be on the dendogram and not the row names (participant names)
  
  # plot and save as pdf
  pdf(file = paste0("fig_output/dendogram_",item,".pdf"),width=4, height=3)
  par(mar =c(0, 2, 2, 0)) # plot margins
  plot(hc_FL, labels = colnames(FL_table), main = paste0(item)) # set CODE item names as words in the dendogram
  
  dev.off()
  }

# Create dendograms
clust_dendo(rel_y, "rel_young") # religiosity, < 28 years
clust_dendo(rel_o, "rel_older") # religiosity, > 28 years
clust_dendo(spir_y, "spir_young") # spirituality, < 28 years
clust_dendo(spir_o, "spir_older") # spirituality, > 28 years
clust_dendo(merge_rel, "all_rel")
clust_dendo(merge_spir, "all_spir")

####### Conceptual network and co-occurence matrix #######

# create matrix
concept_net <- function(dat, item){
  # run item salience function
  dat_sal <- item_saliens(dat)
  
  labs <- c("PARTID", "CODE") 
  d <- dat_sal[labs]
  d.tab <- table(d[1:2]) # make a table of frequencies
  
  # re-code all frequencies <1 to 1 (relevant when same participant lists the same item more than once)
  d.bin <- apply(d.tab, 2, function(x) ifelse (x > 0, 1, 0)) 
  d.corr <- crossprod(d.bin)
  diag(d.corr) <- 0
  
  # save plot as pdf
  pdf(file = paste0("fig_output/network_",item,".pdf"),width=4, height=3)
  
  # create network
  network <- graph_from_adjacency_matrix(d.corr, mode = "undirected")
  # plot
  par(mar = c(2, 1, 2, 1), mfrow = c(1, 1))
  plot(network, main = paste0(item),
       vertex.color = "lavender",
       vertex.frame.color = "darkblue", # Border color
       vertex.label.color = "black", # label color
       vertex.label.cex = 0.7)     # Label size
  
  dev.off()
}

# Create conceptual network plots
concept_net(rel_y, "Religiosity young") # religiosity, < 28 years
concept_net(rel_o, "Religiosity old") # religiosity, > 28 years
concept_net(spir_y, "Spirituality young") # spirituality, < 28 years
concept_net(spir_o, "Spirituality old") # spirituality, > 28 years
