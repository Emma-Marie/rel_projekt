##########################################
############# Main free-list #############
#########################################

# Prelims
setwd("/Users/Emma-Marie/Documents/AU/ka_2.semester/rel_projekt/rel_projekt") # set working directory

library(readxl)
library(AnthroTools) 
library(igraph)

# function to load all individual sheets in excel file and turn into data.frame
df_function <- function(sheetname) { # create a function with the name df_function
  xlsx_file <- read_excel("data/DATA_24_cleaning.xlsx", sheet = sheetname, na = "NA") # load the excel sheet 
  as.data.frame(xlsx_file) # turn into data frame
}

# load data
FL_REL <- df_function("RELIGIOSITET") # religiosity data
FL_SPIR <- df_function("SPIRITUALITET") # spiritualiet data

# function to save data frame as .txt
save_df <- function(dat){
  # Collect the original name
  originalName <- deparse(substitute(dat))
  # assign name to data
  assign(originalName, dat)
  # save data frame with the right name
  write.table(dat, file = paste0("data/",originalName, ".txt"), sep = "\t", row.names = TRUE, col.names = NA)
}

# save data as .txt
save_df(FL_REL)
save_df(FL_SPIR)

############# Merge data ###################

# load clean survey data
surv_dat <- read.delim("data/dat_survey_cleaned.txt")
surv_dat[1] <- NULL # remove first column containing row numbers

# Merge survey and FL data by "PARTID"
merge_rel <- merge(surv_dat, FL_REL, by = "PARTID",
                   all.x = TRUE, all.y = TRUE)
merge_spir <- merge(surv_dat, FL_SPIR, by = "PARTID",
                    all.x = TRUE, all.y = TRUE)

# divide data into young and old
rel_y <- subset(merge_rel, UNG == 0)
rel_o <- subset(merge_rel, UNG == 1)
spir_y <- subset(merge_spir, UNG == 0)
spir_o <- subset(merge_spir, UNG == 1)

# check number of participants in each group
table(surv_dat$UNG) # 119 participants in group 0 and 102 in group 1

# check row lengths
length(rel_y$UNG) # 274 rows
length(rel_o$UNG) # 146 rows
length(spir_y$UNG) # 271 rows
length(spir_o$UNG) # 142 rows
# NB: the young data set are much larger than the old data set, even though the number
# of individuals in the two groups are almost the same.

######## Flower plot ########

# function to calculate item salience
item_saliens <- function(dat){
  FL.S <- CalculateSalience(dat,
                            CODE = "CODE",
                            Salience = "Salience", # adds column with salience score to the data
                            Subj = "PARTID",
                            Order = "order")
  # Subsetting data and removing NAs
  labs <- c("PARTID", "order", "CODE", "Salience") # create vector of variable names
  FL.sub <- FL.S[labs] # subsetting the variables
  FL.c <- FL.sub[complete.cases(FL.sub), ] # takes only rows without NAs
  return(FL.c) # return the FL.c value for calculating cultural salience later
}

# function to calculate cultural salience
cul_sal <- function(dat){
  FL.c <- item_saliens(dat) # use the item_salience function
  SAL.tab <- SalienceByCode(FL.c,
                            CODE = "CODE",
                            Salience = "Salience",
                            Subj = "PARTID",
                            #GROUPING
                            dealWithDoubles = "MAX") # takes only the highest salient value of an item item, when a person lists the same item several times 
  SAL.tab[order(-SAL.tab$SmithsS),, drop = F] # sorting the Smiths S values
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
flo_plot(rel_y, "rel young") # religiosity, ≤ 28 years
flo_plot(rel_o, "rel old") # religiosity, > 28 years
flo_plot(spir_y, "spir young") # spirituality, ≤ 28 years
flo_plot(spir_o, "spir old") # spirituality, > 28 years

###### Conceptual network #######

# function for conceptual networks
concept_net <- function(dat, item){
  # run item salience function
  dat_sal <- item_saliens(dat)
  
  # make a table of frequencies
  labs <- c("PARTID", "CODE") 
  d <- dat_sal[labs]
  d.tab <- table(d[1:2])
  
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
       vertex.frame.color = "darkblue", # border color
       vertex.label.color = "black", # label color
       vertex.label.cex = 0.7)     # Label size
  
  dev.off()
}

# Plot conceptual networks
concept_net(rel_y, "Religiosity young") # religiosity, < 28 years
concept_net(rel_o, "Religiosity old") # religiosity, > 28 years
concept_net(spir_y, "Spirituality young") # spirituality, < 28 years
concept_net(spir_o, "Spirituality old") # spirituality, > 28 years
