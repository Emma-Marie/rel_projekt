#### Workshop 3 Free-listing, 25th September ####
#################################################

# Prelims
setwd("/Users/Emma-Marie/Documents/AU/ka_2.semester/rel_projekt/rel_projekt") # set working directory
library(AnthroTools)

# load data (which I saved as .txt in assignment 1, and therefore the loaded data is a data.frame)
RELSPIR <- read.delim("data/FL_23_RELSPIR.txt")

# get column names
colnames(RELSPIR) # one way, str() is another way.

# item salience
RELSPIR.s <- CalculateSalience(RELSPIR, 
                               Order = "order",
                               Subj = "PARTID",
                               CODE = "CODE",
                               GROUPING = NA,
                                Rescale = FALSE,
                               Salience = "Salience")
# cultural salience
rel_cs <- SalienceByCode(RELSPIR.s,
                      CODE = "CODE",
                      Salience = "Salience",
                      Subj = "PARTID",
                      #GROUPING =
                      dealWithDoubles = "MAX") # handles when on person list the same item several times, and only takes the highest salient value of the item.

# flower plot
par(mar =c(0, 0, 0, 0)) # making the graph have bigger margins, so the plot is easier to see (but doesn't alter the actual plot)
FlowerPlot(rel_cs, "rel/spir")

### Functions
# cultural salience function
c_sal <- function(dat){
  # item salience
  d.s <- CalculateSalience(RELSPIR, 
                    Order = "order",
                    Subj = "PARTID",
                    CODE = "CODE",
                    #GROUPING = NA,
                    Rescale = FALSE,
                    Salience = "Salience")
  # cultural salience
  d_cs <- SalienceByCode(d.s,
                         CODE = "CODE",
                         Salience = "Salience",
                         Subj = "PARTID",
                         #GROUPING =
                         dealWithDoubles = "MAX")
  return(d_cs)
}

rel_cul <- c_sal(RELSPIR)

# create some data
set.seed(99)
fatiqueinprojects <- rnorm(10, 5, 1) # create some data

#create mean function
cal_mean <- function(x){
  y <- mean(x)
  par(mar =c(4, 4, 4, 0)) # play with margins for histogram
  his_plot<- hist(x)
  return(y)
  }

cal_mean(fatiqueinprojects) # calculate mean with function

# function for circles
circ.area <- function (r){
  Area <- pi*r^2
  return(Area)
}

x <- c(1, 2, 3, 4, 5) # five circles with area 1 to 5
circ.area(x)

graphcir <- function(r){
  Area <- pi*r^2
  return(plot(r, Area, xlab = "radius", ylab = "Area", type = "l"))
}

graphcir(x)

x <- 1:1e3 # defining x as all numbers from 1 to 1000
graphcir(x)

# create a lame world clock:-)
timeinaarhus <- function(t, s){
  if (s=="fall")  {
    NYC <- t-6
    VAN <- t-9
    BEI <- t+6
    return(data.frame("Vancouver"=VAN, 
                      "New York City" = NYC,
                      "Beijing" = BEI))
    }
  if (s=="spring") {
    NYC <- t-5 # obs number is random:)
    VAN <- t-8 # obs number is random:)
    BEI <- t+7 # obs number is random:)
    return(data.frame("Vancouver"=VAN, 
                      "New York City" = NYC,
                      "Beijing" = BEI))
  }
    else {
      print("You must type 'spring' or 'fall' :-)")
    }
}

timeinaarhus(11.19, "fall")
timeinaarhus(11.19, "spring")
timeinaarhus(11.19, "hej")

