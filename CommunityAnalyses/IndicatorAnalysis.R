# Author = Hilary Barker
# This script identifies which species in a community (e.g., insect community) appear to respond to 
#   environmental variables (e.g., traits of the tree that the insect community is found on)


# Load packages ---------------------------------------------------------------
library(labdsv)
library(dplyr)


# Import data -----------------------------------------------------------------
Trait <- read.csv("~/Desktop/WisAsp Finalized Trait Data 2014_2015.csv")
Trait["Blk"] <- as.factor(Trait$Blk)
str(Trait)
Trait <- cbind(Trait[,1:3], Trait[,5:10], Trait[,19:22], Trait[,26:58])
str(Trait)
colnames(Trait) <- c("Blk", "Row", "Pos", "ID", "GENO", "PlantingDate", "PlantingYear", "Latitude", "Longitude", "AvgBA2014", "AvgBA2015", 
                     "AvgSize2014", "AvgSize2015", "Growth2012_2014", "Growth2014_2015", "Growth2012_2015", "BAI2012_2014", "BAI2014_2015",
                     "BAI2012_2015", "AvgEFN2014", "SDEFN2014", "TotEFN2014", "SLA2014", "LA2014", "SLA2015", "LA2015", "BB2014", "BB2015", "BS2014",
                     "BS2015", "GS2014", "GS2015", "CT2014", "CT2015", "Salicortin2014","Salicortin2015", "Tremulacin2014", "Tremulacin2015",
                     "PG2014", "PG2015", "TotalDefChem2014", "TotalDefChem2015", "CN2014", "CN2015", "N2014", "N2015")
Trait2014 <- cbind(Trait[,1:9], Trait[ , grepl( "2014" , names( Trait ) ) ])
str(Trait2014)

InsSp <- read.csv("~/Dropbox/Hils ToDo List/PROJECTS/WISASP TREE TRAIT SHAPING INSECT COMMUNITIES/WisAsp Insect Data/WisAsp_InsectSurvey2014_species.csv")
str(InsSp)


# Combine data -----------------------------------------------------------------
InsMean <- InsSp %>% group_by(GENO) %>% summarise_each(funs(mean))  # get insect means by genotype
str(InsMean)
TraitMean <- Trait2014 %>% group_by(GENO) %>% summarise_each(funs(mean))  # get trait means by genotype
str(TraitMean)

All <- merge(TraitMean, InsMean, by.x = "GENO", by.y = "GENO")  # merge dataframes 
All <- na.omit(cbind(All$GENO, All[,8:30], All[,38:51]))
str(All)


# Idenfity the extreme trees  -------------------------------------------------
  #  separate the data into two groups: (1) insect communities found on trees that have extremely 
  #  high values of the trait in interest, (2) insect communities found on trees that have extremely
  #  low values of the trait in interest. Then perform species indicator analyses wich identifies
  #  the insect species that define the two different community groups and shed light on which
  #  insects are influenced by a given trait and how (do they favor high or low trait levels?)
extremetrees <- function(trait) {
  # Computes species indicator analysis on the communities found in extreme environments 
  #   (in this case, extreme tree traits)
  #
  # Args:
  #   trait = quantitative variable that may influence species within the community
  #
  # Returns:
  #   Species indicator results for whether particular insects are significantly 
  #   associated with environmental/tree variables
  All <- All[order(trait),]
  Mostextreme <- All[86:114,25:38]  # get the most extreme insect data
  Mostextreme$Ext <- rep(2,nrow(Mostextreme))  # add on a column of ones
  Leastextreme <- All[1:30,25:38]  # get the least extreme insect data
  Leastextreme$Ext <- rep(1,nrow(Leastextreme))  # add on a column of zeros
  Extremes <- rbind(Leastextreme, Mostextreme)  # add the two dataframes together
  indval(Extremes[,1:14], Extremes[,15], numitr= 1000)
}

extremetrees(All$AvgSize2014)

apply(All[,2:5], 2, extremetrees)
apply(All[,6:10], 2, extremetrees)
apply(All[,11:16], 2, extremetrees)
apply(All[,17:23], 2, extremetrees)
