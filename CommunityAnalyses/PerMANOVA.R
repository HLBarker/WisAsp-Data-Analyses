# Author = Hilary Barker
# This script calculates whether quantitative variables (e.g., tree traits) are 
# associated with differences in community composition (e.g., insect communities
# found in the tree's canopies)


# Load packages ---------------------------------------------------------------
library(labdsv)
library(dplyr)
library(vegan)

# Import data -----------------------------------------------------------------
Trait <- read.csv("~/Desktop/WisAsp2014_15_TreeTraits/WisAsp Finalized Trait Data 2014_2015.csv")
Trait["Blk"] <- as.factor(Trait$Blk)
Trait <- cbind(Trait[,1:3], Trait[,5:10], Trait[,19:22], Trait[,26:58])
str(Trait)
colnames(Trait) <- c("Blk", "Row", "Pos", "ID", "Geno", "PlantingDate", "PlantingYear", "Latitude", "Longitude", "AvgBA2014", "AvgBA2015", 
                    "AvgSize2014", "AvgSize2015", "Growth2012_2014", "Growth2014_2015", "Growth2012_2015", "BAI2012_2014", "BAI2014_2015",
                    "BAI2012_2015", "AvgEFN2014", "SDEFN2014", "TotEFN2014", "SLA2014", "LA2014", "SLA2015", "LA2015", "BB2014", "BB2015", "BS2014",
                    "BS2015", "GS2014", "GS2015", "CT2014", "CT2015", "Salicortin2014","Salicortin2015", "Tremulacin2014", "Tremulacin2015",
                    "PG2014", "PG2015", "TotalDefChem2014", "TotalDefChem2015", "CN2014", "CN2015", "N2014", "N2015")
Trait2014 <- cbind(Trait[,1:9], Trait[ , grepl( "2014" , names( Trait ) ) ])

InsSpecies <- read.csv("~/Desktop/WisAsp Insect Data/WisAsp_InsectSurvey2014_species.csv")

# Combine data -----------------------------------------------------------------
InsSpdata <- InsSpecies[,9:22]
InsSpdata["Geno"] <- InsSpecies$GENO
InsSpdataMean <- InsSpdata %>% group_by(Geno) %>% summarise_each(funs(mean))  # get insect means by genotype
str(InsSpdataMean)
Trait2014data <- Trait2014[,8:30]
Trait2014data["Geno"] <- Trait2014$Geno
Trait2014Mean <- Trait2014 %>% group_by(Geno) %>% summarise_each(funs(mean))  # get trait means by genotype

All <- merge(Trait2014Mean, InsSpdataMean, by.x = "Geno", by.y = "Geno")  # merge dataframes = resulting dataframe 
  # only has the tree information for the trees that were present in both the insect data AND the trait data!
str(All)

# PerMANOVA -----------------------------------------------------------------
permanova <- function(trait) {
  # PerMANOVA calculates whether an x-variable (in this case a tree trait) is associated with a 
  # distance matrix (in this case the distance matrix determines how similar insect communities 
  # are on pairs of trees)
  #
  # Args:
  #    trait = quantitative variable that may be linked to the composition of the insect community
  #
  # Returns:
  #   The summary of the PerMANOVA model, showing whether "trait" is significantly associated
  #   with the given trait
  DATA <- as.data.frame(cbind(trait, All$Harmandia, All$Phyllocolpa, All$PetioleGall, All$LeafEdgeMine,All$SerpMine, 
                              All$BlotchMine, All$LombardyMine, All$Gluphisia, All$GreenNematus, All$RustylinedLeaftier, All$ObliqueBandedLeafRoller,
                              All$SmokeyAphids, All$GreenAphids, All$Lasius_neoniger))
  DATA <- na.omit(DATA)
  InsDis <- vegdist(sqrt(DATA[,2:15]), method = "bray")
  adonis(InsDis ~ trait, method = "bray", na.action = na.omit)
}

permanova(All$Latitude)  # test to make sure that the function works

apply(All[,8:30], 2, permanova)  # run the PerMANOVA function on all of the tree traits of interest



