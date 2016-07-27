# WisAsp Community Analyses
## PerMANOVA to identify tree traits that shape insect communities

Trait <- read.csv("~/Desktop/WisAsp Finalized Trait Data 2014_2015.csv") # import tree trait data
Trait["Blk"] <- as.factor(Trait$Blk)
str(Trait)

InsSpecies <- read.csv("~/Desktop/WisAsp_InsectSurvey2014_species.csv") # import insect community data
str(InsSpecies)


All <- merge(Trait, InsSpecies, by.x = "ID", by.y = "ID") # merge dataframes = resulting dataframe only has the tree information for the trees that were present in both the insect data AND the trait data!
str(All)

AllnoNA <- na.omit(All) # remove rows with NAs (missing data)
str(AllnoNA) 
View(AllnoNA)


Ins <- AllnoNA[,36:49]  # just extract the insect data to then calculate a distance matrix
View(Ins) 

library(vegan)
DisIns <- vegdist(Ins, method="bray") # calculates a bray curtis distance matrix between the trees using the insect community data

# PerManova's for traits, where Y = distance matrix; X = tree trait; stratified by block within garden to account for spatial variation (could instead stratify by surveyor)
adonis(DisIns ~ AllnoNA$Spring.2015.tree.size..cm3..NO.OUTLIER, method = "bray", strata = AllnoNA$Blk)

adonis(DisIns ~ AllnoNA$Spring.2014.tree.size..cm3..NO.OUTLIER, method = "bray", strata = AllnoNA$Blk)

adonis(DisIns ~ AllnoNA$Tree.Growth..cm3..2012_2014, method = "bray", strata = AllnoNA$Blk)

adonis(DisIns ~ AllnoNA$X2014.Avg.EFN., method = "bray", strata = AllnoNA$Blk)

adonis(DisIns ~ AllnoNA$X2014.SD.EFN., method = "bray", strata = AllnoNA$Blk)

adonis(DisIns ~ AllnoNA$X2014.Total.number.of.EFNs.for.11.leaves, method = "bray", strata = AllnoNA$Blk)

adonis(DisIns ~ AllnoNA$X2014.Specific.leaf.area..cm2.g., method = "bray", strata = AllnoNA$Blk)

adonis(DisIns ~ AllnoNA$X2014.Leaf.Area..cm2., method = "bray", strata = AllnoNA$Blk)

adonis(DisIns ~ AllnoNA$X2014.Julian.Budbreak.date, method = "bray", strata = AllnoNA$Blk)

adonis(DisIns ~ AllnoNA$X2014.Julian.Budset.date, method = "bray", strata = AllnoNA$Blk)

