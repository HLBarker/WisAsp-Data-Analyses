# Author = Hilary Barker
# Heritability function
library(lme4)
heritability <- function(trait, genotype, block, covariate, transformation, boxcoxlambda) { 
    #  Computes the broad-sense heritability (H^2) for a given trait in a complete randomized block experimental design
    #
    #  Args:
    #    trait = quantitative trait of interest
    #    genotype = grouping factor for the replicated genotypes in the experiment
    #    block = grouping factor of the experimental blocks 
    #    covariate = any covariate/confounding variable (e.g., tree size)
    #    transformation of covariate (e.g., log, sqrt, etc.)
    #    boxcoxlambda = the maximum lambda value from boxcox that is used to transform the y-variable (AKA the trait of interest) 
    #  Returns:
    #    The broad-sense heritability of trait  
  m <- lmer(trait^boxcoxlambda ~ (1|block) + (1|genotype) + transformation(covariate)) # mixed effects model (both block and genotype are
    # random effects)
  VarianceComponents <- as.data.frame(VarCorr(m), comp = "Variance") # extract the variance components from the lmer model
  H2 <- VarianceComponents[1,4] / sum(VarianceComponents$vcov) # calculate H^2 (genotypic variance / phenotypic variance)
  print(H2)
}
