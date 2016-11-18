library(lme4)
heritability <- function(trait, genotype, block, covariate, transformation, boxcoxlambda) { 
    # trait of interest, genotype grouping variable (must be a factor), blocking variable (must be a factor), 
    # covariate (e.g., tree size), transformation for covariate, the maximum lambda value from boxcox that is
    # used to transform the y-variable (AKA the trait of interest)
  m <- lmer(trait^boxcoxlambda ~ (1|block) + (1|genotype) + transformation(covariate)) # mixed effects model (both block and genotype are
    # random effects)
  VarianceComponents <- as.data.frame(VarCorr(m), comp = "Variance") # extract the variance components from the lmer model
  H2 <- VarianceComponents[1,4] / sum(VarianceComponents$vcov) # calculate the broad-sense heritability (genotypic variance / phenotypic variance)
  print(H2)
}
