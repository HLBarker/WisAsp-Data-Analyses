# Author = Hilary Barker
# Function for finding the BoxCox transformation for calculating heritability
library(MASS)
transformation <- function(trait, genotype, block, covariate, transformation) { 
    #  Identifies the BoxCox transformation for a trait y-variable in a heritability model
    #
    #  Args:
    #    trait = trait = quantitative trait of interest
    #    genotype = grouping factor for the replicated genotypes in the experiment
    #    block = grouping factor of the experimental blocks 
    #    covariate = any covariate/confounding variable (e.g., tree size)
    #    transformation of covariate (e.g., log, sqrt, etc.)
    #
    #  Returns:
    #    The maximum lambda value identified by BoxCox 
  bc <- boxcox(trait ~ genotype + block + transformation(covariate)) 
  lambda <- bc$x[which.max(bc$y)] # find the lambda value with the largest log likelihood value 
  print(lambda) # this lambda value can be used to transform the y-variable (in this case: trait) with y^lambda
}
