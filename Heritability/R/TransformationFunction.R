library(MASS)
transformation <- function(trait, genotype, block, covariate, transformation){ 
    # trait of interest, genotype grouping variable (must be a factor), blocking variable (must be a factor), 
    # covariate (e.g., tree size), transformation for covariate
  bc <- boxcox(trait ~ genotype + block + transformation(covariate)) 
  lambda <- bc$x[which.max(bc$y)] # find the lambda value with the largest log likelihood value 
  print(lambda) # this lambda value can be used to transform the y-variable (in this case: trait) with y^lambda
}