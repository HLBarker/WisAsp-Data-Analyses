# Author = Hilary Barker
# Function for heritability confidence intervals
confH2 <- function(lmermodel) {
  #  Computes 95% confidence intervals for broad-sense heritability (H^2) estimates
  #  
  #  Args:
  #    lmermodel = mixed effects model of class mer.mod that is used to calculate H^2
  #      these models are made using the lmer() in the lme4 package
  #
  #  Returns: 
  #    95% Confidence intervals around the H^2 estimates for a trait
  ConfVar <- (as.data.frame(confint(lmermodel, level = 0.95)^2))
  colnames(ConfVar) <- c("lower", "upper")
  H2lower <- ConfVar[1,1] / sum(ConfVar[1:3,1])
  H2upper <- ConfVar[1,2] / sum(ConfVar[1:3,2])
  print(H2lower)
  print(H2upper)
}
