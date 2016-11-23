# Function for 95% confidence intervals for the broad-sense heritability estimates
confH2 <- function(lmermodel) {
  ConfVar <- (as.data.frame(confint(lmermodel, level = 0.95)^2))
  colnames(ConfVar) <- c("lower", "upper")
  H2lower <- ConfVar[1,1] / sum(ConfVar[1:3,1])
  H2upper <- ConfVar[1,2] / sum(ConfVar[1:3,2])
  print(H2lower)
  print(H2upper)
}
