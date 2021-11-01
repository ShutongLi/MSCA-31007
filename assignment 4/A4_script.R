dataPath = './'
dat <- read.table(paste(dataPath,
                        'Week4_Test_Sample.csv',sep = '/'), header=TRUE)
model1 = lm(Y~X, data = dat)
Estimated.Residuals <- model1$residuals
Probability.Density.Residuals <- density(Estimated.Residuals)
plot(Probability.Density.Residuals, ylim = c(0, 0.17))
lines(Probability.Density.Residuals$x, dnorm(Probability.Density.Residuals$x, 
                                             mean = mean(Estimated.Residuals), sd = sd(Estimated.Residuals)))
# residuals is bimodal on left and right of 0.
c(Left.Mean = mean(Estimated.Residuals[Estimated.Residuals < 0]), 
  Right.Mean = mean(Estimated.Residuals[Estimated.Residuals > 0]))
Unscrambled.Selection.Sequence = as.integer(Estimated.Residuals > 0)
res <- list(Unscrambled.Selection.Sequence =  Unscrambled.Selection.Sequence)
write.table(res, file = paste(dataPath,'result.csv',sep = '/'), row.names = F)
