# getting data
dataPath<-"./"
dat <- read.table(paste(dataPath,'Week5_Test_Sample.csv',sep = '/'), header=TRUE)
plot(dat$Input,dat$Output, type="p",pch=19)
nSample<-length(dat$Input)
# training the base model
GeneralModel = lm(Output~Input,dat)
matplot(dat$Input,cbind(dat$Output,GeneralModel$fitted.values),type="p",pch=16,ylab="Sample and Fitted Values")
estimatedResiduals<-GeneralModel$residuals
plot(dat$Input,estimatedResiduals)

# checking the residuals 1
Probability.Density.Residuals<-density(estimatedResiduals)
plot(Probability.Density.Residuals,ylim=c(0,.7))
lines(Probability.Density.Residuals$x,
      dnorm(Probability.Density.Residuals$x,mean=mean(estimatedResiduals),sd=sd(estimatedResiduals)))

### Checking residuals through deviation
# plot deviation
plot(dat$Input,(dat$Output-mean(dat$Output))^2, type="p",pch=19,
     ylab="Squared Deviations")
# there are two obvious parabolas, 
# we should separate them
# find parabola corresponding to fitted model Generalmodel
deviation = (dat$Output-mean(dat$Output))^2
y_hat = GeneralModel$fitted.values
model_deviation = (y_hat - mean(y_hat))^2
matplot(
  dat$Input,cbind(deviation,model_deviation),
  type="p",
  pch=16,ylab="actual and model deviation")

above_idx = deviation > model_deviation
below_idx = !above_idx
above_X = dat$Input[above_idx]
above_Y = dat$Output[above_idx]

below_X = dat$Input[below_idx]
below_Y = dat$Output[below_idx]

mSteep = lm(above_Y~above_X)
mFlat = lm(below_Y~below_X)

mSteep_Y = mSteep$fitted.values
mSteep_dev = (mSteep_Y - mean(mSteep_Y))^2

mFlat_Y = mFlat$fitted.values
mFlat_dev = (mFlat_Y - mean(mFlat_Y))^2
plot(dat$Input,
     model_deviation,
     type="p",pch=19,ylab="Squared Deviations", col='red')
points(above_X, mSteep_dev, col = 'blue')
points(below_X, mFlat_dev, col = 'green')
res <- list( GeneralModel = GeneralModel,mSteep = mSteep,mFlat = mFlat)
saveRDS(res, file = paste(dataPath,'result.rds',sep = '/'))
