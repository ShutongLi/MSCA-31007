library(tibble)
#(A)
X = rnorm(1000, 0, 1)
Y = 1 + X + rnorm(1000, 0, 0.5)
base_model = lm(Y~X+1)
base_model_summary = summary(base_model)
X_weight_base = base_model_summary$coefficients["X", "Estimate"]
R2_base = base_model_summary$r.squared

# repeat the following ten times

# initialize an array of length 10 to store the ten weights for X
X_weights = rep(0, 10)
# initialize an array of length 10 to store the ten weights for Z
Z_weights = rep(0, 10)
# initialize an array to store the ten R2
R2s = rep(0, 10)
# same for 10 models
models = list()

for (i in 1:10){
  Z = X + rnorm(1000, 0, 0.5)
  # Z has a much lower weight because Z can be inferred from X
  collinear_model = lm(Y~1+X+Z)
  model_summary = summary(collinear_model)
  X_weights[i] = model_summary$coefficients["X", "Estimate"]
  Z_weights[i] = model_summary$coefficients["Z", "Estimate"]
  R2s[i] = model_summary$r.squared
  models[[i]] = collinear_model
}
#Answer: 
# Z has very little predictive weight in the model with a coefficient
# near 0
# Coefficient of X and R2 for simple regression is not much different
# from that of our model under collinearity. 


#(B)
X2 = rnorm(1000, 0, 1)
Y2 = 1 + X2 + rnorm(1000, 0, 0.5)
Z2 = X2 + rnorm(1000, 0, 0.05)
newdata = tibble(X2, Y2, Z2)
colnames(newdata) = c("X", "Y", "Z")
base_prediction = predict(base_model, newdata)
MSE_base = mean(sum((base_prediction - Y2)^2))
predictions = list()
MSE_collinear = rep(0, 10)
# should be using apply instead of for loop
for (i in 1:10) {
  a_prediction = predict(models[[i]], newdata)
  predictions[[i]] = a_prediction
  MSE_collinear[i] = mean(sum((a_prediction - Y2)^2))
}
# Answer: Due to lack of predictive power in Z shown in part (A),
# models with Z have little difference in terms of performance 
# compared to the simple regression one. In terms of predictive
# accuracy, collinearity does not negatively affect the outcome
# as long as it doesn't cause numerical instability in calculation