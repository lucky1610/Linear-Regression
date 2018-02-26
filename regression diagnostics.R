regressor = lm(formula = Profit ~ .,
               data = training_set)
summary(regressor)

#Regression Diagnostics
#Multicollinearity
#library(car)
vif(Model1)
#heteroscedasticity, normality, and influential observerations. 
plot(Model1)


#Other methods
#Non-Normality of residuals
qqPlot(regressor, main="QQ Plot")
#Non constant error variance-heteroscedasticity
ncvTest(Model)
plot(fitted(Model), residuals(Model), xlab="Fitted", ylab="Residuals")
abline(h=0, col="red")
#Non independence of errors
# Test for Autocorrelated Errors
durbinWatsonTest(Model)
