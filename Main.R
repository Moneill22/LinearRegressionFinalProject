
rm(list = ls())
data = read_csv("~/downloads/collegebasketball.csv")

#Extracting the data for all of our covariates and the response
Y = data$W
n = length(Y)
X1 = data$ADJOE
X2 = data$ADJDE

#Finding the means and standard deviations for both covariates and the response
Y.bar = mean(Y)
Y.stdev = sd(Y, na.rm = FALSE)

X1.bar = mean(X1)
X1.stdev = sd(X1, na.rm = FALSE)

X2.bar = mean(X2)
X2.stdev = sd(X2, na.rm = FALSE)

#Using the summary command to get the range for both covariates and the response
summary(Y)
summary(X1)
summary(X2)

#Plotting the response against each of the two covariates
plot(X1, Y, pch = 1, col = "blue", xlab = "ADJOE",ylab = "Wins",xlim = c(70,130), ylim = c(0,40))
plot(X2, Y, pch = 1, col = "blue", xlab = "ADJDE", ylab = "Wins", xlim = c(70, 130), ylim = c(0,40))

#Using the lm function to get an estimated linear model using the covariates and the response
linmod = lm(Y~X1+X2)
summary(linmod)

#The following is all of the code necessary to conduct an F test on whether ot not the beta's are equal to 0
Y.hat = linmod$fitted.values #b0 +b1*X1

SSR = sum((Y.hat - mean(Y))^2)
SSE = sum((Y - Y.hat)^2)
SSTO = sum((Y - mean(Y))^2)

MSR = SSR / 1
MSE = SSE / (n-2)

F.statistic = MSR/MSE
alpha = 0.05
fquantile = qf(1 - alpha, 2, n - 3)
pvalue = 1 - pf(MSR/MSE,2,n-3)

#The following is the code to obtain p-values using a t-test to test the hypotheses of if beta1 = 0 or beta2 = 0

b1 = linmod$coef[2]
s.b1 = summary(linmod)$coef[2,2]
b2 = linmod$coef[3]
s.b2 = summary(linmod)$coef[3,2]
alpha = 0.05
pvalue.b1 = pt(-abs(b1/s.b1),n-3) + (1 - pt(abs(b1/s.b1),n-3))
pvalue.b2 = pt(-abs(b2/s.b2),n-3) + (1 - pt(abs(b1/s.b1),n-3))
t.quant = qt(alpha/2, n-3)


#Redoing the linear model, pulling residuals, fitted values, and values needed to produce the estimated function

linmod = lm(Y~X1+X2)
Y.hat = linmod$fitted.values
e = linmod$residuals
r.sq = summary(linmod)$r.squared
b0 = linmod$coef[1]
b1 = linmod$coef[2]
b2 = linmod$coef[3]

#PLotting the residuals vs the fitted values
par(mfrow = c(1,1))
plot(Y.hat, e ,pch = 1,
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 3)

#Plots for the residuals vs. the predictors
par(mfrow = c(1,2))
plot(X1, e, pch = 1,
     xlab = "ADJOE",
     ylab = "Residuals")
abline(h=0,lty = 3)
plot(X2, e, pch = 1,
     xlab = "ADJDE",
     ylab = "Residuals")
abline(h = 0, lty = 3)

#Plots for a predictor vs. the estimated model where said predictor is removed
par(mfrow = c(1,2))
plot(X1, Y - b0 - b2*X2, pch = 1,
     xlab = "ADJOE",
     ylab = expression(Y-b[0]-b[2]*X[2]))
abline(a = 0, b = b1, col = "pink")
plot(X2, Y - b0 - b1*X1, pch = 1,
     xlab = "ADJDE",
     ylab = expression(Y - b[0]-b[1]*X[1]))
abline(a = 0, b = b2, col = "pink")









