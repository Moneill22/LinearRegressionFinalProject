data = read_csv("~/downloads/collegebasketball.csv")

Y = data$W
n = length(Y)
X1 = data$ADJOE
X2 = data$ADJDE

linmod = lm(Y~X1+X2)

Y.hat = linmod$fitted.values #b0 +b1*X1

SSR = sum((Y.hat - mean(Y))^2)
SSE = sum((Y - Y.hat)^2)
SSTO = sum((Y - mean(Y))^2)

#Remove individual predictors to compare porportion of variance accounted for by each variable

#Model with X2 removed
linmodX1 = lm(Y~X1)

Y.hatX1 = linmodX1$fitted.values
SSEX1 = sum((Y - Y.hatX1)^2)
proportionX1 = (SSEX1 - SSE) / SSTO
proportionX1


#Model with X1 removed
linmodX2 = lm(Y~X2)

Y.hatX2 = linmodX2$fitted.values
SSEX2 = sum((Y - Y.hatX2)^2)
proportionX2 = (SSEX2 - SSE) / SSTO
proportionX2

#Thus X1, or ADJOE, accounts for a higher proportion of the variance of the normal model.