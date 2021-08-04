#Check for interaction effects by checking a model containing X1*X2

data = read_csv("~/downloads/collegebasketball.csv")
Y = data$W
n = length(Y)
X1 = data$ADJOE
X2 = data$ADJDE
X3 = X1*X2

linmod = lm(Y~X1+X2+X3)
summary(linmod)

Y.hat = linmod$fitted.values
e.X3 = linmod$residuals
b0.X3 = linmod$coefficients[1]
b1.X3 = linmod$coefficients[2]
b2.X3 = linmod$coefficients[3]
b3.X3 = linmod$coefficients[4]

#F Test
#Reduced model is simply the original model
linmod.reduced = lm(Y~X1 + X2)

Y.hat.full = Y.hat
Y.hat.reduced = linmod.reduced$fitted.values
#Get degrees of freedom
df.full = n - 3
df.reduced = n-2
sse.full = sum((Y - Y.hat.full)^2)
sse.reduced = sum((Y - Y.hat.reduced)^2)
f.stat = (sse.reduced - sse.full)/(df.reduced - df.full)/(sse.full/df.full)
alpha = 0.05
fstatistic = qf(1 - alpha, df.reduced - df.full, df.full)
pval = 1 - pf(f.stat, df.reduced, df.full)

f.stat
fstatistic



#T-Test

s.b3 = summary(linmod)$coef[4,2]
alpha = 0.05
tquantile = qt(1 - alpha/2, n - 3)
t.test = b3 / s.b3
t.test
tquantile


#For both tests we fail to reject H0 and conclude there are no interaction effects present