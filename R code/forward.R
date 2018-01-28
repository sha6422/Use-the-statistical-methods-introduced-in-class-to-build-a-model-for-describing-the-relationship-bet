# perform forward stepwise selection ## -----------------------
reg.fit = regsubsets(V1~., data = train.data, method = "forward")
reg.summary = summary(reg.fit)

par(mfrow = c(1, 3))
plot(reg.summary$cp, xlab = "Number of variables", ylab = "Cp", type = "l")
points(which.min(reg.summary$cp), reg.summary$cp[which.min(reg.summary$cp)], 
       cex=2, pch=20, col="red")

plot(reg.summary$bic, xlab = "Number of variables", ylab = "BIC", type='l')
points(which.min(reg.summary$bic), reg.summary$bic[which.min(reg.summary$bic)], 
       cex=2, pch=20, col="red")

plot(reg.summary$adjr2, xlab = "Number of variables", ylab = "Adjusted R2", type = "l")
points(which.max(reg.summary$adjr2), reg.summary$adjr2[which.max(reg.summary$adjr2)], 
       cex=2, pch=20, col="red")

## forward stepwise select 4-variable model ## -------------------------------
reg.full = regsubsets(V1 ~ ., data = train.data, method = "forward")
coeffs = coef(reg.full, id = 4)
names(coeffs)
