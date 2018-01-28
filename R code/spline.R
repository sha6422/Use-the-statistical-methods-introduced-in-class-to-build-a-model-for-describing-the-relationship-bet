# fit spline function to V2 # ------------------------------------
cvs = rep(NA, 10)
for (i in 3:10) {
  fit = glm(V1 ~ bs(V2, df = i), data = train.data)
  cvs[i] = cv.glm(train.data, fit, K = 10)$delta[1]
}
plot(3:10, cvs[-c(1, 2)], xlab = "Cuts", ylab = "Test MSE", type = "l")
which.min(cvs)

# fit spline function to V5 # ------------------------------------
library(splines)
cvs = rep(NA, 10)
for (i in 3:10) {
  fit = glm(V1 ~ bs(V5, df = i), data = train.data)
  cvs[i] = cv.glm(train.data, fit, K = 10)$delta[1]
}
plot(3:10, cvs[-c(1, 2)], xlab = "Cuts", ylab = "Test MSE", type = "l")
which.min(cvs)

# fit spline function to V6 # ------------------------------------
cvs = rep(NA, 10)
for (i in 3:10) {
  fit = glm(V1 ~ bs(V6, df = i), data = train.data)
  cvs[i] = cv.glm(train.data, fit, K = 10)$delta[1]
}
plot(3:10, cvs[-c(1, 2)], xlab = "Cuts", ylab = "Test MSE", type = "l")
which.min(cvs)
