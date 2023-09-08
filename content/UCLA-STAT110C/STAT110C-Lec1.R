
set.seed(1) # we will generate different trainging data and fit a model
x <- seq(0,3,by= .01)
t.model <- x^3-4*x^2+4*x
plot(x, t.model, type="l", lwd=2) # black line = true model
y.obs <- t.model+rnorm(length(x), sd=0.2)
points(x, y.obs, col="red", pch=19, cex=0.3)
model1 <- loess(y.obs~x)
lines(x, predict(model1), col="skyblue", lwd=2) # model with less flexibility
model2 <- loess(y.obs~x, span = 0.05)
lines(x, predict(model2), col="forestgreen", lwd=2) # model with high flexbility




set.seed(2) # we will generate different trainging data and fit a model
x <- seq(0,3,by= .01)
t.model <- x^3-4*x^2+4*x
plot(x, t.model, type="l", lwd=2) # black line = true model
y.obs <- t.model+rnorm(length(x), sd=0.2)
points(x, y.obs, col="red", pch=19, cex=0.3)
model1 <- loess(y.obs~x)
lines(x, predict(model1), col="skyblue", lwd=2) # model with less flexibility
model2 <- loess(y.obs~x, span = 0.05)
lines(x, predict(model2), col="forestgreen", lwd=2) # model with high flexbility
