

data("swiss")


Model1 <- lm(Fertility~Agriculture,data=swiss)



n <- length(swiss$Agriculture)

x <- swiss$Agriculture

x <- cbind(1,x)

Xmean <- mean(x[,2])
XSigma <- sd(x[,2])


#These 3 are the same:
HatValues3 <- (1/n)+(1/(n-1)*((x-Xmean)/XSigma)^2)
#The first just works with 1 parameter

H <- x %*% solve(t(x) %*% x) %*% t(x)

HatValues2 <- diag(H)

HatValues <- hatvalues(Model1)

#----------------------------------------

Sigma <- summary(Model1)$sigma

e <- resid(Model1)

#These 2 are the same:
rstandard2 <- e/(Sigma*sqrt(1-HatValues))
rstandard <- rstandard(Model1)


#----------------------------------------------
set.seed(24)
x <- c(rnorm(9,mean=5,sd=5),12,15)

set.seed(23)
y <- c(2*x[1:9]+rnorm(9,sd=0.1),36,30)


fit1 <- lm(y~x)

plot(x,y)
abline(fit1)

hatvalues(fit1)

dfbeta(fit1)

#Very high rstudent in 10th:
rstudent(fit1)
rstandard(fit1)

plot(fit1)

x <- x[-10]
y <- y[-10]

fit2 <- lm(y~x)
plot(x,y)
abline(fit1)
abline(fit2)


