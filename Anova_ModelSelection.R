library(UsingR)

data("swiss")

## Check how the predit function calculates




Model1 <- lm(Fertility~Agriculture+Examination,data=swiss)


Model2 <- lm(Fertility~Agriculture+Examination+Education+Catholic,data=swiss)

AnovaRes <- data.frame(anova(Model1,Model2))

#Residual Sum of Square:
RSS1 <- sum( resid(Model1)^2 )

RSS2 <- sum( resid(Model2)^2 )

SS <- RSS1-RSS2

RSq1 <- summary(Model1)$r.square

RSq2 <- summary(Model2)$r.square
n <- length(swiss$Fertility)

Constraints <- AnovaRes[1,1] - AnovaRes[2,1]  #Differnece of DF between Model2 & Model1

UnConstrained <- 4 #Number of variables in Model2

F <- (RSq2-RSq1)* (n - UnConstrained - 1) /  ((1 - RSq2) * Constraints) 

P_Value <- pf(F,Constraints,(n - UnConstrained - 1),lower.tail = FALSE)


