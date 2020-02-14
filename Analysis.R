fit1 <- lm(mpg~factor(am),mtcars)
CoefFit1 <- summary(fit1)$coef

fit2 <- lm(mpg~factor(am)+cyl-1,mtcars)

anova(fit1,fit2)

CoefFit2 <- summary(fit2)$coef

VIF(fit2)
##Significant difference comparing transmissions