library(dplyr)
library(ggplot2)

str(mtcars)
summary(mtcars$mpg)


ggplot(mtcars,aes(x=factor(mtcars$am),y=mtcars$mpg,fill=factor(mtcars$am))) +
  geom_boxplot()+ 
  scale_fill_discrete(name = "Transmission", labels = c("Automatic", "Manual"))+
  xlab("Transmission") + ylab("MPG")


group_by(mtcars,am) %>% summarise(mean(mpg),sd(mpg)) %>% as.data.frame()



fit1 <- lm(mpg~factor(am),mtcars)
CoefFit1 <- summary(fit1)$coef
    
fit2 <- lm(mpg~factor(am)+cyl-1,mtcars)
    

anova(fit1,fit2)
##Significant difference comparing transmissions




CoefFit2 <- summary(fit2)$coef

vif(lm(mpg~factor(am)+cyl,mtcars))
##Almost no relative variables

cor(mtcars$cyl,mtcars$disp)
cor(mtcars$cyl,mtcars$hp)
cor(mtcars$cyl,mtcars$wt)
##High

fit3 <- lm(mpg~factor(am)+cyl+drat-1,mtcars)


anova(fit1,fit2,fit3)
##No significant result


fit4 <- lm(mpg~factor(am)+cyl+qsec-1,mtcars)


anova(fit1,fit2,fit4)
##No significant result


fit5 <- lm(mpg~factor(am)+cyl+factor(vs)-1,mtcars)

anova(fit1,fit2,fit5)
##No significant result



fit6 <- lm(mpg~factor(am)+cyl+factor(gear)-1,mtcars)

anova(fit1,fit2,fit6)
##No significant result


fit7 <- lm(mpg~factor(am)+cyl+factor(carb)-1,mtcars)

anova(fit1,fit2,fit7)
##No significant result




FittedMPGs <- as.integer(predict(fit2))



mtcars <- mutate(mtcars,FittedMPGs)



