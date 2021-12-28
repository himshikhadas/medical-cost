library(readr)
insure <- read.csv("C:/Users/dashi/Downloads/insurance1.csv")
head(insure)
summary(insure)
library(gridExtra)
plot.age <- ggplot(insure, aes(x = age, y = charges)) + geom_point()
plot.bmi <- ggplot(insure, aes(x = bmi, y = charges)) +
  geom_point()
grid.arrange(plot.age, plot.bmi, ncol=2)
#trend is wit holder age, charges increase
plot.sex <- ggplot(insure, aes(x = sex, y = charges)) + geom_boxplot()
plot.smoker <- ggplot(insure, aes(x = smoker, y = charges)) + geom_boxplot()
#smoker pay higher charges compared to non smokers
plot.child <- ggplot(insure, aes(x = as.factor(children), y = charges)) + geom_boxplot()
plot.region <- ggplot(insure, aes(x = region, y = charges)) + geom_boxplot()
grid.arrange(plot.sex, plot.smoker, plot.child, plot.region, ncol=2, nrow=2)
#making plot
ggplot(insure, aes(x = age, y = charges)) +
  geom_point() + geom_hline(yintercept = mean(insure$charges)) + #add line representing the mean charges
geom_smooth(method='lm')
#simple linear regression using age as the predictor variable
mod1 = lm(charges ~ age, data = insure)
summary(mod1)
#square R between age and charges and also tells us that 8.94% of the variation in the outcome variable charges is explained by the predictor variable age. 
#When you take the square root of square-R, you also get the correlation coefficient between age and charges.
sqrt(0.08941)
cor(insure$age, insure$charges, method = "spearman")
#multiple linear regression using age and bmi as predictor variables
mod2 = lm(charges ~ age + bmi, data = insure)
summary(mod2)
anova(mod1, mod2)
mod3 <- lm(charges ~ smoker + age + bmi, data = insure)
summary(mod3)
mod4 <- lm(charges ~ smoker + age + bmi + sex + children + region, data = insure)
summary(mod4)
mod5 <- lm(charges ~ smoker + age + bmi + children + region, data = insure)
summary(mod5)
anova(mod3, mod5)
#checking assumptions about residuals
plot(mod5)
ggplot(insure, aes(x = bmi,y = charges, col = smoker)) + geom_point()
mod7 <- lm(charges ~ smoker + age.square + bmi + children + region + smoker*bmi, data = insure)
summary(mod7)
plot(mod7)
ggplot(insure, aes(x = bmi, y = charges, col = smoker)) + geom_point()
