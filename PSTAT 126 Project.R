getwd()
pd<-read.table("C:/Users/ingda/Desktop/projdata.txt",header=TRUE)
View(pd)
summary(pd)
attach(pd)
#scatterplot matrix
pairs(happy~gender+workhrs+relationship, main="Scatterplot of Happiness Based on Gender, Workhrs, and Relationship")
## happy and gender: Can't detect any patten as gender has only 2 levels:0 and 1
## happy and workhrs: no relationship
## Postitive linear relationship
## gender and workhrs: can't detect any pattern as gender has only 2 levels: 0 and 1
## gender and relationship: Cant'detect any pattern as gender has only 2 levels: 0 and 1
## workhrs and relationship: no relationship
#first-order model
fitpd<-lm(happy~gender+workhrs+relationship)
summary(fitpd)
## happy = 3.54123 + 1.55447(gender) + (-0.07118)(workhrs) + .48538(relationship)
## R^2 = .907: About 90.7 percent of the total variantion in the response can be explained by its linear relationship wth predictors
## The p-value is less then 0.05 so the linear relationship between the response variable and predictors is significant
## the full model is significant
#full model
fit.full<-lm(happy~.^2,data=pd)
summary(fit.full)
#anova
anova(fitpd,fit.full)
## since the p-value is for this f-test is less than 0.05, we can reject the null hypothesis. We prefer the model with interaction terms
##the full model does a better job at fitting the data
#stepback Regression (backward elimination)
fit.back<-step(fit.full,direction="backward")
summary(fit.back)
## best model: happy = 4.287745
#stepwise regression (forward addition)
fit.null<-lm(happy~1,data=pd)
fit.forward<-step(fit.null,scope=list(lower=fit.null,upper=fit.full),direction="forward")
summary(fit.forward)
## we get the sane equation
#residual plot
resd<-residuals(fit.full)
pred<-fitted(fit.full)
plot(pred,resd,ylim=c(-1,1),xlab="Predicted Value",ylab="residual",main="Residual Versus Predicted Value",pch=19)
abline(h=0)
##independent? Ask in lecture
#qq normal plot with qq lind
qqnorm(resd,pch=19)
qqline(resd)
## fairly normal, but small sample size so we can't if it'll curve out and be non-normal with a larger sample size
#Histogram
hist(resd,breaks=20,main="Histogram of Residual")
hist(pd)
#scatterplot of residual(test for independence)a
plot(resd,main="Scatterplot of Residual",pch=19)
abline(h=0)
