library(readxl)
heart <- read_excel("C:/Users/LENOVO/Downloads/regression project/heart.xlsx")
View(heart)

#assign variable
y=heart$heart.disease
x1=heart$biking
x2=heart$smoking

#regression model
Reg = lm(y~x1+x2)
Reg

#regression equation is given by
#y=14.9847 - 0.2001*x1 + 0.1783*x2

#check the assumption
#1.independece of the observation  (no autocorrelation)
cor(x1,x2)
#The correlation between biking and smoking is small (0.015 is only a 1.5% correlation), so we can include both parameters in our model.

#2.Normality of y
hist(y)
#here dependent variable is normally distributed

#3.Linearity
#first check for y and x1
plot(y,x1)
#here y and x1 are negatively correlated hence there is linear relationship between x1 and y
#noe for y and x2
plot(y,x2)
#the relationship between smoking and heart disease is a bit less clear, it still appears linear.

#4.error follow normal distribution
par(mfrow=c(2,2))
plot(Reg)

#from first plot we can conclude that model is adequate
#fron second plot we can conclude that data is normally distributed.


#anova table and test the significance of the regression
a=anova(Reg)
a

#Summary of the model
s=summary(Reg)
s

#Multiple r sq and adjusted R-sq
M_R_2 = s$r.squared
M_R_2
#Interpretation: Coefficient of determination of 97.96% shows that 97.96% of the variation in Y is explained by regressors X1, X2.
#here we have two regressor x1 and x2
adj_R2 = s$adj.r.squared
adj_R2
sigma_2 = a$`Mean Sq`[3] #sigma_2
sigma_2

confint(Reg)
library(car)
vif(Reg)
#here we can conclude that Vif value are less than 5 or 10 hence multicollinearity is absent 
