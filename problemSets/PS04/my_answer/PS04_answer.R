# remove objects
rm(list=ls())
# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(ggplot2)
library(tidyr)
install.packages(car)
library(car)
data(Prestige)
help(Prestige)
View(Prestige)

#Question1
#(a)
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)
print(View(Prestige))


#(b)
lm1<-lm(prestige~income
                +professional
                +income*professional,data=Prestige)


#(c)
#general equation for both professional and non professional
print(paste("prestige=",lm1$coefficients[[1]],"+",
            lm1$coefficients[[2]],"*", names(lm1$coefficients)[2],"+",
            lm1$coefficients[[3]],"*", names(lm1$coefficients)[3],"+",
            lm1$coefficients[[4]],"*", names(lm1$coefficients)[4]))
#equation for both professional
print(paste("prestige=",lm1$coefficients[[1]]+lm1$coefficients[[3]],"+",
            lm1$coefficients[[2]]+lm1$coefficients[[4]],"*", names(lm1$coefficients)[2]))
#equation for non-professional
print(paste("prestige=",lm1$coefficients[[1]],"+",
            lm1$coefficients[[2]],"*", names(lm1$coefficients)[2]))



#(d)
#The coefficient for income is representing: 
#1.For non-professionals, with every additional 1 dollar of income, the estimated prestige
#increases by 0.00317090909728508 scale points.
#2.For non-professionals, with every additional 1 dollar of income, the estimated prestige
#increases by 0.00317090909728508 scale points




#(e)
#





#(f)
#According to the meaning of the model, when the variable professional = 1, the 
#marginal effect of variable income equals the coefficient of variable income plus
#the coefficient of variable income:professional * income.
delta_income<-1000
delta_prestige<-lm1$coefficients[[2]]*delta_income+lm1$coefficients[[4]]*delta_income*1
print(delta_prestige)




#(g)
#According to the meaning of the model, when the variable income = 6000 and professional = 0, the 
#marginal effect of variable prefessional equals the coefficient of variable professional deduct
#the coefficient of variable income:professional * income.
delta_professional<-1
income<-6000
delta_prestige2<-lm1$coefficients[[3]]*delta_professional+lm1$coefficients[[4]]*income*delta_professional
print(delta_prestige2)



#Question2
#(a)
t_value<-0.042/0.016
p_value <- 2 * (1 - pt(abs(t_value), df=131-3))
print(p_value)
#Because p < 0.05, reject hypothesis



#(b)
t_value2<-0.042/0.013
p_value2<-2 * (1 - pt(abs(t_value2), df=131-3))
print(p_value2)



#(c)
#



#(d)
f_value<-(0.094/3)/((1-0.094)/(131-3))
p_value3<-1 - pf(f_value, 3, 131-3)
p_value3
