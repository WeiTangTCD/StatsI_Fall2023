#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(ggplot2)
# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/incumbents_subset.csv")
View(inc.sub)
str(inc.sub)
#Question1
#1.
model1 <- lm(voteshare ~ difflog, data = inc.sub)
summary(model1)
#2.

ggplot(inc.sub, aes(difflog, voteshare)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "voteshare ~ difflog", x = "difflog", y = "voteshare")
#3.
residuals1 <- model1$residuals
print(residuals1)
#4.
coefficients1 <- coef(model1)
print(coefficients1)
prediction_equation1 <- paste("voteshare =", round(coefficients1[1], 5), "+", round(coefficients1[2], 5), "* difflog")
print(prediction_equation1)

#Question2
#1.
model2 <- lm(presvote ~ difflog, data = inc.sub)
summary(model2)
#2.
ggplot(inc.sub, aes(difflog,presvote)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "presvote ~ difflog", x = "difflog", y = "presvote")
#3.
residuals2 <- model2$residuals
print(residuals2)
#4.
coefficients2 <- coef(model2)
print(coefficients2)
prediction_equation2 <- paste("presvote =", round(coefficients2[1], 5), "+", round(coefficients2[2], 5), "* difflog")
print(prediction_equation2)

#Question3
#1.
model3 <- lm(voteshare~presvote, data = inc.sub)
summary(model3)
#2.
ggplot(inc.sub, aes(presvote,voteshare)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "voteshare~presvote", x = "presvote", y = "voteshare")



#3.
coefficients3 <- coef(model3)
print(coefficients3)
prediction_equation3 <- paste("voteshare =", round(coefficients3[1], 5), "+", round(coefficients3[2], 5), "* presvote")
print(prediction_equation3)

#Question4
#1.
model4 <- lm(residuals1~residuals2)
summary(model4)
#2.
ggplot(inc.sub, aes(residuals2,residuals1)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "residuals1~residuals2", x = "residuals2", y = "residuals1")
#3.
coefficients4 <- coef(model4)
print(coefficients4)
prediction_equation4 <- paste("residuals1 =", round(coefficients4[1], 5), "+", round(coefficients4[2], 5), "* residuals2")
print(prediction_equation4)

#Question5
#1.
model5 <- lm(voteshare ~  difflog + presvote, data = inc.sub)
summary(model5)

#2.
coefficients5 <- coef(model5)
print(coefficients5)
prediction_equation5 <- paste("voteshare =", round(coefficients5[1], 5), "+", round(coefficients5[2], 5), "* difflog", "+", round(coefficients4[2], 5), "* presvote")


print(prediction_equation1)
print(prediction_equation2)
print(prediction_equation3)
print(prediction_equation4)
print(prediction_equation5)
#coefficient of residuals1 in q4 equals coefficient of presvote in q5
print((model4$coefficients))
print((model5$coefficients))
#delete
print(cor(inc.sub$voteshare,inc.sub$difflog))
print(cor(inc.sub$presvote,inc.sub$difflog))
print(cor(inc.sub$voteshare,inc.sub$presvote))
print(cor(residuals1,residuals2))
