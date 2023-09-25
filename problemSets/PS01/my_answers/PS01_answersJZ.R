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
library(ggplot2)

lapply(c(),  pkgTest)
setwd("D:/GitHubSpace/StatsI_Fall2023/problemSets/PS01/my_answers")
#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100,82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
#####Question 1
n <- length(y)
sample_mean <- mean(y)
sample_sd <- sd(y)

#step-by-step method
t_score <- qt(1-(1-0.9)/2,df<- length(y)-1)
lower_90_t <- mean(y) - (t_score)*(sd(y)/sqrt(length(y)))
higher_90_t <- mean(y) + (t_score)*(sd(y)/sqrt(length(y)))

#quicker method
t.test(y,conf.level = 0.9 , alternative = "two.sided")

#answer
CI <- c(lower_90_t,higher_90_t)
CI

#####Question 2
se <- sample_sd/sqrt(n)
t_value <- (mean(y)-100)/(se)
p <- pt(t_value,df = n-1,lower.tail = FALSE)
t.test(y, mu = 100,  alternative = "greater")
#p_value == 0.7215 > 0.05, null hypothesis rejected.

#####################
# Problem 2
#####################
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/expenditure.txt", header=T)
#####Question 1
str(expenditure[c("Y", "X1", "X2", "X3")])

pdf(file="Y-X1_X2_X3_Relationship_Plot.pdf")
plot(expenditure[c("Y", "X1", "X2", "X3")])
dev.off()

cor(expenditure$Y,expenditure$X1)
cor(expenditure$Y,expenditure$X2)
cor(expenditure$Y,expenditure$X3)
cor(expenditure$X2,expenditure$X3)
cor(expenditure$X1,expenditure$X3)
cor(expenditure$X2,expenditure$X3)

#####Question 2
pdf(file="Y-Region_Relationship_Plot.pdf")
plot(expenditure[c("Y","Region")])
str(expenditure)
R_1 <- expenditure[expenditure$Region == 1, ]
R_2 <- expenditure[expenditure$Region == 2, ]
R_3 <- expenditure[expenditure$Region == 3, ]
R_4 <- expenditure[expenditure$Region == 4, ]

t.test(R_1$Y,conf.level = 0.95 , alternative = "two.sided")
t.test(R_2$Y,conf.level = 0.95 , alternative = "two.sided")
t.test(R_3$Y,conf.level = 0.95 , alternative = "two.sided")
t.test(R_4$Y,conf.level = 0.95 , alternative = "two.sided")

abline(v=mean(R_1$Y),col="red")
abline(v=mean(R_2$Y),col="blue")
abline(v=mean(R_3$Y),col="green")
abline(v=mean(R_4$Y),col="black")

legend("topright", legend = c("R_1 Mean", "R_2 Mean", "R_3 Mean", "R_4 Mean"), col = c("red", "blue", "green", "black"),lty=1 )
dev.off()
#Answer: Region 4 has the highest per capita expenditure on housing assistance

#####Question 3


pdf(file="Y-X1_Relationship_Plot.pdf")
plot(expenditure[c("Y","X1")])
dev.off()
expenditure$Region <- factor(expenditure$Region)
pdf(file="Y-X1-Region_Relationship_Plot.pdf")
ggplot(expenditure, aes(x = Y  , y = X1, color = Region ,shape = Region)) +
  geom_point() +
  labs(x = "Y", y = "X1", title = "Y-X1") +
  scale_color_manual(values = c( "1" = "red", "2" = "blue", "3" = "green", "4" = "black")) +
  scale_shape_manual(values = c("1" = 1, "2" = 2, "3" = 3, "4" = 4)) +
  theme_minimal()
dev.off()



