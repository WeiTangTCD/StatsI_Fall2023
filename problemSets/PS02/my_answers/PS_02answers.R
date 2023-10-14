##########Problem 1
#####(1a)
rm(list=ls())
#build the data matrix
driver_class<-c("Upper class","Lower class")
reaction<-c("Not stopped","Bribe requested","Stopped/given warning")
main_table<-matrix(nrow=length(driver_class),ncol=3)
rownames(main_table)<-driver_class
colnames(main_table)<-reaction
main_table[,]<-c(14,7,6,7,7,1)

#chi^2_test by hand
prop.table(main_table)
#function fe() is built to calculate the estimate value
fe<-function(table,x,y){
  num<-table[x,y]
  row_sum<-sum(table[x,])
  col_sum<-sum(table[,y])
  grand_sum<-sum(table[,])
return(row_sum*col_sum/grand_sum)
}
fe(main_table,1,2)
#calculate the chi-square value
chi_square<-0
for(i in 1:length(reaction)){
  for(j in 1:length(driver_class)){
    chi_square<-chi_square+(((main_table[j,i]-fe(main_table,j,i))^2)/fe(main_table,j,i))
  }
}
print(chi_square)

#####(1b)
#calculate p-value
p_value<-pchisq(chi_square,df<-(length(driver_class)-1)*(length(reaction)-1),lower.tail=FALSE)
print(p_value)
#check_my_answer
chisq.test(main_table)

#####(1c)
#build a blank matrix for standardized residual
std_residual_table<-matrix(nrow=length(driver_class),ncol=3)
rownames(std_residual_table)<-driver_class
colnames(std_residual_table)<-reaction
#for loop to fill in the corresponding standardized residuals
for(i in 1:length(driver_class)){
  for(j in 1:length(reaction)){
    std_residual_table[i,j]<-(main_table[i,j]-fe(main_table,i,j))/sqrt(fe(main_table,i,j)*(1-sum(main_table[i,])/sum(main_table[,]))*(1-sum(main_table[,j])/sum(main_table[,])))
    #for every element in the matrix, I use the formula to calculate the standardized residual.
  }
}
print(std_residual_table)
#check my answer
(chisq.test(main_table))$stdres
(chisq.test(main_table))$expected
#####(1d)
#My conclusion is that....
#Absolute value of standardized residuals are lower than 1.96

##########Problem 2
#####(2a)

#H0:reservation policy puts no effect on the number of new or repaired drinking water facilities
#Ha:reservation policy puts effect on the number of new or repaired drinking water facilities

#####(2b)
#import the dataset and build the data frame
data_set <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv", header=T)
str(data_set)
#run bivariate regression
temp_model<-lm(water~reserved,data_set)
summary(temp_model)$coefficients

#####(2c)


