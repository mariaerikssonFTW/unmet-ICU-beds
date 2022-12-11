library(datasets)
library(tidyverse)
library(dplyr)
library(tableone)
library(lubridate)
library(noacsr)
library(glm2)
library(ggplot2)
library(pROC)
library(boot)
load("reps.rdata")
#eller vilket reps objekt som helst på github

#kontrollera med en random fördelning
x1<-rnorm(50,2,0.25)
reps<-boot(x1,function(u,i) mean(u[i]),R=1000)


# R program to determine the mean
tvec=reps[["t"]]
#tvec=data.frame(tvec)

# Calculate the mean of the Sepal.Length
mean_value <- mean(tvec)

# Compute the size
n <- length(tvec)

# Find the standard deviation
standard_deviation <- sd(tvec)

# Find the standard error
standard_error <- standard_deviation / sqrt(n)

alpha = 0.05
degrees_of_freedom = n - 1
t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F)


margin_error <- t_score * standard_error
margin_error <- t_score * standard_deviation

# Calculating lower bound and upper bound
lower_bound <- mean_value - margin_error
upper_bound <- mean_value + margin_error

#print mean of t
print(mean_value)
#print sd dev
print(standard_deviation)
#print sd error
print(standard_error)
#print t-score
print(t_score)
# Print the confidence interval by manual calculation
print(c(lower_bound,upper_bound))
#why is it so damn small?
#och rangen av t
print(range(reps[["t"]]))
# print confidence interval by boot.ci. norm är rimligt givet hist
print(boot.ci(reps,type="norm"))
#why is it so damn high?

plot(reps,index=1)
#ok so t0 is mean of original data. It can be out here because it is
#"just another bootstrap" so it's fine if it is in the 95% CI, but 
#shouldn't it be closer to the mean of t? 
hist(tvec,index=1,breaks=50)

#what is an acceptable bias? it is the difference in t0 of original 
#data and the mean of t
print(reps)
