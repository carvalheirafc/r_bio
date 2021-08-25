# Import all =)
#install.packages("corrplot")
library(tidyverse)
library(corrplot)

# Load File
# Check if this file are in your working directory
load(file='nhgh.rda')
data_frame <- as.data.frame(nhgh)

#Data Cleaning and Transformations
categorical <- sapply(data_frame, is.factor)
new_data_frame <- sapply(data_frame[, categorical], unclass)
nhgh_converted <- as.data.frame(cbind(data_frame[, !categorical], new_data_frame))
nhgh_converted$age <- as.integer(nhgh_converted$age)
nhgh_converted <- select(nhgh_converted, !c('seqn'))


# Categorical Values:
# seqn (not gonna use that, is only the sequency value index)
# sex
# age
# re
# dx: Diabetes Diagnosis

# Target Variable 
# gh
cor_data <- rcorr(as.matrix(nhgh_converted))
cor_data
corrplot(cor_data$r, method='square', type='lower', order='FPC', 
         p.mat=cor_data$P, sig.level=0.01, insig='blank', addCoef.col='black', 
         title='Matriz de Correlação NHANES Data',
         mar=c(0,0,5,0))  
  
# Relations
# Positives
# gh x age: 0.33
# gh x waist: 0.29
# gh x bmi: 0.23
# gh x sub: 0.21
# Negatives
# gh x albumin -0.23
# gh x leg: -0.17


lm1 <- lm(nhgh_converted, formula=gh~.)
summary(lm1)
step(lm1, direction = 'backward')


lm_optimal <- lm(nhgh_converted, formula=gh~age+tx+dx+wt)
summary(lm_optimal)


