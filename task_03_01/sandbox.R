# Universidade Federal do Rio Grande do Norte
# Bioestatística - IMD0601
# Professores: Beatriz Stransky e Tetsu Sakamoto
# Atividade Regressão Linear
# Aluno Carlos Frederico Carvalheira Mello

# Import all =)
#install.packages("Hmisc")
#install.packages("corrplot")
library(tidyverse)
library(Hmisc)
library(corrplot)

# Load File
# Check if this file are in your working directory
load(file='nhgh.rda')
data_frame <- as.data.frame(nhgh)

# Summary of the dataset
summary(data_frame)
glimpse(data_frame)
cols_numeric <-c('age', 'dx', 'wt', 'ht', 'bmi', 'leg', 'arml', 'waist', 'tri', 'sub', 'gh', 'albumin', 'bun', 'SCr')
data_numeric <- select(data_frame, cols_numeric)
data_numeric$age <- as.integer(data_numeric$age)

# Categorical Values:
# seqn (not gonna use that, is only the sequency value index)
# sex
# age
# re
# dx: Diabetes Diagnosis

# Target Variable 
# gh
ggplot(nhgh, aes(x = sex, y = gh, color = sex)) +
  geom_boxplot() + 
  facet_grid(.~dx)

cor_data <- rcorr(as.matrix(data_numeric))
cor_data


corrplot(cor_data$r, method='square', type='lower', order='FPC', 
         p.mat=cor_data$P, sig.level=0.01, insig='blank', addCoef.col='black' )
# Relations
# Positives
# gh x age: 0.33
# gh x waist: 0.29
# gh x bmi: 0.23
# gh x sub: 0.21
# Negatives
# gh x albumin -0.23
# gh x leg: -0.17


#Model with only numeric values
linear_model_numeric <- lm(formula=gh~., data=data_numeric)
summary(linear_model_numeric)
step(linear_model_numeric, direction = "backward")

#Model with select fields 
fpc <- c('gh','age', 'waist', 'bmi', 'sub', 'albumin')
fpc_data_numeric <- select(data_numeric, fpc)
linear_model_gh <- lm(formula=gh~., data=fpc_data_numeric)
summary(linear_model_gh)
step(linear_model_gh, direction = "backward")

#Model Optimal

optimal_data_numeric <- select(data_numeric, c('gh','sex','age','waist'))
linear_model_optimal <- lm(formula=gh~., data=optimal_data_numeric)
summary(linear_model_optimal)
step(linear_model_optimal, direction = "backward")

# Original Function found at
# https://sejohnston.com/2012/08/09/a-quick-and-easy-function-to-plot-lm-results-in-r/
ggplotRegression <- function (fit) {
  require(ggplot2)
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

ggplotRegression(linear_model_gh)

a<- as.data.frame(scale(fpc_data_numeric))
l_m <- lm(formula=gh~., data=a)
summary(l_m)
anova(l_m, linear_model_all)
