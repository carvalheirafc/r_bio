load(file='nhgh.rda')
data_frame <- as.data.frame(nhgh)
cols_numeric <-c('age', 'dx', 'wt', 'ht', 'bmi', 'leg', 'arml', 'waist', 'tri', 'sub', 'gh', 'albumin', 'bun', 'SCr')
data_numeric <- select(data_frame, cols_numeric)
data_numeric$age <- as.integer(data_numeric$age)

lm1 <- lm(data_frame,formula=gh ~.)
summary(lm1)

install.packages('leaps')
library(leaps)

best_subset <-
  regsubsets(gh~.,
             data =data_numeric,
             nbest = 1,      # 1 best model for each number of predictors
             nvmax = NULL,    # NULL for no limit on number of variables
             force.in = NULL, force.out = NULL,
             method = "exhaustive")

plot(best_subset, scale="adjr2")

data_selected <- select(data_numeric, c('gh','age', 'dx', 'sub','albumin','SCr'))
lm2 <- lm(data_selected, formula=gh~.)
summary(lm2)


cov(scale(data_frame), use='complete.obs')


## Probably the best solution so far

categorical <- sapply(data_frame, is.factor)
new_data_frame <- sapply(data_frame[, categorical], unclass)
nhgh_converted <- as.data.frame(cbind(data_frame[, !categorical], new_data_frame))
nhgh_converted$age <- as.integer(nhgh_converted$age)
nhgh_converted <- select(nhgh_converted, !c('seqn'))

lm1 <- lm(nhgh_converted, formula=gh~.)
summary(lm1)
step(lm1, direction = 'backward')

lm_optimal <- lm(nhgh_converted, formula=gh~age+tx+dx+wt)
summary(lm_optimal)
