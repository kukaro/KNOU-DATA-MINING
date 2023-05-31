wine <- read.csv('./ch1/data/winequalityCLASS.csv', header = T)
wine$quality <- factor(wine$quality)
library(rpart)
library(adabag)
set.seed(1234)
my.control <- rpart.control(xval = 0, cp = 0, minsplit = 5)
bag.wine <- bagging(quality ~ ., data = wine, mfinal = 100, control = my.control)

# Variable importance
bag.wine$importance
importanceplot(bag.wine)

# Error vs. number of trees
evol.wine <- errorevol(bag.wine, newdata = wine)
plot.errorevol(evol.wine)

# Making predictions
prob.bag.wine <- predict.bagging(bag.wine, newdata = wine)$prob
t(prob.bag.wine)
cutoff <- 0.5 #cutoff
yhat.bag.wine <- ifelse(prob.bag.wine[, 2] > cutoff, 1, 0)
yhat.bag.wine

# Evaluation
tab <- table(wine$quality, yhat.bag.wine, dnn = c('Observed', 'Predicted'))
tab
sum(diag(tab)) / sum(tab)
tab[2, 2] / sum(tab[2,])
tab[1, 1] / sum(tab[1,])