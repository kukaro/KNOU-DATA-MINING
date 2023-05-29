wine <- read.csv('./ch1/data/winequalityCLASS.csv', header = T);
fit.all <- glm(quality ~ sulphates, family = binomial, data = wine)
p <- predict(fit.all, newdata = wine, type = "response")
cutoff <- 0.5
yhat <- ifelse(p > cutoff, 1, 0)
tab <- table(wine$quality, yhat, dnn = c('Observed', 'Predicated'))
sum(diag(tab)) / sum(tab) #accuracy
tab[2, 2] / sum(tab[2,]) #sensitivity
tab[1, 1] / sum(tab[1,]) #specificity