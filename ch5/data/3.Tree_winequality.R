####################################
# Decision Tree with wine data
####################################
setwd('c:/data')
# Importing data
wine = read.csv("winequalityCLASS.csv", header=TRUE)

# Factorize for classification
wine$quality = factor(wine$quality)

# Classification Tree
library(rpart)
set.seed(1234)
my.control = rpart.control(xval=10, cp=0, minsplit=20)
tree.wine = rpart(quality~., data=wine, method="class", control=my.control)
print(tree.wine)
# Display tree
library(rpart.plot)
prp(tree.wine, type=4, extra=1, digits=2, box.palette="Grays")

# Pruning with c-s.e.
cps = printcp(tree.wine)
k = which.min(cps[,"xerror"])
err = cps[k,"xerror"]; se = cps[k,"xstd"]
c = 1 # 1-s.e.
k1 = which(cps[,"xerror"] <= err+c*se)[1]
cp.chosen = cps[k1,"CP"]
tree.pruned.wine = prune(tree.wine, cp=cp.chosen)
print(tree.pruned.wine)
# Display tree
prp(tree.pruned.wine, type=4, extra=1, digits=2, box.palette="Grays")

# Making predictions - probability prediction
prob.tree.wine = predict(tree.pruned.wine, newdata=wine, type="prob")
head(prob.tree.wine, 5)
cutoff = 0.5 #cutoff
yhat.tree.wine = ifelse(prob.tree.wine[,2] > cutoff, 1, 0)

# Evaluation
tab = table(wine$quality, yhat.tree.wine, dnn=c("Observed","Predicted"))
print(tab)              # confusion matrix
sum(diag(tab))/sum(tab) # accuracy
tab[2,2]/sum(tab[2,])   # sensitivity
tab[1,1]/sum(tab[1,])   # specificity
