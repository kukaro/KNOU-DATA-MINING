x1 <- factor(c(rep(2, 2), rep(3, 5), rep(2, 4), rep(3, 6), rep(1, 2), rep(3, 1),
               rep(1, 2), rep(1, 3), rep(2, 7), rep(1, 4), rep(2, 1), rep(3, 2)))
x2 <- factor(c(rep(1, 7), rep(2, 10), rep(3, 3), rep(1, 2), rep(2, 10), rep(3, 7)))
y <- factor(c(rep(1, 20), rep(2, 19)))
df <- data.frame(x1, x2, y)
library(rpart)
my.control <- rpart.control(xval=10, cp=0, minsplit=20)
tree.test <- rpart(y ~ ., data=df, method='class', control=my.control)
tree.test
summary(tree.test)
library(rpart.plot)
prp(tree.test, type=4, extra=1, digits=2, box.palette='Grays')
y_new <- factor(c(rep(2,25),rep(1,14)))
df$y_new <- y_new
t(df)