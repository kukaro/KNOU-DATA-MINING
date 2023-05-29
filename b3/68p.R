x1 <- factor(c(rep(1, 5), rep(2, 7), rep(3, 5), rep(4, 8)))
y <- factor(c(rep(1, 0), rep(2, 5), rep(1, 5), rep(2, 2), rep(1, 5), rep(1, 1), rep(2, 7)))
df <- data.frame(x1, y)
for (i in 1:4) {
  leftx <- 0
  lefty <- 0
  rightx <- 0
  righty <- 0
  for (j in 1:i) {
    leftx <- leftx + length(df[df$x1 == j,]$x1)
    lefty <- lefty + length(df[df$x1 == j & df$y == 1,]$x1)
  }
  if (i + 1 > 4) {
    break
  }
  for (j in (i + 1):4) {
    rightx <- rightx + length(df[df$x1 == j,]$x1)
    righty <- righty + length(df[df$x1 == j & df$y == 1,]$x1)
  }
  gini1 <- 1 -
    (lefty / leftx)^2 -
    ((leftx - lefty) / leftx)^2
  gini2 <- 1 -
    (righty / rightx)^2 -
    ((rightx - righty) / rightx)^2
  gini <- gini1 * (leftx / length(df$x1)) + gini2 * (rightx / length(df$x1))
  print('##########')
  print(gini)
}