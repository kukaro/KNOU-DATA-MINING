x1 <- factor(c(rep(2, 2), rep(3, 5), rep(2, 4), rep(3, 6), rep(1, 2), rep(3, 1),
               rep(1, 2), rep(1, 3), rep(2, 7), rep(1, 4), rep(2, 1), rep(3, 2)))
x2 <- factor(c(rep(1, 7), rep(2, 10), rep(3, 3), rep(1, 2), rep(2, 10), rep(3, 7)))
y <- factor(c(rep(1, 20), rep(2, 19)))
df <- data.frame(x1, x2, y)

start.comb <- function() {
  for (largest in 1:2) {
    comb(c(), 1, 0, largest)
  }
}

comb <- function(arr, n, cnt, largest) {
  if (cnt == largest) {
    print('##########')
    gini(arr, df)
  }else if (n <= largest + 1) {
    new_arr <- c(arr, n)
    comb(new_arr, n + 1, cnt + 1, largest)
    comb(arr, n + 1, cnt, largest)
  }
}

gini <- function(arr, df) {
  for (type in c('x1', 'x2')) {
    left <- '왼쪽 : '
    right <- '- 오른쪽 : '
    leftx <- 0
    lefty <- 0
    rightx <- 0
    righty <- 0
    for (j in c(1, 2, 3)) {
      if (j %in% arr) {
        left <- paste0(left, as.character(j), ' ')
        leftx <- leftx + length(df[df[[type]] == j,][[type]])
        lefty <- lefty + length(df[df[[type]] == j & df$y == 1,][[type]])
      }
      else {
        right <- paste0(right, as.character(j), ' ')
        rightx <- rightx + length(df[df[[type]] == j,][[type]])
        righty <- righty + length(df[df[[type]] == j & df$y == 1,][[type]])
      }
    }
    gini1 <- 1 -
      (lefty / leftx)^2 -
      ((leftx - lefty) / leftx)^2
    gini2 <- 1 -
      (righty / rightx)^2 -
      ((rightx - righty) / rightx)^2
    gini <- gini1 * (leftx / length(df[[type]])) + gini2 * (rightx / length(df[[type]]))
    print(paste0('변수 : ', type, ' - ',left, right))
    print(gini)
  }
}

start.comb()
