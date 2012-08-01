library("pwr")
size <- seq(30, 400, 1)
f <- c(0.1, 0.25, 0.4) # размеры эффекта для ANOVA
sig <- c(0.05, 0.01) # уровни значимости
groups <- 3

# Функция вычисляет мощность для заданного объёма выборки
# n - общий объём выборки
power.test.anova <- function(groups, n, f, sig = 0.05) {
  fit <- pwr.anova.test(k = groups, n = size/groups, f = f, sig.level = sig, power = NULL)
  power <- fit$power
}
# Расчёт мощности для всех f и sig и внесение значений в столбцы матрицы tab
tab.size <- NULL
for (i in sig) {
  for (j in f) {
    b <- power.test.anova(f = j, sig = i, groups = groups, n = size)
    tab.size <- cbind(tab.size, b)
    rm(b)
  }
}
colnames(tab.size) <- rep(f,2)
rownames(tab.size) <- size

# График зависимости мощности\n от размера выборки, размера эффекта\n и уровня значимости на основании tab и size
colors <- rainbow(length(f)*length(sig))
par(mar=c(8, 4, 4, 2) + 0.1, xpd = TRUE)
matplot(size, tab.size, type = "l", lwd = 2, lty = 1, col = colors, xlab = "Размер выборки", ylab = "Мощность", xlim = c(0, max(size)), ylim = c(0, 1), xaxs = "i", yaxs = "r", xaxt = "n")
axis(1, at= seq(0, 400, by = 25))
abline(h = 0.8, lty = "longdash", lwd = 0.5, xpd = FALSE)
title(main = "График зависимости мощности\n от размера выборки, размера эффекта\n и уровня значимости")
legend(0, -0.6, legend = c("p=0.05; f=0.1", "p=0.05; f=0.25", "p=0.05; f=0.4", "p=0.01; f=0.1", "p=0.01; f=0.25", "p=0.01; f=0.4"), col = colors, lwd = 1, lty = 1, bty = "n", xpd = TRUE, xjust=0, yjust=0.5, ncol = 2)
size.x <- c(63, 90, 156, 228)
for (i in size.x) points(i, 0.8, pch = 20)
for (i in size.x) abline(v = i, lty = "longdash", lwd = 0.5, xpd = FALSE)
#for (i in size.x) text(i + 20, 0.03, labels = i)
par(mar=c(5, 4, 4, 2) + 0.1, xpd = FALSE)

tab.power <- NULL
f <- seq(0, 1, length = 100)
size=60
for (i in sig) {
  b <- power.test.anova(f = f, sig = i, groups = groups, n = size)
  tab.power <- cbind(tab.power, b)
  rm(b)
}
colnames(tab.power) <- sig

# График зависимости мозности от размера эффекта и уровня значимости для выборки в 60 человек
colors <- rainbow(length(sig))
par(mar=c(6, 4, 4, 2) + 0.1, xpd = TRUE)
matplot(f, tab.power, type = "l", lwd = 2, lty = 1, col = colors, xlab = "Размер эффекта", ylab = "Мощность")
abline(h = 0.8, lty = "longdash", lwd = 0.5, xpd = FALSE)
title(main = "График зависимости мощности\nот размера эффекта (n=60)")
legend(0, -0.4, legend = c("p=0.05", "p=0.01"), col = colors, lwd = 1, lty = 1, bty = "n", xpd = TRUE, xjust=0, yjust=0.5, ncol = 2)
size.x <- c(0.41, 0.5)
for (i in size.x) points(i, 0.8, pch = 20)
for (i in size.x) abline(v = i, lty = "longdash", lwd = 0.5, xpd = FALSE)
#for (i in size.x) text(i + 0.05, 0.03, labels = i)
par(mar=c(5, 4, 4, 2) + 0.1, xpd=FALSE)

d <- c(0.2, 0.5, 0.8)
size <- seq(20, 400, 1)
power.test.paired <- function(n, d, sig = 0.05) {
  fit <- pwr.t.test(n = n, d = d, sig.level = sig, power = NULL, type = "paired")
  power <- fit$power
}
# Расчёт мощности для всех f и sig и внесение значений в столбцы матрицы tab
tab.size.p <- NULL
for (i in sig) {
  for (j in d) {
    b <- power.test.paired(d = j, sig = i, n = size)
    tab.size.p <- cbind(tab.size.p, b)
    rm(b)
  }
}
colnames(tab.size.p) <- rep(d,2)
rownames(tab.size.p) <- size

# График зависимости мощности от размеры выборки, размеры эффекта и уровня значимости для зависимых выборок
colors <- rainbow(length(d)*length(sig))
par(mar=c(8, 4, 4, 2) + 0.1, xpd = TRUE)
matplot(size, tab.size.p, type = "l", lwd = 2, lty = 1, col = colors, xlab = "Размер выборки", ylab = "Мощность", xlim = c(10, max(size)), ylim = c(0, 1),  xaxs = "r", yaxs = "r", xaxt = "n")
axis(1, at= seq(0, 400, by = 25))
abline(h = 0.8, lty = "longdash", lwd = 0.5, xpd = FALSE)
title(main = "График зависимости мощности\n от размера выборки, размера эффекта\n и уровня значимости")
legend(0, -0.6, legend = c("p=0.05; d=0.2", "p=0.05; d=0.5", "p=0.05; d=0.8", "p=0.01; d=0.2", "p=0.01; d=0.5", "p=0.01; d=0.8"), col = colors, lwd = 1, lty = 1, bty = "n", xpd = TRUE, xjust=0, yjust=0.5, ncol = 2)
size.x <- c(14, 22, 33, 50, 198, 295)
for (i in size.x) points(i, 0.8, pch = 20)
for (i in size.x) abline(v = i, lty = "longdash", lwd = 0.5, xpd = FALSE)
#for (i in size.x) text(i + 20, 0.03, labels = i)
par(mar=c(5, 4, 4, 2) + 0.1, xpd = FALSE)