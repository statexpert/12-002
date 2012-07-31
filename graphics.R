library("pwr")
size <- seq(30, 400, 1)
f <- c(0.1, 0.25, 0.4) # размеры эффекта для ANOVA
sig <- c(0.05, 0.01) # уровни значимости
groups <- 3

# Функция вычисляет мощность для заданного объёма выборки
# n - общий объём выборки
power.test.anova <- sample.size.anova <- function(groups, n, f, sig = 0.05) {
  fit <- pwr.anova.test(k = groups, n = size/groups, f = f, sig.level = sig, power = NULL)
  power <- fit$power
}

# Расчёт мощности для всех f и sig и внесение значений в столбцы матрицы tab
tab <- NULL
for (i in sig) {
  for (j in f) {
    b <- power.test.anova(f = j, sig = i, groups = groups, n = size)
    tab <- cbind(tab, b)
    rm(b)
  }
}

colnames(tab) <- rep(f,2)
rownames(tab) <- size
colors <- rainbow(length(f)*length(sig))

# par(mar=c(4, 4, 4, 2), xpd=TRUE)
matplot(size, tab, type = "l", lty = 1, col = colors, xlab = "Размер выборки", ylab = "Мощность")
abline(h = 0.8, lty = "longdash")
title(main = "График зависимости мощности\n от размера выборки, размера эффекта\n и уровня значимости")
# legend("right", inset = c(0,-5,0), legend = c("знач.=0.05; f=0.1", "знач.=0.05; k=0.25", "знач.=0.05; f=0.4", "знач.=0.01; f=0.1", "знач.=0.01; f=0.25", "знач.=0.04; f=0.1"), col = colors, lwd = 1, lty = 1)