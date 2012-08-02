source("functions.R")

size <- seq(10, 400, 1)
f <- c(0.1, 0.25, 0.4) # размеры эффекта для ANOVA
sig <- c(0.05, 0.01) # уровни значимости
groups <- 3

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

## График зависимости мощности\n от размера выборки, размера эффекта\n и уровня значимости на основании tab и size
# Задаём цвета
colors <- rainbow(length(f)*length(sig))
# Задаём отсутпы
par(mar=c(8, 4, 4, 2) + 0.1, xpd = TRUE)
# Строим график по tab.size
matplot(size, tab.size, type = "l", lwd = 2, lty = 1, col = colors, xlab = "Размер выборки", ylab = "Мощность", xlim = c(0, max(size)), ylim = c(0, 1), xaxs = "i", yaxs = "r", xaxt = "n")
# Добавляем ось x
axis(1, at= seq(0, 400, by = 25))
# Добавляем пунктир на b=0.8
abline(h = 0.8, lty = "longdash", lwd = 0.5, xpd = FALSE)
# Добавляем заголовок
title(main = "График зависимости мощности\n от размера выборки, размера эффекта\n и уровня значимости")
# Добавляем легенду
legend(0, -0.6, legend = c("p=0.05; f=0.1", "p=0.05; f=0.25", "p=0.05; f=0.4", "p=0.01; f=0.1", "p=0.01; f=0.25", "p=0.01; f=0.4"), col = colors, lwd = 1, lty = 1, bty = "n", xpd = TRUE, xjust=0, yjust=0.5, ncol = 2)

# Вычисляем точки пересечения кривых с пунктиром (b=0.8)
points <- sort(mapply(FUN = sample.size.anova, f = rep(f, 2), sig = sig, groups = groups))
# Добавляем точки
for (i in points) points(i, 0.8, pch = 20)
# Добавляем пунктирные линии
for (i in points) abline(v = i, lty = "longdash", lwd = 0.5, xpd = FALSE)
#for (i in size.x) text(i + 20, 0.03, labels = i)
par(mar=c(5, 4, 4, 2) + 0.1, xpd = FALSE)