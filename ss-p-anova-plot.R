source("functions.R")

opar <- par(no.readonly=TRUE)

size <- seq(10, 400, 1)
f <- c(0.1, 0.25, 0.4) # размеры эффекта для ANOVA
sig <- c(0.05, 0.01) # уровни значимости
groups <- 3

# Расчёт мощности для всех f и sig и внесение значений в столбцы матрицы tab
tab.size <- matrix(
  unlist(lapply(size, function(x) {
    mapply(power.test.anova, n = x, f = f, sig = rep(sig, each=length(f)), groups = groups)
    })), ncol=6, byrow=TRUE, dimnames=list(size, rep(f, 2)))

## График зависимости мощности\n от размера выборки, размера эффекта\n и уровня значимости на основании tab и size
# Задаём цвета
colors <- rainbow(length(f)*length(sig))
# Задаём отсутпы
par(mar=c(8, 4, 4, 2) + 0.1, xpd = TRUE)
# Строим график по tab.size
matplot(size, tab.size, type = "l", lwd = 2, lty = 1, col = colors, xlim = c(0, max(size)), ylim = c(0, 1), xaxs = "i", yaxs = "r", xaxt = "n", cex.axis = 0.8, xlab = "", ylab = "")
# Добавляем ось x
axis(1, at = seq(0, 400, by = 25), cex.axis = 0.8)
# Добавляем пунктир на b=0.8
abline(h = 0.8, lty = "longdash", lwd = 0.5, xpd = FALSE)
# Добавляем заголовок
title(main = "График зависимости мощности\n от размера выборки, размера эффекта\n и уровня значимости", xlab = "Размер выборки", ylab = "Мощность")
# Добавляем легенду
legend("bottom", inset=c(0, -0.7), legend = c("p=0.05; f=0.1", "p=0.05; f=0.25", "p=0.05; f=0.4", "p=0.01; f=0.1", "p=0.01; f=0.25", "p=0.01; f=0.4"), col = colors, lwd = 1, lty = 1, bty = "n", xpd = TRUE, xjust=0, yjust=0.5, ncol = 2)

# Вычисляем точки пересечения кривых с пунктиром (b=0.8)
points <- sort(mapply(FUN = sample.size.anova, f = rep(f, 2), sig = sig, groups = groups))
# Добавляем точки
points(points, rep(0.8, length(points)), pch = 20)
# Добавляем пунктирные линии
abline(v = points, lty = "longdash", lwd = 0.5, xpd = FALSE)
text((points + 10), rep(0.03, length(points)), labels = points, cex=0.7)
par(opar)