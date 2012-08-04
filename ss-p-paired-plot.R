source("functions.R")

opar <- par(no.readonly=TRUE)

size <- seq(10, 400, 1)
d <- c(0.2, 0.5, 0.8)
sig <- c(0.05, 0.01) # уровни значимости

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
matplot(size, tab.size.p, type = "l", lwd = 2, lty = 1, col = colors, xlim = c(10, max(size)), ylim = c(0, 1),  xaxs = "r", yaxs = "r", xaxt = "n", cex.axis = 0.8, xlab = "", ylab = "")
axis(1, at = seq(0, 400, by = 25), cex.axis = 0.8)
abline(h = 0.8, lty = "longdash", lwd = 0.5, xpd = FALSE)
title(main = "График зависимости мощности\n от размера выборки, размера эффекта\n и уровня значимости", xlab = "Размер выборки", ylab = "Мощность")
legend(0, -0.6, legend = c("p=0.05; d=0.2", "p=0.05; d=0.5", "p=0.05; d=0.8", "p=0.01; d=0.2", "p=0.01; d=0.5", "p=0.01; d=0.8"), col = colors, lwd = 1, lty = 1, bty = "n", xpd = TRUE, xjust=0, yjust=0.5, ncol = 2)
size.x <- c(14, 22, 33, 50, 198, 295)
for (i in size.x) points(i, 0.8, pch = 20)
abline(v = size.x, lty = "longdash", lwd = 0.5, xpd = FALSE)
#for (i in size.x) text(i + 20, 0.03, labels = i)
par(opar)