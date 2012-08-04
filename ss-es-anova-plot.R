source("functions.R")

opar <- par(no.readonly=TRUE)

f <- seq(0, 1, length = 100)
size=60 # размеры эффекта для ANOVA
sig <- c(0.05, 0.01) # уровни значимости
groups <- 3

tab.power <- NULL
for (i in sig) {
  b <- power.test.anova(f = f, sig = i, groups = groups, n = size)
  tab.power <- cbind(tab.power, b)
  rm(b)
}
colnames(tab.power) <- sig

# График зависимости мозности от размера эффекта и уровня значимости для выборки в 60 человек
colors <- rainbow(length(sig))
par(mar=c(6, 4, 4, 2) + 0.1, xpd = TRUE)
matplot(f, tab.power, type = "l", lwd = 2, lty = 1, col = colors, cex.axis = 0.8, xlab = "", ylab = "")
abline(h = 0.8, lty = "longdash", lwd = 0.5, xpd = FALSE)
title(main = "График зависимости мощности\nот размера эффекта (n=60, k=3)", xlab = "Размер эффекта", ylab = "Мощность")
legend(0, -0.4, legend = c("p=0.05", "p=0.01"), col = colors, lwd = 1, lty = 1, bty = "n", xpd = TRUE, xjust=0, yjust=0.5, ncol = 2)
size.x <- c(0.41, 0.5)
points <- sort(mapply(FUN = effect.size.anova, sig = sig, groups = groups, n = size))
for (i in points) points(i, 0.8, pch = 20)
abline(v = size.x, lty = "longdash", lwd = 0.5, xpd = FALSE)
#for (i in size.x) text(i + 0.05, 0.03, labels = i)
par(opar)