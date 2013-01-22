source("functions.R")

opar <- par(no.readonly=TRUE)

f <- seq.int(0, 1, length.out = 100)
size=60 # размеры эффекта для ANOVA
sig <- c(0.05, 0.01) # уровни значимости
groups <- 3

tab.power <- matrix(
  mapply(power.test.anova, f = rep(f, each = 2), sig = sig, groups = groups, n = size),
  ncol=2, byrow=TRUE, dimnames=list(f, sig))

# График зависимости мозности от размера эффекта и уровня значимости для выборки в 60 человек
colors <- rainbow(length(sig))
par(mar=c(6, 4, 4, 2) + 0.1, xpd = TRUE)
matplot(f, tab.power, type = "l", lwd = 2, lty = 1, col = colors, cex.axis = 0.8, xlab = "", ylab = "")
abline(h = 0.8, lty = "longdash", lwd = 0.5, xpd = FALSE)
title(main = paste0("График зависимости мощности\nот размера эффекта (n=", size, ", k=", groups, ")"), xlab = "Размер эффекта", ylab = "Мощность")
legend("bottom", inset=c(0, -0.45), legend = c("p=0.05", "p=0.01"), col = colors, lwd = 1, lty = 1, bty = "n", xpd = TRUE, xjust=0, yjust=0.5, ncol = 2)
points <- sort(mapply(FUN = effect.size.anova, sig = sig, groups = groups, n = size))
points(points, rep(0.8, length(points)), pch = 20)
abline(v = points, lty = "longdash", lwd = 0.5, xpd = FALSE)
text((points + 0.03), rep(0.03, length(points)), labels = points, cex=0.7)
par(opar)