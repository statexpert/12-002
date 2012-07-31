library("pwr")
sample.n <-data.frame(NULL)
for (i in 1:6) {
  for (j in 1:2) {
    if (j == 1) sig.lvl <- 0.05
    if (j == 2) sig.lvl <- 0.01
    if (i == 1 || i == 2|| i == 3) groups <- 2
    if (i == 4 || i == 5 || i == 6) groups <- 3
    if (i == 1 || i == 4) effect.size <- 0.1
    if (i == 2 || i == 5) effect.size <- 0.25
    if (i == 3 || i == 6) effect.size <- 0.4
    sample.n[i,j] <- pwr.anova.test(k = groups, n = NULL, f = effect.size, sig.level = sig.lvl, power = 0.8)$n
  }
}
colnames(sample.n) <- c("sig.lvl=0.05", "sig.lvl=0.01")
rownames(sample.n) <- c("k=2, f=0.1", "k=2, f=0.25", "k=2, f=0.4", "k=3, f=0.1", "k=3, f=0.25", "k=3, f=0.4")
sample.n <- round(sample.n)

pwr.sample <-data.frame(NULL)
for (i in 1:6) {
  for (j in 1:2) {
    if (j == 1) sig.lvl <- 0.05
    if (j == 2) sig.lvl <- 0.01
    if (i == 1 || i == 2|| i == 3) groups <- 2
    if (i == 4 || i == 5 || i == 6) groups <- 3
    if (i == 1 || i == 4) effect.size <- 0.1
    if (i == 2 || i == 5) effect.size <- 0.25
    if (i == 3 || i == 6) effect.size <- 0.4
    pwr.sample[i,j] <- pwr.anova.test(k = groups, n = 20, f = effect.size, sig.level = sig.lvl, power = NULL)$power
  }
}
colnames(pwr.sample) <- c("sig.lvl=0.05", "sig.lvl=0.01")
rownames(pwr.sample) <- c("k=2, f=0.1", "k=2, f=0.25", "k=2, f=0.4", "k=3, f=0.1", "k=3, f=0.25", "k=3, f=0.4")
pwr.sample <- round(pwr.sample, digits=2)

# график зависимости мощности от размера эффекта
f <- seq(0.1, 0.5, length = 100)
p <- NULL
for (i in f) p <- c(p, pwr.anova.test(k = 3, n = 20, f = i, sig.level = 0.05, power = NULL)$power)
plot(p ~ f, type = "l", col = "green")
p <- NULL
for (i in f) p <- c(p, pwr.anova.test(k = 3, n = 20, f = i, sig.level = 0.01, power = NULL)$power)
lines(p ~ f, col = "red")
abline(h = 0.8, lty = "longdash")
p <- NULL
for (i in f) p <- c(p, pwr.anova.test(k = 2, n = 30, f = i, sig.level = 0.05, power = NULL)$power)
lines(p ~ f, type = "l", col = "green4")
p <- NULL
for (i in f) p <- c(p, pwr.anova.test(k = 2, n = 30, f = i, sig.level = 0.01, power = NULL)$power)
lines(p ~ f, col = "red4")
abline(h = 0.8, lty = "longdash")
legend("bottomright", legend = c("sig.lvl=0.05; k=3", "sig.lvl=0.01; k=3", "sig.lvl=0.05; k=2", "sig.lvl=0.01; k=2"), col = c("green", "red", "green4", "red4"), lwd = 1, lty = 1)
title(main = "График зависимости мощности от размера эффекта")

# график зависимости мощности от размера выборки
size <- seq(24, 250, 1)
p <- NULL
for (i in size) p <- c(p, pwr.anova.test(k = 3, n = i, f = 0.25, sig.level = 0.05, power = NULL)$power)
plot(p ~ size, type = "l", col = "green", xaxt = "n")
axis(1, seq(25,250,25))
p <- NULL
for (i in size) p <- c(p, pwr.anova.test(k = 3, n = i, f = 0.25, sig.level = 0.01, power = NULL)$power)
lines(p ~ size, col = "red")
abline(h = 0.8, lty = "longdash")
p <- NULL
for (i in size) p <- c(p, pwr.anova.test(k = 2, n = i, f = 0.25, sig.level = 0.05, power = NULL)$power)
lines(p ~ size, type = "l", col = "green4")
p <- NULL
for (i in size) p <- c(p, pwr.anova.test(k = 2, n = i, f = 0.25, sig.level = 0.01, power = NULL)$power)
lines(p ~ size, col = "red4")
abline(h = 0.8, lty = "longdash")
legend("bottomright", legend = c("sig.lvl=0.05; k=3", "sig.lvl=0.01; k=3", "sig.lvl=0.05; k=2", "sig.lvl=0.01; k=2"), col = c("green", "red", "green4", "red4"), lwd = 1, lty = 1)
title(main = "График зависимости мощности от размера выборки")


pwr.f2.test(u = 53, v = NULL, f2 = 0.02, sig.level = 0.05, power = 0.8)
pwr.f2.test(u = 53, v = NULL, f2 = 0.15, sig.level = 0.05, power = 0.8)
pwr.f2.test(u = 53, v = NULL, f2 = 0.35, sig.level = 0.05, power = 0.8)