library("pwr")

f <- c(0.1, 0.25, 0.4) # размеры эффекта для ANOVA
f2 <- c(0.02, 0.15, 0.35) # размеры эффекта для MANOVA
d <- c(0.2, 0.5, 0.8)
sig <- c(0.05, 0.01) # уровни значимости
groups <- 3
subgroups <- 54

# Функция расчитывает объём выборки для ANOVA
# Вовзращает общий объём выборки
sample.size.anova <- function(groups, n = NULL, f, sig = 0.05, b = 0.8) {
  fit <- pwr.anova.test(k = groups, f = f, sig.level = sig, power = b)
  size <- round(fit$n)*groups
}
# Сводные данные по расчёту объёма выборки для ANOVA
ssa <- matrix(mapply(FUN = sample.size.anova, f = rep(f, 2), sig = sig, groups = groups), ncol = 2)
rownames(ssa) <- f
colnames(ssa) <- sig

# Функция расчитывает объём выборки для MANOVA
# subgroups - число подгрупп с учётом градаций всех факторов
# Вовзращает общий объём выборки
sample.size.manova <- function(subgroups, v = NULL, f2, sig = 0.05, b = 0.8) {
  fit <- pwr.f2.test(u = (subgroups - 1), f2 = f2, sig.level = sig, power = b)
  size <- round(fit$v) + 1
}
# Сводные данные по расчёту объёма выборки для MANOVA
ssm <- matrix(mapply(FUN = sample.size.manova, f2 = rep(f2, 2), sig = sig, subgroups = subgroups), ncol = 2)
rownames(ssm) <- f2
colnames(ssm) <- sig

# Функция расчитывает мощность для заданного объёма выборки и количества подгрупп
# n - общий объём выборки
power.test.anova <- sample.size.anova <- function(groups, n, f, sig = 0.05) {
  fit <- pwr.anova.test(k = groups, n = round(n/groups), f = f, sig.level = sig, power = NULL)
  power <- round(fit$power, digits = 2)
}
# Мозность для выборки из 60 человек (3 группы)
ssaf <- matrix(mapply(FUN = sample.size.anova, f = rep(f, 2), sig = sig, groups = groups, n = 60), ncol = 2)
rownames(ssaf) <- f
colnames(ssaf) <- sig

# Функция расчитывает объём выборки для зависимых выборок
# Вовзращает число пар
sample.size.paired <- function(n = NULL, d, sig = 0.05, b = 0.8) {
  fit <- pwr.t.test(n = NULL, d = d, sig.level = sig, power = b, type = "paired")
  size <- round(fit$n)
}
# Оюъём выборки для зависимых выборок
ssp <- matrix(mapply(FUN = sample.size.paired, d = rep(d, 2), sig = sig), ncol = 2)
rownames(ssp) <- d
colnames(ssp) <- sig