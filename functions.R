library(pwr)

# Функция расчитывает объём выборки для ANOVA
# Вовзращает общий объём выборки
sample.size.anova <- function(groups, n = NULL, f, sig = 0.05, b = 0.8) {
  result <- pwr.anova.test(k = groups, f = f, sig.level = sig, power = b)
  round(result$n)*groups
}

# Функция расчитывает объём выборки для MANOVA
# subgroups - число подгрупп с учётом градаций всех факторов
# Вовзращает общий объём выборки
sample.size.manova <- function(subgroups, v = NULL, f2, sig = 0.05, b = 0.8) {
  result <- pwr.f2.test(u = (subgroups - 1), f2 = f2, sig.level = sig, power = b)
  round(result$v) + 1
}

# Функция расчитывает мощность для заданного объёма выборки и количества подгрупп
# n - общий объём выборки
power.test.anova <- function(groups, n, f, sig = 0.05) {
  result <- pwr.anova.test(k = groups, n = round(n/groups), f = f, sig.level = sig, power = NULL)
  round(result$power, digits = 2)
}

# Функция расчитывает размер эффекта для заданной мощности, объёма выборки и количества подгрупп
# n - общий объём выборки
effect.size.anova <- function(groups, n, sig = 0.05, b = 0.8) {
  result <- pwr.anova.test(k = groups, n = round(n/groups), f = NULL, sig.level = sig, power = b)
  round(result$f, digits = 2)
}

# Функция расчитывает объём выборки для зависимых выборок
# Вовзращает число пар
sample.size.paired <- function(n = NULL, d, sig = 0.05, b = 0.8) {
  result <- pwr.t.test(n = NULL, d = d, sig.level = sig, power = b, type = "paired")
  round(result$n)
}

# Функция расчитывает мощность для заданного объёма выборки и количества подгрупп
# n - общий объём выборки
power.test.paired <- function(n, d, sig = 0.05) {
  result <- pwr.t.test(n = n, d = d, sig.level = sig, power = NULL, type = "paired")
  round(result$power, digits = 2)
}