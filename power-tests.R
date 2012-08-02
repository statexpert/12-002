source("functions.R")
f <- c(0.1, 0.25, 0.4) # размеры эффекта для ANOVA
f2 <- c(0.02, 0.15, 0.35) # размеры эффекта для MANOVA
d <- c(0.2, 0.5, 0.8)
sig <- c(0.05, 0.01) # уровни значимости
groups <- 3
subgroups <- 54

# Сводные данные по расчёту объёма выборки для ANOVA
ssa <- matrix(mapply(FUN = sample.size.anova, f = rep(f, 2), sig = sig, groups = groups), ncol = 2)
rownames(ssa) <- f
colnames(ssa) <- sig

# Сводные данные по расчёту объёма выборки для MANOVA
ssm <- matrix(mapply(FUN = sample.size.manova, f2 = rep(f2, 2), sig = sig, subgroups = subgroups), ncol = 2)
rownames(ssm) <- f2
colnames(ssm) <- sig

# Мозность для выборки из 60 человек (3 группы)
ssaf <- matrix(mapply(FUN = sample.size.anova, f = rep(f, 2), sig = sig, groups = groups, n = 60), ncol = 2)
rownames(ssaf) <- f
colnames(ssaf) <- sig

# Оюъём выборки для зависимых выборок
ssp <- matrix(mapply(FUN = sample.size.paired, d = rep(d, 2), sig = sig), ncol = 2)
rownames(ssp) <- d
colnames(ssp) <- sig