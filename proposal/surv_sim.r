library(survival)
library(ggplot2)
library(reshape2)

theme_set(theme_bw())

rng <- data.frame(x <- c(0, 5))
xx <- seq(0, 5, by = 0.01)

# exponential survival
# S(t) = exp^{-lambda t}
# h(t) = lambda
exp.surv <- function(time, lambda) {
  exp(-(lambda) * time)
}

exp.haz <- function(time, lambda) {
  lambda
}


# Weibull survival
# S(t) = exp^{-lambda t^p}
# h(t) = lambda p t^{p - 1}
wei.surv <- function(time, lambda, p) {
  tt <- time**p
  exp(-(lambda) * tt)
}

wei.haz <- function(time, lambda, p) {
  tt <- time**(p - 1)
  lambda * p * tt
}


# log-logistic survival
# S(t) = \frac{1}{1 + lambda t^{p}}
# h(t) = \frac{lambda p t^{p - 1}}{1 + lambda t^{p}}
lgn.surv <- function(time, lambda, p) {
  tt <- time**p
  bot <- 1 + (lambda * tt)
  1 / bot
}

lgn.haz <- function(time, lambda, p) {
  t1 <- time**(p - 1)
  top <- lambda * p * t1

  t2 <- time**p
  bot <- 1 + (lambda * t2)

  top / bot
}


lambda <- 1
p1 <- 1.3
p2 <- 0.7

eh <- exp.haz(xx, lambda)
whi <- wei.haz(xx, lambda, p1)
whd <- wei.haz(xx, lambda, p2)
lhi <- lgn.haz(xx, lambda, p1)
lhd <- lgn.haz(xx, lambda, p2)

haz.mod <- cbind(eh, whi, whd, lhi, lhd)
haz.mod <- melt(haz.mod)
haz.mod$Var1 <- rep(xx, 5)
haz.mod$Var2 <- as.character(haz.mod$Var2)
haz.mod$Var2[haz.mod$Var2 == 'eh'] <- 'Exponential'
haz.mod$Var2[haz.mod$Var2 == 'whi'] <- 'Weibull: shape > 1'
haz.mod$Var2[haz.mod$Var2 == 'whd'] <- 'Weibull: shape < 1'
haz.mod$Var2[haz.mod$Var2 == 'lhi'] <- 'Log-logistic: shape > 1'
haz.mod$Var2[haz.mod$Var2 == 'lhd'] <- 'Log-logistic: shape < 1'

ghaz <- ggplot(haz.mod, aes(x = Var1, y = value)) + geom_line()
ghaz <- ghaz + labs(x = 't', y = 'h(t)')
ghaz <- ghaz + theme(axis.title.y = element_text(angle = 0, size = 18),
                     axis.title.x = element_text(size = 18),
                     axis.text.y = element_blank(),
                     axis.ticks.y = element_blank(),
                     panel.grid = element_blank())
ghaz <- ghaz + facet_grid(. ~ Var2)

ggsave(filename = 'figure/hazard.png', plot = ghaz, width = 10, height = 4)
