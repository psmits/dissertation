library(ggplot2)
library(reshape2)
library(geiger)
library(phytools)

theme_set(theme_bw())
cbp.long <- c('#000000', '#004949', '#009292', '#FF7DB6', '#FFB6DB', 
              '#490092', '#006DDB', '#B66DFF', '#6DB6FF', '#B6DBFF', 
              '#920000', '#924900', '#DBD100', '#24FF24', '#FFFF6D')

nsim <- 100
set.seed(420)


# brownian motion
o <- list()
for(jj in seq(nsim)) {
  x <- c()
  x[1] <- 0
  for(ii in seq(from = 2, to = 100)) {
    x[ii] <- x[ii - 1] + rnorm(1, 0, 1)
  }
  x <- cbind(x, seq(length(x)), jj)
  o[[jj]] <- x
}

o <- data.frame(Reduce(rbind, o))
names(o) <- c('value', 'time', 'sim')

brown.sim <- ggplot(o, aes(x = time, y = value, group = sim))
brown.sim <- brown.sim + geom_line()
ggsave(filename = 'figure/brown_sim.png', plot = brown.sim,
       width = 6, height = 4)


# birth death
b <- list()
for(ii in seq(10)) {
  b[[ii]] <- sim.bdtree(b = 1.5, d = 1, stop = 'time', t = 4)
}
class(b) <- 'multiPhylo'
b <- lapply(b, ltt, plot = FALSE)
b <- lapply(b, function(x) x[c('ltt', 'times')])
b <- lapply(b, function(x) Reduce(cbind, x))
b <- Map(function(x, y) data.frame(x, sim = y), x = b, y = seq(length(b)))
b <- Reduce(rbind, b)
names(b) <- c('diversity', 'time', 'sim')
b$sim <- factor(b$sim)
b$diversity <- log1p(b$diversity)

lttg <- ggplot(b, aes(x = time, y = diversity, colour = sim))
lttg <- lttg + geom_step(size = 1)
lttg <- lttg + scale_colour_manual(values = cbp.long, guide = FALSE)
lttg <- lttg + labs(y = 'log(diversity + 1)')
ggsave(filename = 'figure/bd_sim.png', plot = lttg, width = 6, height = 4)
