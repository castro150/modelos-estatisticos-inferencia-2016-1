###########################################
#            TRABALHO FINAL               #
#                                         #
#Equipe: Gustavo Vieira Costa             #
#        Marcus Vinícius do Carmo Bastos  #
#        Rafael Carneiro de Castro        #
#        Thaís Matos Acácio               #
###########################################


library(ExpDE);
selpars <- list(name = "selection_standard");
stopcrit <- list(names = "stop_maxeval", maxevals = 60000, maxiter = 1000);
probpars <- list(name = "sphere", xmin = -seq(1,20), xmax = 20 + 5 * seq(5, 24));

# Grupo A
recpars1 <- list(name = "recombination_arith");
mutpars1 <- list(name = "mutation_rand", f = 4);
popsize1 <- 300;
recpars2 <- list(name = "recombination_bin", cr = 0.7);
mutpars2 <- list(name = "mutation_best", f = 3);
popsize2 <- 300;
recpars3 <- list(name = "recombination_blxAlphaBeta", alpha = 0.4, beta = 0.4);
mutpars3 <- list(name = "mutation_rand", f = 4);
popsize3 <- 230;
recpars4 <- list(name = "recombination_eigen", othername = "recombination_bin", cr = 0.9);
mutpars4 <- list(name = "mutation_best", f = 2.8);
popsize4 <- 85;
a <- c(0);
b <- c(0);
c <- c(0);
d <- c(0);
for (i in seq(1:20)){
  out1 <- ExpDE(popsize1, mutpars1, recpars1, selpars, stopcrit, probpars);
  a[i] <- out1$Fbest;
  out2 <- ExpDE(popsize2, mutpars2, recpars2, selpars, stopcrit, probpars);
  b[i] <- out2$Fbest;
  out3 <- ExpDE(popsize3, mutpars3, recpars3, selpars, stopcrit, probpars);
  c[i] <- out3$Fbest;
  out4 <- ExpDE(popsize4, mutpars4, recpars4, selpars, stopcrit, probpars);
  d[i] <- out4$Fbest;
}

# Grupo B
recpars1 <- list(name = "recombination_exp", cr = 0.6)
mutpars1 <- list(name = "mutation_best", f = 2)
popsize1 <- 130
recpars2 <- list(name = "recombination_geo", alpha = 0.6)
mutpars2 <- list(name = "mutation_rand", f = 1.2)
popsize2 <- 70
recpars3 <- list(name = "recombination_lbga")
mutpars3 <- list(name = "mutation_rand", f = 4.5)
popsize3 <- 300
recpars4 <- list(name = "recombination_blxAlphaBeta", alpha = 0.1, beta = 0.4)
mutpars4 <- list(name = "mutation_rand", f = 3)
popsize4 <- 80
e <- c(0);
f <- c(0);
g <- c(0);
h <- c(0);
for (i in seq(1:20)){
  out1 <- ExpDE(popsize1, mutpars1, recpars1, selpars, stopcrit, probpars);
  e[i] <- out1$Fbest;
  out2 <- ExpDE(popsize2, mutpars2, recpars2, selpars, stopcrit, probpars);
  f[i] <- out2$Fbest;
  out3 <- ExpDE(popsize3, mutpars3, recpars3, selpars, stopcrit, probpars);
  g[i] <- out3$Fbest;
  out4 <- ExpDE(popsize4, mutpars4, recpars4, selpars, stopcrit, probpars);
  h[i] <- out4$Fbest;
}

# Grupo C
recpars1 <- list(name = "recombination_blxAlphaBeta", alpha = 0, beta = 0)
mutpars1 <- list(name = "mutation_rand", f = 4)
popsize1 <- 200
recpars2 <- list(name = "recombination_linear")
mutpars2 <- list(name = "mutation_rand", f = 1.5)
popsize2 <- 250
recpars3 <- list(name = "recombination_mmax", lambda = 0.25)
mutpars3 <- list(name = "mutation_best", f = 4)
popsize3 <- 375
recpars4 <- list(name = "recombination_npoint", N = 17)
mutpars4 <- list(name = "mutation_rand", f = 2.2)
popsize4 <- 225
j <- c(0);
k <- c(0);
l <- c(0);
m <- c(0);
for (i in seq(1:20)){
  out1 <- ExpDE(popsize1, mutpars1, recpars1, selpars, stopcrit, probpars);
  j[i] <- out1$Fbest;
  out2 <- ExpDE(popsize2, mutpars2, recpars2, selpars, stopcrit, probpars);
  k[i] <- out2$Fbest;
  out3 <- ExpDE(popsize3, mutpars3, recpars3, selpars, stopcrit, probpars);
  l[i] <- out3$Fbest;
  out4 <- ExpDE(popsize4, mutpars4, recpars4, selpars, stopcrit, probpars);
  m[i] <- out4$Fbest;
}

# Grupo D
recpars1 <- list(name = "recombination_onepoint", K = 17)
mutpars1 <- list(name = "mutation_best", f = 2.4)
popsize1 <- 225
recpars2 <- list(name = "recombination_pbest", cr = 0.25)
mutpars2 <- list(name = "mutation_rand", f = 3.5)
popsize2 <- 325
recpars3 <- list(name = "recombination_sbx", eta = 90)
mutpars3 <- list(name = "mutation_best", f = 4.5)
popsize3 <- 200
recpars4 <- list(name = "recombination_wright")
mutpars4 <- list(name = "mutation_best", f = 4.8)
popsize4 <- 113
n <- c(0);
o <- c(0);
p <- c(0);
q <- c(0);
for (i in seq(1:20)){
  out1 <- ExpDE(popsize1, mutpars1, recpars1, selpars, stopcrit, probpars);
  n[i] <- out1$Fbest;
  out2 <- ExpDE(popsize2, mutpars2, recpars2, selpars, stopcrit, probpars);
  o[i] <- out2$Fbest;
  out3 <- ExpDE(popsize3, mutpars3, recpars3, selpars, stopcrit, probpars);
  p[i] <- out3$Fbest;
  out4 <- ExpDE(popsize4, mutpars4, recpars4, selpars, stopcrit, probpars);
  q[i] <- out4$Fbest;
}