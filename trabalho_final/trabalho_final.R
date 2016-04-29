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
fbest1 <- c(0);
fbest2 <- c(0);
fbest3 <- c(0);
fbest4 <- c(0);
for (i in seq(1:30)){
  out1 <- ExpDE(popsize1, mutpars1, recpars1, selpars, stopcrit, probpars);
  fbest1[i] <- out1$Fbest;
  out2 <- ExpDE(popsize2, mutpars2, recpars2, selpars, stopcrit, probpars);
  fbest2[i] <- out2$Fbest;
  out3 <- ExpDE(popsize3, mutpars3, recpars3, selpars, stopcrit, probpars);
  fbest3[i] <- out3$Fbest;
  out4 <- ExpDE(popsize4, mutpars4, recpars4, selpars, stopcrit, probpars);
  fbest4[i] <- out4$Fbest;
}
expPiloto <- c(fbest1-fbest2,fbest1-fbest3,fbest1-fbest4,fbest2-fbest3,fbest2-fbest4,fbest3-fbest4);
