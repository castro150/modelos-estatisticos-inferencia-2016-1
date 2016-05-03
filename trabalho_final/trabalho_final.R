###########################################
#            TRABALHO FINAL               #
#                                         #
#Equipe: Gustavo Vieira Costa             #
#        Marcus Vinícius do Carmo Bastos  #
#        Rafael Carneiro de Castro        #
#        Thaís Matos Acácio               #
###########################################

alpha <- 0.05;
beta <- 0.15;
d <- 0.25;
a <- 4;

# Calculando o tamanho amostral (suficiente)
library(pwr);
n <- pwr.anova.test(k = a,f = d, sig.level = alpha, power = 1-beta)$n;
n <- ceiling(n);

library(ExpDE);
selpars <- list(name = "selection_standard");
stopcrit <- list(names = "stop_maxeval", maxevals = 60000, maxiter = 1000);
probpars <- list(name = "sphere", xmin = -seq(1,20), xmax = 20 + 5 * seq(5, 24));

# Grupo C (Operadores para comparação)
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

# Gerando n observações para cada operador
fbest1 <- c(0);
fbest2 <- c(0);
fbest3 <- c(0);
fbest4 <- c(0);
for (i in seq(1:n)){
  out1 <- ExpDE(popsize1, mutpars1, recpars1, selpars, stopcrit, probpars);
  fbest1[i] <- out1$Fbest;
  out2 <- ExpDE(popsize2, mutpars2, recpars2, selpars, stopcrit, probpars);
  fbest2[i] <- out2$Fbest;
  out3 <- ExpDE(popsize3, mutpars3, recpars3, selpars, stopcrit, probpars);
  fbest3[i] <- out3$Fbest;
  out4 <- ExpDE(popsize4, mutpars4, recpars4, selpars, stopcrit, probpars);
  fbest4[i] <- out4$Fbest;
}
algoritmo <- c(rep("A",n), rep("B",n), rep("C",n), rep("D",n));
fbest <- c(fbest1, fbest2, fbest3, fbest4);
dadosColetados <- data.frame(algoritmo, fbest);
summary(dadosColetados);

# Boxplot
boxplot(fbest~algoritmo, 
        data = dadosColetados, 
        xlab = "Algoritmo",
        ylab = "Fbest",
        main = "FBest dos Algoritmos Medidos",
        pch  = 16,
        col  = "gray");

# Teste da hipótese
model <- aov(fbest~algoritmo, data = dadosColetados);
summary.aov(model);
plot(model);

# All vs. all
library(multcomp);
tukey <- glht(model, linfct = mcp(algoritmo = "Tukey"));
tukey_CI <- confint(tukey, level = 0.95);
summary(tukey_CI);
plot(tukey_CI);

# All vs. one
dadosColetados$algoritmo <- relevel(dadosColetados$algoritmo, ref = "B");
model2 <- aov(fbest~algoritmo, data = dadosColetados);
dunnet <- glht(model2, linfct = mcp(algoritmo = "Dunnet"));
dunnet_CI <- confint(dunnet, level = 0.95);
summary(dunnet_CI);
plot(dunnet_CI);

# Verificando normalidade
library(car);
#qqPlot(model$residuals, pch=16, cex=1.0, las=1, main="Figura 3: Normalidade dos resíduos");
residuals1 <- fbest1 - mean(fbest1);
residuals2 <- fbest2 - mean(fbest2);
residuals3 <- fbest3 - mean(fbest3);
residuals4 <- fbest4 - mean(fbest4);
qqPlot(residuals1, pch=16, cex=1.0, las=1, main="Normalidade dos resíduos - Algoritmo A");
qqPlot(residuals2, pch=16, cex=1.0, las=1, main="Normalidade dos resíduos - Algoritmo B");
qqPlot(residuals3, pch=16, cex=1.0, las=1, main="Normalidade dos resíduos - Algoritmo C");
qqPlot(residuals4, pch=16, cex=1.0, las=1, main="Normalidade dos resíduos - Algoritmo D");

# Verificando homoscedasticidade
fligner.test(fbest~algoritmo, data = dadosColetados)
plot(x = model$fitted.values, y=model$residuals, main="Homoscedasticidade dos dados", xlab="Fitted values", ylab="Residuos")

# Verificando independência
durbinWatsonTest(model)
plot(x = seq_along(model$residuals),
     y = model$residuals,
     type = "l", 
     xlab="Residual order", 
     ylab="Residual value")
points(x = seq_along(model$residuals),
         y = model$residuals,
         type = "p",
         cex  = 1,
         pch  = 16,
         col = as.numeric(dadosColetados$algoritmo))
grid(NA,NULL, lwd=2)