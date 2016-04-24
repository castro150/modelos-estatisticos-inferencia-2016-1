###########################################
#          ESTUDO DE CASO 3               #
#                                         #
#Equipe: Gustavo Vieira Costa             #
#        Marcus Vinícius do Carmo Bastos  #
#        Rafael Carneiro de Castro        #
#        Thaís Matos Acácio               #
###########################################

#Ler dados de entrada
dados <- read.table("dados.csv", header=TRUE, sep=",");
aggdata <- aggregate(dados$Peso~dados$Fonte,data=dados,FUN=mean);
summary(aggdata);

#Separando
estimados <- c(0);
ei <- 1;
medidos <- c(0);
mi <- 1;
for (i in seq(1:22)) {
  if (dados$Fonte[i] == 'Estimado') {
    estimados[ei] <- dados$Peso[i];
    ei <- ei + 1;
  } else {
    medidos[mi] <- dados$Peso[i];
    mi <- mi + 1;
  }
}

#Boxplot Pesos Estimados e Medidos em Balança
boxplot(estimados, medidos, ylab="Peso (kg)",
        names=c("Estimados","Medidos"), col = "lightgray",
        main="Figura 1: Pesos Estimados e Medidos em Balança");

#Boxplot Diferença dos Pesos Estimados e Medidos
diffPesos <- estimados - medidos;
boxplot(diffPesos, ylab="Peso (kg)",
        names=c("Diferença"), col = "lightgray",
        main="Figura 2: Diferença dos Pesos Estimados e Medidos");

# qqPlot para testar a normalidade das diferenças entre
# o peso estimado e o peso medido.
require("car");
qqPlot(diffPesos, pch=16, cex=1.5, las=1, main="Figura 3: Normalidade da diferença dos dados");

#Teste t pareado
t.test(dados$Peso~dados$Fonte, paired=T, data=aggdata);

#Teste de potência considerando o desvio padrão da populaçao
#como o desvio padrão da amostra.
s <- sd(diffPesos);
power.t.test(n=11,
             delta=0.5,
             sd=s,
             sig.level=0.05,
             type="paired");

#Parâmetros de plots.
par(bg = "#dddddd");

#Potência variando tamanho de efeito.
delta <- 0.5;
powerBySe <- c(0);
es <- c(0);
for (i in seq(1:300)) {
  powerBySe[i] <- power.t.test(n=11,
                               delta=delta,
                               sd=s,
                               sig.level=0.05,
                               type="paired")$power;
  es[i] <- delta;
  delta <- delta + 0.01;
}
plot(es, powerBySe, type="l", lwd=2, las=1,
     main="Figura 4: Potência x Tamanho de Efeito", xlab="Tamanho de Efeito", ylab="Potência");
grid(NA,NULL,"white",lwd=2,lty=1);

#Potência variando quantidade de amostras.
n <- 11;
powerByN <- c(0);
ni <- c(0);
for (i in seq(1:1000)) {
  powerByN[i] <- power.t.test(n=n,
                              delta=0.5,
                              sd=s,
                              sig.level=0.05,
                              type="paired")$power;
  ni[i] <- n;
  n <- n + 0.5;
}
plot(ni, powerByN, type="l", lwd=2, las=1, 
     main="Figura 5: Potência x Tamanho da Amostra", xlab="Tamanho da Amostra", ylab="Potência");
grid(NA,NULL,"white",lwd=2,lty=1)
