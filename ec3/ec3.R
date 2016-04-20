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
        main="Pesos Estimados e Medidos em Balança");

#Boxplot Diferença dos Pesos Estimados e Medidos
diffPesos <- estimados - medidos;
boxplot(diffPesos, ylab="Peso (kg)",
        names=c("Diferença"), col = "lightgray",
        main="Diferença dos Pesos Estimados e Medidos");

# qqPlot para testar a normalidade das diferenças entre
# o peso estimado e o peso medido.
require("car");
qqPlot(diffPesos,
       pch=16,
       cex=1.5,
       las=1);

t.test(dados$Peso~dados$Fonte, paired=T, data=aggdata);
