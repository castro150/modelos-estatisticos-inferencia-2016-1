###########################################
#          ESTUDO DE CASO 3               #
#                                         #
#Equipe: Gustavo Vieira Costa             #
#        Marcus Vinpicius do Carmo Bastos #
#        Rafael Carneiro de Castro        #
#        Thaís Matos Acácio               #
###########################################

#Ler dados de entrada
dados <- read.table("dados.csv", header=TRUE, sep=",");
aggdata <- aggregate(dados$Peso~dados$Fonte,data=dados,FUN=mean);
summary(aggdata)
t.test(dados$Peso~dados$Fonte, paired=T, data=aggdata);