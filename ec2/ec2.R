########################################
#          ESTUDO DE CASO 2            #
#                                      #
#Equipe: Gustavo Vieira Costa          #
#        Rafael Carneiro de Castro     #
#        Thaís Matos Acácio            #
########################################

#Ler dados de entrada
dados <- read.table("bmi_20161.csv", header=TRUE, sep=",");
sis <- dados[which(dados$Course=='EngSis'),];
ppgee <- dados[which(dados$Course=='PPGEE'),];

#Cálculo do BMI
bmiEngSis <- sis$Weight.kg/((sis$Height.cm/100)^2);
bmiPpgee <- ppgee$Weight.kg/((ppgee$Height.cm/100)^2);
# bmi <- dados$Weight.kg/((dados$Height.cm/100)^2);

#Boxplot
boxplot(bmiEngSis, bmiPpgee, ylab="BMI",
         names=c("Eng. Sistemas","PPGEE"), col = "lightgray",
         main="BMI para Eng. Sistemas e PPGEE (Pós-Graduação)");

# qqPlot para testar a normalidade dos residuos
require("car");
resid <- c(bmiEngSis - mean(bmiEngSis), bmiPpgee - mean(bmiPpgee));
qqPlot(resid,
       pch=16,
       cex=1.5,
       las=1);
# É aproximadamente uma reta. Então a normalidade é aceitável.

# Executar teste de igualdade de variancia
var.test(bmiEngSis, bmiPpgee, alternitive="two.sided",conf.level=0.95);
# Como p-value > alpha, a hipótese nula não foi rejeitada. Sendo
# assim, não se pode falar com certeza que as variâncias não são iguais.

# Test t com variancia igual ou não
t.test(bmiEngSis, bmiPpgee,
      alternative = "two.sided",
      mu = 0,
      var.equal = TRUE, # Mudar aqui pelo resultado do teste
      conf.level = 0.95);
#P-Value > alpha não rejeita a hipótese nula.


# Estimar tamanho de efeito = 1.5
# Calcular a potência do teste
require("pwr");
pwr.t2n.test(n1 = 13,
             n2 = 28,
             d = 1.5,
             sig.level = 0.05,
             alternative = "two.sided");
# Problema que o delta é alto. Reduzindo delta.
pwr.t2n.test(n1 = 13,
             n2 = 28,
             d = 0.8,
             sig.level = 0.05,
             alternative = "two.sided");
# Com o delta reduzido, a potência também. Obter a quantidade
# de amostras da engenharia de sistemas para ter uma motência
# de 80%.
pwr.t2n.test(power = 0.8,
             n2 = 28,
             d = 0.8,
             sig.level = 0.05,
             alternative = "two.sided");

# Dá pra fazer o power.t.test com o menor tamanho amostral e maior sd
power.t.test(n=13,
             delta=1.5,
             sd=sd(bmiEngSis),
             sig.level=0.05,
             type = "two.sample");
