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

# Executar teste de igualdade de variancia
var.test(bmiEngSis, bmiPpgee, alternitive="two.sided",conf.level=0.95)

# Variancias bem diferentes
t.test(bmiEngSis, bmiPpgee,
      alternative = "two.sided",
      mu = 0,
      var.equal = FALSE, # Mudar aqui pelo resultado do teste
      conf.level = 0.95)

# Estimar tamanho de efeito
# Calcular a potência do teste
