########################################
#          ESTUDO DE CASO 1            #
#                                      #
#Equipe: Gustavo Vieira Costa          #
#        Rafael Carneiro de Castro     #
#        Thaís Matos Acácio            #
########################################

mu <- 26.3;
alpha <- 0.05;

#Ler dados de entrada
dados <- read.table("data.csv", header=FALSE, sep=";");

#Cálculo do BMI
BMI <- dados[1]/(dados[2]^2);

#Número de Amostras
n <- nrow(BMI);

#Média
x_bar <- mean(as.matrix(BMI));

#Tamanho de Efeito
size_effect <- x_bar - mu;

#Desvio padrão
s <- sqrt(sum((BMI-x_bar)^2)/(n-1));

#t crítico
t_alpha <- qt(alpha/2, n-1);

#T0
t0 <- (x_bar - mu)/(s/sqrt(n));

#Intervalo de confiança
inter_min <- x_bar + (s*t_alpha / (sqrt(n)));
inter_max <- x_bar - (s*t_alpha / (sqrt(n)));

power.t.test(n=n,
              delta=size_effect,
              sd=s,
              sig.level=0.05,
              type = "one.sample");
message('Valor de mu: ', mu);
message('Valor de alpha: ', alpha);
message('Número de amostras: ', n);
message('Média amostral: ', x_bar);
message('Desvio padrão: ', s);
message('Tamanho de efeito: ', size_effect);
message('T crítico: ', t_alpha);
message('T0: ', t0);
message('Intervalo de confiança: ', inter_min, ' a ', inter_max);

