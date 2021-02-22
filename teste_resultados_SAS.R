library(openxlsx)
library(dae)
library(mvnormtest)
library(QuantPsyc)
options(scipen=100)
##################################################
################### Armadilhas ###################
##################################################
################# Dados Brutos ###################
armadilhas <- read.xlsx(file.choose())
armadilhas <- armadilhas[,c(3,4,5,7,8)]
colnames(armadilhas) <- c("semana","trat","bloco","moscab","rank")
armadilhas$semana <- as.factor(armadilhas$semana)
armadilhas$trat <- as.factor(armadilhas$trat)
armadilhas$bloco <- as.factor(armadilhas$bloco)
attach(armadilhas)
m <- 9 #medidas repetidas
##################### estimação dos parâmetros
##### tratando o experimento sem levar em consideração as medidas repetidas (split plot)
##### Y = Medidas + trat + bloco + erro1 + medidas:trat + medidas:bloco + erro2  
mean(moscab)
tapply(moscab,trat,mean)-mean(moscab)
tapply(moscab,bloco,mean)-mean(moscab)
# Erro 1 (MSE1 fornecido pelo SAS) 3536213.44
tapply(moscab,semana,mean)-mean(moscab)
tapply(moscab,semana:trat,mean)-mean(moscab)
tapply(moscab,semana:bloco,mean)-mean(moscab)
# Erro 2 (MSE2 fornecido pelo SAS) 860076.8

##### separando por semanas
##### ANOVA por medidas repetidas trata cada medida por uma variável resposta
##### Y_ijk = medida_k + trat_i + bloco_j + erro_ijk
tapply(moscab,semana,mean)
tapply(moscab,semana:trat,mean)-rep(tapply(moscab,semana,mean),each=5)
tapply(moscab,semana:bloco,mean)-rep(tapply(moscab,semana,mean),each=3)

plot(moscab~semana)
interaction.plot(semana,trat,moscab)
interaction.plot(semana,bloco,moscab)
################## residuos
residuos <- as.matrix(read.xlsx(file.choose()))
residuos <- residuos[,21:29]
contrastes <- as.matrix(read.xlsx(file.choose()))

s<- t(residuos)%*%residuos
E <- contrastes%*%s%*%t(contrastes) # Matriz E fornecida pelo SAS

#################### teste de mauschly
w <- ((m-1)**(m-1))*det(E)/(sum(diag(E))**(m-1))
gama <- 8-(2*m**2-3*m+3)/(6*(m-1))
chi <- -gama*log(w,base=exp(1))
1-pchisq(chi,(m*(m-1)/2-1))

################### correções matriz de erros dada pelo SAS
gg <- (sum(diag(E))**2)/((m-1)*sum((E)**2))
hfl <- (9*(m-1)*gg-2)/((m-1)*(8-(m-1)*gg))

################### testes de normalidade
shapiro.test(residuos) #todos os resíduos juntos
apply(residuos,2,shapiro.test) # resíduos por medida repetida
apply(residuos,1,shapiro.test) # resíduos por unidade experimental
mshapiro.test(s)
mult.norm(residuos)
##################################################
################### Ranques ######################
attach(armadilhas)
##################### estimação dos parâmetros
##### tratando o experimento sem levar em consideração as medidas repetidas (split plot)
mean(rank)
tapply(rank,trat,mean)-mean(rank)
tapply(rank,bloco,mean)-mean(rank)
# Erro 1 (MSE1 fornecido pelo SAS) 3536213.44
tapply(rank,semana,mean)-mean(rank)
tapply(rank,semana:trat,mean)-mean(rank)
tapply(rank,semana:bloco,mean)-mean(rank)
# Erro 2 (MSE2 fornecido pelo SAS) 860076.8

##### separando por semanas
tapply(rank,semana,mean)
tapply(rank,semana:trat,mean)-rep(tapply(rank,semana,mean),each=5)
tapply(rank,semana:bloco,mean)-rep(tapply(rank,semana,mean),each=3)

plot(rank~semana)
interaction.plot(semana,trat,rank)
interaction.plot(semana,bloco,rank)

################## residuos
residuos <-as.matrix(read.xlsx(file.choose()))
residuos <- residuos[,22:30]
contrastes <- as.matrix(read.xlsx(file.choose()))

s<- t(residuos)%*%residuos
E <- contrastes%*%s%*%t(contrastes) # Matriz E fornecida pelo SAS

#################### teste de mauschly
w <- ((m-1)**(m-1))*det(E)/(sum(diag(E))**(m-1))
gama <- 8-(2*m**2-3*m+3)/(6*(m-1))
chi <- -gama*log(w,base=exp(1))
1-pchisq(chi,(m*(m-1)/2-1))

################### correções matriz de erros dada pelo SAS
gg <- (sum(diag(E))**2)/((m-1)*sum((E)**2))
hfl <- (9*(m-1)*gg-2)/((m-1)*(8-(m-1)*gg))
gg
hfl



##################################################
#################### Plantas #####################
##################################################
################# Dados Brutos ###################

plantas <- read.xlsx(file.choose())
plantas <- plantas[,c(1,4,5,6,7,8)]
colnames(plantas) <- c("semana","trat","bloco","planta","moscab","rank")
plantas$semana <- as.factor(plantas$semana)
plantas$trat <- as.factor(plantas$trat)
plantas$bloco <- as.factor(plantas$bloco)
attach(plantas)
m <- 7 #medidas repetidas
##### tratando o experimento sem levar em consideração as medidas repetidas (split plot)
mean(moscab)
tapply(moscab,trat,mean)-mean(moscab)
tapply(moscab,bloco,mean)-mean(moscab)
# Erro 1 (MSE1 fornecido pelo SAS) 
tapply(moscab,semana,mean)-mean(moscab)
tapply(moscab,semana:trat,mean)-mean(moscab)
tapply(moscab,semana:bloco,mean)-mean(moscab)
# Erro 2 (MSE2 fornecido pelo SAS) 

##### separando por semanas
tapply(moscab,semana,mean)
tapply(moscab,semana:trat,mean)-rep(tapply(moscab,semana,mean),each=5)
tapply(moscab,semana:bloco,mean)-rep(tapply(moscab,semana,mean),each=3)
plot(moscab~semana)
interaction.plot(semana,trat,moscab)
interaction.plot(semana,bloco,moscab)
interaction.ABC.plot(moscab,semana,trat,bloco,data=plantas)
################## residuos
residuos <- as.matrix(read.xlsx(file.choose()))
residuos <- residuos[,19:25]
contrastes <- as.matrix(read.xlsx(file.choose()))

s<- t(residuos)%*%residuos
E <- contrastes%*%s%*%t(contrastes) # Matriz E fornecida pelo SAS

#################### teste de mauschly
w <- ((m-1)**(m-1))*det(E)/(sum(diag(E))**(m-1))
gama <- 84-(2*m**2-3*m+3)/(6*(m-1))
chi <- -gama*log(w,base=exp(1))
1-pchisq(chi,(9*(9-1)/2-1))

################### correções matriz de erros dada pelo SAS
gg <- (sum(diag(E))**2)/((m-1)*sum((E)**2))
hfl <- (85*(m-1)*gg-2)/((m-1)*(84-(m-1)*gg))
gg
hfl



##################################################
################### Ranques ######################
attach(plantas)
##################### estimação dos parâmetros

##### separando por semanas
tapply(rank,semana,mean)
tapply(rank,semana:trat,mean)-rep(tapply(rank,semana,mean),each=5)
tapply(rank,semana:bloco,mean)-rep(tapply(rank,semana,mean),each=3)

interaction.plot(semana,trat,rank)
interaction.plot(semana,bloco,rank)
interaction.ABC.plot(rank,semana,trat,bloco,data=plantas)

################## residuos
residuos <-as.matrix(read.xlsx(file.choose()))
residuos <- residuos[,19:25]
contrastes <- as.matrix(read.xlsx(file.choose()))

s<- t(residuos)%*%residuos
E <- contrastes%*%s%*%t(contrastes) # Matriz E fornecida pelo SAS

#################### teste de mauschly
w <- ((m-1)**(m-1))*det(E)/(sum(diag(E))**(m-1))
gama <- 8-(2*m**2-3*m+3)/(6*(m-1))
chi <- -gama*log(w,base=exp(1))
1-pchisq(chi,(m*(m-1)/2-1))

################### correções matriz de erros dada pelo SAS
gg <- (sum(diag(E))**2)/((m-1)*sum((E)**2))
hfl <- (85*(m-1)*gg-2)/((m-1)*(84-(m-1)*gg))
gg
hfl


