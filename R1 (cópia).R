#PACOTES#
library(tidyverse)
library(diplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(Hmisc)
library(quantmod)
library(openxlsx)
library(sqldf)
library(zoo)
library(xts)
library(urca)
library(lmtest)
library(vars)
library(PairTrading)
library(bizdays)
library(MASS)
library(sde)
library(egcm)

#Mostra o diretório do arquivo a ser carregado

getwd()

#Mostra qual o tipo do dado, dependendo do eixo X ou Y

is.factor(ativos$x)
is.character(ativos$x)
is.numeric(ativos$x)

#Ativos em csv from google Finance

ativos <- read.csv("C:/Users/gabri/OneDrive/Área de Trabalho/L&S/conexaogooglecsv.csv",
                     header = TRUE, sep = ';', dec = ",")
  
na.omit(ativos) 
ativos$DATE <- as.Date.factor(ativos$DATE)
ativos <- xts(ativos[,-1], order.by=ativos[,1])

headers <- read.csv("C:/Users/gabri/OneDrive/Área de Trabalho/L&S/conexaogooglecsv.csv",
                                         header = FALSE, sep=';', dec = ",", 
                                         nrows = 1, as.is = TRUE)
                     
ativos_400 <- read.csv("C:/Users/gabri/OneDrive/Área de Trabalho/L&S/conexaogooglecsv.csv",
                                            header = TRUE, sep=';', dec = ",", skip = 120)
                     colnames(ativos_400) = headers
  
na.omit(ativos_400) 
ativos_400$DATE <- as.Date.factor(ativos_400$DATE)
ativos_400 <- xts(ativos_400[,-1], order.by=ativos_400[,1])                   
                     
  ativos_300 <- read.csv("C:/Users/gabri/OneDrive/Área de Trabalho/L&S/conexaogooglecsv.csv",
                                            header = TRUE, sep=';', dec = ",", skip = 220)
                     colnames(ativos_300) = headers
          
na.omit(ativos_300) 
ativos_300$DATE <- as.Date.factor(ativos_300$DATE)
ativos_300 <- xts(ativos_300[,-1], order.by=ativos_300[,1]) 
                     
  ativos_200 <- read.csv("C:/Users/gabri/OneDrive/Área de Trabalho/L&S/conexaogooglecsv.csv",
                                            header = TRUE, sep=';', dec = ",", skip = 320)
                     colnames(ativos_200) = headers
                     
na.omit(ativos_200) 
ativos_200$DATE <- as.Date.factor(ativos_200$DATE)
ativos_200 <- xts(ativos_200[,-1], order.by=ativos_200[,1]) 
                     
  ativos_100 <- read.csv("C:/Users/gabri/OneDrive/Área de Trabalho/L&S/conexaogooglecsv.csv",
                                            header = TRUE, sep=';', dec = ",", skip = 400)
                     colnames(ativos_100) = headers
                     
na.omit(ativos_100) 
ativos_100$DATE <- as.Date.factor(ativos_100$DATE)
ativos_100 <- xts(ativos_100[,-1], order.by=ativos_100[,1]) 

  ativos_80 <- read.csv("C:/Users/gabri/OneDrive/Área de Trabalho/L&S/conexaogooglecsv.csv",
                       header = TRUE, sep=';', dec = ",", skip = 420)
                     colnames(ativos_80) = headers

na.omit(ativos_80) 
ativos_80$DATE <- as.Date.factor(ativos_80$DATE)
ativos_80 <- xts(ativos_80[,-1], order.by=ativos_80[,1]) 
                     
  ativos_50 <- read.csv("C:/Users/gabri/OneDrive/Área de Trabalho/L&S/conexaogooglecsv.csv",
                                           header = TRUE, sep=';', dec = ",", skip = 470)
                     colnames(ativos_50) = headers
                     
na.omit(ativos_50) 
ativos_50$DATE <- as.Date.factor(ativos_50$DATE)
ativos_50 <- xts(ativos_50[,-1], order.by=ativos_50[,1]) 

  ativos_30 <- read.csv("C:/Users/gabri/OneDrive/Área de Trabalho/L&S/conexaogooglecsv.csv",
                      header = TRUE, sep=';', dec = ",", skip = 490)
                     colnames(ativos_30) = headers

na.omit(ativos_30) 
ativos_30$DATE <- as.Date.factor(ativos_30$DATE)
ativos_30 <- xts(ativos_30[,-1], order.by=ativos_30[,1])
                     
#Código para retornar uma matriz de correlação
                   
matriz <- rcorr(as.matrix(ativos))
matriz_400 <- rcorr(as.matrix(ativos_400))
matriz_300 <- rcorr(as.matrix(ativos_300))
matriz_200 <- rcorr(as.matrix(ativos_200))
matriz_100 <- rcorr(as.matrix(ativos_100))
matriz_50 <- rcorr(as.matrix(ativos_50))

p_value <- corrplot(matriz$r, p.mat = matriz$p, sig.level = 0.05)
p_value_400 <- corrplot(matriz_400$r, p.mat = matriz$p, sig.level = 0.05)
p_value_300 <- corrplot(matriz_300$r, p.mat = matriz$p, sig.level = 0.05)
p_value_200 <- corrplot(matriz_200$r, p.mat = matriz$p, sig.level = 0.05)
p_value_100 <- corrplot(matriz_100$r, p.mat = matriz$p, sig.level = 0.05)
p_value_50 <- corrplot(matriz_50$r, p.mat = matriz$p, sig.level = 0.05)

#Código do pacote Hmisc que plota diferentes tipos de gráfico
#corrplot(c, method = "number")
#corrplot(c, method = "number")
#corrplot(c, method = "shade")
#corrplot(c, method = "color")

gráfico <- corrplot(matriz$r, method = "pie")
gráfico_400 <- corrplot(matriz_400$r, method = "pie")
gráfico_300 <- corrplot(matriz_300$r, method = "pie")
gráfico_200 <- corrplot(matriz_200$r, method = "pie")
gráfico_100 <- corrplot(matriz_100$r, method = "pie")
gráfico_50 <- corrplot(matriz_50$r, method = "pie")

#matriz$r -> retorna os coeficientes de correlação
#matriz$p -> retorna a probabilidade 
#matriz$n -> retorna a quantidade amostral

p_v_TorF <- p_value > 0.70
p_v_400 <- p_value_400 > 0.70
p_v_300 <- p_value_300 > 0.70
p_v_200 <- p_value_200 > 0.70
p_v_100 <- p_value_100 > 0.70
p_v_50 <- p_value_50 > 0.70


#####################################################################
####### TESTES ESTATÍSTICOS DE COINTEGRAÇÃO ENGLE GRANGER ###########
#####################################################################

#Teste de estacionáriedade variável dependente (EIXO Y)

estacionariedade_y <- ur.df(ativos$BBDC3, type = "none", selectlags = "AIC")
estacionariedade_y_400 <- ur.df(ativos_400$BBDC3, type = "none", selectlags = "AIC")
estacionariedade_y_300 <- ur.df(ativos_300$BBDC3, type = "none", selectlags = "AIC")
estacionariedade_y_200 <- ur.df(ativos_200$BBDC3, type = "none", selectlags = "AIC")
estacionariedade_y_100 <- ur.df(ativos_100$BBDC3, type = "none", selectlags = "AIC")
estacionariedade_y_50 <- ur.df(ativos_50$BBDC3, type = "none", selectlags = "AIC")

summary(estacionariedade_y)
summary(estacionariedade_y_400)
summary(estacionariedade_y_300)
summary(estacionariedade_y_200)
summary(estacionariedade_y_100)
summary(estacionariedade_y_50)

#Teste de estacionáriedade variável independente (EIXO X)

estacionariedade_x <- ur.df(ativos$ITSA4, type = "none", selectlags = "AIC")
estacionariedade_x_400 <- ur.df(ativos_400$ITSA4, type = "none", selectlags = "AIC")
estacionariedade_x_300 <- ur.df(ativos_300$ITSA4, type = "none", selectlags = "AIC")
estacionariedade_x_200 <- ur.df(ativos_200$ITSA4, type = "none", selectlags = "AIC")
estacionariedade_x_100 <- ur.df(ativos_100$ITSA4, type = "none", selectlags = "AIC")
estacionariedade_x_50 <- ur.df(ativos_50$ITSA4, type = "none", selectlags = "AIC")

summary(estacionariedade_x)
summary(estacionariedade_x_400)
summary(estacionariedade_x_300)
summary(estacionariedade_x_200)
summary(estacionariedade_x_100)
summary(estacionariedade_x_50)



############Johansen cointegration test################

#Função attach para transformar o data.frame em objeto

attach(ativos)
#retorna a função attach
detach(ativos)

c = cbind.data.frame(BBAS3, ITSA4)
VARselect(c, lag.max = 10, type ="trend")$selection
co <- ca.jo(c, type = "eigen", ecdet = "const", spec ="longrun", K = 8)
co@cval
co@teststat[1]
co@teststat[2]

attach(ativos_400)
#retorna a função attach
detach(ativos_400)

c_400 = cbind.data.frame(BBAS3, ITSA4)
VARselect(c_400, lag.max = 10, type ="trend")$selection
co_400 <- ca.jo(c_400, type = "eigen", ecdet = "const", spec ="longrun", K = 8)
co_400@cval
co_400@teststat[1]
co_400@teststat[2]

attach(ativos_300)
#retorna a função attach
detach(ativos_300)

c_300 = cbind.data.frame(BBAS3, ITSA4)
VARselect(c_300, lag.max = 10, type ="trend")$selection
co_300 <- ca.jo(c_300, type = "eigen", ecdet = "const", spec ="longrun", K = 8)
co_300@cval
co_300@teststat[1]
co_300@teststat[2]

attach(ativos_200)
#retorna a função attach
detach(ativos_200)

c_200 = cbind.data.frame(BBAS3, ITSA4)
VARselect(c_200, lag.max = 10, type ="trend")$selection
co_200 <- ca.jo(c_200, type = "eigen", ecdet = "const", spec ="longrun", K = 8)
co_200@cval
co_200@teststat[1]
co_200@teststat[2]

attach(ativos_100)
#retorna a função attach
detach(ativos_100)

c_100 = cbind.data.frame(BBAS3, ITSA4)
VARselect(c_300, lag.max = 10, type ="trend")$selection
co_100 <- ca.jo(c_100, type = "eigen", ecdet = "const", spec ="longrun", K = 8)
co_100@cval
co_100@teststat[1]
co_100@teststat[2]

attach(ativos_50)
#retorna a função attach
detach(ativos_50)

c_300 = cbind.data.frame(BBAS3, ITSA4)
VARselect(c_50, lag.max = 10, type ="trend")$selection
co_50 <- ca.jo(c_50, type = "eigen", ecdet = "const", spec ="longrun", K = 8)
co_50@cval
co_35@teststat[1]
co_50@teststat[2]

#########################################################################################
?geom_smooth()
?confint() # intervalo de confiança.
?diff() #transforma os dados na primeira diferença

#### Caso os ativos não tenho o mesmo número de linhas, utiliza-se do comando "merge" 

#Códigos de datas
a <- seq(from=as.Date("2013-03"), to=as.Date("2019-01"), by="month")

d <- bizseq('2017-01-10', '2019-02-27', cal = "Brazil/ANBIMA")

##########ESTUDO DE LAPPLY PARA UNIT ROT TEST
data2 = data.frame(matrix(rnorm(30), nrow=10))
f1 <- function(x) ur.df(x, type = "none", lags = 3, selectlags = "AIC")
lapply(data2,f2)
class(data2)
