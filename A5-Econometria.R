# Econometria Avançada Aula 5

install.packages("readxl") #instala o pacote readxl
library(readxl) #roda o pacote readxl
install.packages("urca") #instala o pacote urca
library(urca) #roda o pacote urca
IPCA <- read_excel("G:/USJT/Econometria/A5/IPCA.xls", col_types = c("date","numeric")) #carrega o arquivo excel IPCA
Inflação <- ts(IPCA$IPCA,start = 2008-01, frequency = 12) #cria uma série temporal Inflação
View(Inflação)
write.csv(Inflação,file = "Inflação.csv")

# Teste de Estacionariedade

TesteDF <- summary(ur.df(Inflação, type="none", lags=0))
TesteDF

# Gráfico de Autocorrelação

acf(IPCA$IPCA, main="Inflação Mensal") #FAC função de auto correlação
pacf(IPCA$IPCA, main="Inflação Mensal") #FACP função de auto correlação parcial

# Modelo Autoregressivo

AR1 <- arima(Inflação,order = c(1,0,0)) #modelo AR de ordem 1
AR1
AR2 <- arima(Inflação, order=c(2,0,0)) #modelo AR de ordem 2
AR2