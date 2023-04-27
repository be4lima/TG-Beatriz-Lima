# Packages
packages <- c("BETS","urca","TSA","forecast","lmtest","nortest","FinTS","xlsx", "MLmetrics", "TTR")

# Checando se os pacotes já estão instalados
is.installed <- function(mypkg){
  is.element(mypkg, installed.packages()[,1])
}
for(packages in packages){
  if(!is.installed(packages)){
    install.packages((packages), repos = "http://cran.us.r-project.org")
  }
}
library(dplyr)
library(forecast)
library(astsa)
library(MLmetrics)
library(TTR)

# Carregando base de dados
# Fonte: https://www.kaggle.com/datasets/kvnxls/co2-emissions-dataset-1750-2020

emissions <- read.csv("Data/co2data.csv", sep = ",", dec = ".")
View(emissions)
str(emissions)

# Filtrando somente as emissões do Brasil
emissions_br <- emissions %>%
  filter(country == 'Brazil')

View(emissions_br)

# Separando somente as informações importantes para a análise
# Para a análise, precisamos somente da coluna de ano e de concentração de CO2

emissions_br <- emissions_br[c(3,4)]

# Mudando o nome das linhas para os anos da série temporal
year <- emissions_br[,1]

row.names(emissions_br) <- year

# Criando tabela somente com as concentrações de CO2 como coluna
emissions_br <- emissions_br[c(2)]

# EDA
str(emissions_br)
head(emissions_br)
summary(emissions_br)

# Criando a série temporal (ts)
emissions_br.ts <- ts(emissions_br$co2,start=1901, end=2020, frequency=1)

# Análise gráfica

# Tendência de aumento
plot.ts(emissions_br.ts, ylab="Emissão de CO2", xlab="Anos")

# Comentários: Tendência de aumento clara, não aparenta possuir sazonalidade já que não existem padrões que se repetem ao longo dos anos

# Estatística de teste: Estacionariedade

adf.drift@teststat 
adf.drift@cval #valores tabulados por MacKinnon (1996)

#A partir do gráfico, é possível afirmar que a estatística teste (1,765614)
#é maior do que o valor máximo associado ao nível de confiança (-2,88).
#conclui-se que a série não é estacionária

#
 adf.test(emissions_br.ts, alternative="stationary", k=0)

# A série é estacionária! 

ts.plot(diff(emissions_br.ts, differences = 1))

# Com uma diferenciação é possível verificar que a série está estacionária na média. 
# Porém, a ST está crescendo ao longo do tempo e, dessa forma, sua variância não está constante. 
# Uma estratégia importante para tornar a variância constante, é aplicar log na série temporal.

# Aplicação do log para tornar a variância constante
ts.plot(diff(log(emissions_br.ts)))
library(tseries)

# Realizando uma diferenciação para tornar a série estacionária

# Conferindo se a série realmente é estacionária
# H0: não é estacionária
# H1: é estacionária

adf.test(diff(log(emissions_br.ts), differences = 1), alternative="stationary", k=0)

# Identificação
#FAC
BETS::corrgram(diff(log(emissions_br.ts)), lag.max = 36, style = "normal")
# Última observação significativa: 18

#FACP
BETS::corrgram(diff(log(emissions_br.ts)), type = "partial", lag.max = 36,  style = "normal")
# Última observação significativa: 18

# Estimação
library(forecast)
# arima(emissions_br.ts, order = c(18,18,1), method = "ML")
# Não está sendo possível devido ao número grande de p e q.