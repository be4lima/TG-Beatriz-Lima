# Packages
packages <- c("BETS","urca","TSA","forecast","lmtest","normtest","FinTS","xlsx")

# Checando se os pacotes já estão instalados
is.installed <- function(mypkg){
  is.element(mypkg, installed.packages()[,1])
}
for(packages in packages){
  if(!is.installed(packages)){
    install.packages((packages), repos = "http://cran.us.r-project.org")
  }
}

# Carregando base de dados
# Fonte: https://www.kaggle.com/datasets/kvnxls/co2-emissions-dataset-1750-2020
emissions <- read.csv("Data/co2data.csv", sep = ",", dec = ".")
View(emissions)

# Separando somente as informações importantes para a análise
analise_co2 <- emissions[]
ts.plot(emissions, ylab="Emissão de GEEs", xlab="Anos")