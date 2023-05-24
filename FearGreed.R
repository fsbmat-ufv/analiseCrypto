rm(list = ls())
cat("\014")
library("tidyverse")
library("httr")
library("lubridate")
library("gtable")

#install.packages("remotes")
#remotes::install_github("d3treeR/d3treeR")
#library("d3treeR")
library("data.table")
options(DT.options = list(scrollY="300px",
                          scrollX="300px", 
                          pageLength = 100, 
                          columnDefs = list(list(className = 'dt-center', 
                                                 targets = "_all"))))
Sys.time()
# Apenas a primeira vez
count_acessos <- 0
saveRDS(count_acessos, file = "./count_acessos.rds")

# Definindo tempo zero com 18h/horário de São Paulo:
t0 <- as_datetime(as.Date("2018-02-01") + dhours(21), tz = "America/Sao_Paulo")
#saveRDS(t0, file = "./time0.rds")
t0 <- readRDS(file = "./time0.rds")

# Pegando hora atual de acordo com horário de São Paulo:
t1 <- as_datetime(Sys.time(), tz = "America/Sao_Paulo")

# if (duration(int_length(interval(t0, t1))) >= duration(43200)) {
# # Baixando os dados de 12 em 12 horas de acordo com acessos a aplicação:
# download.file(url = "https://brasil.io/dataset/covid19/caso?format=csv", destfile = "./covid19.csv")
#   t0 <- t1
#   saveRDS(t1, file = "./time0.rds")
# }

if (duration(int_length(interval(t0, t1))) >= duration(43200)) {
  # Baixando os dados de 12 em 12 horas de acordo com acessos a aplicação:
  download.file(url = "https://api.alternative.me/fng/?limit=0&format=csv", destfile = "./dados.csv")
  t0 <- t1
  saveRDS(t1, file = "./time0.rds")
}

library(crypto2)

criptoTest <- "BNB"

cryptos_list <- c(criptoTest)
cryptos <- crypto_list() %>% 
  filter(symbol %in% cryptos_list)
crypto_prices <- crypto_history(cryptos, start_date="20111101", end_date="20230824")
crypto_prices <- crypto_prices %>% 
  mutate(date = as.Date(strftime(timestamp, format = "%Y-%m-%d"))) 

cryptos_notbtc <- crypto_prices %>% 
  filter(symbol != criptoTest)

cripto <- crypto_prices %>% 
  filter(symbol == criptoTest) %>% 
  select(symbol, date, close)


fear <- fread("dados.csv")
names(fear) <- c("data", "valor", "classificacao")
str(fear)
fear$data <- dmy(fear$data)
#cripto <- fread("cripto.csv")
minaate <- min(fear$data)
maxDate <- max(fear$data)-1
cripto <- cripto %>% filter(date>=minDate&date<=maxDate)
names(cripto) <- c("symbol", "data", "close")
df = merge(x=fear,y=cripto,by="data")
#data1 <- "2018-03-01"
#data2 <- "2019-06-01"
#df <- df %>% filter(data>=data1&data<=data2)
df$valor2 <- round(df$valor/max(df$valor), digits = 2)
df$close2 <- round(df$close/max(df$close), digits = 2)
df <- df %>% select(c(data, valor2, close2))
df_data_long <- melt(df, id="data")  
ggplot(df_data_long, aes(x = data, y = value, colour=variable)) +
  geom_line() +
  scale_x_date(date_labels= "%d-%b-%Y", breaks = "month")+theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
