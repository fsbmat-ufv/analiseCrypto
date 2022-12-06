rm(list = ls())
cat("\014")
library(crypto2)
library(tidyverse)
cryptos_list <- c("BTC","ADA","ERTHA")
cryptos <- crypto_list() %>% 
  filter(symbol %in% cryptos_list)
crypto_prices <- crypto_history(cryptos, start_date="20180101", end_date="20221203")
crypto_prices <- crypto_prices %>% 
  mutate(date = as.Date(strftime(timestamp, format = "%Y-%m-%d"))) 

cryptos_notbtc <- crypto_prices %>% 
  filter(symbol != "BTC")

btc <- crypto_prices %>% 
  filter(symbol == "BTC") %>% 
  select(date, close) %>% 
  rename("closebtc" = "close")

cryptos_notbtc <- left_join(cryptos_notbtc, btc, by = "date")

df <- cryptos_notbtc %>% 
  group_by(symbol) %>% 
  summarize(r = cor(close, closebtc), sample = n()) %>% 
  arrange(r)

df$symbol <- factor(df$symbol, levels = df$symbol)

df %>% 
  ggplot() +
  geom_bar(aes(x = symbol, y = r, fill = symbol), stat = "identity", alpha = 0.8) +
  scale_fill_manual(values = c("#FF007A", "#ff00af", "#8dc351", "black", "#d3d3d3", "#4D7DBF", "#215CAF")) +
  scale_x_discrete(labels = labels) +
  labs(x = "", y = "") +
  theme(legend.position = "none")

dfi_btc <- right_join(crypto_prices[crypto_prices$symbol == "BTC", ], crypto_prices[crypto_prices$symbol == "DFI", ], by = "date")

dfi_btc %>%
  group_by(date) %>% 
  summarize(btc = mean(close.x, na.rm = T), dfi = mean(close.y, na.rm = T)) %>% 
  mutate(ratio = btc / dfi) %>% 
  ggplot() +
  geom_point(aes(x = date, y = ratio))
