if (!require("tidyverse")) install.packages("tidyverse")
if (!require("rugarch")) install.packages("rugarch")
library(tidyverse)
library(rugarch)
library(ggplot2)
df <- read_csv("exchange_rate.csv") %>%
  mutate(date = as.Date(date))
df <- df %>%
  arrange(date) %>%
  mutate(log_return = log(rate/lag(rate)))
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
  distribution.model = "norm"
)
fit <- ugarchfit(spec = spec, data = na.omit(df$log_return))
vol_df <- tibble(
  date = df$date[-1],
  log_return = df$log_return[-1],
  sigma = as.numeric(sigma(fit))
)
# Gráfico de retornos e volatilidade
ggplot(vol_df, aes(x = date)) +
  geom_line(aes(y = log_return), color = "blue", alpha = 0.5) +
  geom_line(aes(y = sigma), color = "red") +
  labs(title = "Retornos e Volatilidade GARCH",
       y = "Valor",
       caption = "Azul: retorno | Vermelho: volatilidade") +
  theme_minimal()
