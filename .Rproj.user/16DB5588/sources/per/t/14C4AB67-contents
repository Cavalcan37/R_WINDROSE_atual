# PROBABILIDADES WINDROSE

# Pacote inicial
library(readxl)
library(ggthemes)
library(ggplot2)

# Carregamento dos Dados
dados <- read_excel("EVTEA WINDROSE_2024_13_Atualizado.xlsm", sheet = "RESULT")
dados
str(dados)

################################################################################
# PAYBACK 35 Anos ##############################################################
################################################################################
# Probabilidade PAYBACK
# Valor de Referência
valor_referencia <- 10

# Média e DP para VPL
media_valor <- mean(dados$PAYBACK)
media_valor

dp_valor <- sd(dados$PAYBACK)
dp_valor

# Cálculo da Probabilidade
probabilidade <- 1-pnorm(valor_referencia, mean = media_valor, sd = dp_valor)
probabilidade

# Gerar o gráfico ajustado
GPB <- ggplot(dados, aes(x = PAYBACK)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "blue", color = "black", alpha = 0.7)+
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  stat_function(
    fun = dnorm,
    args = list(mean = media_valor, sd = dp_valor),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = media_valor, sd = dp_valor),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$PAYBACK))
  ) +
  labs(
    x = "Payback (anos)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 15, y = 0.05,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = 5, y = 0.06,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()
GPB
