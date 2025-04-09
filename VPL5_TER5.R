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
# Até 5 Anos ###################################################################
################################################################################
# Probabilidade VPL5
# Valor de Referência
valor_referencia <- 0

# Média e DP para VPL5
media_valor <- mean(dados$VPL5)
media_valor

dp_valor <- sd(dados$VPL5)
dp_valor

# Cálculo da Probabilidade
probabilidade <- pnorm(valor_referencia, mean = media_valor, sd = dp_valor)
probabilidade

# Gerar o gráfico ajustado
GVPL5 <- ggplot(dados, aes(x = VPL5)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "blue", color = "black", alpha = 0.7)+
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",",
                                                    scale = 1e-3)) +
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
    xlim = c(valor_referencia, min(dados$VPL5))
  ) +
  labs(
    x = "VPL até 5º Ano (mil R$)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = -3000000, y = 5e-8,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = 10000000, y = 1.2e-7,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()
GVPL5

# Probabilidade TER5 ###########################################################
# Valor de Referência
valor_referencia <- 0

# Média e DP para VPL5
media_valor <- mean(dados$TER5)
media_valor

dp_valor <- sd(dados$TER)
dp_valor

# Cálculo da Probabilidade
probabilidade <- pnorm(valor_referencia, mean = media_valor, sd = dp_valor)
probabilidade

# Gerar o gráfico ajustado
GTER5 <- ggplot(dados, aes(x = TER5)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "blue", color = "black", alpha = 0.7)+
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::percent_format(big.mark = ".", decimal.mark = ",",
                                                     accuracy = 0.01)) +
  stat_function(
    fun = dnorm,
    args = list(mean = media_valor, sd = dp_valor),
    color = "red", linetype = "solid", size = 1
  ) +
  labs(
    x = "TER até 5º Ano (% a.a.)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 0.08, y = 20,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_gray()
GTER5

# Probabilidade TER5 (com 95% IC)###############################################
# Calcular os limites do intervalo de 95%
limite_inferior <- media_valor - 1.96 * dp_valor
limite_superior <- media_valor + 1.96 * dp_valor

GTER5_95 <- ggplot(dados, aes(x = TER5)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "blue", color = "black", alpha = 0.7) +
  # Área do intervalo de 95%
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = media_valor, sd = dp_valor),
    fill = "red", alpha = 0.3,
    xlim = c(limite_inferior, limite_superior)
  ) +
  # Linhas verticais (média e limites)
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  geom_vline(xintercept = c(limite_inferior, limite_superior), linetype = "dotted", color = "darkred", size = 0.7) +
  # Curva normal
  stat_function(
    fun = dnorm,
    args = list(mean = media_valor, sd = dp_valor),
    color = "red", linetype = "solid", size = 1
  ) +
  # Rótulos dos limites no eixo x
  annotate("text", 
           x = limite_inferior, 
           y = 10, 
           label = paste0(round(limite_inferior * 100, 2), "%"),  # Formata como %
           color = "darkred", vjust = 2, size = 3.5) +
  annotate("text", 
           x = limite_superior, 
           y = 10, 
           label = paste0(round(limite_superior * 100, 2), "%"),
           color = "darkred", vjust = 2, size = 3.5) +
  # Formatação do eixo x
  scale_x_continuous(
    labels = scales::percent_format(big.mark = ".", decimal.mark = ",", accuracy = 0.01)
  ) +
  labs(
    x = "TER até 5º Ano (% a.a.)",
    y = "Densidade"
  ) +
  # Anotação da média
  annotate(
    "text", x = media_valor, y = 20,
    label = paste0("Média = ", round(media_valor * 100, 2), "%"),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_gray()
GTER5_95

library(patchwork)
library(magrittr)

GVPL5 + GTER5_95



