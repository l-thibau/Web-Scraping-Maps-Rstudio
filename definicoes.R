# Bibliotecas -------------------------------------------------------------

library(RSelenium)
library(tidyverse)
library(writexl)

# Tibbles ----------------------------------------------------

informacoes <- tibble(
  Loja = character(),
  Categoria = character(),
  Endereço = character(),
  Plus_Code = character(),
  Site = character(),
  Celular = factor(),
  Estrelas = character(),
  Loc = character()
)

dados_completos <- tibble(
  Loja = character(),
  Categoria = character(),
  Endereço = character(),
  Plus_Code = character(),
  Site = character(),
  Celular = character(),
  Estrelas = character()#, Loc = character()?
)

# Objetos de suporte ------------------------------------------------------

# XPath base
base_xpath <- "/html/body/div[1]/div[3]/div[8]/div[9]/div/div/div[1]/div[2]/div/div[1]/div/div/div[1]/div[1]/div["
final_xpath <- "]/div/a"

# XPath inicial
xpath_num <- 3L

# Colunas que devem ser consideradas para identificar duplicatas

colunas_para_verificar <- c("Loja", "Categoria", "Endereço", "Plus_Code", "Site", "Celular", "Estrelas")

Sys.sleep(3)

# Iniciando RSelenium -----------------------------------------------------

remDr <- remoteDriver(
  remoteServerAddr = "localhost", # Endereço do Docker
  port = 4445,                   # Porta mapeada
  browserName = "firefox"        # Navegador a ser usado
)


remDr$open()
