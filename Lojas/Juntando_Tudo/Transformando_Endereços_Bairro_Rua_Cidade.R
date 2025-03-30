# Carregar pacotes
library(tidyverse)
library(readxl)
library(writexl)
library(stringr)
library(dplyr)

# Definir o caminho da pasta
caminho_pasta <- "C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/Lojas/Juntando_Tudo"

# Listar todos os arquivos .xlsx na pasta
arquivos <- list.files(caminho_pasta, pattern = "\\.xlsx$", full.names = TRUE)

# Ler todas as planilhas e combiná-las em um único dataframe
dados <- arquivos %>%
  map_df(~read_xlsx(.x) %>% mutate(across(everything(), as.character)))

# Função para extrair informações de endereço
extrair_endereco <- function(Endereço) {
  rua_aven <- str_extract(Endereço, "^[^-]+")
  bairro <- str_extract(Endereço, "(?<=-\\s)[^,]+")
  cidade <- str_extract(Endereço, "(?<=,\\s)[^-]+")
  
  if (!is.na(bairro) && (str_detect(bairro, "[0-9,-]") || str_detect(bairro, "^BA$") || str_length(str_trim(bairro)) == 1)) {
    bairro <- "NA"
  }
  
  if (!is.na(cidade) && (str_detect(cidade, "[0-9,-]") || str_length(str_trim(cidade)) == 1)) {
    cidade <- "NA"
  }
  
  if (is.na(bairro) || str_trim(bairro) == "") {
    bairro <- "NA"
  }
  
  return(list(
    rua_aven = str_trim(rua_aven),
    bairro = str_trim(bairro),
    cidade = str_trim(cidade)
  ))
}

# Função para extrair bairro e cidade do Plus Code
extrair_bairro_cidade <- function(plus_code) {
  padrao <- "[A-Z0-9+]+\\s([^,]+),\\s([^\\-]+)"
  match <- str_match(plus_code, padrao)
  bairro <- str_trim(match[, 2])
  cidade <- str_trim(match[, 3])
  return(list(bairro = bairro, cidade = cidade))
}

# Fallback: extrair cidade de Plus Code simples
extrair_cidade_de_pluscode_simples <- function(plus_code) {
  cidade <- str_extract(plus_code, "(?<=^[A-Z0-9+]{7}\\s)[^,]+")
  return(str_trim(cidade))
}

# VERSÃO VETORIZADA da função para transformar em "NA"
transformar_para_NA <- function(texto) {
  termos_na <- c("^r\\.", "^rua", "^av\\.", "^aven\\.", "^avenida", "^s/n", "^sn", "^na")
  texto_corrigido <- ifelse(
    is.na(texto) | str_trim(texto) == "" |
      str_detect(str_to_lower(texto), str_c(termos_na, collapse = "|")),
    "NA",
    texto
  )
  return(texto_corrigido)
}

# PROCESSAMENTO PRINCIPAL
dados <- dados %>%
  rowwise() %>%
  mutate(
    endereco_extraido = list(extrair_endereco(Endereço)),
    `Rua/Aven` = endereco_extraido$rua_aven,
    Bairro = endereco_extraido$bairro,
    Cidade = endereco_extraido$cidade
  ) %>%
  ungroup() %>%
  select(-endereco_extraido) %>%
  mutate(
    Bairro = transformar_para_NA(Bairro),
    Cidade = transformar_para_NA(Cidade)
  ) %>%
  rowwise() %>%
  mutate(
    Bairro = ifelse(is.na(Bairro) | Bairro == "" | Bairro == "NA",
                    extrair_bairro_cidade(Plus_Code)$bairro, Bairro),
    Cidade = ifelse(is.na(Cidade) | Cidade == "" | Cidade == "NA",
                    extrair_bairro_cidade(Plus_Code)$cidade, Cidade)
  ) %>%
  ungroup() %>%
  mutate(
    Cidade = ifelse(is.na(Cidade) | Cidade == "" | Cidade == "NA",
                    extrair_cidade_de_pluscode_simples(Plus_Code),
                    Cidade)
  )

# Salvar resultado final
write_xlsx(dados, "C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/Lojas.xlsx")

