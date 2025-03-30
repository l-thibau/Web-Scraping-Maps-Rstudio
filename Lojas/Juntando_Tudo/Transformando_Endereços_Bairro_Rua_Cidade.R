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
  partes <- str_match(plus_code, "[A-Z0-9+]+\\s([^,]+),\\s([^\\-]+)")
  n_partes_validas <- ifelse(!is.na(partes[,2]) & !is.na(partes[,3]), 2, 0)
  
  if (n_partes_validas == 2) {
    # Testar se há traço após o segundo grupo (indicando estado)
    estado_presente <- str_detect(plus_code, "-\\s*[A-Z]{2}$")
    if (estado_presente) {
      # Plus code completo: bairro + cidade + estado
      bairro <- str_trim(partes[,2])
      cidade <- str_trim(partes[,3])
    } else {
      # Plus code incompleto: cidade + estado → ignorar o bairro
      bairro <- "NA"
      cidade <- str_trim(partes[,2])
    }
  } else {
    bairro <- "NA"
    cidade <- "NA"
  }
  
  return(list(bairro = bairro, cidade = cidade))
}

# Fallback: extrair cidade de Plus Code simples
extrair_cidade_de_pluscode_simples <- function(plus_code) {
  cidade <- str_extract(plus_code, "(?<=^[A-Z0-9+]{7}\\s)[^,]+")
  cidade <- str_trim(cidade)
  # Verifica se a cidade extraída contém apenas uma palavra (possivelmente um estado), ignora nesses casos
  if (is.na(cidade) || str_detect(cidade, "^BA$|^SP$|^RJ$|^MG$|^\\w{1,2}$")) {
    return("NA")
  }
  return(cidade)
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

# PROCESSAMENTO PRINCIPAL (corrigido)
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
    bairro_cidade = list(extrair_bairro_cidade(Plus_Code)),
    Cidade = ifelse(is.na(Cidade) | Cidade == "" | Cidade == "NA",
                    bairro_cidade$cidade, Cidade),
    Bairro = ifelse((is.na(Bairro) | Bairro == "" | Bairro == "NA") & 
                      bairro_cidade$bairro != "NA",
                    bairro_cidade$bairro, Bairro)
  ) %>%
  ungroup() %>%
  select(-bairro_cidade) %>%
  rowwise() %>%
  mutate(
    Cidade = ifelse(is.na(Cidade) | Cidade == "" | Cidade == "NA",
                    extrair_cidade_de_pluscode_simples(Plus_Code),
                    Cidade)
  ) %>%
  ungroup()



# Salvar resultado final
write_xlsx(dados, "C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/Lojas.xlsx")
