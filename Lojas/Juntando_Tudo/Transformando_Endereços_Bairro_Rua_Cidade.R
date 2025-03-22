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

# Filtrar as lojas onde algum desses campos está ausente (NA)
lojas_com_valores_faltantes <- dados %>%
  filter(is.na(Bairro) | Bairro == "NA" |
           is.na(`Rua/Aven`) | `Rua/Aven` == "NA" |
           is.na(Cidade) | Cidade == "NA")

# Exibir as lojas com valores ausentes
print(lojas_com_valores_faltantes)

# Função para extrair informações de endereço
extrair_endereco <- function(Endereço) {
  rua_aven <- str_extract(Endereço, "^[^-]+")
  bairro <- str_extract(Endereço, "(?<=-\\s)[^,]+")
  cidade <- str_extract(Endereço, "(?<=,\\s)[^-]+")
  
  # Verificar se bairro ou cidade contêm números, hífens, texto único 'BA' ou apenas uma letra
  if (!is.na(bairro) && (str_detect(bairro, "[0-9,-]") || str_detect(bairro, "^BA$") || str_length(str_trim(bairro)) == 1)) {
    bairro <- "NA"
  }
  
  if (!is.na(cidade) && (str_detect(cidade, "[0-9,-]") || str_length(str_trim(cidade)) == 1)) {
    cidade <- "NA"
  }
  
  # Verificar se o bairro está vazio e substituir por "NA"
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
  # Regex para capturar o bairro e a cidade
  padrao <- "[A-Z0-9+]+\\s([^,]+),\\s([^-]+)\\s-\\s[A-Z]{2}"
  
  # Extrair bairro e cidade
  bairro <- str_match(plus_code, padrao)[,2]
  cidade <- str_match(plus_code, padrao)[,3]
  
  # Retornar como uma lista
  return(list(bairro = bairro, cidade = cidade))
}

# Função para transformar em "NA" se começar com termos específicos
transformar_para_NA <- function(texto) {
  # Verificar se o valor é NA
  if (is.na(texto)) {
    return("NA")
  }
  
  # Lista de termos que devem ser transformados em "NA"
  termos_na <- c("^r\\.", "^rua", "^av\\.", "^aven\\.", "^avenida", "^s/n", "^sn", "^na")
  
  # Verificar se o texto começa com algum dos termos (ignorando maiúsculas/minúsculas)
  if (any(str_detect(str_to_lower(texto), termos_na))) {
    return("NA")
  }
  
  return(texto)
}

# Aplicar a extração de endereço diretamente nas colunas já existentes
dados <- dados %>%
  rowwise() %>%
  mutate(
    endereco_extraido = list(extrair_endereco(Endereço)),
    `Rua/Aven` = endereco_extraido$rua_aven,
    Bairro = endereco_extraido$bairro,
    Cidade = endereco_extraido$cidade
  ) %>%
  select(-endereco_extraido)  # Remover a lista de endereço extraído após a alteração

# Aplicar a função transformar_para_NA nas colunas Bairro e Cidade
dados <- dados %>%
  mutate(
    Bairro = transformar_para_NA(Bairro),
    Cidade = transformar_para_NA(Cidade)
  )

# Verificar e corrigir NAs usando o Plus Code, alterando diretamente as colunas existentes
dados <- dados %>%
  rowwise() %>%
  mutate(
    Bairro = ifelse(is.na(Bairro) || Bairro == "NA", extrair_bairro_cidade(Plus_Code)$bairro, Bairro),
    Cidade = ifelse(is.na(Cidade) || Cidade == "NA", extrair_bairro_cidade(Plus_Code)$cidade, Cidade)
  )

# Salvar trabalho
write_xlsx(dados, "C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/Lojas.xlsx")

