# Carregar pacotes
library(tidyverse)
library(readxl)
library(writexl)
library(stringr)
library(dplyr)

# Definir o caminho da pasta
caminho_pasta <- "C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/Lojas/Salvador"

# Listar todos os arquivos .xlsx na pasta
arquivos <- list.files(caminho_pasta, pattern = "\\.xlsx$", full.names = TRUE)

# Ler todas as planilhas e combiná-las em um único dataframe
dados <- arquivos %>%
  map_df(~read_xlsx(.x) %>% mutate(across(everything(), as.character)))

# Função para extrair informações de endereço
extrair_endereco <- function(endereco) {
  rua_aven <- str_extract(endereco, "^[^-]+")
  bairro <- str_extract(endereco, "(?<=-\\s)[^,]+")
  cidade <- str_extract(endereco, "(?<=,\\s)[^-]+")
  
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
  
  tibble(
    `Rua/Aven` = str_trim(rua_aven),
    Bairro = str_trim(bairro),
    Cidade = str_trim(cidade)
  )
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

# Aplicar a função e criar as novas colunas
dados <- dados %>%
  bind_cols(map_dfr(dados$Endereço, extrair_endereco))

# Verificar se as colunas "Bairro" e "Cidade" existem
if (!"Bairro" %in% colnames(dados)) {
  stop("A coluna 'Bairro' não foi criada corretamente.")
}
if (!"Cidade" %in% colnames(dados)) {
  stop("A coluna 'Cidade' não foi criada corretamente.")
}

# Aplicar a função transformar_para_NA às colunas Bairro e Cidade
dados <- dados %>%
  rowwise() %>%
  mutate(
    Bairro = transformar_para_NA(Bairro),
    Cidade = transformar_para_NA(Cidade)
  )

# Verificar e corrigir NAs usando o Plus Code
dados <- dados %>%
  rowwise() %>%
  mutate(
    Bairro = ifelse(is.na(Bairro) || Bairro == "NA", extrair_bairro_cidade(`Plus_Code`)$bairro, Bairro),
    Cidade = ifelse(is.na(Cidade) || Cidade == "NA", extrair_bairro_cidade(`Plus_Code`)$cidade, Cidade)
  )

# Salvar o resultado em uma nova planilha
write_xlsx(dados, "C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/Lojas/Salvador/Coleta_Unida_Salvador/Enderecos_Processados2.xlsx")

# Função para modificar os bairros
modificar_bairro <- function(bairro) {
  if (is.na(bairro) || trimws(bairro) == "" || grepl("-", bairro) || grepl("\\d", bairro)) {
    return("NA")
  }
  return(bairro)
}

# Função para transformar cidade
transformar_cidade <- function(cidade) {
  # Verificar se a cidade é vazia ou não é uma string
  if (is.null(cidade) || cidade == "" || !is.character(cidade)) {
    return("NA")
  }
  
  # Verificar se a cidade contém "S/N", números, hífen ou pontuação
  if (grepl("S/N|\\d|[-]|[[:punct:]]", cidade)) {
    return("NA")
  }
  
  return(cidade)
}

# Carregar a planilha
df_lojas <- read_excel("C:\\Users\\leona\\Github\\Web-Scraping-Maps-Rstudio\\Lojas\\Salvador\\Coleta_Unida_Salvador\\Enderecos_Processados2.xlsx")
# Aplicar a função à coluna "Bairro"
df_lojas$Bairro <- sapply(df_lojas$Bairro, modificar_bairro)

# Salvar a planilha modificada
write_xlsx(df_lojas, "C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/Lojas/Salvador/Coleta_Unida_Salvador/Enderecos_Processados2.xlsx")         

