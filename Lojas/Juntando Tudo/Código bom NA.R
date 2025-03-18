#  carregar pacotes
library(tidyverse)
library(readxl)
library(writexl)
library(stringr)
library(dplyr)

# Definir o caminho da pasta
caminho_pasta <- "C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/Lojas/Juntando Tudo"

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

# Aplicar a função e criar as novas colunas
dados <- dados %>%
  bind_cols(map_dfr(dados$Endereço, extrair_endereco))

# Verificar e corrigir NAs usando o segundo código
dados <- dados %>%
  rowwise() %>%
  mutate(
    Bairro = ifelse(is.na(Bairro) || Bairro == "NA", extrair_bairro_cidade(Endereço)$bairro, Bairro),
    Cidade = ifelse(is.na(Cidade) || Cidade == "NA", extrair_bairro_cidade(Endereço)$cidade, Cidade)
  )

# Salvar o resultado em uma nova planilha
write_xlsx(dados, "C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/Lojas/Enderecos_Processados.xlsx")

transformar_bairro <- function(bairro) {
  # Verificar se o bairro é vazio ou não é uma string
  if (is.null(bairro) || bairro == "" || !is.character(bairro)) {
    return("NA")
  }
  
  # Verificar se o texto contém números, hífen, "BA" ou uma letra isolada
  if (grepl("\\d|[-]|\\bBA\\b|\\b\\w\\b", bairro)) {
    return("NA")
  }
  
  return(bairro)
}

# Teste
bairro <- "bairro"
texto_bairro <- "BA"
print(transformar_bairro(texto_bairro))  # Saída: "NA"


# Carregar pacotes necessários
library(readxl)
library(openxlsx)

# Carregar a planilha
df <- read_excel("C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/Lojas/Enderecos_Processados.xlsx")

# Função para modificar os bairros
modificar_bairro <- function(bairro) {
  if (is.na(bairro) || trimws(bairro) == "" || grepl("-", bairro) || grepl("\\d", bairro)) {
    return("NA")
  }
  return(bairro)
}

df_lojas <- read_excel("C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/Lojas/Enderecos_Processados.xlsx")

# Aplicar a função à coluna "Bairro"
df_lojas$Bairro <- sapply(df_lojas$Bairro, modificar_bairro)

# Salvar a planilha modificada
write.xlsx(df_lojas, "C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/Lojas/Enderecos_Processados.xlsx", rowNames = FALSE)

