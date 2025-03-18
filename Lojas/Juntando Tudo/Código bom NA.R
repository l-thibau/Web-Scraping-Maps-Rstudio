# Instalar e carregar pacotes
library(tidyverse)
library(readxl)
library(writexl)

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
  
  # Verificar se bairro ou cidade contêm números, hífens ou vírgulas
  if (!is.na(bairro) && str_detect(bairro, "[0-9,-]")) {
    bairro <- NA
  }
  
  if (!is.na(cidade) && str_detect(cidade, "[0-9,-]")) {
    cidade <- NA
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

# Salvar o resultado em uma nova planilha
write_xlsx(dados, "C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/Lojas/Juntando Tudo/Enderecos_Processados.xlsx")

