# Carregar pacotes
library(httr)
library(jsonlite)
library(readxl)
library(dplyr)
library(writexl)
library(tidyr)

# Definir API Key da HERE Maps (Primeira tentativa)
api_key <- "8__EX014cinJzotz2TuJPXeoWKB9v_ZnPQjiR-3lJvA"

# Função para geocodificação usando a API da HERE Maps
geocode_here <- function(endereco) {
  if (is.na(endereco) || endereco == "") return(c(NA, NA))  # Evita chamadas desnecessárias
  
  url <- paste0("https://geocode.search.hereapi.com/v1/geocode?q=", 
                URLencode(endereco), "&apiKey=", api_key)
  
  resposta <- GET(url)
  
  if (status_code(resposta) == 200) {
    dados <- content(resposta, as = "parsed", type = "application/json")
    
    if (length(dados$items) > 0) {
      latitude <- dados$items[[1]]$position$lat
      longitude <- dados$items[[1]]$position$lng
      return(c(latitude, longitude))
    }
  }
  
  return(c(NA, NA))
}

# Caminho do arquivo
arquivo_xlsx <- "C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/Lojas/Juntando Tudo/Dados_Combinados_Unicos_Feira_SSA.xlsx"

# Leia o arquivo
dados <- read_excel(arquivo_xlsx)

# **Aplicar a geocodificação SOMENTE onde latitude e longitude são NA**
dados_geo <- dados %>%
  mutate(coord = ifelse(is.na(latitude) | is.na(longitude),
                        list(geocode_here(`Endereço`)), 
                        list(c(latitude, longitude)))) %>%
  unnest_wider(coord, names_sep = "_") %>%
  rename(latitude = coord_1, longitude = coord_2)

# Identificar falhas após a primeira tentativa
dados_na <- dados_geo %>%
  filter(is.na(latitude) | is.na(longitude))

# **Segunda tentativa de geocodificação (Apenas para falhas)**
if (nrow(dados_na) > 0) {
  
  # Nova API Key da HERE Maps (Segunda tentativa)
  api_key <- "gwuKJLfxYYmARME7OlC7rIFjJgweQhI_1nFB8QS3DAs"
  
  # Função para tentar geocodificar novamente
  geocode_here_retry <- function(endereco) {
    if (is.na(endereco) || endereco == "") return(c(NA, NA))  # Evita chamadas desnecessárias
    
    url <- paste0("https://geocode.search.hereapi.com/v1/geocode?q=", 
                  URLencode(endereco), "&apiKey=", api_key)
    
    resposta <- GET(url)
    
    if (status_code(resposta) == 200) {
      dados <- content(resposta, as = "parsed", type = "application/json")
      
      if (length(dados$items) > 0) {
        latitude <- dados$items[[1]]$position$lat
        longitude <- dados$items[[1]]$position$lng
        return(c(latitude, longitude))
      }
    }
    
    return(c(NA, NA))
  }
  
  # Aplicar geocodificação somente nas falhas
  dados_na_corrigidos <- dados_na %>%
    rowwise() %>%
    mutate(coord = list(geocode_here_retry(`Endereço`))) %>%
    unnest_wider(coord, names_sep = "_") %>%
    rename(latitude_corrigido = coord_1, longitude_corrigido = coord_2)
  
  # **Corrigir a fusão dos dados para evitar colunas duplicadas**
  dados_geo <- dados_geo %>%
    left_join(dados_na_corrigidos %>% select(Endereço, latitude_corrigido, longitude_corrigido),
              by = "Endereço") %>%
    mutate(
      latitude = ifelse(is.na(latitude), latitude_corrigido, latitude),
      longitude = ifelse(is.na(longitude), longitude_corrigido, longitude)
    ) %>%
    select(-latitude_corrigido, -longitude_corrigido)  # Remover colunas extras
}

# Salvar os dados finais em um novo arquivo Excel
arquivo_saida <- "C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/Lojas/Juntando Tudo/Dados_Geocodificados_Final.xlsx"
write_xlsx(dados_geo, arquivo_saida)

# Exibir resumo do processo
print(paste("Total de endereços no dataset original:", nrow(dados)))
print(paste("Endereços sem coordenadas na primeira tentativa:", nrow(dados_na)))
print(paste("Endereços corrigidos na segunda tentativa:", sum(!is.na(dados_geo$latitude))))
print(paste("Total de endereços geocodificados com sucesso:", sum(!is.na(dados_geo$latitude))))

# Salvar o resultado atualizado
write_xlsx(dados_geo, "C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/Lojas/Juntando Tudo/Atualizando_here_coord.xlsx")
