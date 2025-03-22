# Carregar pacotes
library(httr)
library(jsonlite)
library(readxl)
library(dplyr)
library(writexl)
library(tidyr)

# Definir API Key da HERE Maps
api_key <- "8__EX014cinJzotz2TuJPXeoWKB9v_ZnPQjiR-3lJvA"

# Função para geocodificação com mensagens de progresso
geocode_here <- function(endereco, idx) {
  if (is.na(endereco) || endereco == "") return(NA)  # Evita chamadas desnecessárias
  
  # Mensagem de progresso
  cat(paste("Processando endereço", idx, ":", endereco, "\n"))
  
  url <- paste0("https://geocode.search.hereapi.com/v1/geocode?q=", 
                URLencode(endereco), "&apiKey=", api_key)
  
  resposta <- GET(url)
  
  if (status_code(resposta) == 200) {
    dados <- content(resposta, as = "parsed", type = "application/json")
    
    if (length(dados$items) > 0) {
      return(dados$items[[1]]$position)  # Retorna as coordenadas completas
    }
  }
  
  return(NA)
}

# Carregar os dados do arquivo
dados <- read_xlsx(arquivo_xlsx)

# Aplicar a geocodificação SOMENTE onde a coluna "Loc" está vazia com mensagens de progresso
dados_geo <- dados %>%
  rowwise() %>%  # Processar linha por linha
  mutate(coord = ifelse(is.na(Loc), 
                        list(geocode_here(`Endereço`, row_number())),  # Adiciona índice para progresso
                        list(Loc))) %>%
  unnest_wider(coord, names_sep = "_") %>%
  rename(latitude = coord_lat, longitude = coord_lng) %>%  # Renomeando as colunas corretamente
  ungroup()  # Remover o agrupamento após o mutate

# Identificar falhas após a primeira tentativa
dados_na <- dados_geo %>%
  filter(is.na(latitude) | is.na(longitude))

# Função para excluir a coluna coord_1
remover_coord_1 <- function(dados) {
  dados <- dados %>%
    select(-coord_1)  # Remove a coluna coord_1
  return(dados)
}

# Aplicar a função no DataFrame 'dados_geo'
dados_geo <- remover_coord_1(dados_geo)

# Função para combinar latitude e longitude na coluna Loc, apenas se estiver vazia
combinar_lat_lon_em_loc <- function(dados) {
  dados <- dados %>%
    mutate(Loc = ifelse(is.na(Loc) | Loc == "", paste(latitude, longitude, sep = ", "), Loc))  # Só preenche se Loc estiver vazio
  return(dados)
}

# Aplicar a função no DataFrame 'dados_geo'
dados_geo <- combinar_lat_lon_em_loc(dados_geo)


# Remover as colunas latitude e longitude
dados_geo <- dados_geo %>%
  select(-latitude, -longitude)








# **Segunda tentativa de geocodificação (Apenas para falhas)**
if (nrow(dados_na) > 0) {
  
  # Nova API Key da HERE Maps (Segunda tentativa)
  api_key <- "gwuKJLfxYYmARME7OlC7rIFjJgweQhI_1nFB8QS3DAs"
  
  # Função para tentar geocodificar novamente com mensagens de progresso
  geocode_here_retry <- function(endereco, idx) {
    if (is.na(endereco) || endereco == "") return(NA)  # Evita chamadas desnecessárias
    
    # Mensagem de progresso
    cat(paste("Tentando novamente o endereço", idx, ":", endereco, "\n"))
    
    url <- paste0("https://geocode.search.hereapi.com/v1/geocode?q=", 
                  URLencode(endereco), "&apiKey=", api_key)
    
    resposta <- GET(url)
    
    if (status_code(resposta) == 200) {
      dados <- content(resposta, as = "parsed", type = "application/json")
      
      if (length(dados$items) > 0) {
        return(dados$items[[1]]$position)  # Retorna as coordenadas completas
      }
    }
    
    return(NA)
  }
  
  # Aplicar geocodificação somente nas falhas com índice de progresso
  dados_na_corrigidos <- dados_na %>%
    rowwise() %>%
    mutate(coord = list(geocode_here_retry(`Endereço`, row_number()))) %>%
    unnest_wider(coord, names_sep = "_") %>%
    rename(latitude_corrigido = coord_lat, longitude_corrigido = coord_lng)
  
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
cat("Total de endereços no dataset original:", nrow(dados), "\n")
cat("Endereços sem coordenadas na primeira tentativa:", nrow(dados_na), "\n")
cat("Endereços corrigidos na segunda tentativa:", sum(!is.na(dados_geo$latitude)), "\n")
cat("Total de endereços geocodificados com sucesso:", sum(!is.na(dados_geo$latitude)), "\n")

# Salvar o resultado atualizado
write_xlsx(dados_geo, "C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/Lojas/Juntando_Tudo/Atualizando_here_coord.xlsx")
