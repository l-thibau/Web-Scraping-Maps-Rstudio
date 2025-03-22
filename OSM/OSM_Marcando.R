# Carregar pacotes
library(readxl)
library(osrm)
library(dplyr)
library(sf)
library(leaflet)
library(writexl)
library(TSP)
library(stringr)
library(tidyr)

# Definir caminho do arquivo
caminho <- "C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/OSM/Book1.xlsx"
arquivo <- caminho

# Ler o arquivo Excel
cat("Lendo o arquivo Excel...\n")
df <- read_excel(caminho)

# Criar colunas "latitude" e "longitude" a partir da coluna "Loc"
df <- df %>%
  separate(Loc, into = c("latitude", "longitude"), sep = ", ", convert = TRUE)

# Renomear colunas
df <- df %>%
  rename(Latitude = latitude, Longitude = longitude, Loja = Loja)

# Remover pontos com coordenadas NA
cat("Removendo pontos com coordenadas NA...\n")
df <- df %>% filter(!is.na(Latitude) & !is.na(Longitude))

# Verificar se algum ponto foi removido
num_removidos <- nrow(read_excel(arquivo)) - nrow(df)
if (num_removidos > 0) {
  cat("Atenção: ", num_removidos, " pontos com NA foram removidos da rota.\n")
}

# Converter Latitude e Longitude para números (evita erros de coerção)
df <- df %>%
  mutate(
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude)
  ) %>%
  filter(!is.na(Latitude) & !is.na(Longitude))  # Remover pontos sem coordenadas

# Criar objeto spatial features (sf)
df_sf <- st_as_sf(df, coords = c("Longitude", "Latitude"), crs = 4326)

# Extrair coordenadas separadamente para uso no popup
df_sf$lon <- st_coordinates(df_sf)[,1]
df_sf$lat <- st_coordinates(df_sf)[,2]

# Criar mapa interativo apenas com os pontos
mapa <- leaflet(df_sf) %>%
  addTiles() %>%
  addMarkers(
    ~lon, 
    ~lat, 
    popup = ~paste0(Loja, "<br>", lat, ", ", lon),  # Agora usa as colunas extraídas
    label = ~Loja
  )

# Exibir mapa
mapa



