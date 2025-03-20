# Instalar pacotes caso não estejam instalados
if (!require(readxl)) install.packages("readxl", dependencies = TRUE)
if (!require(dplyr)) install.packages("dplyr", dependencies = TRUE)
if (!require(sf)) install.packages("sf", dependencies = TRUE)
if (!require(leaflet)) install.packages("leaflet", dependencies = TRUE)

# Carregar pacotes
library(readxl)
library(dplyr)
library(sf)
library(leaflet)

# Definir caminho do arquivo
caminho <- "C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/Lojas/Juntando_Tudo"
arquivo <- file.path(caminho, "Unindo_coord.xlsx")

# Ler os dados
df <- read_excel(arquivo)

# Renomear colunas para garantir consistência
df <- df %>%
  rename(Latitude = latitude, Longitude = longitude, Loja = Loja)

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

