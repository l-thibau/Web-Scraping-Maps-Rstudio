# Instalar pacotes caso não estejam instalados
if (!require(readxl)) install.packages("readxl", dependencies = TRUE)
if (!require(osrm)) install.packages("osrm", dependencies = TRUE)
if (!require(dplyr)) install.packages("dplyr", dependencies = TRUE)
if (!require(sf)) install.packages("sf", dependencies = TRUE)
if (!require(leaflet)) install.packages("leaflet", dependencies = TRUE)
if (!require(writexl)) install.packages("writexl", dependencies = TRUE)
if (!require(TSP)) install.packages("TSP", dependencies = TRUE)

# Carregar pacotes
library(readxl)
library(osrm)
library(dplyr)
library(sf)
library(leaflet)
library(writexl)
library(TSP)

# Definir caminho do arquivo
caminho <- "C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/Lojas/Juntando_Tudo"
arquivo <- file.path(caminho, "Unindo_coord.xlsx")

# Ler os dados
df <- read_excel(arquivo)

# Certificar-se de que as colunas possuem os nomes corretos
df <- df %>%
  rename(Latitude = latitude, Longitude = longitude, Loja = Loja)

# Converter Latitude e Longitude para número (evita erros de coerção)
df <- df %>%
  mutate(
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude)
  )

# Criar a coluna "loc" concatenando Latitude e Longitude
df <- df %>%
  mutate(loc = ifelse(is.na(Latitude) | is.na(Longitude), NA, paste0(Latitude, ", ", Longitude)))

# Remover pontos com coordenadas NA
df <- df %>% filter(!is.na(Latitude) & !is.na(Longitude))

# Verificar se algum ponto foi removido
num_removidos <- nrow(read_excel(arquivo)) - nrow(df)
if (num_removidos > 0) {
  cat("Atenção: ", num_removidos, " pontos com NA foram removidos da rota.\n")
}

# Adicionar ponto inicial
ponto_inicial <- data.frame(
  Loja = "Ponto Inicial",
  Latitude = -13.0034672,
  Longitude = -38.5187736,
  loc = paste0(-13.0034672, ", ", -38.5187736)
)

# Adicionar o ponto inicial ao dataframe
df <- bind_rows(ponto_inicial, df)

# Criar um dataframe de coordenadas para OSRM
locs <- df %>%
  select(Longitude, Latitude)

# Verificar se há pelo menos 2 pontos para calcular a rota
if (nrow(locs) < 2) {
  stop("Erro: O dataset precisa de pelo menos dois pontos para calcular a rota.")
}

# Definir o número máximo de pontos por requisição
max_pontos_por_requisicao <- 50  # Ajustável conforme necessário

# Criar uma matriz vazia para armazenar as distâncias
# Criar a matriz de distâncias
matriz_distancias <- as.matrix(osrm_result$durations)

# Verificar se matriz_distancias está correta
if (is.null(matriz_distancias) || nrow(matriz_distancias) != ncol(matriz_distancias)) {
  stop("Erro: A matriz de distâncias não foi gerada corretamente.")
}

# Tornar a matriz simétrica
matriz_distancias <- (matriz_distancias + t(matriz_distancias)) / 2

# Substituir NAs por um valor alto para evitar erros no TSP
matriz_distancias[is.na(matriz_distancias)] <- 9999999

# Imprimir matriz de distâncias para depuração
print("Matriz de distâncias:")
print(matriz_distancias)

# Resolver o problema do caixeiro viajante (TSP)
tsp_inst <- TSP(matriz_distancias)
ordem <- solve_TSP(tsp_inst)

# Reordenar dataframe conforme a rota otimizada
df_ordenado <- df[as.integer(ordem), ]

# Criar um objeto sf (spatial features) para mapear
df_sf <- st_as_sf(df_ordenado, coords = c("Longitude", "Latitude"), crs = 4326)

# Criar mapa interativo com Leaflet
mapa <- leaflet(df_sf) %>%
  addTiles() %>%
  addMarkers(
    ~st_coordinates(df_sf)[,1],
    ~st_coordinates(df_sf)[,2],
    popup = ~paste0(Loja, "<br>", loc),
    label = ~Loja
  ) %>%
  addPolylines(
    lng = st_coordinates(df_sf)[,1],
    lat = st_coordinates(df_sf)[,2],
    color = "blue",
    weight = 3
  )

# Mostrar o mapa
mapa

# Salvar a ordem otimizada em Excel
write_xlsx(df_ordenado, file.path(caminho, "rota_otimizada.xlsx"))

# Mensagem final
cat("Rota traçada com sucesso! Arquivo 'rota_otimizada.xlsx' salvo na pasta:", caminho)

