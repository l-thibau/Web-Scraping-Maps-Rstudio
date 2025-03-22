# Instalar pacotes caso não estejam instalados
if (!require(readxl)) install.packages("readxl", dependencies = TRUE)
if (!require(osrm)) install.packages("osrm", dependencies = TRUE)
if (!require(dplyr)) install.packages("dplyr", dependencies = TRUE)
if (!require(sf)) install.packages("sf", dependencies = TRUE)
if (!require(leaflet)) install.packages("leaflet", dependencies = TRUE)
if (!require(writexl)) install.packages("writexl", dependencies = TRUE)
if (!require(TSP)) install.packages("TSP", dependencies = TRUE)
if (!require(stringr)) install.packages("stringr", dependencies = TRUE)
if (!require(tidyr)) install.packages("tidyr", dependencies = TRUE)

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
caminho <- "C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/OSM/Lojas_Unicas_Loc_Emails_coord.xlsx"
arquivo <- caminho

# Ler o arquivo Excel
cat("Lendo o arquivo Excel...\n")
df <- read_excel(arquivo)

# Extrair latitude e longitude da coluna Loc
cat("Extraindo latitude e longitude...\n")
df <- df %>%
  mutate(
    latitude = as.numeric(str_extract(Loc, "-?\\d+\\.\\d+")),  # Extrai a latitude
    longitude = as.numeric(str_extract(Loc, "-?\\d+\\.\\d+(?=,|$)"))  # Extrai a longitude
  )

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

# Adicionar ponto inicial
cat("Adicionando ponto inicial...\n")
ponto_inicial <- data.frame(
  Loja = "Ponto Inicial",
  Latitude = -13.0034672,
  Longitude = -38.5187736,
  Loc = paste0(-13.0034672, ", ", -38.5187736)
)

# Adicionar o ponto inicial ao dataframe
df <- bind_rows(ponto_inicial, df)

# Criar um dataframe de coordenadas para OSRM
cat("Preparando coordenadas para OSRM...\n")
locs <- df %>%
  select(Longitude, Latitude)

# Verificar se há pelo menos 2 pontos para calcular a rota
if (nrow(locs) < 2) {
  stop("Erro: O dataset precisa de pelo menos dois pontos para calcular a rota.")
}

# Função para dividir os dados em lotes
split_into_batches <- function(df, batch_size) {
  split(df, ceiling(seq_along(df[[1]]) / batch_size))
}

# Definir tamanho do lote
batch_size <- 20  # Reduza o tamanho do lote

# Dividir locs em lotes
cat("Dividindo os dados em lotes...\n")
batches <- split_into_batches(locs, batch_size)

# Verificar se batches foi criado corretamente
if (length(batches) == 0) {
  stop("Erro: Nenhum lote foi criado. Verifique o objeto 'locs' e o tamanho do lote.")
}

# Função para combinar resultados de forma robusta
combine_results <- function(results) {
  # Verificar o número de colunas de cada matriz
  ncols <- sapply(results, function(x) ncol(x$durations))
  
  # Se houver diferenças no número de colunas, ajustar as matrizes
  if (length(unique(ncols)) > 1) {
    max_cols <- max(ncols)
    
    # Ajustar cada matriz para ter o mesmo número de colunas
    results <- lapply(results, function(x) {
      if (ncol(x$durations) < max_cols) {
        # Adicionar colunas extras preenchidas com NA
        x$durations <- cbind(x$durations, matrix(NA, nrow = nrow(x$durations), ncol = max_cols - ncol(x$durations)))
      }
      x
    })
  }
  
  # Combinar as matrizes de durações
  durations <- do.call(rbind, lapply(results, function(x) x$durations))
  
  # Combinar as matrizes de fontes (sources)
  sources <- do.call(rbind, lapply(results, function(x) x$sources))
  
  # Retornar o resultado combinado
  list(durations = durations, sources = sources)
}

# Calcular distâncias para cada lote
cat("Calculando distâncias para cada lote...\n")
results <- lapply(seq_along(batches), function(i) {
  cat("Processando lote ", i, " de ", length(batches), "...\n")
  tryCatch({
    Sys.sleep(1)  # Adiciona um atraso de 1 segundo entre as requisições
    osrmTable(batches[[i]])
  }, error = function(e) {
    message("Erro ao processar lote ", i, ": ", e$message)
    Sys.sleep(5)  # Espera 5 segundos antes de tentar novamente
    osrmTable(batches[[i]])
  })
})

# Combinar os resultados
cat("Combinando resultados dos lotes...\n")
final_result <- combine_results(results)

# Verificar se os resultados foram combinados corretamente
if (is.null(final_result$durations)) {
  stop("Erro: Não foi possível combinar os resultados dos lotes.")
}

# Criar a matriz de distâncias
cat("Criando matriz de distâncias...\n")
matriz_distancias <- as.matrix(final_result$durations)

# Resolver o problema do caixeiro viajante (TSP)
cat("Resolvendo o problema do caixeiro viajante (TSP)...\n")
tsp_inst <- TSP(matriz_distancias)
ordem <- solve_TSP(tsp_inst)

# Reordenar dataframe conforme a rota otimizada
cat("Reordenando os pontos conforme a rota otimizada...\n")
df_ordenado <- df[as.integer(ordem), ]

# Criar um objeto sf (spatial features) para mapear
cat("Criando mapa interativo...\n")
df_sf <- st_as_sf(df_ordenado, coords = c("Longitude", "Latitude"), crs = 4326)

# Criar mapa interativo com Leaflet
mapa <- leaflet(df_sf) %>%
  addTiles() %>%
  addMarkers(
    ~st_coordinates(df_sf)[,1],
    ~st_coordinates(df_sf)[,2],
    popup = ~paste0(Loja, "<br>", Loc),
    label = ~Loja
  ) %>%
  addPolylines(
    lng = st_coordinates(df_sf)[,1],
    lat = st_coordinates(df_sf)[,2],
    color = "blue",
    weight = 3
  )

# Mostrar o mapa
cat("Exibindo o mapa...\n")
mapa

# Salvar a ordem otimizada em Excel
cat("Salvando a rota otimizada em Excel...\n")
write_xlsx(df_ordenado, file.path(dirname(caminho), "rota_otimizada.xlsx"))

# Mensagem final
cat("Processo concluído! Arquivo 'rota_otimizada.xlsx' salvo na pasta:", dirname(caminho), "\n")