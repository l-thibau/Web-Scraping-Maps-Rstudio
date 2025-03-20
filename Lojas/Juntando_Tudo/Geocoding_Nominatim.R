# Carregar pacotes
library(tidygeocoder)
library(readxl)
library(dplyr)
library(writexl)

# Caminho do arquivo
arquivo_xlsx <- "C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/Lojas/Juntando Tudo/rodar_de_novo_here_maps.xlsx"

# Leia o arquivo
dados <- read_excel(arquivo_xlsx)

# Primeira tentativa de geocodificação usando apenas o endereço completo
dados_geo <- dados %>%
  geocode(address = `Endereço`, method = "osm", lat = latitude, long = longitude)

# Identificar os casos em que a geocodificação falhou (NA)
dados_na <- dados_geo %>%
  filter(is.na(`latitude...13`) | is.na(`longitude...14`))

dados_geo <- dados_geo %>%
  select(-c(11, 12))


# Exibir os primeiros resultados
head(dados_geo)

# Se houver falhas, tentar novamente usando "Bairro", "Rua/Aven", "Cidade" e "Loja"
if (nrow(dados_na) > 0) {
  # Atualizar a coluna address diretamente com os novos dados
  dados_na <- dados_na %>%
    mutate(
      address = paste(`Rua/Aven`, `Bairro`, `Cidade`, `Loja`, sep = ", ")
    ) %>%
    geocode(address = address, method = "osm", lat = latitude, long = longitude)
  
  # Substituir os valores de latitude e longitude nos dados originais
  dados_geo <- dados_geo %>%
    rows_update(dados_na, by = "Endereço")
}

# Exibir os primeiros resultados
head(dados_geo)

# Salvar o resultado atualizado em um novo arquivo
write_xlsx(dados_geo, "C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/Lojas/ajustar.xlsx")
