# Carregar pacotes
library(readxl)
library(dplyr)
library(writexl)
library(here)

# Definir o caminho base
caminho_base <- "C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/Lojas/Juntando_Tudo"

# Listar todos os arquivos .xlsx no diretório
arquivos_xlsx <- list.files(caminho_base, pattern = "\\.xlsx$", full.names = TRUE)

# Carregar os arquivos Excel em uma lista de dataframes
todos_dfs <- lapply(arquivos_xlsx, read_excel)

# Unir os dataframes
df_total <- bind_rows(todos_dfs)

# Função para remover a loja duplicada com menos dados
remover_duplicados <- function(df) {
  # Contar o número de NAs em cada linha
  df <- df %>%
    mutate(na_count = rowSums(is.na(.))) %>%
    arrange(na_count) # Ordenar pelo número de NAs (menos NAs primeiro)
  
  # Manter apenas a primeira linha (menos NAs)
  df <- df %>%
    slice(1) %>%
    select(-na_count) # Remover a coluna de contagem de NAs
  
  return(df)
}

# Transferir os valores da coluna "estrelas" para a coluna "⭐" apenas se "⭐" estiver vazio
df_total <- df_total %>%
  mutate(`⭐` = ifelse(is.na(`⭐`) & !is.na(Estrelas), Estrelas, `⭐`)) %>% # Transferir só se ⭐ estiver vazio
  select(-Estrelas)  # Remover a coluna "estrelas"

# Verificar lojas repetidas e criar df_repetidas (detectando as lojas repetidas a partir das colunas "Bairro" e "Loja")
df_repetidas <- df_total %>%
  group_by(Endereço, Loja) %>%
  filter(n() > 1) %>%
  ungroup()

# Preencher valores ausentes nas lojas repetidas com base em outras lojas do mesmo grupo
df_total <- df_total %>%
  group_by(Loja, Endereço) %>%
  mutate(
    # Preencher Bairro
    Bairro = ifelse(is.na(Bairro), first(na.omit(Bairro)), Bairro),
    # Preencher Rua/Aven
    `Rua/Aven` = ifelse(is.na(`Rua/Aven`), first(na.omit(`Rua/Aven`)), `Rua/Aven`),
    # Preencher Cidade
    Cidade = ifelse(is.na(Cidade), first(na.omit(Cidade)), Cidade),
    # Preencher Loc
    Loc = ifelse(is.na(Loc), first(na.omit(Loc)), Loc)
  ) %>%
  ungroup()

# Verificar lojas repetidas com base no nome e bairro
lojas_repetidas <- df_total %>%
  group_by(Loja, Bairro) %>%  # Agrupa por nome da loja e bairro
  filter(n() > 1) %>%         # Filtra grupos com mais de uma ocorrência
  ungroup()                   # Remove o agrupamento

# Remover lojas repetidas com base no nome e bairro, mantendo apenas uma ocorrência
df_total_unico <- df_total %>%
  distinct(Loja, Bairro, .keep_all = TRUE)  # Remove duplicatas com base em "Loja" e "Bairro"

# Verificar o número de lojas após a remoção de duplicatas
num_lojas_unicas <- nrow(df_total_unico)
cat("Número de lojas únicas após remoção de duplicatas:", num_lojas_unicas, "\n")

# Definir o caminho de saída para o arquivo .xlsx
caminho_saida <- "C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/Lojas/df_total_unico.xlsx"

# Salvar o dataframe com lojas únicas em um novo arquivo .xlsx
write_xlsx(df_total_unico, caminho_saida)
