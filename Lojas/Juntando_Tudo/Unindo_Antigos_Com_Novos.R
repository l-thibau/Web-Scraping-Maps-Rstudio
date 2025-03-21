# Carregar pacotes
library(readxl)
library(dplyr)
library(writexl)

# Definir o caminho base
caminho_base <- "Lojas/Juntando_Tudo/"

# Listar todos os arquivos .xlsx no diretório
arquivos_xlsx <- list.files(caminho_base, pattern = "\\.xlsx$", full.names = TRUE)

# Carregar os arquivos Excel em uma lista de dataframes
todos_dfs <- lapply(arquivos_xlsx, read_excel)

# Unir os dataframes
df_total <- bind_rows(todos_dfs)

# Verificar lojas repetidas e criar df_repetidas (detectando as lojas repetidas a partir das colunas "Bairro" e "Loja")
df_repetidas <- df_total %>%
  group_by(Bairro, Loja) %>%
  filter(n() > 1) %>%
  ungroup()

# Criar df_loc e df_semloc
df_loc <- df_total %>% filter(!is.na(Loc))  # Lojas com loc
df_semloc <- df_total %>% filter(is.na(Loc))  # Lojas sem loc

# Criar df_colun_extra (com lojas que possuem colunas além de Loja, Categoria, Endereço, Plus_Code, Site, Celular, Estrelas, Loc)
df_colun_extra <- df_total %>% 
  select(Loja, Categoria, Endereço, Plus_Code, Site, Celular, Estrelas, Loc) %>%
  select(-c(Loja, Categoria, Endereço, Plus_Code, Site, Celular, Estrelas, Loc)) %>%
  colnames()

# Excluir as lojas repetidas que não possuem loc e só deixar as lojas que possuem loc
df_total_filtrado <- df_total %>% 
  filter(Loja %in% df_loc$Loja)

# Observar df_total e criar coluna loc para lojas que não tem loc, inserindo NA
df_total <- df_total %>% 
  mutate(Loc = ifelse(is.na(Loc), NA, Loc))

# Separar em dois dataframes
loc_pre <- df_total %>% filter(!is.na(Loc))   # Com loc preenchida
na_pre <- df_total %>% filter(is.na(Loc))      # Com NA na longitude

# Exibir resultado
num_presentes <- sum(na_pre$Loja %in% loc_pre$Loja)
cat("Número de lojas em na_pre que já estão no loc_pre:", num_presentes)
