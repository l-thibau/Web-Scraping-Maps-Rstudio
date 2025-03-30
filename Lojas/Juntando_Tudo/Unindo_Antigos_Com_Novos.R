# Carregar pacotes
library(readxl)
library(dplyr)
library(openxlsx)
library(here)

# Definir o caminho base
caminho_base <- "C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/Lojas/Juntando_Tudo"

# Listar todos os arquivos .xlsx no diretório
arquivos_xlsx <- list.files(caminho_base, pattern = "\\.xlsx$", full.names = TRUE)

# Carregar os arquivos Excel em uma lista de dataframes
todos_dfs <- lapply(arquivos_xlsx, read_excel)

# Unir os dataframes
df_total <- bind_rows(todos_dfs)

# Transferir valores da coluna "Estrelas" para "⭐", se "⭐" estiver vazio
df_total <- df_total %>%
  mutate(`⭐` = ifelse(is.na(`⭐`) & !is.na(Estrelas), Estrelas, `⭐`)) %>%
  select(-Estrelas)

# Preencher valores ausentes com base em duplicatas por "Loja" e "Endereço"
df_total <- df_total %>%
  group_by(Loja, Endereço) %>%
  mutate(
    Bairro = ifelse(is.na(Bairro), first(na.omit(Bairro)), Bairro),
    `Rua/Aven` = ifelse(is.na(`Rua/Aven`), first(na.omit(`Rua/Aven`)), `Rua/Aven`),
    Cidade = ifelse(is.na(Cidade), first(na.omit(Cidade)), Cidade),
    Loc = ifelse(is.na(Loc), first(na.omit(Loc)), Loc)
  ) %>%
  ungroup()

# Remover duplicatas com base em "Loja" e "Bairro"
df_total_unico <- df_total %>%
  distinct(Loja, Bairro, .keep_all = TRUE)

# Verificar número de lojas únicas
num_lojas_unicas <- nrow(df_total_unico)
cat("Número de lojas únicas após remoção de duplicatas:", num_lojas_unicas, "\n")

# Caminho para o modelo com formatação
modelo_path <- "C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/Lojas/Juntando_Tudo/ACOMPANHAMENTO_ROTA_SOLLAR_Lojas_Unicas_Loc_Emails_coord.22.03.25.xlsx"

# Carregar o modelo com formatação
wb <- loadWorkbook(modelo_path)

# Substituir os dados na primeira planilha, a partir da linha 2 (mantendo cabeçalhos formatados)
writeData(wb, sheet = 1, x = df_total_unico, startRow = 2, colNames = FALSE)

# Caminho de saída para novo arquivo formatado
caminho_saida_formatado <- "C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/Lojas/df_total_unico_formatado.xlsx"

# Salvar o novo arquivo com os dados e formatação preservada
saveWorkbook(wb, caminho_saida_formatado, overwrite = TRUE)

list.files("C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/Lojas", pattern = "\\.xlsx$")
