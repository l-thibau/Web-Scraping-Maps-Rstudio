# Carregar pacotes
library(readxl)
library(dplyr)
library(writexl)

# Definir caminho dos arquivos
caminho <- "C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/Lojas/Juntando Tudo"

# Ler os arquivos XLSX
df1 <- read_excel(file.path(caminho, "criando_coord.xlsx"))
df2 <- read_excel(file.path(caminho, "criando_coord_here.xlsx"))

# Unir os dataframes
df_total <- bind_rows(df1, df2)

# Verificar lojas repetidas (assumindo que a coluna da loja se chama "loja")
repetidas <- df_total %>%
  group_by(Loja) %>%
  filter(n() > 1) %>%
  ungroup()

# Separar em dois dataframes
long_rep <- repetidas %>% filter(!is.na(longitude))   # Com longitude preenchida
na_rep <- repetidas %>% filter(is.na(longitude))      # Com NA na longitude

# Remover duplicatas dentro de na_rep considerando "Bairro" e "Loja"
df2_long <- df2_long %>%
  distinct(Bairro, Loja, .keep_all = TRUE)

# Criar df2_long unindo long_rep com df2
df2_long <- bind_rows(df2, long_rep)

# Verificar quantas lojas em na_rep já estão presentes em df2_long
na_rep_presentes <- na_rep %>%
  filter(Loja %in% df2_long$Loja)

# Contar o número de lojas presentes
num_presentes <- nrow(na_rep_presentes)

# Exibir resultado
cat("Número de lojas em na_rep que já estão no df2_long:", num_presentes)





# Salvar os resultados em arquivos Excel
write_xlsx(df2_long, file.path(caminho, "Unindo_coord.xlsx"))

cat("Processo concluído. Arquivos foram salvos na pasta:", caminho)