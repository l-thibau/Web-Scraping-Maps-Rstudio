library(openxlsx)

# Caminhos dos arquivos
modelo_path <- "D:/Meu Drive/Rota/ACOMPANHAMENTO_ROTA_SOLLAR_Lojas_Unicas_Loc_Emails_coord.22.03.25.xlsx"
dados_path <- "D:/Meu Drive/Rota/df_total_unico_formatado.xlsx"

# Carregar a planilha modelo com formatação
wb <- loadWorkbook(modelo_path)

# Carregar os dados a serem inseridos
df_total_unico_formatado <- read.xlsx(dados_path, sheet = 1)

# Ajustar os nomes das colunas para combinar com o modelo
names(df_total_unico_formatado) <- names(read.xlsx(modelo_path, sheet = 1, rows = 1))

# Sobrescrever os dados na primeira planilha (sheet = 1), a partir da linha 2
writeData(
  wb,
  sheet = 1,
  x = df_total_unico_formatado,
  startRow = 2,
  colNames = FALSE
)

# Salvar no mesmo arquivo
saveWorkbook(wb, modelo_path, overwrite = TRUE)

