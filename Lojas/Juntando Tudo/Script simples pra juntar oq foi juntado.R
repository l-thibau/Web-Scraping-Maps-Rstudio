# Carregar pacotes necessários
library(readxl)  # Para ler arquivos Excel
library(writexl) # Para salvar arquivos Excel
library(dplyr)   # Para manipulação de dados

# Definir o caminho da pasta onde os arquivos estão localizados
caminho_pasta <- "C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/Lojas/Juntando Tudo"

# Listar todos os arquivos .xlsx na pasta
arquivos <- list.files(caminho_pasta, pattern = "\\.xlsx$", full.names = TRUE)

# Verificar se há arquivos na pasta
if (length(arquivos) == 0) {
  stop("Nenhum arquivo .xlsx encontrado na pasta especificada.")
}

# Ler todos os arquivos e combiná-los em um único dataframe
dados_combinados <- arquivos %>%
  lapply(read_xlsx) %>%       # Lê cada arquivo .xlsx
  bind_rows()                # Combina todos os dataframes em um único

# Verificar se os dados foram combinados corretamente
if (nrow(dados_combinados) == 0) {
  stop("Nenhum dado foi lido ou combinado.")
}

# Definir o caminho e o nome do arquivo de saída
caminho_saida <- file.path(caminho_pasta, "Dados_Combinados.xlsx")

# Salvar o dataframe combinado em um novo arquivo .xlsx
write_xlsx(dados_combinados, caminho_saida)

# Mensagem de sucesso
cat("Todos os arquivos .xlsx foram combinados e salvos em:", caminho_saida, "\n")

# Verificar se os dados foram combinados corretamente
if (nrow(dados_combinados) == 0) {
  stop("Nenhum dado foi lido ou combinado.")
}

# Verificar lojas repetidas com base no nome e bairro
lojas_repetidas <- dados_combinados %>%
  group_by(Loja, Bairro) %>%  # Agrupa por nome da loja e bairro
  filter(n() > 1) %>%              # Filtra grupos com mais de uma ocorrência
  ungroup()                        # Remove o agrupamento

# Contar o número de lojas repetidas
num_lojas_repetidas <- nrow(lojas_repetidas)

# Exibir o número de lojas repetidas
cat("Número de lojas repetidas (mesmo nome e bairro):", num_lojas_repetidas, "\n")

# Exibir as lojas repetidas (opcional)
if (num_lojas_repetidas > 0) {
  cat("\nLojas repetidas:\n")
  print(lojas_repetidas)
} else {
  cat("Nenhuma loja repetida encontrada.\n")
}
