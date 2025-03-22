# Carregar pacotes necessários
library(readxl)
library(dplyr)
library(writexl)
library(here)

caminho_base <- here("C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/Lojas/Juntando_Tudo")

# Listar todos os arquivos .xlsx no diretório
arquivos_xlsx <- list.files(caminho_base, pattern = "\\.xlsx$", full.names = TRUE, ignore.case = TRUE)

# Verificar se arquivos foram encontrados
if (length(arquivos_xlsx) == 0) {
  stop("Nenhum arquivo .xlsx encontrado no diretório especificado: ", caminho_base)
} else {
  print("Arquivos encontrados:")
  print(arquivos_xlsx)
}

# Verificar se o diretório existe
if (!dir.exists(caminho_base)) {
  stop("O diretório especificado não existe: ", caminho_base)
}

# Verificar o diretório atual
print(getwd())

# Ler todos os arquivos e combiná-los em um único data.frame
dados_combinados <- arquivos_xlsx %>%
  lapply(read_excel) %>% # Ler cada arquivo
  bind_rows() # Combinar todos os data.frames em um único

# Verificar duplicatas com base nas colunas "Loja" e "Endereço"
duplicatas <- dados_combinados %>%
  group_by(Loja, Endereço) %>% # Agrupar por "Loja" e "Endereço"
  filter(n() > 1) %>% # Manter apenas grupos com mais de uma ocorrência
  ungroup() # Remover o agrupamento

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

# Aplicar a função para remover duplicatas
dados_sem_duplicatas <- dados_combinados %>%
  group_by(Loja, Endereço) %>% # Agrupar por "Loja" e "Endereço"
  group_modify(~ remover_duplicados(.x)) %>% # Remover duplicatas em cada grupo
  ungroup() # Remover o agrupamento

# Exibir os dados sem duplicatas
print(dados_sem_duplicatas)

# Salvar os dados sem duplicatas em um novo arquivo .xlsx
write_xlsx(dados_sem_duplicatas, "Todos_Estabelecimentos_Sem_Duplicatas_loc.xlsx")
