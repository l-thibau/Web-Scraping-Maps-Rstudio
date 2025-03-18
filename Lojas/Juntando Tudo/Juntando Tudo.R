# Instale e carregue as bibliotecas necessárias (se ainda não estiverem instaladas)
if (!require("openxlsx")) install.packages("openxlsx")
library(openxlsx)

# Função para extrair rua/avenida, bairro e cidade
extrair_endereco <- function(endereco) {
  # Inicializa as variáveis
  rua_aven <- NA
  bairro <- NA
  cidade <- NA
  
  # 1° etapa: Extrair rua/aven
  partes <- strsplit(endereco, "-")[[1]]
  if (length(partes) > 0) {
    rua_aven <- trimws(partes[1])
    if (grepl("^[0-9]|^BR|^KM|^S/N", rua_aven)) {
      rua_aven <- NA
    }
  }
  
  # 2° etapa: Extrair bairro
  if (length(partes) > 1) {
    bairro_partes <- strsplit(partes[2], ",")[[1]]
    if (length(bairro_partes) > 0) {
      bairro <- trimws(bairro_partes[1])
    }
  }
  
  # 3° etapa: Extrair cidade
  if (length(partes) > 2) {
    cidade_partes <- strsplit(partes[3], "-")[[1]]
    if (length(cidade_partes) > 0) {
      cidade <- trimws(cidade_partes[1])
    }
  }
  
  return(list(rua_aven = rua_aven, bairro = bairro, cidade = cidade))
}

# Caminho da pasta onde os arquivos Excel estão armazenados
caminho_pasta <- "C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/Lojas/Juntando Tudo"

# Listar todos os arquivos .xlsx na pasta
arquivos_xlsx <- list.files(caminho_pasta, pattern = "\\.xlsx$", full.names = TRUE)

# Verificar se há arquivos .xlsx na pasta
if (length(arquivos_xlsx) == 0) {
  stop("Nenhum arquivo .xlsx encontrado na pasta especificada.")
}

# Caminho da pasta "Documentos" do usuário
caminho_documentos <- file.path(Sys.getenv("USERPROFILE"), "Documents")

# Processar cada arquivo .xlsx na pasta
for (caminho_arquivo in arquivos_xlsx) {
  # Carregar o arquivo Excel
  dados <- read.xlsx(caminho_arquivo, sheet = 1)  # Assume que os dados estão na primeira planilha
  
  # Verificar se a coluna "Endereço" existe
  if (!"Endereço" %in% colnames(dados)) {
    warning(paste("A coluna 'Endereço' não foi encontrada no arquivo:", caminho_arquivo))
    next  # Pula para o próximo arquivo
  }
  
  # Criar uma cópia dos dados para processamento
  dados_processados <- dados
  
  # Remover "xml:space='preserve'>" da coluna "Endereço"
  dados_processados$`Endereço` <- gsub("xml:space=\"preserve\">", "", dados_processados$`Endereço`, fixed = TRUE)
  
  # Extrair a coluna "Endereço"
  enderecos <- dados_processados$`Endereço`
  
  # Aplicar a função a todos os endereços
  enderecos_extraidos <- lapply(enderecos, extrair_endereco)
  
  # Converter a lista em um data frame
  enderecos_df <- do.call(rbind, lapply(enderecos_extraidos, as.data.frame))
  
  # Adicionar as novas colunas ao data frame
  dados_processados <- cbind(dados_processados, enderecos_df)
  
  # Caminho para salvar o novo arquivo Excel na pasta "Documentos"
  nome_arquivo <- basename(caminho_arquivo)  # Pega o nome do arquivo sem o caminho completo
  caminho_saida <- file.path(caminho_documentos, paste0("Processado_", nome_arquivo))
  
  # Salvar o data frame processado em um novo arquivo Excel
  write.xlsx(dados_processados, caminho_saida, rowNames = FALSE)  # Usar rowNames em vez de row.names
  
  print(paste("Arquivo processado e salvo em:", caminho_saida))
}

