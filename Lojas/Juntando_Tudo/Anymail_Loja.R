library(httr)
library(jsonlite)
library(readxl)
library(writexl)

# Definir caminho do arquivo de entrada
file_path <- "C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/Lojas/Juntando_Tudo/Unindo_coord.xlsx"

# Ler a planilha
df <- read_excel(file_path)

# Verificar nomes das colunas
print("Colunas do arquivo:")
print(colnames(df))

# Verificar se as colunas essenciais existem
colunas_necessarias <- c("Loja", "Endereço", "Cidade", "Rua/Aven", "Celular", "Site", "Categoria")
colunas_faltantes <- setdiff(colunas_necessarias, colnames(df))

if (length(colunas_faltantes) > 0) {
  stop(paste("Erro: As seguintes colunas estão faltando no arquivo:", paste(colunas_faltantes, collapse=", ")))
}

# Criar nova coluna de e-mails, se não existir
if (!"email" %in% colnames(df)) {
  df$email <- NA
}

# Chave da API do AnyMailFinder
api_key <- "nNTgomHCWF7De0LdSYpUYTDk"

# Função para buscar e-mails com base no domínio do site
buscar_email <- function(site, loja) {
  if (is.na(site) || site == "") {
    cat("⚠️ Loja:", loja, "- Nenhum site encontrado. Pulando...\n")
    return(NA)
  }
  
  cat("🔍 Buscando e-mail para a loja:", loja, "com site:", site, "\n")
  
  url <- paste0("https://api.anymailfinder.com/v4.0/search?domain=", site, "&key=", api_key)
  resposta <- GET(url)
  
  # Exibir resposta bruta para depuração
  resposta_texto <- content(resposta, as = "text")
  print(paste("📜 Resposta bruta da API para", loja, ":", resposta_texto))
  
  if (status_code(resposta) == 200) {
    dados <- fromJSON(resposta_texto)
    
    if (!is.null(dados$emails) && length(dados$emails) > 0) {
      email_encontrado <- dados$emails[[1]]$email
      cat("✅ Loja:", loja, "- E-mail encontrado:", email_encontrado, "\n")
      return(email_encontrado)  # Retorna o primeiro e-mail encontrado
    } else {
      cat("❌ Loja:", loja, "- Nenhum e-mail encontrado na resposta JSON.\n")
    }
  } else {
    cat("❌ Erro na requisição para loja:", loja, "- Código HTTP:", status_code(resposta), "\n")
  }
  
  return(NA)
}


# Aplicar a função para buscar e-mails em cada loja
df$email <- mapply(buscar_email, df$Site, df$Loja)

# Definir caminho do arquivo de saída
output_path <- "C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/Lojas/Juntando_Tudo/AnyMailFinder_Dados.xlsx"

# Salvar novo arquivo XLSX
write_xlsx(df, output_path)

# Mensagem final
cat("\n✅ Arquivo atualizado com e-mails! Salvo em:", output_path, "\n")
