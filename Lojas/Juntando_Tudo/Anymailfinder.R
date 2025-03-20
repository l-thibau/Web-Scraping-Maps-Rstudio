install.packages(c("readxl", "httr", "rvest", "stringr", "writexl"))

library(readxl)   # Para ler arquivos .xlsx
library(httr)     # Para acessar sites
library(rvest)    # Para extrair informações das páginas
library(stringr)  # Para manipulação de strings
library(writexl)  # Para salvar o novo arquivo

# Definir caminho do arquivo
file_path <- "C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/Lojas/Juntando_Tudo/Unindo_coord.xlsx"

# Ler a planilha
df <- read_excel(file_path)

# Verificar nomes das colunas
print("Colunas do arquivo:")
print(colnames(df))

# Verificar se a coluna "Website" existe
if (!"site" %in% colnames(df)) {
  stop("Erro: A coluna 'Website' não foi encontrada no arquivo. Verifique o nome exato.")
}

# Criar uma nova coluna para armazenar os e-mails encontrados
df$email <- NA

# Função para buscar e-mails em um site com logs no console
buscar_email <- function(url, index, total) {
  if (!is.na(url) & grepl("http", url)) {
    cat(sprintf("[%d/%d] Acessando: %s\n", index, total, url))  
    resultado <- tryCatch({
      page <- GET(url, user_agent("Mozilla/5.0")) %>% read_html()  
      text <- html_text(page)  
      emails <- str_extract_all(text, "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}")[[1]]
      if (length(emails) > 0) {
        unique_emails <- paste(unique(emails), collapse = "; ")
        cat("   → E-mails encontrados:", unique_emails, "\n")
        return(unique_emails)
      }
      return(NA)  # Retorna NA se não encontrar nada
    }, error = function(e) { 
      cat("   ⚠ Erro ao acessar:", url, "\n")
      return(NA) 
    })
    return(resultado)  
  }
  return(NA)
}

#remover linhas sem site
df <- df[!is.na(df$Site) & df$Site != "", ]  # Remove linhas sem site


# Aplicar a função na coluna Website com progresso
total_sites <- nrow(df)
df$email <- mapply(buscar_email, df$Site, seq_len(total_sites), MoreArgs = list(total = total_sites))

# Salvar novo arquivo XLSX
output_path <- "C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/Lojas/Juntando_Tudo/Unindo_coord_com_emails.xlsx"
write_xlsx(df, output_path)

# Mensagem final
cat("\n✅ Processo concluído! Arquivo salvo em:", output_path, "\n")

