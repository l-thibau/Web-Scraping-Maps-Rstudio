library(readxl)
library(httr)
library(rvest)
library(stringr)
library(writexl)
library(RSelenium)

#considerar api do hunter.io

# Caminho local do arquivo .crx da extensão Email Extractor
ext_path <- "C:/caminho/para/email_extractor.crx"
testar_rselenium_e_extractor(ext_path)

testar_rselenium_e_extractor <- function(ext_path) {
  cat("🔧 Testando RSelenium + Email Extractor...\n")
  eCaps <- list(
    chromeOptions = list(
      extensions = list(base64enc::base64encode(ext_path))
    )
  )
  
  rD <- tryCatch({
    rsDriver(browser = "chrome", port = 4567L, verbose = FALSE, extraCapabilities = eCaps)
  }, error = function(e) {
    cat("❌ Erro ao iniciar RSelenium:\n", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(rD)) return(FALSE)
  
  remDr <- rD$client
  remDr$navigate("https://www.email-verify.my-addr.com/free-email-verifier.html")
  Sys.sleep(10)  # tempo para extensão agir
  
  page_source <- remDr$getPageSource()[[1]]
  remDr$close()
  rD$server$stop()
  
  emails <- str_extract_all(page_source, "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}")[[1]]
  if (length(emails) > 0) {
    cat("✅ RSelenium + Extensão funcionando. E-mails encontrados:\n", paste(emails, collapse = "; "), "\n")
    return(TRUE)
  } else {
    cat("⚠ RSelenium iniciado, mas nenhum e-mail detectado pela extensão.\n")
    return(FALSE)
  }
}


# Caminho do arquivo
file_path <- "C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/Lojas/Juntando_Tudo/Lojas_Unicas_Loc_Emails_coord.xlsx"
df <- read_excel(file_path)

# Verificação da coluna de site
if (!"Site" %in% colnames(df)) {
  stop("Erro: A coluna 'Site' não foi encontrada.")
}

# Corrigir http para https
df$Site <- gsub("^http://", "https://", df$Site)

# Remover linhas sem site ou com redes sociais
df <- df[!is.na(df$Site) & df$Site != "" & !grepl("instagram\\.com|facebook\\.com|linkedin\\.com|shopee\\.com|whatsapp|linktr\\.ee|wa\\.me", df$Site), ]
df$email <- NA

# Mover a coluna site para a primeira posição

df <- df[, c("Site", "Loja", "Bairro", "email")]

# Função auxiliar para buscar links relevantes
buscar_links_relevantes <- function(html) {
  links <- html %>% html_elements("a") %>% html_attr("href")
  links <- tolower(links)
  padroes <- c("contato", "fale-conosco", "atendimento", "suporte", "sobre", "quem-somos", "contact")
  links_relevantes <- links[grepl(paste(padroes, collapse = "|"), links)]
  links_relevantes <- unique(links_relevantes)
  links_relevantes <- links_relevantes[!grepl("instagram|facebook|linkedin", links_relevantes)]
  return(links_relevantes)
}

# ─────────────────────────────────────────────
# Função auxiliar: buscar email via PhantomBuster
buscar_email_phantombuster <- function(url_site) {
  api_key <- "EPQM1GJkPgksEffS4HUrv3y6PKjE3U7MXIaeEM0nZn8"
  phantom_id <- "3780568242361364"
  
  launch_url <- paste0("https://api.phantombuster.com/api/v2/agents/launch?id=", phantom_id)
  
  payload <- list(
    output = "first-result-object",
    argument = list(
      url = url_site
    )
  )
  
  resp <- tryCatch({
    POST(
      url = launch_url,
      add_headers(`X-Phantombuster-Key-1` = api_key, `Content-Type` = "application/json"),
      body = jsonlite::toJSON(payload, auto_unbox = TRUE)
    )
  }, error = function(e) return(NULL))
  
  if (is.null(resp) || status_code(resp) >= 400) {
    cat("   ⚠ PhantomBuster falhou ao iniciar.\n")
    return(NA)
  }
  
  content_resp <- content(resp, as = "parsed", encoding = "UTF-8")
  container_id <- content_resp$data$containerId
  if (is.null(container_id)) {
    cat("   ⚠ containerId não encontrado na resposta.\n")
    return(NA)
  }
  
  # Espera até o Phantom finalizar
  for (i in 1:10) {
    Sys.sleep(5)
    result_url <- paste0("https://api.phantombuster.com/api/v2/containers/fetch-output?id=", container_id)
    result_resp <- tryCatch({
      GET(
        url = result_url,
        add_headers(`X-Phantombuster-Key-1` = api_key)
      )
    }, error = function(e) return(NULL))
    
    if (!is.null(result_resp) && status_code(result_resp) == 200) {
      result_data <- content(result_resp, as = "parsed", encoding = "UTF-8")
      result_obj <- result_data$container$output$resultObject
      if (!is.null(result_obj$emails) && length(result_obj$emails) > 0) {
        email_result <- paste(unique(result_obj$emails), collapse = "; ")
        cat("   → E-mails encontrados via PhantomBuster:", email_result, "\n")
        return(email_result)
      }
    }
  }
  
  cat("   ⚠ PhantomBuster não retornou e-mails após 10 tentativas.\n")
  return(NA)
}

# ─────────────────────────────────────────────


# Função principal de extração com PhantomBuster integrado
buscar_email <- function(url, index, total) {
  if (is.na(url) || !grepl("^http", url)) return(NA)
  cat(sprintf("[%d/%d] Acessando: %s\n", index, total, url))
  
  extrair_emails <- function(texto) {
    emails1 <- str_extract_all(texto, "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}")[[1]]
    emails2 <- str_extract_all(texto, "[a-zA-Z0-9._%+-]+\\s?(\\[at\\]|\\(arroba\\))\\s?[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}")[[1]]
    emails2 <- gsub("\\s?(\\[at\\]|\\(arroba\\))\\s?", "@", emails2)
    unique(c(emails1, emails2))
  }
  
  # → Primeiro tenta via PhantomBuster
  email_pb <- buscar_email_phantombuster(url)
  if (!is.na(email_pb)) return(email_pb)
  
  tryCatch({
    res <- GET(url,
               user_agent("Mozilla/5.0"),
               config(ssl_verifypeer = FALSE, followlocation = TRUE))
    if (status_code(res) >= 400) stop("Página inacessível.")
    
    html <- read_html(res)
    
    # → mailto links
    mailto_links <- html %>%
      html_elements(xpath = "//a[contains(@href, 'mailto')] | //button[contains(@onclick, 'mailto')] | //*[@role='button'][contains(@onclick, 'mailto')]") %>%
      html_attr("href")
    
    emails_mailto <- if (!is.null(mailto_links)) {
      found <- str_extract_all(mailto_links, "mailto:([a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,})")
      unique(gsub("mailto:", "", unlist(found)))
    } else character(0)
    
    if (length(emails_mailto) > 0) {
      email_result <- paste(emails_mailto, collapse = "; ")
      cat("   → E-mails encontrados via mailto:", email_result, "\n")
      return(email_result)
    }
    
    # → texto da homepage
    texto <- html_text(html)
    emails <- extrair_emails(texto)
    if (length(emails) > 0) {
      email_result <- paste(emails, collapse = "; ")
      cat("   → E-mails encontrados na homepage:", email_result, "\n")
      return(email_result)
    }
    
    # → links internos
    links_relativos <- buscar_links_relevantes(html)
    for (link in links_relativos) {
      link_completo <- ifelse(grepl("^http", link), link, paste0(url, link))
      cat("   ↪ Buscando em:", link_completo, "\n")
      try({
        sub_html <- read_html(GET(link_completo, user_agent("Mozilla/5.0")))
        sub_texto <- html_text(sub_html)
        sub_emails <- extrair_emails(sub_texto)
        if (length(sub_emails) > 0) {
          email_result <- paste(unique(sub_emails), collapse = "; ")
          cat("   → E-mails encontrados:", email_result, "\n")
          return(email_result)
        }
      }, silent = TRUE)
    }
    
    # → HTML bruto
    html_raw <- as.character(html)
    emails_raw <- extrair_emails(html_raw)
    if (length(emails_raw) > 0) {
      email_result <- paste(emails_raw, collapse = "; ")
      cat("   → E-mails encontrados no HTML bruto:", email_result, "\n")
      return(email_result)
    }
    
    # → fallback RSelenium
    cat("   ↪ Tentando com RSelenium e Email Extractor...\n")
    try({
      remDr <- start_selenium()
      remDr$navigate(url)
      Sys.sleep(10)
      page_source <- remDr$getPageSource()[[1]]
      emails_selenium <- extrair_emails(page_source)
      remDr$close()
      
      if (length(emails_selenium) > 0) {
        email_result <- paste(unique(emails_selenium), collapse = "; ")
        cat("   → E-mails encontrados com RSelenium:", email_result, "\n")
        return(email_result)
      }
    }, silent = TRUE)
    
    return(NA)
  }, error = function(e) {
    cat("   ⚠ Erro ao acessar:", url, "\n")
    return(NA)
  })
}


# Aplicar função com progresso
total_sites <- nrow(df)
df$email <- mapply(buscar_email, df$Site, seq_len(total_sites), MoreArgs = list(total = total_sites))

# Salvar resultados
output_path <- "C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/Lojas/Juntando_Tudo/Unindo_coord_com_emails.xlsx"
write_xlsx(df, output_path)
cat("\n✅ Extração concluída. E-mails salvos em:", output_path, "\n")

# Reunir com dados originais
df_original <- read_excel(file_path)
df_com_emails <- read_excel(output_path)

df_original$Loja <- as.character(df_original$Loja)
df_original$Bairro <- as.character(df_original$Bairro)
df_com_emails$Loja <- as.character(df_com_emails$Loja)
df_com_emails$Bairro <- as.character(df_com_emails$Bairro)

df_final <- merge(df_original, df_com_emails[, c("Loja", "Bairro", "email")],
                  by = c("Loja", "Bairro"), all.x = TRUE)

# Salvar final
final_path <- "C:/Users/leona/Github/Web-Scraping-Maps-Rstudio/Lojas/Juntando_Tudo/Lojas_Unidas_Unicas__Loc_emails.xlsx"
write_xlsx(df_final, final_path)
cat("\n✅ Lojas unidas com e-mails. Arquivo final salvo em:", final_path, "\n")


