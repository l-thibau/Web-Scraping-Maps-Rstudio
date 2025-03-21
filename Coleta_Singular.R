library(RSelenium)
library(tidyverse)
library(writexl)

# Conectar ao Selenium Server no Docker
remDr <- remoteDriver(
  remoteServerAddr = "localhost", # Endereço do Docker
  port = 4445,                   # Porta mapeada
  browserName = "firefox"        # Navegador a ser usado
)

# Abrir a conexão
remDr$open()

# Função para coletar informações de uma loja
coletar_informacoes <- function(remDr) {
  # Coletar nome
  nome <- tryCatch({
    nome <- remDr$findElement(using = "xpath", value = "/html/body/div[1]/div[3]/div[8]/div[9]/div/div/div[1]/div[3]/div/div[1]/div/div/div[2]/div[2]/div/div[1]/div[1]/h1")$getElementText()[[1]]
    cat("\nColetando loja:", nome, "\n")  # Adicionando mensagem de log
    nome
  }, error = function(e) {
    cat("\nErro ao coletar o nome. Retornando NA.\n")
    NA_character_
  })
  
  # Coletar categoria
  categoria <- tryCatch({
    remDr$findElement(using = "xpath", value = "/html/body/div[1]/div[3]/div[8]/div[9]/div/div/div[1]/div[3]/div/div[1]/div/div/div[2]/div[2]/div/div[1]/div[2]/div/div[2]/span/span/button")$getElementText()[[1]]
  }, error = function(e) {
    cat("\nErro ao coletar a categoria. Retornando NA.\n")
    NA_character_
  })
  
  # Coletar endereço (segunda tentativa como primeira)
  endereco <- tryCatch({
    elemento <- remDr$findElement(using = "xpath", value = "//button[contains(@aria-label, 'Endereço:')]")
    aria_label <- elemento$getElementAttribute("aria-label")[[1]]
    # Remover o prefixo "Endereço: " para obter apenas o endereço
    sub("^Endereço: ", "", aria_label)
  }, error = function(e) {
    cat("\nSem endereço identificado na primeira tentativa. \n")
    NA_character_
  })
  
  # Segunda tentativa de coleta do endereço (XPath alternativo)
  if (is.na(endereco)) {
    endereco <- tryCatch({
      remDr$findElement(using = "xpath", value = "/html/body/div[1]/div[3]/div[8]/div[9]/div/div/div[1]/div[3]/div/div[1]/div/div/div[2]/div[9]/div[3]/button/div/div[2]/div[1]")$getElementText()[[1]]
    }, error = function(e) {
      cat("\nEndereço não identificado na segunda tentativa. \n")
      NA_character_
    })
    
    # Aviso sobre o sucesso da segunda tentativa
    if (!is.na(endereco)) {
      cat("\n✅ Endereço coletado com sucesso na segunda tentativa. \n")
    } else {
      cat("\n 🗺 Falha na segunda tentativa de coleta do endereço. \n")
    }
  }
  
  # Coletar Plus Code
  plus_code <- tryCatch({
    elemento <- remDr$findElement(using = "xpath", value = "//button[contains(@aria-label, 'Plus Code:')]")
    aria_label <- elemento$getElementAttribute("aria-label")[[1]]
    # Remover o prefixo "Plus Code: " para obter apenas o código
    sub("^Plus Code: ", "", aria_label)
  }, error = function(e) {
    cat("\nPlus Code não identificado. \n")
    NA_character_
  })
  
  # Função para coletar o link do site
  coletar_link_site <- function(remDr) {
    site <- tryCatch({
      elemento <- remDr$findElement(using = "xpath", value = "//a[@class='CsEnBe' and contains(@aria-label, 'Website:')]")
      elemento$getElementAttribute("href")[[1]]
    }, error = function(e) {
      cat("\nSem Site 💻\n")
      NA_character_
    })
    return(site)
  }
  
  # Coletar site
  site <- coletar_link_site(remDr)
  
  # Coletar telefone
  celular <- tryCatch({
    # Localizar o botão pelo atributo aria-label que contém o texto "Telefone"
    telefone_element <- remDr$findElement(using = "css selector", value = "button[aria-label^='Telefone:']")
    
    # Extrair o texto do botão
    telefone_text <- telefone_element$getElementText()[[1]]
    
    # Extrair apenas o número de telefone usando uma expressão regular
    telefone_numero <- regmatches(telefone_text, regexpr("\\(?\\d{2}\\)?\\s?\\d{4,5}-?\\d{4}", telefone_text))
    
    telefone_numero
  }, error = function(e) {
    cat("\nSem telefone 📱")
    NA_character_
  })
  
  # Coletar avaliação (estrelas)
  estrelas <- tryCatch({
    remDr$findElement(using = "xpath", value = "/html/body/div[1]/div[3]/div[8]/div[9]/div/div/div[1]/div[3]/div/div[1]/div/div/div[2]/div[2]/div/div[1]/div[2]/div/div[1]/div[2]/span[1]/span[1]")$getElementText()[[1]]
  }, error = function(e) {
    cat("\nSem estrelas ✨")
    NA_character_
  })
  
  # Coletar URL da página atual
  url_atual <- tryCatch({
    remDr$getCurrentUrl()[[1]]
  }, error = function(e) {
    cat("\nErro ao coletar URL. Retornando NA.\n")
    NA_character_
  })
  
  # Se o URL foi capturado com sucesso, extrair coordenadas
  loc <- tryCatch({
    if (!is.na(url_atual)) {
      latitude <- str_extract(url_atual, "(?<=!3d)-?[0-9]+\\.[0-9]+")
      longitude <- str_extract(url_atual, "(?<=!4d)-?[0-9]+\\.[0-9]+")
      
      if (!is.na(latitude) & !is.na(longitude)) {
        loc <- paste0(latitude, ", ", longitude)
        cat("\n🚓 Coordenada coletada com sucesso:", loc, "\n") # Mensagem no console
      } else {
        cat("\n🚩️ Coordenada não encontrada no URL.\n")
        loc <- NA_character_
      }
    } else {
      loc <- NA_character_
    }
    loc
  }, error = function(e) {
    cat("\nErro ao processar coordenadas. Retornando NA.\n")
    NA_character_
  })
  
  # Criar um novo tibble com as informações coletadas
  informacoes <- tibble(
    Loja = nome,
    Categoria = categoria,
    Endereço = endereco,
    Plus_Code = plus_code,
    Site = site,
    Celular = celular,
    Estrelas = estrelas,
    Loc = loc # Adicionando a nova coluna de coordenadas
  )
  
  return(informacoes)
}

# Função principal para coletar dados
pegar_dados <- function(local = "", termo = "", scrolls = 0) {
  cat("\n\n################\n\nAcessando maps...")
  remDr$navigate("https://www.google.com.br/maps")
  Sys.sleep(3)
  
  cat("\n\n################\n\nAjustes da página...")
  search_box <- remDr$findElement(using = "id", value = "searchboxinput")
  Sys.sleep(4)
  search_box$click()
  search_box$clearElement()
  Sys.sleep(2)
  search_box$sendKeysToElement(list(local, key = "enter"))
  Sys.sleep(3)
  
  search_box$click()
  Sys.sleep(3)
  search_box$clearElement()
  search_box$sendKeysToElement(list(termo, key = "enter"))
  Sys.sleep(4)
  
  # Contador de falhas
  contador_falhas_xpath <- 0
  
  # Tentar encontrar o elemento alvo com o SEGUNDO XPath primeiro
  elemento_alvo <- tryCatch({
    remDr$findElement(using = "xpath", "/html/body/div[1]/div[3]/div[8]/div[9]/div/div/div[1]/div[2]/div/div[1]/div/div/div[1]/div[1]/div[3]/div/a")
  }, error = function(e) {
    cat("\n\n👁👄👁 Primeiro XPath não encontrado. Tentando o segundo XPath...\n")
    tryCatch({
      remDr$findElement(using = "xpath", "/html/body/div[1]/div[3]/div[8]/div[9]/div/div/div[1]/div[2]/div/div[1]/div/div/div[1]/div[1]/div[2]/div[3]/div/div/button")
    }, error = function(e) {
      cat("\n\n🚑 Segundo XPath também não encontrado. Incrementando contador de falhas...\n")
      contador_falhas_xpath <<- contador_falhas_xpath + 1
      return(NULL)
    })
  })
  
  # Verificar se o elemento foi encontrado
  if (is.null(elemento_alvo)) {
    cat("\n\n⚠️ Nenhum dos XPaths foi encontrado. Contador de falhas:", contador_falhas_xpath, "\n")

    # Se o contador de falhas atingir um limite, acionar a lógica de regressão de XPath
    if (contador_falhas_xpath >= 2) {
      cat("\n\n🤖 2 falhas consecutivas ao tentar encontrar o elemento. Acionando regressão de XPath...\n")
      
      # Lógica de regressão de XPath
      if (length(xpaths_com_êxito) >= 6) {
        xpath_num <- xpaths_com_êxito[length(xpaths_com_êxito) - 3]  # 3 XPaths antes
        cat('\n\nRegressão de 3 XPath após falha. Coletando a partir do penúltimo XPath anterior com êxito:', xpath_num, "\n")
        
        # Aqui você pode tentar novamente encontrar o elemento com o XPath regredido
        elemento_alvo <- tryCatch({
          remDr$findElement(using = "xpath", xpath_num)
        }, error = function(e) {
          cat("\n\n🚑 XPath regredido também não encontrado. Encerrando a função...\n")
          return(NULL)
        })
      }
    }
  } else {
    # Resetar o contador de falhas se o elemento for encontrado
    contador_falhas_xpath <- 0
    cat("\n\n✅ Elemento encontrado com sucesso. Contador de falhas resetado.\n")
  }
  
  # Se o elemento ainda não foi encontrado após a regressão, encerrar a função
  if (is.null(elemento_alvo)) {
    cat("\n\n🤖 Nenhum XPath funcionou. Encerrando a função...\n")
    return(dados_completos)
  }        
    
  Sys.sleep(4)
  elemento_encontrado <- elemento_alvo$getElementText() %>% .[[1]] %>% str_remove("\n")
  cat("\n\nElemento encontrado:", elemento_encontrado)
  
  remDr$mouseMoveToLocation(webElement = elemento_alvo)
  Sys.sleep(4)
  
  cat("\n\n################\n\nCriando objetos de suporte...")
  
  # XPath base
  base_xpath <- "/html/body/div[1]/div[3]/div[8]/div[9]/div/div/div[1]/div[2]/div/div[1]/div/div/div[1]/div[1]/div["
  final_xpath <- "]/div/a"
  xpath_num <- 3L
  
  # Dataframe para armazenar todas as informações
  dados_completos <- tibble(
    Loja = character(),
    Categoria = character(),
    Endereço = character(),
    Plus_Code = character(),
    Site = character(),
    Celular = character(),
    Estrelas = character()
  )
  
  # Lista para armazenar lojas já coletadas
  lojas_coletadas <- list()
  
  # Lista para armazenar XPaths já coletados com êxito
  xpaths_com_êxito <- c()
  
  # Contador de tentativas de scroll
  tentativas_scroll <- 0
  
  # Contador de elementos não encontrados consecutivos
  contador_nao_encontrado <- 0
  
  # Contador de fenômenos "Não encontrados"
  contador_fenomenos_nao_encontrados <- 0
  
  # Dentro do loop principal
  for (i in seq_len(scrolls)) {
    cat("\n\nLaço nº:", i)
    Sys.sleep(4)
    
    num <- 1L
    novos_elementos_encontrados <- FALSE  # Resetar a variável no início de cada laço
    
    # Contador de rolagens
    if (i %% 30 == 0) {  # A cada 30 rolagens
      cat("\n\n⛔ 30 rolagens realizadas. Ajustando XPath...\n")
      
      # Determinar o XPath inicial para este laço
      if (length(xpaths_com_êxito) >= 6) {
        xpath_num <- xpaths_com_êxito[length(xpaths_com_êxito) - 6]  # 6 XPaths antes
        cat('\n\nColetando a partir do sexto XPath anterior com êxito:', xpath_num, "\n")
      } else {
        xpath_num <- 3L  # Começar do início
      }
    }
    
    # Contador de lojas repetidas
    contador_lojas_repetidas <- 0
    
    while (num <= 40) {
      xpath_completo <- paste0(base_xpath, xpath_num, final_xpath)
      cat("\n\nXPath:", xpath_completo)
      
      elemento_regiao <- tryCatch({
        remDr$findElement("xpath", xpath_completo)
      }, error = function(e) NULL)
      
      if (!is.null(elemento_regiao)) {
        elemento_regiao$clickElement()
        cat("\n\n✅ Clicado!")
        Sys.sleep(15)
        
        informacoes_loja <- coletar_informacoes(remDr)
        
        if (!is.null(informacoes_loja) && nrow(informacoes_loja) > 0 && "Loja" %in% colnames(informacoes_loja)) {
          # Verificar se a loja já existe no banco de dados com todas as informações idênticas
          loja_repetida <- any(sapply(lojas_coletadas, function(loja) {
            all(loja$Loja == informacoes_loja$Loja,
                loja$Categoria == informacoes_loja$Categoria,
                loja$Endereço == informacoes_loja$Endereço,
                loja$Plus_Code == informacoes_loja$Plus_Code,
                loja$Site == informacoes_loja$Site,
                loja$Celular == informacoes_loja$Celular,
                loja$Estrelas == informacoes_loja$Estrelas)
          }))
          
          if (!isTRUE(loja_repetida)) {
            dados_completos <- bind_rows(dados_completos, informacoes_loja)
            lojas_coletadas <- c(lojas_coletadas, list(informacoes_loja))
            xpaths_com_êxito <- c(xpaths_com_êxito, xpath_num)  # Adicionar XPath à lista de XPaths com êxito
            
            contador_nao_encontrado <- 0  # Resetar o contador de elementos não encontrados
            novos_elementos_encontrados <- TRUE  # Marcar que novos elementos foram encontrados
            contador_lojas_repetidas <- 0  # Zerar o contador de lojas repetidas
            cat("\n🙌 Novo elemento encontrado ")
          } else {
            cat("\n\n⚠️ Loja repetida", informacoes_loja$Loja, "\n")
            contador_lojas_repetidas <- contador_lojas_repetidas + 1
            
            if (contador_lojas_repetidas >= 3) {
              cat("\n\n⛔3 lojas repetidas seguidas. Usando último XPath com êxito.\n")
              if (length(xpaths_com_êxito) > -2) {
                xpath_num <- xpaths_com_êxito[length(xpaths_com_êxito)]
                cat("\n\nUsando último XPath com êxito:", xpath_num, "\n")
              }
              contador_lojas_repetidas <- 0  # Zerar o contador de lojas repetidas
            }
          }
        }
      } else {
        cat("\n\n❌ Elemento não encontrado. Pulando para o próximo XPath.\n")
        contador_nao_encontrado <- contador_nao_encontrado + 1  # Incrementa o contador
        
        if (contador_nao_encontrado >= 2) {
          cat("\n\n⛔ 2 elementos não encontrados consecutivos. Executando rolagem...\n")
          cat("\n\nRolando a página...\n\n################")
          for (k in 1:28) {  # Faz 28 rolagens por iteração
            elemento_alvo$sendKeysToElement(list(key = "page_down"))
            Sys.sleep(0.4)
          }
          
          # Verifica se houve duas ocorrências consecutivas
          if (contador_nao_encontrado >= 4) {
            cat("\n\n🎉 Código irá finalizar daqui a pouco. Tenha calma!\n")
            break  # Encerra o loop ou o código inteiro
          }
        }
      }
      
      xpath_num <- xpath_num + 2L  # Avançar para o próximo XPath
      num <- num + 1L
      cat("\n\nLaço!")
    }
    
    # Verificar se novos elementos foram encontrados após o loop interno
    if (!novos_elementos_encontrados) {
      contador_fenomenos_nao_encontrados <- contador_fenomenos_nao_encontrados + 1
      cat("\n\n⚠️ Nenhum novo elemento encontrado após a rolagem. Contador de fenômenos 'Não encontrados':", contador_fenomenos_nao_encontrados, "\n")
    } else {
      contador_fenomenos_nao_encontrados <- 0  # Resetar o contador se novos elementos forem encontrados
      cat("\n\n✅ Novos elementos encontrados. Contador de fenômenos 'Não encontrados' resetado.\n")
    }
    
    if (contador_fenomenos_nao_encontrados >= 1) {
      cat("\n\n🤖 2 fenômenos consecutivos sem novos elementos. Encerrando a função...\n")
      return(dados_completos)
    }
    
    cat("\n\nRolando a página...\n\n################")
    for (k in 1:28) {  # Faz 28 rolagens por iteração
      elemento_alvo$sendKeysToElement(list(key = "page_down"))
      Sys.sleep(0.4)
    }
    
    # Adicionar uma pequena espera após o scroll
    Sys.sleep(10)  # Espera de 10 segundos após a rolagem da página
    
    # Regressão de 3 XPaths após cada rolagem
    if (length(xpaths_com_êxito) >= 6) {
      xpath_num <- xpaths_com_êxito[length(xpaths_com_êxito) - 3]  # 3 XPaths antes
      cat('\n\nRegressão de 3 XPath após rolagem. Coletando a partir do penultimo XPath anterior com êxito:', xpath_num, "\n")
    }
    
    # Adicionar uma pequena espera após o scroll
    Sys.sleep(4)
  }
  
  cat("\n\nFim da função💕")
  return(dados_completos)
}

# Chamar a função para coletar dados
dados_lojas_feira <- pegar_dados(local = "Petrolina", termo = "Fornecedor de produtos de limpeza", scrolls = 3)

# Função personalizada para remover duplicatas considerando múltiplas colunas
remover_duplicatas <- function(dados, colunas) {
  dados %>%
    group_by(across(all_of(colunas))) %>%
    filter(row_number() == 1) %>%
    ungroup()
}

# Colunas que devem ser consideradas para identificar duplicatas
colunas_para_verificar <- c("Loja", "Categoria", "Endereço", "Plus_Code", "Site", "Celular", "Estrelas")

# Remover duplicatas considerando as colunas especificadas
dados_lojas_feira <- remover_duplicatas(dados_lojas_feira, colunas_para_verificar)

# Exportar para Excel
write_xlsx(dados_lojas_feira, "loc_dados_lojas_Fornecedor_Limpeza_petrolina.xlsx")

