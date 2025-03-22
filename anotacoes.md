# Anotações

Aqui consta meus delírios enquanto leio os scripts, testo algumas coisas, altero aqui e ali, enfim.

## Mudanças gerais

-   Criei o projeto `Web-Scraping-Maps-Rstudio.Rproj` para corrigir os caminhos *futuramente*, não fiz ainda

-   Adicionei os scripts `definicoes.R` e `funcoes.R`:

    -   `definicoes.R`

1)  Chama as bibliotecas necessárias

2)  Cria os objetos de suporte (utilizados nas funções)

3)  Abre o RSelenium

-   `funcoes.R`

Contêm todas as funções utilizadas nos scripts `coleta_singular.R`, no momento

-   Marquei alguns dos trechos problemáticos que notei com uns (*muitos*) `💥`. Esse trecho da função `pegar_dados()`, por exemplo:

```{r}

# 💥💥💥💥💥💥💥💥💥💥💥💥💥💥💥💥💥💥💥💥💥💥💥💥💥💥💥💥💥💥
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
  
```

Os trechos marcados contêm ou algum problema lógico no *debug* do código, no próprio processo de coleta ou usa de um método ruim (ineficiente ou que ***dificulta*** nossa vida).

No trecho de exemplo, além do *tamanho* (há vários `if`s ENORMES) e aninhamento de `if`s (PÉSSIMO pra debugar, pra entender a lógica, pra dar manutenção), há dois erros no *debug* do código, pois a mensagem `🤖 2 falhas consecutivas ao tentar encontrar o elemento. Acionando regressão de XPath...` se repete de forma indevida, pelo que vi, e parece que toda vez que o processo simplesmente inicia bem, a mensagem `✅ Elemento encontrado com sucesso. Contador de falhas resetado.` é printada (não faz sentido dizer que "Contador de falhas" foi resetado aqui, mas toda vez que laço repetir, não? Não investiguei essa lógica tão a fundo, foi o que reparei observando a coleta rodando e lendo o que os meus olhos aguentaram, visão de velho).

------------------------------------------------------------------------

## Mudanças específicas

### `pegar_dados()`

#### Laço `for()` ❌ `while()`

A função é criada da seguinte forma:

`pegar_dados \<- function(local = "", termo = "", scrolls = 0)`

O problema é que o argumento "scrolls" não está funcionando *como deveria*, dado o nome do argumento. O papel do `for()` e `while()` estão confusos!

-   `scrolls` deve ser adaptado ou retirado (investigue essa lógica)

-   Deve-se criar um parâmetro *opcional* da função chamado `quantidade_estabalecimentos`, por exemplo, que seria o número de `while()` (acredito que isso seja o mais próximo que seu número signifique, pelo que vi)

-   O número padrão de estabelecimentos coletados poderiam ser 5 ou 10, como mera *demonstração* dos resultados da coleta (pois o processo todo é demorado)

Do que vi por cima hoje, é claro que precisa reconstruir a lógica de verificações, muitos `if`s aninhados [^1] (alguns desnecessários, outros numa ordem não tão lógica) — acho que a maioria pode ser substituída por um `case_when()`, `if`s *individuais* ou associados por objetos momentâneos — e há algumas verificações desnecessárias em outras partes.

[^1]: Aninhados e com TRY CATCH AINDA, aí pronto, aí vira uma loucura pra ler. Fora que não há QUEBRAS DE LINHAS ENTRE AS COISAS!!! TÁ COM DÓ DE GASTAR LINHA, JOVEM????????? PROGRAME PENSANDO NO SER HUMANO!!!!!!!!!!!!!!!!!!!

------------------------------------------------------------------------

### Dúvidas pq né, o cabra né de ferro

Por que diabos `remover_duplicatas()` é assim:

```{r}

remover_duplicatas <- function(dados, colunas) {
  dados %>%
    group_by(across(all_of(colunas))) %>%
    filter(row_number() == 1) %>%
    ungroup()
}

```

Ela remove apenas as colunas indicadas, *por que?* Por que não remover casos **totalmente** exatos, ou pelo nome, que sempre tem?

Fora que esse `filter(row_number() == 1)` não garante que pegou o **melhor** caso coletado. Ele manterá apenas a primeira ocorrência. Então se a primeira observação tiver 5 das 8 variáveis preenchidas, o segundo (que pode ter mais) não será pego.

A minha sugestão é que, antes de fazer isso, ordene os dados, tipo:

```{r}

remover_duplicatas_2 <- function(dados, colunas = NULL) {

  if (is.null(colunas)) { # caso cujas colunas não são passadas
  
    dados %>%
      arrange(desc(rowSums(is.na(dados)))) %>%  # Ordena de forma que as linhas com menos NAs apareçam primeiro
      distinct() %>%  # Remove duplicatas em TODAS as colunas
      ungroup()
  
  } else { # caso cujas colunas são passadas
  
    dados %>%
      arrange(desc(rowSums(is.na(dados)))) %>%  # Ordena de forma que as linhas com MENOS NAs apareçam primeiro
      group_by(across(all_of(colunas))) %>%  # Agrupa pelas colunas especificadas
      filter(row_number() == 1) %>%  # Mantém a primeira linha de cada grupo
      ungroup()  # Remove o agrupamento
  }
  
}


```

Nesse caso que verifiquei, funcionou normal:

```{r}

# Chamar a função para coletar dados --------------------------------------

dados_lojas_feira <- pegar_dados(local = "Petrolina", termo = "Fornecedor de produtos de limpeza", scrolls = 2, num_while = 10)

# Remover duplicatas considerando as colunas especificadas --------------

remover_duplicatas(dados_lojas_feira, colunas_para_verificar) %>%
  glimpse()

remover_duplicatas_2(dados_lojas_feira, colunas_para_verificar) %>%
  glimpse()

remover_duplicatas_2(dados_lojas_feira) %>%
  glimpse()

```
