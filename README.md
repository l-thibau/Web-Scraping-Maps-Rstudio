# Web Scraper do Google Maps 🗺️

Este repositório contém um web scraper desenvolvido em R para coletar informações de estabelecimentos listados no Google Maps. O script é funcional, mas admite melhorias futuras. Abaixo, descrevo o funcionamento do código e suas principais características.

---

## 📚 Bibliotecas Utilizadas

- **`RSelenium`**: Para automatizar a interação com o navegador e coletar dados dinâmicos.
- **`tidyverse`**: Para manipulação de dados e organização em tibbles.
- **`writexl`**: Para exportar os dados coletados para um arquivo Excel.

---

## 🛠️ Funcionamento do Código

### 1. **Conexão com o Selenium** 🤖
 
O código inicia conectando-se ao Selenium Server rodando em um contêiner Docker. O navegador utilizado é o Firefox.

### 2. Coleta de Informações 📋

A função coletar_informacoes é responsável por extrair os dados de cada estabelecimento, como:

    Nome 🏷️

    Categoria 🏪

    Endereço 🏠

    Plus Code 🔢

    Site 🌐

    Telefone 📱

    Avaliação (estrelas) ⭐

Cada informação é coletada usando XPath, e há tratativas de erro para lidar com casos em que os dados não estão disponíveis.

### 3. Navegação e Coleta Principal 🕵️‍♂️

A função pegar_dados realiza a navegação no Google Maps, busca os estabelecimentos com base em um termo e localização, e coleta as informações de cada um. O script faz rolagens na página para carregar mais resultados e evita duplicatas.

### 4. Remoção de Duplicatas e Exportação 📤

Após a coleta, os dados são limpos para remover duplicatas com base em todas as colunas relevantes. Em seguida, os dados são exportados para um arquivo Excel.

### 📦 Como Usar

    Instale o Docker e inicie o Selenium Server.

    Execute o script R para coletar os dados.

    Verifique o arquivo Excel gerado com os dados coletados.

### 🙌 Contribuições

Contribuições são bem-vindas! Sinta-se à vontade para abrir issues e pull requests.

Feito com ❤️ por [@l-thibau e @tutzlima]. Espero que seja útil! 🚀

