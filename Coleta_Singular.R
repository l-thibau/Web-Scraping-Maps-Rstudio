# Chamar a função para coletar dados --------------------------------------

dados_lojas_feira <- pegar_dados(local = "Petrolina", termo = "Fornecedor de produtos de limpeza", scrolls = 2, num_while = 10)

# Remover duplicatas considerando as colunas especificadas --------------

remover_duplicatas(dados_lojas_feira, colunas_para_verificar) %>%
  glimpse()

remover_duplicatas_2(dados_lojas_feira, colunas_para_verificar) %>%
  glimpse()

remover_duplicatas_2(dados_lojas_feira) %>%
  glimpse()

# Exportar para Excel -----------------------------------------------------

write_xlsx(dados_lojas_feira, "loc_dados_lojas_Fornecedor_Limpeza_petrolina.xlsx")

