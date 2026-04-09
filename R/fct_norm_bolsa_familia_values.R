fct_norm_bolsa_familia_values <- function(con){

  valores <- dbGetQuery(con,"
    SELECT bf.estado, SUM(bp.qtd_ben_bpi) as total_bpi
    FROM bolsa_familia_ibge bf
    LEFT JOIN benef_primeirainfancia bp ON bp.id_familias = bf.id_familias
    GROUP BY bf.estado
    ORDER BY total_bpi DESC
  ")

  habitantes <- dbGetQuery(con,"SELECT estado, populacao FROM habitantes_2022")

  # padronizar nomes
  valores$estado <- tolower(valores$estado)
  habitantes$estado <- tolower(habitantes$estado)

  dados <- merge(valores, habitantes, by = "estado")

  dados$percentual_bpi <- (dados$total_bpi / dados$populacao) * 100

  # ordenar pelo percentual top 5
  dados <- head(dados[order(-dados$percentual_bpi), ],5)

  return(dados)
}
