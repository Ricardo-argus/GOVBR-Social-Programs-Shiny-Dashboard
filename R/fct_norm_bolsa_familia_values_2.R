fct_norm_bolsa_familia_values_2 <- function(con){

  valores <- dbGetQuery(con,"
    SELECT top.estado, top.total_bvg FROM (
      SELECT SUM(bsf.qtd_ben_bvg) as total_bvg, bf.estado
      FROM bolsa_familia_ibge bf
      LEFT JOIN benef_somafamiliares bsf ON bsf.id_familias = bf.id_familias
      GROUP BY bf.estado
      ORDER BY total_bvg DESC
    ) top;
  ")

  habitantes <- dbGetQuery(con,"SELECT estado, populacao FROM habitantes_2022")

  # padronizar nomes
  valores$estado <- tolower(valores$estado)
  habitantes$estado <- tolower(habitantes$estado)

  dados <- merge(valores, habitantes, by = "estado")

  dados$percentual_bvg <- (dados$total_bvg / dados$populacao) * 100

  # ordenar pelo percentual top 5
  dados <- head(dados[order(-dados$percentual_bvg), ],10)

  return(dados)
}
