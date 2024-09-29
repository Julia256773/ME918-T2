#' @title Gráfico de Valores Preditos vs Observados
#'
#' @description
#' A função faz um gráfico cujos eixos correspondem aos valores observados da variável resposta de um banco de dados, e aos valores preditos através de uma regressão linear do mesma variável.
#'
#' @param modelo Saída de uma função de regressão linear
#' @param x_var Nome da variável correspondente ao eixo X
#' @param fixas Lista das outras variáveis do modelo que assumem um valor fixo especificado
#' @param dados Banco de dados utilizado na regressão linear feita previamente
#'
#' @details
#' É necessário que x_var seja no formato Character e fixas seja uma lista na forma list(xi = valor, xj = valor).
#'
#' @return Retorna um gráfico de valores preditos vs valores observados, adicionado de uma reta de regressão calculada entre os dois vetores.
#'
#' @examples
#' graf_pvo(modelo, x_var = "x1", fixas = list(x2 = 0.5, x3 = 0.5), dados = df)
#' graf_pvo(modelo, "x2", list(x1 = 0.7, x3 = 0.1), df)
#' graf_pvo(saida_regressao_linear, "x3", fixas = list(x1 = 0.5, x2 = 0.9), df)
#'
#' @import ggplot2
#'
#' @export
#'

graf_pvo = function(modelo, x_var, fixas = list(), dados) {

  # Verifica se os preditos têm o mesmo tamanho que os dados
  if (length(modelo$preditos) != nrow(dados)) stop("O vetor de preditos deve ter o mesmo comprimento que os dados.")

  dados$predito = modelo$preditos # Adiciona os valores preditos no conjunto de dados pra fazer o gráfico

  # Cria uma base de dados para ser possível fazer a reta de regressão
  grid_dados = data.frame(x_var = seq(min(dados[[x_var]]), max(dados[[x_var]]), length.out = length(dados[[x_var]])))
  names(grid_dados)[1] = x_var

  # Adiciona os valores fixos para as demais variáveis no grid de dados
  for (var in names(fixas)) {
    grid_dados[[var]] = fixas[[var]]
  }

  # Adiciona o intercepto e as variáveis no grid de dados para fazer a reta de regressão
  matrix_grid = cbind(1, grid_dados)  # Intercepto
  coef = modelo$coeficientes$coeficientes
  grid_dados$predito = as.vector(as.matrix(matrix_grid) %*% coef)  # Calcula preditos pra reta

  grafico = ggplot(dados, aes_string(x = x_var, y = "predito")) +
    geom_point(color = "black", size = 2) +  # Pontos preditos vs observados
    geom_line(data = grid_dados, aes_string(x = x_var, y = "predito"), color = "blue", size = 1) +  # Reta de regressão
    labs(x = paste("Valores de", x_var), y = "Valores Preditos") +
    theme_bw() +
    ggtitle("Gráfico de Valores Observados vs Preditos")

  return(grafico)
}
