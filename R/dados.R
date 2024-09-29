#' @title Dados para o Projeto 2
#'
#' @description
#' Dados criados para serem usados nas funções de Regressão Linear Múltipla desenvolvidas para o Projeto 2.
#'
#' @format Objeto \code{data.frame} com 4 colunas:
#'
#' \describe{
#' \item{x1, x2, x3}{Variáveis explicativas geradas pela distribuição Uniforme}
#' \item{y}{Variável resposta gerada por uma exponencial}
#' }
#'
#' @source Dados gerados através de simulações.
#'
#' @details
#' Os dados seguem as suposições de regressão linear, sendo elas: linearidade entre as variáveis dependentes e independente; ausência de correlação entre as variáveis dependentes; resíduos apresentam distribuição normal.
#'
#' @examples
#'  data(df)
#'  df[4] #retorna a Variável Resposta
"df"

