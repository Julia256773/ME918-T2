
#' @title Predição de uma Regressão Linear
#'
#' @description
#' A função tem como objetivo fazer uma prediçaõ a aprtir de um modelo de regressão linear feito através da função reg_lin
#'
#'
#' @param modelo saída da funçaõ reg_lin
#' @param valores matriz com uma linha para cada valor predito desejado e cada coluna o valor da variável preditora na ordem e que o resultado é dado na saída do modelo
#'
#' @details
#' É necessário que
#'
#' @return Retorna uma lista com os resultados dos valores preditos
#'
#' @examples
#' modelo = reg_lin(pacote2::df, "y", c("x1", "x2"))
#' valores = matrix(c(1, 5,
#'                    2, 6,
#'                    3, 7), nrow = 3, byrow = TRUE)
#' predicts(modelo, valores)
#'
#' @export
#'


predicts = function(modelo, valores){
  resultado = list()
  for(i in 1:nrow(valores)){
    pred = modelo$coeficientes$coeficientes[1]
    for(k in 2:nrow(modelo$coeficientes)){
      pred = pred + modelo$coeficientes$coeficientes[k]*valores[i ,k-1]
    }
    resultado = c(resultado, pred)
  }
  return(resultado)
}
