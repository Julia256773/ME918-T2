
#' @title Predição de uma Regressão Linear
#'
#' @description
#' A função tem como objetivo fazer uma predição a partir de um modelo de regressão linear feito através de uma função de regressão linear.
#'
#' @param modelo Saída da função reg_lin.
#' @param valores Matriz em que cada linha correponde aos valores preditos desejados, e cada coluna a variável preditora na ordem em que o resultado é dado na saída do modelo.
#'
#' @details
#' É necessário que os valores sejam numéricos e possuam pelo menos um valor de predição para cada variável pertencente ao modelo inserido na função.
#'
#' @return Retorna uma lista com os resultados dos valores preditos.
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
  if(ncol(valores) != nrow(modelo$coeficientes) - 1){stop("O número de parâmetros fornecido e do modelo são diferentes.")}
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
