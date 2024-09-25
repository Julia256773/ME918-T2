#' @title Cálculo de Regressão Linear
#'
#' @description
#' A função tem como objetivo aplicar o cálculo da regressão linear simples a um banco de dados
#'
#'
#' @param dados Banco de dados a ser utilizado
#' @param Y string com o nome da Variável resposta da forma como está no banco
#' @param Xs vetor de strings com os nomes das variáveis preditoras
#'
#' @details
#' É necessário que tanto a variável resposta quanto as variáveis preditoras sejam numéricas e tenham o mesmo tamanho
#'
#' @return Retorna uma lista com 3 objetos: um dataframe de nome "coeficientes" com os valores dos betas calculados, um vetor "preditos" com os valores preditos pelo modelo e um vetor "residuos" com os erros dos valores preditos.
#'
#' @examples
#' reg_lin(df, "y", c("x1", "x2", "x3"))
#' reg_lin(df, "y", c("x1", "x3"))
#' reg_lin(df, "y", c("x2"))
#'
#' @export
#'


reg_lin = function(dados, Y, Xs){

  if (!is.character(Y) || !is.character(Xs[1])) { stop("Erro: A resposta ou preditoras não estão no formato de strings") }

  # DATAFRAME COM COEFICIENTES
  #calculo dos betas
  um = rep(1, nrow(dados))
  matrix_X = c(um)
  i=1
  for (i in 1:length(Xs)){ matrix_X = cbind(matrix_X, dados[[Xs[i]]])
  i=i+1
  }
  matrix_X = as.matrix(matrix_X)
  for(i in 2:ncol(matrix_X)){ if(all(matrix_X[1,i] == matrix_X[,i])){ stop("Uma das preditoras é constante.") }}
  if(ncol(matrix_X) < nrow(matrix_X)){posto = ncol(matrix_X)}else{posto = nrow(matrix_X)}
  if(posto != qr(matrix_X)$rank){stop("A matriz tem posto incompleto.")}
  matrix_Y = as.matrix(dados[[Y]])
  coeficientes = solve(t(matrix_X)%*%matrix_X)  %*%t(matrix_X) %*% matrix_Y
  #criando dataframe
  betas = paste0("beta ", 0:(ncol(matrix_X)-1))
  nomes = append(c("intercepto"), Xs)
  coeficientes = as.vector(coeficientes)
  dataframe = data.frame(betas, nomes, coeficientes)


  # LISTA COM VALORES PREDITOS
  preditos = as.vector(matrix_X %*% as.matrix(coeficientes))


  # LISTA COM OS RESÍDUOS
  residuos = dados[[Y]]-preditos

  return(list(coeficientes = dataframe, preditos = preditos, residuos = residuos))

}
