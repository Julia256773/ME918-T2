test_that("não é string", {
  expect_error(reg_lin(df, y, c(x1,x2,x3)))
})

test_that("vetores de mesmo comprimento", {
  expect_length(reg_lin(df, "y", c("x1", "x2", "x3"))$residuos, length(reg_lin(df, "y", c("x1", "x2", "x3"))$preditos))
})

test_that("retorna uma lista", {
  expect_type(reg_lin(df, "y", c("x1", "x2", "x3")), "list")
})

test_that("preditora de constantes", {
  x1 = c(1,2,3,4,5,6,7,8,9)
  x2 = c(4,4,4,4,4,4,4,4,4)
  y = c(1,2,5,3,6,4,7,9,6)
  data = data.frame(y, x1, x2)
  expect_error(reg_lin(data, "y", c("x1","x2")))
})

test_that("retorna uma lista", {
  modelo = reg_lin(df, "y", c("x1", "x2"))
  valores = matrix(c(7, 6), nrow = 1, byrow = TRUE)
  expect_type(predicts(modelo, valores), "list")
})

test_that("resíduos nulos", {
  x1 = c(13, 21, 33)
  x2 = c(20, 37, 49)
  y = 5 + 2*x1 + 3*x2
  teste <- data.frame(y, x1, x2)
  expect_equal(round(reg_lin(teste, "y", c("x1", "x2"))$preditos), y)
})

test_that("posto incompleto", {
  x1 = c(1, 2, 3)
  x2 = c(2, 4, 6)
  y = c(20, 15, 1)
  teste <- data.frame(y, x1, x2)
  expect_error(reg_lin(teste, "y", c("x1", "x2")))
})

test_that("valores incompletos", {
  modelo = reg_lin(df, "y", c("x1", "x2"))
  valores = matrix(c(1, 5, 2), nrow = 3, byrow = TRUE)
  expect_error(predicts(modelo, valores))
})

test_that("predição completa", {
  modelo = reg_lin(df, "y", c("x1", "x2", "x3"))
  valores = matrix(c(1, 5, 2, 6, 3, 7, 4, 9, 8), nrow = 3, byrow = TRUE)
  expect_length(predicts(modelo, valores), ncol(valores))
})
