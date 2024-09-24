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
