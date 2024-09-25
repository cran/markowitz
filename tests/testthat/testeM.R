# Testes unitários


library(tidyverse)
library(testthat)
library(dplyr)  # Certifique-se de que dplyr está carregado


test_that("markowitzcalc returns the correct results", {

  criterios <- data.frame(
    criterio = c('a1', 'a2', 'a3'),
    peso = c(0.25, 0.5, 0.25)
  )

  alternativas <- data.frame(
    alternativas = c('outdoor', 'televisao', 'jornal'),
    a1 = c(12, 36, -3),
    a2 = c(-6, 12, 60),
    a3 = c(24, 48, 30)
  )

  # Chamada da função a ser testada
  result <- markowitzcalc(criterios, alternativas)

  result <- markowitzcalc(criterios, alternativas, lambda_selec = 2)



  expected_result <- data.frame(
    "Compara.A.k." = c('televisao', 'televisao', 'jornal'),
    "Com.A.i." = c('jornal', 'outdoor', 'outdoor'),
    "Delta.Esperanca" = c(-9.75, 21, 30.75),
    "Delta.Desvio" = c(-10.4247605,2.8605352, 13.2852957),
    "Delta.Esp.Delta.Desvio" = c(0.94, 7.34,2.31),
    "Preferencia.da.Acao" = c('jornal', 'televisao', 'jornal'),
    stringsAsFactors = FALSE
  )
  # Convert any factor columns to character
  result[] <- lapply(result, function(x) if (is.factor(x)) as.character(x) else x)

  # Ignore differences in attributes like row names
  expect_equal(result, expected_result, ignore_attr = TRUE)
})

