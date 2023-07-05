library(rsample)
library(ggplot2)
library(tibble)
library(purrr)

regressao_verdadeira <- function(x)
  (1 + 10*x - 5*x^2)*atan(10*x^3) 

gerar_observacoes <- function(n, desvio_padrao = 0.3) {
  # Permitindo que o mesmo x possa ter dois pontos de y, como ocorre na 
  # pratica
  seq_x <- sample(seq(0, 1, length.out = n), size = n, replace = TRUE)
  
  step <- function(x)
    regressao_verdadeira(x) + rnorm(n = 1L, mean = 0, sd = desvio_padrao)
  
  tibble::tibble(x = seq_x, y = purrr::map_vec(.x = seq_x, .f = step))
}

# Usaremos uma regressão polinomial para tentar ajustar à regressão -------
regressao_polinomial <-
  function(grau = 1L,
           dados) {
    
    ajuste <- 
      lm(
        formula = y ~ poly(x, degree = grau),
       data = dados
      )
    
    # Renomeando os coeficientes estimados pelo método de mínimos quadrados
    names(ajuste$coefficients) <- paste0("beta_", 0L:grau) 
    ajuste$coefficients
  }

plotando <- function(dados){
  dados |>  
    ggplot(aes(x = x, y = y)) +
    geom_point()
}

d <- gerar_observacoes(n = 50, desvio_padrao = 0.3)

d |> ggplot(aes(x = x, y = y)) +
  geom_point()

mc <- function(){
  step <- function(d){
    dados <- gerar_observacoes(n = 30L)
    regressao_polinomial(grau = d, )
  }
  
  purrr::map(.x = 2L:)
  
}