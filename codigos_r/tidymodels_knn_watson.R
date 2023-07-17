library(tidymodels)
library(glue)
library(dplyr)
library(ggplot2)
library(patchwork)

# Resolvendo eventuais conflitos entre tidymodels e outros pacotes eventualmente
# carregados:
tidymodels::tidymodels_prefer()

# Lendo a base de dados:
url <- "https://github.com/prdm0/dados/raw/main/dados_expectativa_renda.RData"
arquivo_temp <- tempfile()
download.file(url = url, destfile = arquivo_temp)
load(arquivo_temp)

dados_expectativa_renda <-
  dados_expectativa_renda |>
  dplyr::select(-CountryName)

# Divisão da base de dados ------------------------------------------------
# Divisão inicial (treino e teste)
splits <- 
  rsample::initial_split(
    dados_expectativa_renda,
    strata = "LifeExpectancy", 
    prop = 0.8 
  )

# O conjunto de dados de treinamento será utilizado para ajustar/treinar o
# modelo:
treinamento <- rsample::training(splits)

# O conjunto de teste será utilizado apenas no fim, para avaliar o desempenho
# preditivo final do modelo:
teste <- rsample::testing(splits) 

# Criando uma receita para os dados ---------------------------------------

# Poderia ter passado o conjunto treinamento ou posso passar o conjunto "splits".
# O comando recipes já entende que deverá utilizar o conjunto de treinamento.
receita <- 
  recipes::recipe(formula = LifeExpectancy ~ ., data = treinamento) |> 
  step_YeoJohnson(all_predictors()) |> # Ajuda na normalização dos dados. Pode ser bom!
  step_normalize(all_predictors()) # Normalizando variáveis numéricas.

# Construindo modelo kNN --------------------------------------------------
modelo_knn <- 
  parsnip::nearest_neighbor(neighbors = tune()) |> 
  parsnip::set_mode("regression") |> 
  parsnip::set_engine("kknn")

# Construindo um workflow (pipeline) --------------------------------------
wf_knn <-
  workflows::workflow() |>
  workflows::add_recipe(receita) |>
  workflows::add_model(modelo_knn)

# Cross-validation --------------------------------------------------------
cv <- 
  treinamento |> 
  rsample::vfold_cv(v = 10L, strata = "LifeExpectancy")

# Busca do hiperparâmetro k -----------------------------------------------
metrica <- metric_set(rmse)

tunagem <-
  wf_knn |> 
  tune::tune_grid(
    resamples = cv,
    grid = 100,
    metrics = metrica,
    control = control_grid(save_pred = TRUE, verbose = TRUE)
  )

autoplot(tunagem) +
  labs(title = "KNN - Seleção do número k (vizinhos)", subtitle = "Sintonização do hiperparâmetro (valor de k)") +
  theme(
    title = element_text(face = "bold")
  )

# Atualizando workflow ----------------------------------------------------
wf_knn <- 
  wf_knn |> 
  finalize_workflow(select_best(tunagem))

# Ajustar o modelo ao conjunto de treinamento e avaliar no teste --------
ajuste_final <- last_fit(wf_knn, splits)

# Ajuste final com toda a base de dados -----------------------------------
modelo_final <- fit(wf_knn, data = dados_expectativa_renda)