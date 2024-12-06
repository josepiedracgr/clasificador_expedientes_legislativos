suppressMessages(library(tidyverse))
library(rvest)
library(xml2)
library(tm)

encontrarTablaExpedientes <- function(html) {
  
  cols <- c("Número de Expediente", "Título de Expediente")
  tablas <- html_table(html)
  
  for(tabla in tablas) {
    if(all(cols %in% colnames(tabla))) {
      tabla_proyectos <- tabla |>
        select(all_of(cols)) |>
        slice(1:10)
      return(tabla_proyectos)
    }
  }
  
  warning("No se encontró la tabla de proyectos de ley!")
  return(NULL)
}

crearCorpus <- function(titulos) {
  
  corpus <- Corpus(VectorSource(titulos))
  
  corpus_preproc <- corpus |>
    tm_map(content_transformer(tolower)) |>
    tm_map(removePunctuation) |>
    tm_map(removeWords, stopwords("spanish")) |>
    tm_map(stripWhitespace)
  
  return(corpus_preproc)
}

crearDTM <- function(corp, sparse_val=0.95, as_matrix=T) {
  
  dtm <- DocumentTermMatrix(corp) |>
    removeSparseTerms(sparse_val)
  
  if(as_matrix) {
    return(as.matrix(dtm))
  } else {
    return(dtm)
  }
  
}

separarTrainTest <- function(exp_train, exp_nuevos) {
  
  expedientes_total <- c(exp_train$`Título de Expediente`,
                         exp_nuevos$`Título de Expediente`)
  corpus_expedientes <- crearCorpus(expedientes_total)
  dtm_expedientes <- crearDTM(corpus_expedientes,0.99)
  
  train <- dtm_expedientes[1:nrow(exp_train),]
  labs <- exp_train$`Proyecto de interés`
  test <- dtm_expedientes[-(1:nrow(exp_train)),]
  
  return(list(
    test=test, train=train, labs=labs
  ))
}

entrenarClasificador <- function(train_dtm, labs, step=F) {
  
  stopifnot(nrow(train_dtm)==length(labs))
  
  model_matrix <- as_tibble(train_dtm) |>
    mutate(y=labs)
  
  model <- glm(y ~ ., data=model_matrix, family="binomial")
  if(step) {
    message("Seleccionando el mejor modelo (AIC)... puede tardar varios minutos")
    tictoc::tic()
    model <- step(model)
    write_rds(model, "clasificador.RDS")
    tictoc::toc()
  }
  
  return(model)
}

predecirInteres <- function(modelo, test_data) {
  
  if(!("data.frame" %in% class(test_data))) {
    test_data <- as_tibble(test_data)
    warning("test fue convertido a 'data.frame'.")
  }
  
  pred <- predict(modelo, newdata=test_data, type="response")
  
  return(pred)
}

mostrarProyectosInteresantes <- function(exp_nuevos, pred, umbral=0.4) {
  
  proyectos <- exp_nuevos |>
    mutate(prob_interes=pred) |>
    filter(prob_interes>=umbral) |>
    arrange(desc(prob_interes))
  
  if(nrow(proyectos)==0) {
    message("Ninguno de los proyectos parece ser de interés...")
    return(NULL)
  }
  
  return(proyectos)
}

runner <- function() {

  # enlace de nuevos expedientes
  consultas_url <- "https://consultassil3.asamblea.go.cr/frmConsultaProyectos.aspx"
  consultas_html <- consultas_url |>
    read_html()
  
  # expedientes nuevos
  expedientes_nuevos <- encontrarTablaExpedientes(consultas_html)
  
  # expedientes viejos
  if(file.exists("expedientes_train.RDS")) {
    expedientes_train <- read_rds("expedientes_train.RDS")
  } else {
    source("prep.R")
  }
  
  model_data <- separarTrainTest(expedientes_train, expedientes_nuevos)
  
  if(file.exists("clasificador.RDS")) {
    modelo <- read_rds("clasificador.RDS")
  } else {
    modelo <- with(model_data, entrenarClasificador(train,labs,step=T))
  }
  
  pred <- predecirInteres(modelo, model_data$test)
  proyectos_interesantes <- mostrarProyectosInteresantes(expedientes_nuevos, pred)
  
  return(proyectos_interesantes)
  
}

runner()
