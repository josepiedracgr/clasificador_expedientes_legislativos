library(readr)

expedientes_train <- read_csv("expedientes_train.csv",
  col_types = cols(
    `Número de Expediente` = col_character(),
    `Fecha de inicio` = col_date(format = "%d/%m/%Y")
  ),
  skip = 1
)

prepExpedientesTrain <- function(expedientes) {
  
  expedientes_preproc <- expedientes |>
    mutate(`Proyecto de interés` = 1*`Proyecto de interés`) |>
    filter(!is.na(`Título de Expediente`), !is.na(`Proyecto de interés`))
  
  return(expedientes_preproc)
}

expedientes_train <- prepExpedientesTrain(expedientes_train)
write_rds(expedientes_train, "expedientes_train.RDS")
