#' Create table 7.7
#'
#' @importFrom mirt mirt
#'

create_table_7.7 <- function(){

  df <- readRDS("inst/df_t1.rds")

  set.seed(123)
  fit_1 <- mirt::mirt(df, 1, technical = list(NCYCLES=2000))
  set.seed(123)
  fit_2 <- mirt::mirt(df, 2, technical = list(NCYCLES=3000))

  res <- get_fit_index(fit_1, model_name = "1-faktoriell")
  res <- rbind(res, get_fit_index(fit_2, model_name = "2-faktoriell"))

  res

}

#' Create table 7.8
#'

create_table_7.8 <- function(){

  models <- readRDS("inst/models/models_27items.rds")

  result <- get_fit_index(models$model_1, model_name = "1-faktoriell")
  result <- rbind(result, get_fit_index(models$model_2, model_name = "2-faktoriell"))
  result <- rbind(result, get_fit_index(models$model_3, model_name = "3-faktoriell"))

  return(result)

}

#' Create table 7.9
#'

create_table_7.9 <- function(){

  models <- readRDS("inst/models/models_27items.rds")

  report_loadings(models$model_2)

}

#' Create table 7.10
#'

create_table_7.10 <- function(){

  models <- readRDS("inst/models/models_27items.rds")

  report_loadings(models$model_3)

}

#' Create table 7.11
#'

#'
create_table_7.11 <- function(){

  models <- readRDS("inst/models/models_27items.rds")

  report_item_fit(models$model_2)

}

#' Create table 7.12
#'
create_table_7.12 <- function(){

  models <- readRDS("inst/models/models_23items.rds")

  result <- get_fit_index(models$model_1, model_name = "1-faktoriell")
  result <- rbind(result, get_fit_index(models$model_2, model_name = "2-faktoriell"))
  result <- rbind(result, get_fit_index(models$model_3, model_name = "3-faktoriell"))

  return(result)

}

#' Create table 7.13
#'
create_table_7.13 <- function(){

  models <- readRDS("inst/models/models_23items.rds")

  report_loadings(models$model_2)

}

#' Create table 7.14
#'
create_table_7.14 <- function(){

  models <- readRDS("inst/models/models_23items.rds")

  report_item_fit(models$model_2)

}

#' Create table 7.15
#'
create_table_7.15 <- function(){

  models <- readRDS("inst/models/models_22items.rds")

  result <- get_fit_index(models$model_1, model_name = "1-faktoriell")
  result <- rbind(result, get_fit_index(models$model_2, model_name = "2-faktoriell"))
  result <- rbind(result, get_fit_index(models$model_3, model_name = "3-faktoriell"))

  return(result)

}
