#' Run model with 27 items
#'
#' @param save_model if TRUE, then the results are saved under inst/models. File name: models_27items.rds
#'
#' @importFrom mirt mirt
#' @return a list of model objects returned by mirt::mirt()
#'

run_exploratory_mirt_27items <- function(save_model = FALSE){

  df <- readRDS("inst/df_explore.rds")

  set.seed(123)
  fit1 <- mirt::mirt(df, 1, technical = list(NCYCLES=2000))
  set.seed(123)
  fit2 <- mirt::mirt(df, 2, technical = list(NCYCLES=3000))
  set.seed(123)
  fit3 <- mirt::mirt(df, 3, technical = list(NCYCLES=3000))

  result <- list(model_1 = fit1, model_2 = fit2, model_3 = fit3)

  if (isTRUE(save_model)) {
    saveRDS(result, "inst/models/models_27items.rds")
  }

  return(result)

}

#' Run model with 23 items
#'
#' @param save_model if TRUE, then the results are saved under inst/models. File name: models_27items.rds
#'
#' @importFrom mirt mirt
#' @return a list of model objects returned by mirt::mirt()
#'

run_exploratory_mirt_23items <- function(save_model = FALSE) {

  new_set <- c("AG_Teilnahme",
               "Profilklasse",
               "IU01",
               "MA01_01",
               "MA01_02",
               "MA01_03",
               "MA01_04",
               "MA01_05",
               "KV01_02",
               "KV01_03",
               "KV01_04",
               "KV01_05",
               "MA02_01",
               "MA02_03",
               "MA02_04",
               "MA02_05",
               "MA01_07",
               "MA01_09",
               "MA01_10",
               "IM01_01",
               "IM01_02",
               "IM01_05",
               "IM01_06")

  df <- readRDS("inst/df_explore.rds")
  df <- df[,new_set]
  set.seed(123)
  fit1 <- mirt::mirt(df, 1, technical = list(NCYCLES=2000))
  set.seed(123)
  fit2 <- mirt::mirt(df, 2, technical = list(NCYCLES=3000))
  set.seed(123)
  fit3 <- mirt::mirt(df, 3, technical = list(NCYCLES=3000))

  result <- list(model_1 = fit1, model_2 = fit2, model_3 = fit3)

  if (isTRUE(save_model)) {
    saveRDS(result, "inst/models/models_23items.rds")
  }

  return(result)
}
