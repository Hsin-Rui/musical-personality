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
