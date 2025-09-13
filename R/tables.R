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

