#' Get fit index from an mirt model object
#'
#' @param x an object return by the mirt::mirt() function
#'
#' @importFrom mirt extract.mirt
#' @importFrom mirt M2
#' @importFrom mirt summary
#' @importFrom stringr str_remove
#'
#' @return a data frame with all fit index
#'

get_fit_index <- function(x, model_name="model") {


  AIC <- round(mirt::extract.mirt(x, "AIC"),2)
  BIC <- round(mirt::extract.mirt(x, "BIC"),2)
  LL <- round(mirt::extract.mirt(x, "logLik"),2)
  M2_fit <- mirt::M2(x, type = "M2*", na.rm=TRUE)
  M_squared <- round(M2_fit$M2, 2)
  SRMR <- stringr::str_remove(as.character(round(M2_fit$SRMSR, 3)), "^0")
  sum_x <- mirt::summary(x, verbose = FALSE)
  EV <- paste(round(sum(sum_x$h2) / length(sum_x$h2)*100, 1), "%")

  if (M2_fit$p == 0) {
    p <- "<.001"
  } else {
    p <- stringr::str_remove(as.character(round(M2_fit$p, 3)), "^0")
  }

  res <- data.frame(Modell = model_name,
                    LL = LL,
                    M2 = M_squared,
                    df = M2_fit$df,
                    p = p,
                    SRMR = SRMR,
                    AIC = AIC,
                    BIC = BIC,
                    EV = EV)

  return (res)
}

#' Report factor loadings from an mirt model object
#'
#' @param model an MIRT model object returned by mirt::mirt()
#'
#' @importFrom mirt summary
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom dplyr coalesce
#'
#' @return a data frame with factor loadings & h2. Loadings under .30 are surpressed. Character strings are reported (instead of numeric values)
#'

report_loadings <- function(model){

  sum_model  <- mirt::summary(model, verbose = FALSE)
  sum_model$rotF [abs(sum_model$rotF) < 0.3] <- NA
  result <- data.frame(sum_model$rotF, sum_model$h2)
  result <- round(result, digits = 3)

  result <- reorder_items(result)

  for (i in 1:ncol(result)) {
    result[,i] <- dplyr::coalesce(as.character(result[,i]), "")
    result[,i] <- stringr::str_replace(result[,i], "0\\.", "\\.")
  }

  result

}

#' Reorder items according to the reports in the manuscript
#'
#' @param df a data frame with row.names which are the item ids
#'
reorder_items <- function(df){

  KV <- row.names(df)[stringr::str_detect(row.names(df), "KV")]
  MA02 <- row.names(df)[stringr::str_detect(row.names(df), "MA02")]
  MA01 <- row.names(df)[stringr::str_detect(row.names(df), "MA01")]
  MA01 <- MA01[order(MA01)]
  IM01 <- row.names(df)[stringr::str_detect(row.names(df), "IM01")]

  new_order <-
    c("AG_Teilnahme",
      "Profilklasse",
      KV,
      MA02,
      "IU01",
      MA01,
      IM01)

  df <- df[new_order,]

  return(df)

}
