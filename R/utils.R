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
#' @param surpress_small_loadings if FALSE, all loadings will be reported. Otherwise, loadings <.30 are surpressed.
#'
#' @importFrom mirt summary
#' @importFrom stringr str_replace
#' @importFrom dplyr coalesce
#'
#' @return a data frame with factor loadings & h2. Default setting: Loadings under .30 are surpressed. Character strings are reported (instead of numeric values)
#'

report_loadings <- function(model, surpress_small_loadings = TRUE){

  sum_model  <- mirt::summary(model, verbose = FALSE)

  if (isTRUE(surpress_small_loadings)){
    sum_model$rotF [abs(sum_model$rotF) < 0.3] <- NA
  }
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
#' @importFrom stringr str_detect

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
#' Report item fit index from an mirt model object
#'
#' @param model an MIRT model object returned by mirt::mirt()
#' @importFrom mirt itemfit
#' @importFrom dplyr select
#' @importFrom stringr str_replace

report_item_fit <- function(model){

  result <- mirt::itemfit(model, na.rm = TRUE) #complete dataset (N=1332)
  row.names(result) <- result$item

  result <- reorder_items(result)

  result <- result %>%
    dplyr::select(-item)

  result <-round(result, digits=3)

  for (i in 1:ncol(result)){

    result[,i] <- stringr::str_replace(as.character(result[,i]), "^0\\.", "\\.")

  }

  result[result=="0"] <- "<.001"
  return(result)
}
#' Calculate factor scores from a model object
#'
#' @param model an MIRT model object
#' @param save_scores if TRUE, scores will be saved under inst/scores, file name: scores_t1.rds
#' @importFrom mirt fscores
#'
calculate_factore_score <- function(model, save_scores = FALSE){

  set.seed(123465)
  scores_EAP_error <- mirt::fscores(model, full.scores.SE = TRUE)

  set.seed(123465)
  scores_MAR_error <- mirt::fscores(model, method = "MAP", full.scores.SE = TRUE, QMC = TRUE)

  set.seed(123465)
  EAP_reliability <- mirt::fscores(model, full.scores.SE = TRUE, returnER = TRUE)

  set.seed(123465)
  MAR_reliability <- mirt::fscores(model, method = "MAP", full.scores.SE = TRUE, QMC = TRUE, returnER = TRUE)

  result <- list(EAP_score = scores_EAP_error, MAP_score = scores_MAR_error, EAP_reliability = EAP_reliability, MAR_reliability = MAR_reliability)

  if(isTRUE(save_scores)) {

    saveRDS(result, "inst/scores/scores_t1.rds")

  }

  return(result)

}
