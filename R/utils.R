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
