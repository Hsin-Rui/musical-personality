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
  M2_fit <- mirt::M2(x, type = "M2*")
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
#' @param save_scores if TRUE, scores will be saved under inst/scores
#' @param path the rds file saved
#' @importFrom mirt fscores
#'
calculate_factore_score <- function(model, save_scores = FALSE, path="inst/scores/scores_t1.rds"){

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

    saveRDS(result, path)

  }

  return(result)

}

#' GGplot2 theme for apa style
#'
#' @import ggplot2
get_apatheme <- function(){

  apatheme <- ggplot2::theme_bw()+
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   text=ggplot2::element_text(family='sans'),
                   legend.title=ggplot2::element_blank(),
                   legend.position=c(.7,.8),
                   axis.line.x = ggplot2::element_line(color='black'),
                   axis.line.y = ggplot2::element_line(color='black'))

  return(apatheme)

}

#' Report bifactor model based reliability
#'
#' @param models a list of models. model_1 is the bifactor model, model_2 is the one factorial solution (base line model)
#' @importFrom BifactorIndicesCalculator bifactorIndices
#'

report_bifactor_reliability <- function(models){

  # make the bifactor calculator quiet:
  null_device <- if (Sys.info()['sysname'] == "Windows") "NUL" else "/dev/null"

  # sink to redirect output
  sink(null_device)
  result <- BifactorIndicesCalculator::bifactorIndices(models$model_1, UniLambda = models$model_2)
  sink()

  result <- result$FactorLevelIndices
  result <- result[,c(-1,-3)]
  result$FD_squared <- result$FD * result$FD
  result$FD_worst_cor <- 2 * result$FD_squared - 1
  result <- round(result, digits = 3)

  for (i in 1:ncol(result)) {

    result[,i] <- stringr::str_remove(as.character(result[,i]), "^0")

  }

  result %>%
    dplyr::select(
      Omega,
      OmegaH,
      ECV_SG,
      H,
      FD,
      FD_squared,
      FD_worst_cor
    )

}

#' Report fit indices for state invariance models
#'
#' @param x a model object
#' @param model_name string of model name (for better output)
#'
#' @importFrom mirt extract.mirt
#' @importFrom mirt M2
#'

get_fit_index_invariance_state <- function(x, model_name="model") {

  AIC <- round(mirt::extract.mirt(x, "AIC"),2)
  BIC <- round(mirt::extract.mirt(x, "BIC"),2)
  LL <- round(mirt::extract.mirt(x, "logLik"),2)
  M2_fit <- mirt::M2(x, type = "M2*")
  M_squared <- round(M2_fit$M2, 2)
  names(M2_fit)[stringr::str_detect(names(M2_fit), "SRM")] <- c("SRMSR_Group1", "SRMSR_Group2")

  SRMSR_Group1 <- round(M2_fit$SRMSR_Group1, 3)
  SRMSR_Group2 <- round(M2_fit$SRMSR_Group2, 3)
  RMSEA <- M2_fit$RMSEA
  CFI <- M2_fit$CFI

  res <- data.frame(Modell = model_name,
                    LL = LL,
                    M2 = M_squared,
                    df = M2_fit$df,
                    p = M2_fit$p,
                    SRMSR_Group1 = SRMSR_Group1,
                    SRMSR_Group2 = SRMSR_Group2,
                    RMSEA = RMSEA,
                    CFI = CFI,
                    AIC = AIC,
                    BIC = BIC)

  return (res)
}

#' Report fit indices for invariance models (table 8.13 and 8.14)
#'
#' @param models a model object
#' @import dplyr
#' @importFrom stringr str_replace
#' @return a data frame with fit indices
#'
report_invariance_fit <- function(models) {

  result <- get_fit_index_invariance_state(models$model_configural, "konfigurale Invarianz")
  result <- rbind(result, get_fit_index_invariance_state(models$model_slope, model_name = "metrische Invarianz"))
  result <- rbind(result, get_fit_index_invariance_state(models$model_full, model_name = "skalare Invarianz"))

  result[,2:ncol(result)] <- round(result[,2:ncol(result)],3)

  result<- result %>%
    dplyr::mutate(delta_M2 = M2-dplyr::lag(M2),
                  delta_df = df-dplyr::lag(df),
                  delta_RMSEA = RMSEA-dplyr::lag(RMSEA),
                  delta_CFI = round(dplyr::lag(CFI)-CFI,3)) %>%
    dplyr::relocate(delta_M2, delta_df, .before = SRMSR_Group1) %>%
    dplyr::relocate(delta_RMSEA, .before=CFI) %>%
    dplyr::relocate(delta_CFI, .before=AIC)

  model_names <- result$Modell
  result <- data.frame(t(result[,2:ncol(result)]))
  names(result) <- model_names

  for (i in 1:ncol(result)){

    result[,i] <- dplyr::coalesce(as.character(result[,i]), "")
    result[,i] <- stringr::str_replace(result[,i], "^0\\.", "\\.")

  }

  result[result=="0"] <- "<.001"
  result[result==""] <- "-"
  return(result)
}

#' Plot scores distribution of multigroup model and single group model  (figure 8.3 and 8.4)
#'
#' @param models a list of model object
#' @param scores_whole_sample a list of factor scores (saved earlier), here the scores of bi factor model from whole sample are expected
#' @param group grouping variable
#' @import ggplot2
#' @importFrom ggpubr ggarrange
#' @importFrom stringr str_replace
#' @importFrom mirt fscores
#' @importFrom psych cohen.d
#'
#' @return a ggplot object combining 3 plots
#'

plot_multigroup_scores_distribution <- function(models, scores_whole_sample, group){

  scores_whole_sample <-scores_whole_sample$EAP_score[,c(1:3)]
  scores_whole_sample <- data.frame(scores_whole_sample)

  scores_whole_sample <- scores_whole_sample[!is.na(group),]
  group <- group[!is.na(group)]

  scores_whole_sample$Gruppe <- group
  scores_whole_sample$Modell <- "Einzelgruppe"

  set.seed(12345)
  scores_full_invariance <- mirt::fscores(models$model_full)
  scores_full_invariance <- data.frame(scores_full_invariance)
  scores_full_invariance$Gruppe <- group
  scores_full_invariance$Modell <- "Mehrgruppen"

  d1 <- psych::cohen.d(scores_whole_sample$G, scores_whole_sample$Gruppe)
  d1 <- round(abs(d1[[1]][2]), digits = 2)
  d2 <- psych::cohen.d(scores_full_invariance$G, scores_full_invariance$Gruppe)
  d2 <- round(abs(d2[[1]][2]), digits = 2)

  label_1 <- paste0("italic(d)==", d1)
  label_2 <- paste0("italic(d)==", d2)

  scores <- rbind(scores_whole_sample, scores_full_invariance)

  p1 <-
    scores %>%
    ggplot2::ggplot(ggplot2::aes(x=Modell,y=G,fill=Gruppe))+
    ggplot2::geom_boxplot()+
    ggplot2::ylab("Theta")+
    ggplot2::xlab("")+
    ggplot2::scale_y_continuous(limits=c(-2.5,3),breaks = c(-2,-1,0,1,2))+
    ggplot2::scale_fill_brewer(palette = "Pastel1")+
    ggplot2::theme_classic()+
    ggplot2::ggtitle("FG: Generelle Teilhabe")+
    ggplot2::annotate("text", x=1,y=2.8,label=label_1,parse=TRUE)+
    ggplot2::annotate("text", x=2,y=2.8,label=label_2,parse=TRUE)

  d1 <- psych::cohen.d(scores_whole_sample$S1, scores_whole_sample$Gruppe)
  d1 <- round(abs(d1[[1]][2]), digits = 2)
  d2 <- psych::cohen.d(scores_full_invariance$S1, scores_full_invariance$Gruppe)
  d2 <- round(abs(d2[[1]][2]), digits = 2)

  label_1 <- paste0("italic(d)==", d1)
  label_2 <- paste0("italic(d)==", d2)

  p2 <- scores %>%
    ggplot2::ggplot(ggplot2::aes(x=Modell,y=S1,fill=Gruppe))+
    ggplot2::geom_boxplot()+
    ggplot2::ylab("")+
    ggplot2::scale_y_continuous(limits=c(-2.5,3),breaks = c(-2,-1,0,1,2))+
    ggplot2::scale_fill_brewer(palette = "Pastel1")+
    ggplot2::theme_classic()+
    ggplot2::ggtitle("F1: Teilhabe im formalen Kontext")+
    ggplot2::annotate("text", x=1,y=2.8,label=label_1,parse=TRUE)+
    ggplot2::annotate("text", x=2,y=2.8,label=label_1,parse=TRUE)

  d1 <- psych::cohen.d(scores_whole_sample$S2, scores_whole_sample$Gruppe)
  d1 <- round(abs(d1[[1]][2]), digits = 2)
  d2 <- psych::cohen.d(scores_full_invariance$S2, scores_full_invariance$Gruppe)
  d2 <- round(abs(d2[[1]][2]), digits = 2)

  label_1 <- paste0("italic(d)==", d1)
  label_2 <- paste0("italic(d)==", d2)

  p3 <- scores %>%
    ggplot2::ggplot(ggplot2::aes(x=Modell,y=S2,fill=Gruppe))+
    ggplot2::geom_boxplot()+
    ggplot2::ylab("Theta")+
    ggplot2::scale_y_continuous(limits=c(-2.5,3),breaks = c(-2,-1,0,1,2))+
    ggplot2::scale_fill_brewer(palette = "Pastel1")+
    ggplot2::theme_classic()+
    ggplot2::ggtitle("F2: Teilhabe im informellen Kontext")+
    ggplot2::annotate("text", x=1,y=2.8,label=label_1,parse=TRUE)+
    ggplot2::annotate("text", x=2,y=2.8,label=label_2,parse=TRUE)

  p <- ggpubr::ggarrange(p1, p2, p3, ncol=2,nrow=2,common.legend = TRUE, legend= "bottom")

  return(p)

}
