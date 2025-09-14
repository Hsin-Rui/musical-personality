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

#' Create table 7.16
#'
create_table_7.16 <- function(){

  models <- readRDS("inst/models/models_22items.rds")

  report_loadings(models$model_2)

}

#' Create table 7.17
#'
create_table_7.17 <- function(){

  models <- readRDS("inst/models/models_22items.rds")

  report_item_fit(models$model_2)

}

#' Create table 7.18
#'
create_table_7.18 <- function(){

  models <- readRDS("inst/models/models_confirmatory.rds")

  result <- get_fit_index(models$model_1, model_name = "1-faktoriell")
  result <- rbind(result, get_fit_index(models$model_2, model_name = "2-faktoriell"))
  result <- rbind(result, get_fit_index(models$model_3, model_name = "bifaktoriell"))

  return(result)

}

#' Create table 7.19
#'
create_table_7.19 <- function(){

  models <- readRDS("inst/models/models_confirmatory.rds")

  report_loadings(models$model_2, surpress_small_loadings = FALSE)

}

#' Create table 7.20
#'
create_table_7.20 <- function(){

  models <- readRDS("inst/models/models_confirmatory.rds")

  report_loadings(models$model_3, surpress_small_loadings = FALSE)

}

#' Create table 7.21
#'
create_table_7.21 <- function(){

  models <- readRDS("inst/models/models_confirmatory.rds")

  report_item_fit(models$model_3)

}

#' Create table 7.22
#'
create_table_7.22 <- function(){

  models <- readRDS("inst/models/model_bifactor_full_sample.rds")

  result <- get_fit_index(models$model_1, model_name = "bifaktoriell")

  return(result)

}

#' Create table 7.23
#'
create_table_7.23 <- function(){

  models <- readRDS("inst/models/model_bifactor_full_sample.rds")

  report_loadings(models$model_1, surpress_small_loadings = FALSE)

}

#' Create table 7.24
#'
create_table_7.24 <- function(){

  models <- readRDS("inst/models/model_bifactor_full_sample.rds")

  report_item_fit(models$model_1)

}

#' Create table A7.1
#'
create_table_A7.1 <- function(){

  models <- readRDS("inst/models/model_bifactor_full_sample.rds")

  bif.coef <- mirt::coef(models$model_1, simplify=T)
  bif.coef <- bif.coef$items
  bif.coef <- round(bif.coef, digits=3)

  reorder_items(bif.coef)

}

#' Create table 8.1
#'
#' @importFrom BifactorIndicesCalculator bifactorIndices
#' @importFrom stringr str_remove
#'
create_table_8.1 <- function(){

  models <- readRDS("inst/models/model_bifactor_full_sample.rds")

  report_bifactor_reliability(models = models)

}

#' Create table 8.2
#'
create_table_8.2 <- function(){

  scores <- readRDS("inst/scores/scores_t1.rds")

  EAP_scores <- data.frame(scores$EAP_score)
  MAP_scores <- data.frame(scores$MAP_score)

  scores_mean <- c(round(mean(EAP_scores$G), digits = 3),
    round(mean(EAP_scores$S1), digits = 3),
    round(mean(EAP_scores$S2), digits = 3),
    round(mean(MAP_scores$G), digits = 3),
    round(mean(MAP_scores$S1), digits = 3),
    round(mean(MAP_scores$S2), digits = 3))

  scores_sd <- c(round(sd(EAP_scores$G), digits = 3),
                 round(sd(EAP_scores$S1), digits = 3),
                 round(sd(EAP_scores$S2), digits = 3),
                 round(sd(MAP_scores$G), digits = 3),
                 round(sd(MAP_scores$S1), digits = 3),
                 round(sd(MAP_scores$S2), digits = 3))

  min_se <- c(round(min(EAP_scores$SE_G), digits = 3),
              round(min(EAP_scores$SE_S1), digits = 3),
              round(min(EAP_scores$SE_S2), digits = 3),
              round(min(MAP_scores$SE_G), digits = 3),
              round(min(MAP_scores$SE_S1), digits = 3),
              round(min(MAP_scores$SE_S2), digits = 3))

  max_se <- c(round(max(EAP_scores$SE_G), digits = 3),
              round(max(EAP_scores$SE_S1), digits = 3),
              round(max(EAP_scores$SE_S2), digits = 3),
              round(max(MAP_scores$SE_G), digits = 3),
              round(max(MAP_scores$SE_S1), digits = 3),
              round(max(MAP_scores$SE_S2), digits = 3))

  er <- round(c(scores$EAP_reliability, scores$MAR_reliability), digits = 3)

  result <- data.frame(M = scores_mean,
             SD = scores_sd,
             Min.SE = min_se,
             Max.SE = max_se,
             ER = er)

  row.names(result) <- c("FG (EAP)",
                         "F1 (EAP)",
                         "F2 (EAP)",
                         "FG (MAP)",
                         "F1 (MAP)",
                         "F2 (MAP)")

  return(result)

}

#' Create table 8.3
#'
create_table_8.3 <- function() {

  D1 <- c("AG_Teilnahme", #1
          "Profilklasse", #2
          "IU01", #3
          "MA01_01", #4
          "KV01_02", #5
          "KV01_03", #6
          "KV01_04", #7
          "KV01_05", #8
          "MA02_01", #9
          "MA02_03", #10
          "MA02_04", #11
          "MA02_05") #12
  D2 <- c("MA01_02", #13
          "MA01_03", #14
          "MA01_04", #15
          "MA01_05", #16
          "MA01_07", #17
          "MA01_09", #18
          "MA01_10", #19
          "IM01_01", #20
          "IM01_02", #21
          "IM01_05") #22

  scores <- readRDS("inst/scores/scores_t1.rds")
  df <- readRDS("inst/df_t1.rds")

  df <- df [,c(D1, D2)]

  n_items <- 22-rowSums(is.na(df))
  n_items_D1 <- 12-rowSums(is.na(df[,D1]))
  n_items_D2 <- 10-rowSums(is.na(df[,D2]))
  FG_sum <- rowSums(df[,c(D1,D2)], na.rm = TRUE)/n_items
  F1_sum <- rowSums(df[,D1], na.rm = TRUE)/n_items_D1
  F2_sum <- rowSums(df[,D2], na.rm = TRUE)/n_items_D2

  scores_sum <- data.frame(FG_sum = FG_sum, F1_sum = F1_sum, F2_sum = F2_sum)

  scores_EAP <- data.frame(scores$EAP_score)[,c(1:3)]
  names(scores_EAP) <- c("FG (EAP)", "F1 (EAP)", "F2 (EAP)")

  scores_MAP <- data.frame(scores$MAP_score)[,c(1:3)]
  names(scores_MAP) <- c("FG (MAP)", "F1 (MAP)", "F2 (MAP)")

  cor_matrix <- cor(cbind(scores_EAP, scores_MAP, scores_sum))

  round(cor_matrix, digits = 2)

}

#' Create table 8.4
#'
create_table_8.4 <- function(){

  models <- readRDS("inst/models/model_bifactor_t2_sample.rds")

  result <- get_fit_index(models$model_1, model_name = "bifaktoriell")

  return(result)
}

#' Create table 8.5
#'
create_table_8.5 <- function(){

  models <- readRDS("inst/models/model_bifactor_t2_sample.rds")

  report_loadings(models$model_1, surpress_small_loadings = FALSE)

}

#' Create table 8.6
#'
create_table_8.6 <- function(){

  models <- readRDS("inst/models/model_bifactor_t2_sample.rds")

  report_item_fit(models$model_1)

}

#' Create table 8.7
#'
create_table_8.7 <- function(){

  models <- readRDS("inst/models/model_bifactor_t2_sample.rds")

  report_bifactor_reliability(models = models)

}

#' Create table 8.8
#'
create_table_8.8 <- function(){

  scores <- readRDS("inst/scores/scores_t2.rds")

  EAP_scores <- data.frame(scores$EAP_score)

  EAP_scores <- EAP_scores[,c(1:3)]

  cor_matrix <- cor(EAP_scores)

  cor_matrix[cor_matrix == 1] <- scores$EAP_reliability

  round(cor_matrix, digits = 3)

}
