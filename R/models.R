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
#' @param save_model if TRUE, then the results are saved under inst/models. File name: models_23items.rds
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


#' Run model with 22 items (IM06 deleted from the model)
#'
#' @param save_model if TRUE, then the results are saved under inst/models. File name: models_22items.rds
#'
#' @importFrom mirt mirt
#' @return a list of model objects returned by mirt::mirt()
#'

run_exploratory_mirt_22items <- function(save_model = FALSE) {

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
               "IM01_05")

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
    saveRDS(result, "inst/models/models_22items.rds")
  }

  return(result)
}

#' Run model confirmatory MIRT with 22 items
#'
#' @param save_model if TRUE, then the results are saved under inst/models. File name: models_confirmatory.rds
#'
#' @importFrom mirt mirt
#' @importFrom mirt bfactor
#' @return a list of model objects returned by mirt::mirt() or mirt::bfactor()
#'

run_confirmatory_mirt_22items <- function(save_model = FALSE) {

  df <- readRDS("inst/df_confirm.rds")

  itemtype <- c("2PL", #1
                "2PL", #2
                "graded", #3
                "graded", #4
                "2PL", #5
                "2PL", #6
                "2PL", #7
                "2PL", #8
                "2PL", #9
                "2PL", #10
                "2PL", #11
                "2PL", #12
                "graded", #13
                "graded", #14
                "graded", #15
                "graded", #16
                "graded", #17
                "graded", #18
                "graded", #19
                "graded", #20
                "graded", #21
                "graded") #22

  set.seed(123)
  fit1 <- mirt::mirt(df, 1, itemtype = itemtype, SE=T)

  model_2F <- 'F1 = 1-12
              F2 = 13-22
              COV=F1*F2'
  set.seed(123)
  fit2 <- mirt::mirt(df, model=model_2F, itemtype = itemtype, SE=T)

  model_BI <- c(rep(1,12),rep(2,10))
  fit3 <- mirt::bfactor(df, model=model_BI, itemtype = itemtype)

  result <- list(model_1 = fit1, model_2 = fit2, model_3 = fit3)

  if (isTRUE(save_model)) {
    saveRDS(result, "inst/models/models_confirmatory.rds")
  }

  return(result)
}

#' Run bifactor model with whole sample
#'
#' @param save_model if TRUE, then the results are saved under inst/models. File name: model_bifactor_full_sample.rds
#'
#' @importFrom mirt bfactor
#' @return a list of model objects returned by mirt::bfactor()
#'
run_bifactor_model_full_sample <- function(save_model = FALSE){

  df <- readRDS("inst/df_t1.rds")

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
  itemtype <- c("2PL", #1
                     "2PL", #2
                     "graded", #3
                     "graded", #4
                     "2PL", #5
                     "2PL", #6
                     "2PL", #7
                     "2PL", #8
                     "2PL", #9
                     "2PL", #10
                     "2PL", #11
                     "2PL", #12
                     "graded", #13
                     "graded", #14
                     "graded", #15
                     "graded", #16
                     "graded", #17
                     "graded", #18
                     "graded", #19
                     "graded", #20
                     "graded", #21
                     "graded") #22

  df <- df [,c(D1,D2)]
  model_spec <- c(rep(1,12),rep(2,10))
  set.seed(1235)
  fit1 <- mirt::bfactor(df, model=model_spec, itemtype = itemtype)
  set.seed(1235)
  fit2 <- mirt::mirt(df, 1, technical = list(NCYCLES=2000))

  result <- list(model_1 = fit1, model_2 = fit2)

  if (isTRUE(save_model)) {
    saveRDS(result, "inst/models/model_bifactor_full_sample.rds")
  }

  return(result)
}

#' Run bifactor model with t2 sample
#'
#' @param save_model if TRUE, then the results are saved under inst/models. File name: model_bifactor_t2_sample.rds
#'
#' @importFrom mirt bfactor
#' @importFrom mirt mirt
#' @return a list of model objects returned by mirt::bfactor()
#'
run_bifactor_model_t2 <- function(save_model = FALSE){

  df <- readRDS("inst/df_t2.rds")

  model_spec <- c(rep(1,12),rep(2,10))
  itemtype <- c("2PL", #1
                "2PL", #2
                "graded", #3
                "graded", #4
                "2PL", #5
                "2PL", #6
                "2PL", #7
                "2PL", #8
                "2PL", #9
                "2PL", #10
                "2PL", #11
                "2PL", #12
                "graded", #13
                "graded", #14
                "graded", #15
                "graded", #16
                "graded", #17
                "graded", #18
                "graded", #19
                "graded", #20
                "graded", #21
                "graded") #22

  set.seed(1235)
  fit1 <- bfactor(df, model=model_spec, itemtype = itemtype)
  set.seed(1235)
  fit2 <- mirt::mirt(df, 1, technical = list(NCYCLES=2000))

  result <- list(model_1 = fit1, model_2 = fit2)

  if (isTRUE(save_model)) {
    saveRDS(result, "inst/models/model_bifactor_t2_sample.rds")
  }

  return(result)

}

#' Run DIF analysis for state
#'
#' @importFrom mirt bfactor
#' @importFrom mirt mirt
#' @return a vector of item names (DIF items)
#'

run_dif_analysis_state <- function(){

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
  items <- c(D1,D2)

  model_spec <- c(rep(1,12),rep(2,10))
  itemtype <- c("2PL", #1
                "2PL", #2
                "graded", #3
                "graded", #4
                "2PL", #5
                "2PL", #6
                "2PL", #7
                "2PL", #8
                "2PL", #9
                "2PL", #10
                "2PL", #11
                "2PL", #12
                "graded", #13
                "graded", #14
                "graded", #15
                "graded", #16
                "graded", #17
                "graded", #18
                "graded", #19
                "graded", #20
                "graded", #21
                "graded") #22

  spec_inv <- c(items,"free_var","free_means")

  df <- readRDS("inst/df_t1.rds")
  df <- df[,items]
  BD <- readRDS("inst/BD.rds")

  set.seed(123456)
  fit1 <- mirt::bfactor(df, model=model_spec, itemtype = itemtype, group= BD, invariance = spec_inv) # base line model: assume full invariance
  # values <- mirt::bfactor(df, model=model_spec, itemtype = itemtype, group= BD, invariance = spec_inv, pars="values")

  res_DIF <- rep(TRUE, length(items))
  names(res_DIF) <- items

  for (i in 1:length(items)){

    spec_inv <- c(items[-i], "free_var","free_means") # release constraints of individual items and then compare fit
    set.seed(123456)
    fit_DIF <- mirt::bfactor(df, model=model_spec, itemtype = itemtype, group= BD,invariance=spec_inv)
    res_anova <- mirt::anova(fit1, fit_DIF)
    res <- res_anova$p [2] < 0.005 # single criteria
    res_DIF[i] <- res

  }

  return(names(res_DIF)[res_DIF])

}

#' Run measurement invariance analysis for state
#'
#' @param save_model if TRUE, then the results are saved under inst/models. File name: models_state_invariance.rds
#'
#' @importFrom mirt bfactor
#' @return a list of models
#'

run_measurement_invariance_state <- function(save_model = FALSE){

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
  items <- c(D1,D2)

  model_spec <- c(rep(1,12),rep(2,10))
  itemtype <- c("2PL", #1
                "2PL", #2
                "graded", #3
                "graded", #4
                "2PL", #5
                "2PL", #6
                "2PL", #7
                "2PL", #8
                "2PL", #9
                "2PL", #10
                "2PL", #11
                "2PL", #12
                "graded", #13
                "graded", #14
                "graded", #15
                "graded", #16
                "graded", #17
                "graded", #18
                "graded", #19
                "graded", #20
                "graded", #21
                "graded") #22

  spec_inv <- c(items,"free_var","free_means")

  df <- readRDS("inst/df_t1.rds")
  df <- df[,items]
  BD <- readRDS("inst/BD.rds")

  set.seed(123456)
  fit_full <- mirt::bfactor(df, model=model_spec, itemtype = itemtype, group= BD, invariance = spec_inv) # base line model: assume full invariance

  set.seed(123456)
  #select random items as anchor
  items_BD <- c("KV01_02", "KV01_03", "KV01_05", "MA02_03", "MA02_04", "MA02_05", "MA01_04", "MA01_05",  "MA01_07", "MA01_09", "MA01_10", "IM01_01", "IM01_02", "IM01_05")
  item_number <- 1:length(items_BD)
  anchor_BD <- items_BD[sample(item_number,2)]

  spec_inv <- c(anchor_BD, "free_var", "free_means")
  set.seed(123456)
  # configural invariance: only two anchor items have common slope etc.
  fit_configural <- mirt::bfactor(df, model=model_spec, itemtype = itemtype, group= BD,invariance = spec_inv, technical = list(NCYCLES=3000))

  slope_inv <- list(c(122,273),c(124,275),c(125,276),c(126,277),c(127,278),c(128,279), #anchor IM01_01
                       c(108,259),c(110,261),c(111,262),c(112,263),c(113,264),c(114,265), #anchor MA01_09
                       c(1,152),c(2,153), #slope AG_Teilnahme
                       c(7,158),c(8,159), #slope Profilklasse
                       c(13,164),c(14,165),#slope IU01
                       c(18,169),c(19,170),#slope MA01_01
                       c(25,176),c(26,177),#slope KV01_02
                       c(31,182),c(32,183),#slope KV01_03
                       c(37,188),c(38,189),#slope KV01_04
                       c(43,194),c(44,195),#slope KV01_05
                       c(49,200),c(50,201),#slope MA02_01
                       c(55,206),c(56,207),#slope MA02_03
                       c(61,212),c(62,213),#slope MA02_04
                       c(67,218),c(68,219),#slope MA02_05
                       c(73,224),c(75,226),#slope MA01_02
                       c(80,231),c(82,233),#slope MA01_03
                       c(87,238),c(89,240),#slope MA01_04
                       c(94,245),c(96,247),#slope MA01_05
                       c(101,252),c(103,254), #slope MA01_07
                       c(115,266),c(117,268), #slope MA01_10
                       c(129,280),c(131,282), #slope IM01_02
                       c(136,287),c(138,289)) #slope IM01_05

  set.seed(123456)
  fit_slope <- mirt::bfactor(df, model=model_spec, itemtype = itemtype, group= BD, constrain = slope_inv, technical = list(NCYCLES=3000))

  result <- list(model_configural = fit_configural, model_slope = fit_slope, model_full = fit_full)

  if (isTRUE(save_model)) {
    saveRDS(result, "inst/models/models_state_invariance.rds")
  }

  return(result)

}

#' Run measurement invariance analysis for gender
#'
#' @param save_model if TRUE, then the results are saved under inst/models. File name: models_gender_invariance.rds
#'
#' @importFrom mirt bfactor
#' @return a list of models
#'
run_measurement_invariance_gender <- function(save_model = FALSE){

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
  items <- c(D1,D2)

  model_spec <- c(rep(1,12),rep(2,10))
  itemtype <- c("2PL", #1
                "2PL", #2
                "graded", #3
                "graded", #4
                "2PL", #5
                "2PL", #6
                "2PL", #7
                "2PL", #8
                "2PL", #9
                "2PL", #10
                "2PL", #11
                "2PL", #12
                "graded", #13
                "graded", #14
                "graded", #15
                "graded", #16
                "graded", #17
                "graded", #18
                "graded", #19
                "graded", #20
                "graded", #21
                "graded") #22

  gender <-readRDS("inst/gender.rds")
  df <- readRDS("inst/df_t1.rds")
  df <- df[,items]
  df <- df[!is.na(gender),]
  gender <- gender[!is.na(gender)]

  spec_inv <- c(items,"free_var","free_means")
  set.seed(1256)
  fit_full <- mirt::bfactor(df, model=model_spec, itemtype = itemtype,group=gender, invariance = spec_inv)

  set.seed(1256)
  #select random items
  items_GD <- c("AG_Teilnahme", "KV01_05","IU01", "MA02_03", "MA02_04", "MA02_05", "MA01_01", "MA01_09", "MA01_10", "IM01_02", "IM01_05")
  item_number <- 1:length(items_GD)
  anchor_GD <- items_GD[sample(item_number,2)]
  spec_inv <- c(anchor_GD,"free_var","free_means")

  set.seed(1256)
  fit_configural <- mirt::bfactor(df, model=model_spec, itemtype = itemtype, group=gender, invariance = spec_inv, technical = list(NCYCLES=3000))

  slope_inv <- list(c(61,212),c(62,213),c(64,215), #anchor MA02_04
                       c(129,280),c(131,282),c(132,283),c(133,284),c(134,285),c(135,286), #anchor IM01_02,
                       c(1,152),c(2,153), #slope AG_Teilnahme
                       c(7,158),c(8,159), #slope Profilklasse
                       c(13,164),c(14,165),#slope IU01
                       c(18,169),c(19,170),#slope MA01_01
                       c(25,176),c(26,177),#slope KV01_02
                       c(31,182),c(32,183),#slope KV01_03
                       c(37,188),c(38,189),#slope KV01_04
                       c(43,194),c(44,195),#slope KV01_05
                       c(49,200),c(50,201),#slope MA02_01
                       c(55,206),c(56,207),#slope MA02_03
                       c(67,218),c(68,219),#slope MA02_05
                       c(73,224),c(75,226),#slope MA01_02
                       c(80,231),c(82,233),#slope MA01_03
                       c(87,238),c(89,240),#slope MA01_04
                       c(94,245),c(96,247),#slope MA01_05
                       c(101,252),c(103,254), #slope MA01_07
                       c(108,259),c(110,261), #slope MA01_09
                       c(115,266),c(117,268), #slope MA01_10
                       c(122,273),c(124,275), #slope IM01_01
                       c(136,287),c(138,289)) #slope IM01_05

  set.seed(1256)
  fit_slope <- bfactor(df, model=model_spec, itemtype = itemtype, group= gender, constrain = slope_inv, technical = list(NCYCLES=3000))

  result <- list(model_configural = fit_configural, model_slope = fit_slope, model_full = fit_full)

  if (isTRUE(save_model)) {
    saveRDS(result, "inst/models/models_gender_invariance.rds")
  }

  return(result)

}



#' Run DIF analysis for gender
#'
#' @importFrom mirt bfactor
#' @importFrom mirt mirt
#' @return a vector of item names (DIF items)
#'

run_dif_analysis_gender <- function(){

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
  items <- c(D1,D2)

  model_spec <- c(rep(1,12),rep(2,10))
  itemtype <- c("2PL", #1
                "2PL", #2
                "graded", #3
                "graded", #4
                "2PL", #5
                "2PL", #6
                "2PL", #7
                "2PL", #8
                "2PL", #9
                "2PL", #10
                "2PL", #11
                "2PL", #12
                "graded", #13
                "graded", #14
                "graded", #15
                "graded", #16
                "graded", #17
                "graded", #18
                "graded", #19
                "graded", #20
                "graded", #21
                "graded") #22

  spec_inv <- c(items,"free_var","free_means")

  df <- readRDS("inst/df_t1.rds")
  df <- df[,items]
  gender <- readRDS("inst/gender.rds")

  df <- df[!is.na(gender),]
  gender <- gender[!is.na(gender)]

  models <- readRDS("inst/models/models_gender_invariance.rds")
  fit1 <- models$model_full # base line model: assume full invariance
  # values <- mirt::bfactor(df, model=model_spec, itemtype = itemtype, group= BD, invariance = spec_inv, pars="values")

  res_DIF <- rep(TRUE, length(items))
  names(res_DIF) <- items

  for (i in 1:length(items)){

    spec_inv <- c(items[-i], "free_var","free_means") # release constraints of individual items and then compare fit
    set.seed(123456)
    fit_DIF <- mirt::bfactor(df, model=model_spec, itemtype = itemtype, group= gender,invariance=spec_inv)
    res_anova <- mirt::anova(fit1, fit_DIF)
    res <- res_anova$p [2] < 0.005 # single criteria
    res_DIF[i] <- res

  }

  return(names(res_DIF)[res_DIF])

}
