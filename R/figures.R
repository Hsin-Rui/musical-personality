#' Create figure 7.2
#'
#' @param save_file if TRUE, a pdf file is generated under. File name: Abbildung7.2.pdf
#'
#' @return base plot of factor loadings
#' @importFrom Gifi princals
#'

create_figure_7.2 <- function(save_file = FALSE) {

  df <- readRDS("inst/df_t1.rds")

  df <- df [rowSums(is.na(df))==0, ] # complete case N = 2670
  res <- Gifi::princals(df)

  if (isTRUE(save_file)) {

    width_in_inches <- 2160/ 300
    height_in_inches <- 2160 / 300

    pdf("inst/output/abbildung7.2.pdf", width = width_in_inches, height = height_in_inches)

    plot(res, main = "Ladungen", xlab="Komponente 1", ylab="Komponente 2")
    dev.off()
  }

  plot(res, main = "Ladungen", xlab="Komponente 1", ylab="Komponente 2")

}

#' Create figure 7.3
#'
#' @param save_file if TRUE, a pdf file is generated. File name: Abbildung7.3.pdf
#'
#' @importFrom psych fa.parallel
#' @import ggplot2
#'
create_figure_7.3 <- function(save_file = FALSE) {

  df <- readRDS("inst/df_t1.rds")

  set.seed(123)
  parallel <- psych::fa.parallel(df, fa = "fa", fm="ml", main="Screeplot", ylabel="Eigenwert", show.legend=FALSE)

  obs <- data.frame(parallel$fa.values)
  obs$type<- c("Empirische Daten")
  obs$num <- c(row.names(obs))
  obs$num <- as.numeric(obs$num)
  colnames(obs) <- c("eigenvalue", "type", "num")

  percentile <- apply(parallel$values, 2, function(x) quantile(x, .95))
  min <- as.numeric(nrow(obs))
  min <- (4*min) - (min-1)
  max <- as.numeric(nrow(obs))
  max <- 4*max
  percentile1 <- percentile[min:max]

  sim <- data.frame(percentile1)
  sim$type <- c("Simulierte Daten (95. Perzentil)")
  sim$num <- c(row.names(obs))
  sim$num <- as.numeric(sim$num)
  colnames(sim) <- c("eigenvalue", "type", "num")

  eigendat <- rbind(obs,sim)

  apatheme <- get_apatheme()

  p <- ggplot2::ggplot(eigendat, ggplot2::aes(x=num,y=eigenvalue, shape=type))+
    ggplot2::geom_line()+
    ggplot2::geom_point(size=4)+
    ggplot2::ggtitle("Screeplot")+
    ggplot2::scale_y_continuous(name="Eigenwert")+
    ggplot2::scale_x_continuous(name="Faktor", breaks=min(eigendat$num):max(eigendat$num))+
    ggplot2::scale_shape_manual(values=c(16,1))+
    ggplot2::geom_vline(xintercept=parallel$nfact,linetype="dashed")+
    apatheme+
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,face="bold"))

  if (isTRUE(save_file)) {

    ggplot2::ggsave("inst/output/Abbildung7.3.pdf", plot = p, dpi = 300, width = 5, height = 5)

  }

  p

}

#' Create figure 8.1
#'
#' @param save_file if TRUE, a pdf file is generated. File name: Abbildung8.1.pdf
#'
#' @import ggplot2
#' @import ggpubr
#'

create_figure_8.1 <- function(save_file=FALSE){

  scores <- readRDS("inst/scores/scores_t1.rds")$EAP_score
  apatheme <-get_apatheme()

  p1 <-
    ggplot2::ggplot(scores, ggplot2::aes(x=G))+
    ggplot2::ggtitle("(a) Generelle Teilhabe")+
    ggplot2::geom_histogram(colour="black", fill="grey", bins = 30)+
    ggplot2::scale_x_continuous(name = "")+
    ggplot2::scale_y_continuous("Häufigkeit")+
    apatheme

  p2 <-
    ggplot2::ggplot(scores, ggplot2::aes(x=S1))+
    ggplot2::ggtitle("(b) Teilhabe im formalen Kontext")+
    ggplot2::geom_histogram(colour="black", fill="grey", bins = 30)+
    ggplot2::scale_x_continuous(name = "Theta")+
    ggplot2::scale_y_continuous("")+
    apatheme

  p3 <- ggplot2::ggplot(scores, aes(x=S2))+
    ggplot2::ggtitle("(c) Teilhabe im informellen Kontext")+
    ggplot2::geom_histogram(colour="black", fill="grey", bins = 30)+
    ggplot2::scale_x_continuous(name = "Theta")+
    ggplot2::scale_y_continuous("Häufigkeit")+
    apatheme

  p <- ggpubr::ggarrange(p1, p2, p3, ncol=2,nrow=2)

  if (isTRUE(save_file)) {

    ggplot2::ggsave("inst/output/Abbildung8.1.pdf", plot = p, dpi = 300, width = 8, height = 6)

  }

  p

}

#' Create figure 8.2
#'
#' @param save_file if TRUE, a pdf file is generated. File name: Abbildung8.2.pdf
#'
#' @import ggplot2
#' @import ggpubr
#' @import ggExtra
#'
create_figure_8.2 <- function(save_file=FALSE) {

  scores <- readRDS("inst/scores/scores_t1.rds")

  EAP_scores <- data.frame(scores$EAP_score)
  apatheme <- get_apatheme()

  library(ggExtra)
  library(ggplot2)

  p1 <-
    ggplot2::ggplot(EAP_scores, ggplot2::aes(x=G, y=SE_G))+
    ggplot2::geom_point(alpha=0.05)+
    ggplot2::geom_smooth(colour="black", se=FALSE, linewidth=0.7)+
    ggplot2::ggtitle("(a) Generelle Teilhabe")+
    ggplot2::scale_x_continuous(name = "", breaks = c(-2,-1,0,1,2))+
    ggplot2::scale_y_continuous("Standardfehler", limits = c(0,1))+
    apatheme +
    ggplot2::theme(plot.title = ggplot2::element_text(vjust = -0.2))

  p1 <- ggExtra::ggMarginal(p1, type = "histogram", fill="grey")

  p2 <-
    ggplot2::ggplot(EAP_scores, ggplot2::aes(x=S1, y=SE_S1))+
    ggplot2::geom_point(alpha=0.05)+
    ggplot2::geom_smooth(colour="black", se=FALSE, linewidth=0.7)+
    ggplot2::ggtitle("(b) Teilhabe im formalen Kontext")+
    ggplot2::scale_x_continuous(name = "Theta", breaks = c(-2,-1,0,1,2))+
    ggplot2::scale_y_continuous("", limits = c(0,1))+
    apatheme+
    ggplot2::theme(plot.title = ggplot2::element_text(vjust = -0.2))

  p2 <- ggExtra::ggMarginal(p2, type = "histogram", fill="grey")


  p3 <-
    ggplot2::ggplot(EAP_scores, ggplot2::aes(x=S2, y=SE_S2))+
    ggplot2::geom_point(alpha=0.05)+
    ggplot2::geom_smooth(colour="black", se=FALSE, size=0.7)+
    ggplot2::ggtitle("(c) Teilhabe im informellen Kontext")+
    ggplot2::scale_x_continuous(name = "Theta", breaks = c(-2,-1,0,1,2))+
    ggplot2::scale_y_continuous("Standardfehler", limits = c(0,1))+
    apatheme+
    ggplot2::theme(plot.title = ggplot2::element_text(vjust = -0.2))

  p3 <- ggExtra::ggMarginal(p3, type = "histogram", fill="grey")

  p <- ggpubr::ggarrange(p1, p2, p3, ncol=2,nrow=2)

  if (isTRUE(save_file)) {

    ggplot2::ggsave("inst/output/Abbildung8.2.pdf", plot = p, dpi = 300, width = 8, height = 6.5)

  }

  p

}

#' Create figure 8.3
#'
#' @param save_file if TRUE, a pdf file is generated. File name: Abbildung8.3.pdf
#'
#' @import ggplot2
#' @importFrom mirt fscores
#'

create_figure_8.3 <- function(save_file=FALSE){

  scores_whole_sample <- readRDS("inst/scores/scores_t1.rds")
  models <- readRDS("inst/models/models_state_invariance.rds")
  BD <- readRDS("inst/BD.rds")
  group <- factor(BD, levels=c("RLP", "SH/NS"), labels=c("Rheinland-Pfalz", "Schleswig-Holstein/Niedersachsen"))

  p <- plot_multigroup_scores_distribution(models, scores_whole_sample, group)

  if (isTRUE(save_file)) {

    ggplot2::ggsave("inst/output/Abbildung8.3.pdf", plot = p, dpi = 300, width = 9, height = 7.5)

  }

  p

}

#' Create figure 8.4
#'
#' @param save_file if TRUE, a pdf file is generated. File name: Abbildung8.4.pdf
#'
#' @import ggplot2
#' @importFrom mirt fscores
#'

create_figure_8.4 <- function(save_file=FALSE){

  scores_whole_sample <- readRDS("inst/scores/scores_t1.rds")
  models <- readRDS("inst/models/models_gender_invariance.rds")
  group <- readRDS("inst/gender.rds")

  p <- plot_multigroup_scores_distribution(models, scores_whole_sample, group)

  if (isTRUE(save_file)) {

    ggplot2::ggsave("inst/output/Abbildung8.4.pdf", plot = p, dpi = 300, width = 9, height = 7.5)

  }

  p

}

