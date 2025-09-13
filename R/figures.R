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

  apatheme=ggplot2::theme_bw()+
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank(),
          text=ggplot2::element_text(family='sans'),
          legend.title=ggplot2::element_blank(),
          legend.position=c(.7,.8),
          axis.line.x = ggplot2::element_line(color='black'),
          axis.line.y = ggplot2::element_line(color='black'))

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
