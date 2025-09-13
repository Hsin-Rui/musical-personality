#' Create figure 7.1
#'
#' @param save_file if TRUE, a pdf file generated under the working directory. File name: Abbildung7.1.pdf
#'
#' @return base plot of factor loadings
#' @importFrom Gifi princals
#'

create_figure_7.1 <- function(save_file = FALSE) {

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

