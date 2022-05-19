#' Manually generate kernel density estimates for a range of CDD values, truncated to 0 CDD and the max CDD value in the input data.
#'
#' @param data: data.frame with a cdd column
#'
#' @return A data.frame with a sequence of CDD values and density estimates, suitable for plotting with geom_line()
#' @export
#' @import dplyr
#' @importFrom magrittr %>%
#'
make_cdd_density_tbl <- function(data, bw = "SJ-dpi"){

  dens <- density(data[["cdd"]], na.rm = T, bw = bw)
  tibble(cdd = dens[["x"]], dens = dens[["y"]]) %>%
    filter(cdd > 0, cdd < max(data[["cdd"]], na.rm = T))
}
