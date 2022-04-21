#' GAM Quick Plot
#'
#' @param model A mgcv::gam model object
#' @param plot_x A column name for the axis of plot
#' @param ylab A user-provided string for the y-axis
#' @param xlab A user-provided string for x-axis
#' @param ylims A numeric vector of length two for the y limits
#'
#'
#' @return Plots the raw response against the first independent variable,
#' then overlays a prediction ribbon.
#' @export
#'
#' @examples
plot_gam <- function(model, plot_x=NULL,
                    ylab=NULL, xlab=NULL,
                    ylims=NULL, xlims=NULL) {

  cols <- model$model %>%
    tibble::as_tibble() %>%
    dplyr::select(c(ifelse(is.null(plot_x), 1, plot_x))) %>%
    names() %>%
    as.list()

  # values for prediction
  vals <- model$model %>%
    tidyr::expand(!!!syms(cols))

  model_p <- tidymv::predict_gam(model, values = vals) %>%
    dplyr::mutate(low = fit - se.fit,
           high = fit + se.fit)

  p <- ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes_string(x = ifelse(is.null(plot_x),
                                                       names(model$model)[2], plot_x),
                          y = names(model$model)[1]),
               data = model$model, alpha = 0.05) +
    ggplot2:: geom_ribbon(ggplot2::aes_string(x = ifelse(is.null(plot_x),
                                                         names(model$model)[2], plot_x),
                           ymin = "low", ymax = "high"),
                data = model_p, alpha = .3, color = "pink") +
    ggplot2::geom_line(ggplot2::aes_string(x = ifelse(is.null(plot_x),
                                                      names(model$model)[2], plot_x),
                         y = "fit"),
              data = model_p, cex = .2, color = "red") +
    ggplot2::scale_x_continuous(name=ifelse(is.null(xlab),
                                            names(model$model)[2], xlab),
                       limits = xlims) +
    ggplot2::scale_y_continuous(name=ifelse(is.null(ylab),
                                            names(model$model)[1], ylab),
                       limits = ylims)
  return(p)

}
