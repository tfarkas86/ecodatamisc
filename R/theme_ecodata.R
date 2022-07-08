#' A light ggplot theme for EcoData Technology
#'
#' @param base_family: the base font family
#' @param title_family: font to be used for the plot title
#' @param subtitle_family: font to be used for the subtitle
#' @param axis_family: font to be used for the axis titles
#' @param base_color: base color for text and lines
#' @param primary_color: color for primary components
#' @param accent_color: color for accent components
#' @param base_size: base font size
#' @param gridlines: should gridlines be displayed?
#' @param facet_outlines: should facets be outlined?
#'
#' @return A ggplot theme
#' @export
#' @import ggplot2 ggtext
#'
#' @examples
#' mtcars %>% ggplot(aes(x = wt, y = mpg)) + geom_point() + theme_ecodata()
theme_ecodata <- function(base_family = "RobotoCondensed",
                          title_family = "LexendDecaSemiBold",
                          subtitle_family = "LexendDecaThin",
                          axis_family = "LexendDecaLight",
                          base_color = "gray10",
                          primary_color = "#1b2724",
                          accent_color = "#6eb39c",
                          base_size = 10,
                          gridlines = F,
                          facet_outlines = F) {
  min_theme <- theme_bw() +
    theme(
      text = element_text(
        family = base_family,
        colour = base_color,
        size = base_size
      ),
      line = element_line(
        colour = base_color,
        size = 0.2),
      legend.background = element_blank(),
      legend.key = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(
        fill = NA,
        colour = base_color
      ),
      strip.background = element_blank(),
      plot.background = element_blank(),
      axis.line = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_text(
        family = base_family,
        colour = base_color,
        size = base_size
      ),
      plot.title = element_markdown(
        family = title_family,
        colour = primary_color,
        size = base_size * 1.5 ^ 2
      ),
      plot.subtitle = element_markdown(
        family = subtitle_family,
        colour = accent_color,
        size = base_size * 1.5
      ),
      axis.title = element_markdown(
        family = axis_family,
        colour = primary_color,
        size = base_size * 1.4
      ),
      strip.text = element_markdown(
        family = axis_family,
        colour = primary_color,
        size = base_size * 1.4
      )
    )

  if (!gridlines) {
    min_theme <- min_theme + theme(panel.grid = element_blank())
  }

  if (!facet_outlines) {
    min_theme <- min_theme + theme(panel.border = element_blank())
  }

  return(min_theme)
}


#' A light ggplot theme for EcoData Technology
#'
#' @param base_family: the base font family
#' @param title_family: font to be used for the plot title
#' @param subtitle_family: font to be used for the subtitle
#' @param axis_family: font to be used for the axis titles
#' @param base_color: base color for text and lines
#' @param primary_color: color for primary components, including background
#' @param accent_color: color for accent components
#' @param base_size: base font size
#' @param gridlines: should gridlines be displayed?
#' @param facet_outlines: should facets be outlined?
#'
#' @return A ggplot theme
#' @export
#' @import ggplot2 ggtext
#'
#' @examples
#' mtcars %>% ggplot(aes(x = wt, y = mpg)) + geom_point() + theme_ecodata_dark()
theme_ecodata_dark <- function(base_family = "Roboto Condensed",
                               title_family = "Lexend Deca SemiBold",
                               subtitle_family = "Lexend Deca Thin",
                               axis_family = "Lexend Deca Light",
                               base_color = "white",
                               primary_color = "#1b2724",
                               accent_color = "#6eb39c",
                               gridlines = F,
                               facet_outlines = F,
                               base_size = 10) {
  min_theme <- theme_bw() +
    theme(
      text = element_text(
        family = base_family,
        colour = base_color,
        size = base_size
      ),
      line = element_line(
        colour = base_color,
        size = 0.2),
      legend.background = element_blank(),
      legend.key = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(
        fill = NA,
        colour = base_color,
        size = 0.2
      ),
      strip.background = element_blank(),
      axis.line = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      plot.background = element_rect(fill = primary_color),
      axis.text = element_text(
        family = base_family,
        colour = base_color,
        size = base_size
      ),
      plot.title = element_markdown(
        family = title_family,
        colour = accent_color,
        size = base_size * 1.5 ^ 2
      ),
      plot.subtitle = element_markdown(
        family = subtitle_family,
        colour = accent_color,
        size = base_size * 1.5
      ),
      axis.title = element_markdown(
        family = axis_family,
        colour = accent_color,
        size = base_size * 1.4
      ),
      strip.text = element_markdown(
        family = axis_family,
        colour = accent_color,
        size = base_size * 1.4
      )
    )

  if (!gridlines) {
    min_theme <- min_theme + theme(panel.grid = element_blank())
  }

  if (!facet_outlines) {
    min_theme <- min_theme + theme(panel.border = element_blank())
  }

  return(min_theme)
}
