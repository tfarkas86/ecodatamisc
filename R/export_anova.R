#' Export an ANOVA model summary as a pretty table.
#'
#' @param model A model object.
#' @param file A file-path destination for your table, with extensions pdf or png. Using a value of NULL will return the LaTeX code for the table.
#' @param par_names A character vector of variable names to replace the model object defaults.
#' @param alpha A numeric from 0 to 1 for boldface identification of significant predictors. Default; 0.05.
#' @param bold_sig A logical indicating whether to put p-values for significant predictors in bold-face type. Default: TRUE.
#' @param digits An integer vector denoting the number of significant digits to print for each of the numeric table fields. Default: c(1, 2, 1, 4).
#' @param align A character vector denoting alignment for each of the table fields. "l", "c", and "r" denote left, center, and right, respectively. Default: c("l", "r", "r", "r").
#' @param keep_tex A logical to save the .tex file. Default: FALSE.
#' @param ... Additional arguments to be passed to car::Anova, knitr::kable, and kableExtra::add_footnote.
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#'
#' mod <- glm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
#' renames <- data.frame(old = c("Sepal.Width", "Petal.Length"),
#'                      new = c("Sepal Width", "Petal Length"))
#' file <- "test_table.pdf"
#' export_anova(mod = mod, file = file, par_names = renames)

export_anova <- function(model, file = NULL, par_names = NULL, alpha = 0.05,
                         bold_sig = TRUE, digits = c(1, 2, 1, 4),
                         align = c("l", "r", "r", "r"), keep_tex = FALSE, ...) {

  # TODO: throw error if mismatch between model parameters and par_names
  #   # label = " ... " will print an (ugly) caption
  #   # type = 3 will change to type 3 ANOVA
  #   # lot of potential options to kable!

  antable <- car::Anova(model, ...) %>%
    tibble::as_tibble(rownames = "Parameter")

  pnamelist <- if(!is.null(par_names)) {
    as.list(par_names[,2]) %>% rlang::set_names(par_names[,1])
  } else { as.list(antable$Parameter) %>%
      rlang::set_names(antable$Parameter) }
  # TODO: allow vector of new names?

  latex_out <- antable %>%
    rlang::set_names(c("Parameter", "$\\chi^2$", "DF", "pvaluetemp")) %>%
    dplyr::mutate(dplyr::across(Parameter, ~ dplyr::recode(.x, !!!pnamelist))) %>%
    dplyr::mutate(dplyr::across(Parameter, ~ stringr::str_replace_all(.x, "_", "\\\\_"))) %>% # wow this is a serious hack
    # TODO: Add more special character exceptions
    dplyr::mutate("pvalue" = ifelse(.[[4]] < 10^-digits[4],
                             paste0("< ", format(10^-digits[4], scientific = FALSE)),
                             format(round(.[[4]], digits[4]), scientific = FALSE))) %>%
    dplyr::mutate(dplyr::across(pvalue, ~ kableExtra::cell_spec(.x, bold = ifelse(pvaluetemp < alpha & bold_sig,
                                                        TRUE, FALSE), format = "latex"))) %>%
    dplyr::select(-4) %>%
    dplyr::rename("$P$-value" = pvalue) %>%
    knitr::kable("latex", booktabs = TRUE, linesep = "", escape = FALSE,
          digits = digits, align = align, ...) %>%
    kableExtra::add_footnote(notation = "symbol", ...)

  if(!is.null(file)) kableExtra::save_kable(x = latex_out,
                                            file = file,
                                            keep_tex = keep_tex)
  else return(latex_out)
}
