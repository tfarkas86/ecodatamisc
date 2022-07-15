#' @import sysfonts showtext
.onLoad <- function(libname, pkgname){
  sysfonts::font_add_google("Lexend Deca", "LexendDecaSemiBold", regular.wt = 600)
  sysfonts::font_add_google("Lexend Deca", "LexendDecaThin", regular.wt = 200)
  sysfonts::font_add_google("Lexend Deca", "LexendDecaLight", regular.wt = 400)
  sysfonts::font_add_google("Roboto Condensed", "RobotoCondensed")
  showtext::showtext_auto()
  showtext::showtext_opts(dpi = 300)
}

.onAttach <- function(libname, pkgname){
  packageStartupMessage("Necessary Google fonts added to tempdir(), and showtext_auto() enabled in order to use them.\nRun showtext::showtext_auto(enable = F) to turn off.\n\nNote that showtext and RStudio plot Viewer do not always play nicely.\nTry saving a plot with grDevices::jpeg to see what text will look like.")
}
