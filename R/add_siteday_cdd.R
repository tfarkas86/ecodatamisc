#' Add a cumulative degree day value for each year + year_day + site combination
#'
#' @param data: data.frame with latitude, longitude, year, and year_day columns
#' @param ub: the upper bound for degree day calculation in degrees Fahrenheit
#' @param lb: the lower bound for degree day calculation in degrees Fahrenheit
#'
#' @return A data.frame with with a cumulative degree day added for each year + year_day + site
#' @export
#' @import dplyr
#'
add_siteday_cdd <- function(data, ub = 120, lb = 50){

  left_join(data,
            get_siteyear_cdd(data = data, ub = ub, lb = lb),
            by = c("latitude", "longitude",
                   "year", "year_day"))
}
