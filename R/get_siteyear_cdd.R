#' Get a cumulative degree day value for every day for a given site and year
#'
#' @param data: data.frame with latitude, longitude, year, and year_day columns
#' @param ub: the upper bound for degree day calculation in degrees Fahrenheit
#' @param lb: the lower bound for degree day calculation in degrees Fahrenheit
#'
#' @return A data.frame with cumulative degree days for the whole year for each site
#' @export
#' @import dplyr tidyr purrr lubridate
#' @importFrom magrittr %>%
#'
get_siteyear_cdd <- function(data, ub, lb){
  ad <- data %>%
    distinct(latitude, longitude, year)

  ad <- ad %>%
    mutate(data = pmap(., .f = possibly(function(latitude, longitude, year){

      if(year == year(today())){
        get_daymet(site = "DummySite", lat = latitude, lon = longitude,
                   start = year-1, end = year,
                   use_this_year = T,
                   vars = "tmax,tmin") %>%
          .$data %>%
          filter(year == year) %>%
          select(yday, starts_with("t"))

      }

      get_daymet(site = "DummySite", lat = latitude, lon = longitude,
                 start = year, end = year,
                 use_this_year = F,
                 vars = "tmax,tmin") %>%
        .$data %>%
        select(yday, starts_with("t"))
    }, otherwise = data.frame(NULL))))

  ad %>%
    unnest(data) %>%
    mutate(tavg = ((tmax..deg.c. + tmin..deg.c.)/2) * (9/5) + 32) %>%
    select(-tmax..deg.c., -tmin..deg.c., year_day = yday) %>%
    mutate(tavg = if_else(tavg < lb | tavg > ub, lb, tavg),
           dd = tavg - lb) %>%
    group_by(latitude, longitude, year) %>%
    mutate(cdd = cumsum(dd)) %>%
    select(-tavg, -dd) %>%
    ungroup()
}
