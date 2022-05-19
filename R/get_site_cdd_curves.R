#' Generate CDD curves for each site + year
#'
#' @param sites: data.frame with siteid, latitude, longitude, year, and year_day columns
#' @param ub: the upper bound for degree day calculation in degrees Fahrenheit
#' @param lb: the lower bound for degree day calculation in degrees Fahrenheit
#'
#' @return A data.frame with cumulative degree days for the whole year for each site, truncated to include only the last day at 0 CDD and only the first day with the max CDD for the year + site.
#' @export
#' @import dplyr tidyr lubridate
#' @importFrom magrittr %>%
#'
get_site_cdd_curves <- function(sites, ub, lb){
  s <- sites %>%
    get_siteyear_cdd(ub = ub, lb = lb) %>%
    left_join(sites %>% select(siteid, latitude, longitude),
              by = c("latitude", "longitude")) %>%
    mutate(date = parse_date_time(
      paste(year, as.character(year_day)), orders = "yj") %>%
        as_date()
    ) %>%
    select(siteid, date, cdd) %>%
    group_by(siteid)

  ss <- s %>%
    filter(cdd > min(s$cdd, na.rm = T), cdd < max(s$cdd, na.rm = T))

  s0 <- s %>%
    filter(cdd == min(s$cdd, na.rm = T)) %>%
    slice_max(date, n = 1)

  sm <- s %>%
    filter(cdd == max(s$cdd, na.rm = T)) %>%
    slice_min(date, n = 1)

  bind_rows(s0, ss, sm) %>%
    ungroup()
}
