#' Match observation CDDs to the closest value in a site of interest. The idea here is to say "given a bunch of observations from a range of locations, if I were to make that observation at a specific location, when would I make it?", assuming that CDD is interchangable between observed location and focal location.
#'
#' @param obs_data: data.frame with observations and CDD values
#' @param site_curve: data.frame with a full CDD curve for a given site of interest
#'
#' @return The obs_data data.frame with the closest CDD and date from the
#' @export
#' @import dplyr
#' @importFrom magrittr %>%
#'
match_cdd_obs_site <- function(obs_data, site_curve){
  obs_data %>%
    mutate(site_cdd_index = findInterval(cdd, site_curve[["cdd"]]),
           site_cdd_match = site_curve[["cdd"]][site_cdd_index],
           site_date_match = site_curve[["date"]][site_cdd_index] %>%
             as_date()) %>%
    select(-site_cdd_index)
}
