#' Get Daymet data for a given site and set of years
#'
#'@param site: site name (not important)
#'@param lat: latitude
#'@param lon: longitude
#'@param start: start of the range of years over which to download data
#'@param end: end of the range of years over which to download data
#'@param vars: string of variables to get for the site
#'@param use_this_year: allow end year to be this year, though there might not be complete data
#'
#' @return A data.frame of Daymet data for a given site
#' @export
#' @import httr
#'
#' @examples
#' get_daymet()
get_daymet <- function (site = "Daymet", lat = 36.0133, lon = -84.2625, start = 2000, end = as.numeric(format(Sys.time(), "%Y")) - 1, vars = "tmax,tmin,dayl,prcp,srad,swe,vp", use_this_year = F)
{
  url <- "https://daymet.ornl.gov/single-pixel/api/data"

  if (!use_this_year) {
    max_year <- as.numeric(format(Sys.time(), "%Y")) - 1
  }
  else {
    max_year <- as.numeric(format(Sys.time(), "%Y"))
  }
  if (start < 1980) {
    stop("Data starts in 1980, you tried to get earlier data.")
  }
  if (end > max_year) {
    stop("You've exceeded the maximum year.")
  }

  years <- paste(seq(start, end, by = 1), collapse = ",")

  query <- list(lat = lat, lon = lon, vars = vars,
                year = years)
  tmp_file <- file.path(normalizePath(tempdir()),
                        sprintf("%s_%s_%s.csv",
                                site, start, end))
  message(paste("Downloading DAYMET data for: ", site,
                " at ", lat, "/", lon, " latitude/longitude !\n",
                sep = ""))

  error <- httr::GET(url = url, query = query,
                     httr::write_disk(path = tmp_file, overwrite = TRUE))

  if (httr::status_code(error) == 400) {
    file.remove(tmp_file)
    stop("Data outside DAYMET spatial coverage.")
  }
  if (httr::status_code(error) > 400) {
    file.remove(tmp_file)
    stop("Check connection, could not reach server.")
  }

  read.csv(tmp_file, skip = 7)

}
