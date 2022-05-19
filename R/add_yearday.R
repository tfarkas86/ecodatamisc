#' Add yearday and year columns to a data.frame
#'
#' @param data: a data.frame with a date or datetime column
#' @param date_col: fa quoted name of the date or datetime column
#' @param datetime: should the date_col be parsed as a datetime?
#' @param keep_date_col: should the parsed date_col be kept?
#'
#' @return A data.frame with yearday and year columns
#' @export
#' @import lubridate dplyr
#'
#' @examples
#' add_yearday(data.frame(today()))
add_yearday <- function(data, date_col, datetime = F, keep_date_col = T){

  if(datetime){
    data[[date_col]]<- as_datetime(data[[date_col]])
  } else {
    data[[date_col]]<- as_date(data[[date_col]])
  }

  data$year_day <- yday(data[[date_col]])
  data$year <- year(data[[date_col]])

  if(!keep_date_col){
    data <- select(data, -all_of(date_col))
  }

  return(data)
}

