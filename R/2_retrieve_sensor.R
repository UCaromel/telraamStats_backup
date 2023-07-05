#' Retrieve data associated with a sensor from the Telraam API
#'
#' This function retrieves data associated with a sensor from the Telraam API. The data is retrieved for a specified time period between \code{date1} and \code{date2} (inclusive).
#'
#' @param nom Character. Name of the sensor, obtained from the \code{listeNom} vector.
#' @param date1 Date. Start date in "yyyy-mm-dd" format.
#' @param date2 Date. End date in "yyyy-mm-dd" format.
#' @param listeSegments A character vector containing the ID of each vector
#' @param listeNom A character vector containing the name of each sensor that is displayed to the user
#'
#'
#' @importFrom lubridate ymd_hms days
#' @importFrom purrr pmap
#' @importFrom httr POST add_headers
#' @importFrom dplyr filter bind_rows %>%
#' @export
#'
retrieve_sensor <- function(nom,date1,date2,
                            listeSegments = c(9000002156, 9000001906, 9000001618,9000003090,9000002453,9000001844,
                                               9000001877,9000002666,9000002181,9000002707,9000003703,
                                               9000003746,9000003775,9000003736),
                            listeNom = c("Burel","Leclerc","ParisMarche","rueVignes","ParisArcEnCiel","RteVitre",
                                          "RueGdDomaine","StDidierNord","rueVallee","StDidierSud","RuePrieure",
                                          "RueCottage","RueVeronniere","RueDesEcoles")){
  # Initialization
  result <- data.frame()
  date2 <- date2 + days(1) # so that date2 is included
  idCapteur <- listeSegments[which(listeNom==nom)] # retrieve the identifier associated to the name of the sensor
  dates <- seq_by_3_month(date1,date2) # for the iteration of the retrieving, because when we call the API, the period can not exceed 3 month for each call

  # calling of the API
  resTraffic_list <- pmap(list(dates$start, dates$end), ~ {
    resTraffic <- POST("https://telraam-api.net/v1/reports/traffic",
                       add_headers("X-Api-Key" = key),
                       body = paste0('{
                       "level": "segments",
                       "format": "per-hour",
                       "id": "', idCapteur, '",
                       "time_start": "', ..1, '",
                       "time_end": "', ..2, '"
                     }'))

    content <- resTraffic$content %>%
      rawToChar() %>%
      fromJSON()
    df <- content$report
    df$date <- ymd_hms(df$date, tz = df$timezone[1])
    df
  })

  result <- bind_rows(resTraffic_list)

  if (length(result$date)!=0){
    result$date <- ymd_hms(result$date, tz = result$timezone[1]) # We change the class of date with a time difference of 2
    result$segment_id <- rep(nom,nrow(result)) # we remplace the identifier by the name of the sensor

    # we select the data that we don't consider null (arbitrary choice)
    result <- result %>% filter(.data$uptime > 0.5,
                                .data$heavy_lft + .data$car_lft + .data$pedestrian_lft +
                                  .data$bike_lft + .data$heavy_rgt + .data$car_rgt +
                                  .data$pedestrian_rgt + .data$bike_rgt >0
    )
  }
  return(result)
}


#' Generate sequence of intervals with three-month periods
#'
#' This function is used internally in the \code{retrieve_sensor} function to generate a sequence of intervals with three-month periods. It takes a start date (\code{date1}) and an end date (\code{date2}), and returns a data frame with two columns representing the start and end dates of each interval.
#'
#' @param date1 Date. Start date in "yyyy-mm-dd" format.
#' @param date2 Date. End date in "yyyy-mm-dd" format.
#'
#' @importFrom lubridate ymd
#'
#' @keywords internal
#'
seq_by_3_month <- function(date1, date2){
  if (date1==date2){
    return(data.frame(start = date1, end = date1))
  }else{
    date <- seq(from = date1, to = date2, by = "3 month")
    if (date[length(date)]!=date2){
      date <- c(date,date2)
    }
    return(data.frame(start = date[1:(length(date)-1)],
                      end   = date[2:length(date)]))
  }
}
