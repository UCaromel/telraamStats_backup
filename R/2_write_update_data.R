#' Write or update the sensor data in the data folder
#'
#' This function writes or updates the sensor data in the data folder. It retrieves the data for the specified sensor between \code{date1} and \code{date2} (inclusive) using the \code{retrieve_sensor} function, and then converts certain columns to character strings before writing the data to a CSV file in the data folder.
#'
#' @param id_sensor Numeric. ID of the sensor
#' @param date1 Date. Start date "aaaa-mm-jj"
#' @param date2 Date. End date "aaaa-mm-jj"
#' @param sensor_names list with the name of all the studied sensors
#' @param sensor_ids list with the ids of all the studied sensors
#'
#' @importFrom lubridate ymd
#' @importFrom readr read_csv2 write_csv2
#' @export
#'
write_update_data <- function(id_sensor, date1, date2,
                              sensor_names = c("Burel-01","Leclerc-02","ParisMarche-03","rueVignes-04","ParisArcEnCiel-05","RteVitre-06",
                                               "RueGdDomaine-07","StDidierNord-08","rueVallee-09","StDidierSud-10","RuePrieure-11",
                                               "RueCottage-12","RueVeronniere-13","RueDesEcoles-14","RueManoirs-15","RueToursCarree-16",
                                               "PlaceHotelDeVille-17","BoulevardLiberte-18"),
                              sensor_ids = c(9000002156, 9000001906, 9000001618,9000003090,9000002453,9000001844,
                                             9000001877,9000002666,9000002181,9000002707,9000003703,
                                             9000003746,9000003775,9000003736,9000004971,9000004130,
                                             9000004042,9000004697)
                              ){

  # Preparation of the dataset
  data <- retrieve_sensor(id_sensor,date1, date2)
  # conversion from a numeric vector to a character string of car_speed_hist_0to70plus and car_speed_hist_0to120plus
  data$car_speed_hist_0to70plus <- sapply(data$car_speed_hist_0to70plus, function(x) paste(x, collapse = ", "))
  data$car_speed_hist_0to120plus <- sapply(data$car_speed_hist_0to120plus, function(x) paste(x, collapse = ", "))

  file_name <- paste0("data/",sensor_names[which(sensor_ids==id_sensor)],".csv")

  if (!is.null(data)){
    if (file.exists(file_name)){
      cleaning <- read_csv2(file_name)
      data <- rbind(cleaning,data)
      data <- data[!duplicated(data$date),] # if some lines are repeated they are eliminated
    }
    write_csv2(data, file = file_name)
  }
}
