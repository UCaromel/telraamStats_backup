#' Write or update the sensor data in the data folder
#'
#' This function writes or updates the sensor data in the data folder. It retrieves the data for the specified sensor between \code{date1} and \code{date2} (inclusive) using the \code{retrieve_sensor} function, and then converts certain columns to character strings before writing the data to a CSV file in the data folder.
#'
#' @param nom Character. Name of the sensor.
#' @param date1 Date. Start date in "yyyy-mm-dd" format.
#' @param date2 Date. End date in "yyyy-mm-dd" format.
#' @param listeNom A character vector containing the name of each sensor that is displayed to the user
#' @param listeNombis A character vector containing the identifier name for each vector
#' @param listeSegments A character vector containing the ID of each vector
#'
#' @importFrom lubridate ymd
#' @importFrom readr read_csv2 write_csv2
#' @export
#'
write_update_data <- function(nom, date1, date2,
                              listeNom = c("Burel","Leclerc","ParisMarche","rueVignes","ParisArcEnCiel","RteVitre",
                                           "RueGdDomaine","StDidierNord","rueVallee","StDidierSud","RuePrieure",
                                           "RueCottage","RueVeronniere","RueDesEcoles"),
                              listeNombis = c("Burel-01","Leclerc-02","ParisMarche-03","rueVignes-04","ParisArcEnCiel-05","RteVitre-06",
                                              "RueGdDomaine-07","StDidierNord-08","rueVallee-09","StDidierSud-10","RuePrieure-11",
                                              "RueCottage-12","RueVeronniere-13","RueDesEcoles-14"),
                              listeSegments = c(9000002156, 9000001906, 9000001618,9000003090,9000002453,9000001844,
                                                9000001877,9000002666,9000002181,9000002707,9000003703,
                                                9000003746,9000003775,9000003736)
                              ){
  # Preparation of the dataset
  data <- retrieve_sensor(nom,date1, date2,listeSegments,listeNom)
  # conversion from a numeric vector to a character string of car_speed_hist_0to70plus and car_speed_hist_0to120plus
  data$car_speed_hist_0to70plus <- sapply(data$car_speed_hist_0to70plus, function(x) paste(x, collapse = ", "))
  data$car_speed_hist_0to120plus <- sapply(data$car_speed_hist_0to120plus, function(x) paste(x, collapse = ", "))

  file_name <- paste0("data/",listeNombis[which(listeNom==nom)],".csv")

  if (!is.null(data)){
    if (file.exists(file_name)){
      cleaning <- read_csv2(file_name)
      data <- rbind(cleaning,data)
      data <- data[!duplicated(data$date),] # if some lines are repeated they are eliminated
    }
    write_csv2(data, file = file_name)
  }
}
