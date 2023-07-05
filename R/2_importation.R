#' Import data associated with a list of sensors
#'
#' This function imports data associated with a given list of sensor names.
#'
#' @param list_sensor_name A character vector specifying the names of sensors to import data for.
#' @param listeNom A character vector containing the name of each sensor that is displayed to the user
#' @param listeNombis A character vector containing the identifier name for each vector
#'
#' @return A data.frame containing the imported data.
#'
#' @importFrom purrr map_dfr
#' @importFrom readr read_csv2
#' @importFrom lubridate ymd_hms
#'
#'
#' @export
#'
importation <- function(list_sensor_name,
                        listeNom = c("Burel","Leclerc","ParisMarche","rueVignes","ParisArcEnCiel","RteVitre",
                                      "RueGdDomaine","StDidierNord","rueVallee","StDidierSud","RuePrieure",
                                      "RueCottage","RueVeronniere","RueDesEcoles"),
                        listeNombis = c("Burel-01","Leclerc-02","ParisMarche-03","rueVignes-04","ParisArcEnCiel-05","RteVitre-06",
                                         "RueGdDomaine-07","StDidierNord-08","rueVallee-09","StDidierSud-10","RuePrieure-11",
                                         "RueCottage-12","RueVeronniere-13","RueDesEcoles-14")
                        ){
  list_sensor <- listeNombis[which(listeNom %in% list_sensor_name)]
  data <- data.frame()
  data <- map_dfr(list_sensor, ~ {
    file <- paste0('data/', .x, '.csv')
    if (file.exists(file)) {
      import <- read_csv2(file)
      import$car_speed_hist_0to70plus <-  convert_string_to_list(import$car_speed_hist_0to70plus)
      import$car_speed_hist_0to120plus <- convert_string_to_list(import$car_speed_hist_0to120plus)
      import$date <- ymd_hms(import$date)
      import
    } else {
      NULL
    }
  })
  data
}

#' Convert a character string into a numeric vector
#'
#' @param vector Something in the shape "10,20,30"
#'
#' @return Numeric vector. Something in the shape c(10,20,30)
#'
#' @export
#'
#' @examples
#' convert_string_to_list("10,20,30")
#'
#' @keywords internal
#'
convert_string_to_list <- function(vector){
  convert <- as.list(vector)
  lapply(vector, function(x) {
    elements <- unlist(strsplit(x, ",\\s*"))
    numeric_elements <- as.numeric(elements)
    return(numeric_elements)
  })
}
