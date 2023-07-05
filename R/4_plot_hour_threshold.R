#' Plot Eng React Function
#'
#' This function plots the traffic data for a selected sensor, including the average speed and traffic volume
#' by hour of the day.
#'
#' @param ... data and parameters to filter
#'
#'
#' @return A list containing the plotly figure and the aggregated traffic data by hour.
#'
#' @importFrom dplyr group_by summarise full_join %>%
#' @importFrom lubridate hour
#' @importFrom stats sd
#' @importFrom ggplot2 geom_point scale_y_continuous sec_axis scale_linetype_manual theme_bw element_line
#' @importFrom rlang :=
#'
#' @export
plot_hour_threshold <- function(...) {
  data <- do.call(filtering,list(...))

  if (is_empty(data)) {
    return(NULL)
  }

  v85 <- data$v85
  car_rgt <- data$car_rgt
  heavy_rgt <- data$heavy_rgt
  car_lft <- data$car_lft
  heavy_lft <- data$heavy_lft

  calc <- summarize_data(data, v85, car_rgt, heavy_rgt, car_lft, heavy_lft,
                         "speed", "B_to_A", "A_to_B")

  speed <- calc$speed
  B_to_A <- calc$B_to_A
  A_to_B <- calc$A_to_B



  # These variables are used to center and reduce the second scale (right axe)
  mean_car <- mean(c(B_to_A,B_to_A))
  sd_car <- sd(c(B_to_A,B_to_A))
  mean_speed <- mean(speed)
  sd_speed <- sd(speed)
  calc <- calc %>% mutate(speed = ((speed-mean_speed)/sd_speed)*sd_car+mean_car)

  graph <- ggplot(calc, aes(x = hour)) +
    geom_line(aes(y = B_to_A, color = "B vers A", linetype = "B vers A"), size = 1) +
    geom_line(aes(y = A_to_B, color = "A vers B", linetype = "A vers B"), size = 1) +
    geom_line(aes(y = speed, color = "Vitesse v85 moyen", linetype = "Nombre de vehicules moyen"), size = 1) +
    geom_point(aes(y = B_to_A), color = "blue",size = 3) +
    geom_point(aes(y = A_to_B), color = "blue",size = 3) +
    geom_point(aes(y = speed), color = "red",size = 3) +
    labs(x = "Heure", y = "Nombre de vehicules moyen", color = "Legende", linetype = "Legende") +
    scale_y_continuous(
      sec.axis = sec_axis(~((.-mean_car)/sd_car)*sd_speed+mean_speed, name = "Vitesse v85 moyenne (km/h)")
      # breaks = seq(0,round(max(calc$Vitesse))+100,100)
    ) +
    scale_x_continuous(
      breaks = min(calc$hour):max(calc$hour)
    ) +
    scale_color_manual(values = c("blue", "blue", "red")) +
    scale_linetype_manual(values = c("dotted", "solid", "solid")) +
    theme_bw() +
    theme(legend.position = "bottom", legend.box = "horizontal",
          axis.ticks.y.left = element_line(color = "blue"), # change the color of the ticks
          axis.text.y.left = element_text(color = "blue"), # change the color of the number associated to a tick
          axis.text.y.right = element_text(color = "red"),
          axis.ticks.y.right = element_line(color = "red"),
          axis.title.y.left = element_text(color = "blue"),
          axis.title.y.right = element_text(color = "red"))

  return(list(graph=graph,data=calc))

}

#' Summarize Data
#'
#' This function summarizes the data by calculating the mean values of different variables
#' for each hour of the day.
#'
#' @param data A data frame containing the input data.
#' @param v85 The variable representing v85.
#' @param car_rgt The variable representing car_rgt.
#' @param heavy_rgt The variable representing heavy_rgt.
#' @param car_lft The variable representing car_lft.
#' @param heavy_lft The variable representing heavy_lft.
#' @param speed The name of the variable to store the mean v85 values.
#' @param B_to_A The name of the variable to store the mean sum of car_rgt and heavy_rgt values.
#' @param A_to_B The name of the variable to store the mean sum of car_lft and heavy_lft values.
#'
#' @return A summarized data frame with mean values for each hour.
#'
#' @importFrom dplyr group_by summarise %>%
#' @importFrom rlang :=
#' @importFrom lubridate hour
#'
#'@noRd
#'
summarize_data <- function(data, v85, car_rgt, heavy_rgt, car_lft, heavy_lft,
                           speed, B_to_A, A_to_B) {

    data %>%
      group_by(hour = hour(date)) %>%
      summarise({{ speed }} := mean({{ v85 }}, na.rm = TRUE),
                {{ B_to_A }} := mean({{ car_rgt }} + {{ heavy_rgt }}, na.rm = TRUE),
                {{ A_to_B }} := mean({{ car_lft }} + {{ heavy_lft }}, na.rm = TRUE))
}


