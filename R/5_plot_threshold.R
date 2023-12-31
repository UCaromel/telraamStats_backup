#' Plot Threshold on Speed Chart
#'
#' @param plot_speed A data frame containing the speed data and chart details.
#' @param selected_speed Character indicating the selected speed threshold.
#' @param state_threshold Character indicating the state of the threshold (automatique or manuel).
#' @param threshold Numeric value representing the manual threshold if state_threshold is "manuel".
#'
#' @import ggplot2
#' @importFrom dplyr filter
#'
#' @export
#'
#' @return A ggplot2 chart with the speed plot and threshold line.
#'
plot_threshold <- function(plot_speed, selected_speed, state_threshold, threshold){
  label <- c("plus de 40km/h", "plus de 30km/h", "plus de 20km/h", "plus de 10km/h")
  # the threshold is calculated according to user parameters
  if (selected_speed == "Toute") {
    mean <- mean(plot_speed$DE_test) # if all speed are selected, the threshold is the mean of the threshold of the 4 curves
  } else {
    ind <- which(label == selected_speed)
    mean <- plot_speed$DE_test[ind]
    plot_speed$data_plot <- plot_speed$data_plot %>% filter(.data$legend==label[ind])
  }

  # to place the threshold text
  mean_ord <- (100+min(plot_speed$data_plot$speed))/2

  if (state_threshold == "automatique") {
    plot_speed$chart + geom_vline(xintercept = mean, color = "red", size = 1.5)+
      geom_text(aes(x = mean, y = mean_ord, label = round(mean)),
                size = 5, angle = -90, vjust = -0.5, color = "red")

  } else if (state_threshold == "manuel") {
    plot_speed$chart + geom_vline(xintercept = threshold, color = "red", size = 1.5) +
      geom_text(aes(x = threshold, y = mean_ord, label = threshold),
                size = 5, angle = -90, vjust = -0.5, color = "red")
  }
}
