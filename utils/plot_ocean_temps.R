#' Create ridge line plot of CDT/ADCP bottom temperature data
#'
#' @param clean_data a data frame that has been pre-processed using clean_ocean_temp()
#'
#' @return a plot object
#' @export
#'
#' @examples
#' my_plot <- plot_ocean_temps(clean_data = my_clean_df)
plot_ocean_temps <- function(clean_data) { 
  
  # load dependencies ----
  library(dplyr)
  library(ggplot2)
  library(ggridges)
  
  # get site name for plot title ----
  site_name <- unique(clean_data$site)
  
  # plot data ----
  temp_plot <- clean_data |> 
    group_by(month_name) |> 
    ggplot(aes(x = Temp_bot, y = month_name, fill = after_stat(x))) +
    ggridges::geom_density_ridges_gradient(rel_min_height = 0.01, scale = 3) + 
    scale_x_continuous(breaks = c(9, 12, 15, 18, 21)) +
    scale_y_discrete(limits = rev(month.name)) + 
    scale_fill_gradientn(colors = c("#2C5374","#778798", "#ADD8E6", "#EF8080", "#8B3A3A"), name = "Temp. (°C)") +
    labs(x = "Bottom Temperature (°C)",
         title = paste("Bottom Temperatures at",  site_name, ", Santa Barbara, CA"), # 2
         subtitle = "Temperatures (°C) aggregated by month from 2005 - 2022") +
    ggridges::theme_ridges(font_size = 13, grid = TRUE) +
    theme(
      axis.title.y = element_blank()
    )
  
  # return plot ----
  return(temp_plot)
}