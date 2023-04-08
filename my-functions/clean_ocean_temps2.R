#' Process CDT/ADCP temperature data
#'
#' @param raw_data data frame of CTD/ADCP data collected at SBC LTER site moorings; search for data on the EDI Data Portal (http://portal.edirepository.org:80/nis/simpleSearch?defType=edismax&q=SBC+LTER%5C%3A+Ocean%5C%3A+Currents+and+Biogeochemistry%5C%3A+Moored+CTD+and+ADCP+data&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false)
#' @param include_temps vector of character strings that includes one or more of the following variable names: Temp_top, Temp_mid, Temp_top
#'
#' @return a data frame
#' @export
#'
#' @examples
#' my_clean_df <- clean_ocean_temps(raw_data = my_raw_df, include_temps = c("Temp_bot"))
clean_ocean_temps <- function(raw_data, include_temps = c("Temp_top", "Temp_mid", "Temp_bot")){ 
  
  # load dependencies ----
  library(dplyr) 
  library(tidyr)
  library(stringr) 
  library(chron)
  library(naniar)
  
  # if data contains these colnames, clean the script
  if(all(c("year", "month", "day", "decimal_time", "Temp_bot", "Temp_top", "Temp_mid") %in% colnames(raw_data))) { 
    
    message("Cleaning data...") 
    
    # get site name as character string from object (raw_data) name ----
    site_name <- deparse(substitute(raw_data)) 
    site_name <- paste(str_to_title(site_name), "Reef") 
    
    # columns to select ----
    standard_cols <- c("year", "month", "day", "decimal_time") 
    all_cols <- append(standard_cols, include_temps) 
    
    # clean data ----
    temps_clean <- raw_data |> 
      select(all_of(all_cols)) |> 
      filter(year %in% c(2005:2020)) |> 
      mutate(site = rep(site_name)) |> 
      unite(date, year, month, day, sep = "-", remove = FALSE) |> 
      mutate(time = chron::times(as.numeric(decimal_time))) |> 
      unite(date_time, date, time, sep = " ", remove = TRUE) |> 
      mutate(date_time = as.POSIXct(date_time, "%Y-%m-%d %H:%M:%S", tz = "GMT"),
             year = as.factor(year),
             month = as.factor(month),
             day = as.numeric(day)) |>
      mutate(month_name = as.factor(month.name[month])) |>
      replace_with_na(replace = list(Temp_bot = 9999, Temp_top = 9999, Temp_mid = 9999)) |> 
      select(any_of(c("site", "date_time", "year", "month", "day", "month_name", "Temp_bot", "Temp_mid", "Temp_top"))) 
    
    # return cleaned df ----
    return(temps_clean) 
    
  } else { 
    
    stop("The data frame provided does not include the necessary columns. Double check your data!") 
    
  }
  
}