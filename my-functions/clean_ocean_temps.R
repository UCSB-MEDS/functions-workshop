#' Clean CTD/ADCP temperature data
#'
#' @param df data frame of CTD/ADCP data collected at SBC LTER site moorings; search for data on the EDI Data Portal (http://portal.edirepository.org:80/nis/simpleSearch?defType=edismax&q=SBC+LTER%5C%3A+Ocean%5C%3A+Currents+and+Biogeochemistry%5C%3A+Moored+CTD+and+ADCP+data&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false)
#' @param include_temps vector of character strings that includes one or more of the following variable names: Temp_top, Temp_mid, Temp_top
#'
#' @return data frame 
#' @export
#'
#' @examples
clean_ocean_temps <- function(df, include_temps = c("Temp_top", "Temp_mid", "Temp_bot")) {
  
  require(dplyr)
  require(chron)
  require(naniar)
  
  # if data contains these colnames, clean the script
  if(all(c("year", "month", "day", "decimal_time", "Temp_bot", "Temp_top", "Temp_mid") %in% colnames(df))) { # ! !
    
    message("Cleaning data...")
    
    # columns to select
    standard_cols <- c("year", "month", "day", "decimal_time")
    all_cols <- append(standard_cols, include_temps)
    
    # if(any(c("Temp_top", "Temp_mid", "Temp_bot")) == include_temps) { # check out: https://stackoverflow.com/questions/50431362/r-finding-multiple-string-matches-in-a-vector-of-strings
    #   all_cols <- append(standard_cols, include_temps)
    # } else {
    #   stop("The `include_temps` arg only takes the values, 'Temp_top', 'Temp_mid', and/or 'Temp_top'. Double-check your spelling.")
    # }
    
    # get site name to add as column
    
    # clean data
    temps_clean <- df |> 
      
      # keep only necessary columns
      select(all_of(all_cols)) |> # ! !
      
      # add column with site name
      mutate(site = rep("Alegria Reef")) |> 
      
      # create date time column
      unite(date, year, month, day, sep = "-", remove = FALSE) |> 
      mutate(time = chron::times(as.numeric(decimal_time))) |> 
      unite(date_time, date, time, sep = " ", remove = TRUE) |> 
      
      # coerce data types
      mutate(date_time = as.POSIXct(date_time, "%Y-%m-%d %H:%M:%S", tz = "GMT"),
             year = as.factor(year),
             month = as.factor(month),
             day = as.numeric(day)) |>

      # add month name
      mutate(month_name = as.factor(month.name[month])) |>
      
      # replace 9999s with NAs (will throw warning if var isn't present, but still execute)
      replace_with_na(replace = list(Temp_bot = 9999, Temp_top = 9999, Temp_mid = 9999)) |> 

      # select/reorder desired columns
      select(any_of(c("site", "date_time", "year", "month", "day", "month_name", "Temp_bot", "Temp_mid", "Temp_top"))) # ! !
    
    return(temps_clean)
    
    # if data does not contain those colnames, throw error message
  } else {
    
    stop("The data frame provided does not include the necessary columns. Double check your data!")
    
  }
  
}
