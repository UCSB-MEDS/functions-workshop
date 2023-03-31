library(tidyverse)
library(lubridate)
library(chron)
library(naniar)

# Alegria: https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-sbc.2008.14
ale <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-sbc.2008.14&entityid=15c25abf9bb72e2017301fa4e5b2e0d4")

# Mohawk: https://doi.org/10.6073/pasta/cbe43646b801bf6ee5231c301ea23f51
mko <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-sbc.2007.16&entityid=02629ecc08a536972dec021f662428aa")

# Carpenteria: https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-sbc.2004.26
car <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-sbc.2004.26&entityid=1d7769e33145ba4f04aa0b0a3f7d4a76")



ale_cleaned <- ale |> 
  
  # keep only necessary columns
  select(year, month, day, decimal_time, Temp_bot, Temp_top, Temp_mid) |> 
  
  # create date time column
  unite(date, year, month, day, sep = "-", remove = FALSE) |> 
  mutate(time = chron::times(as.numeric(decimal_time))) |> 
  unite(date_time, date, time, sep = " ", remove = TRUE) |> 
  
  # coerce data types
  mutate(date_time = as.POSIXct(date_time, "%Y-%m-%d %H:%M:%S", tz = "GMT"),
         year = as.factor(year),
         month = as.factor(month),
         day = as.numeric(day),
         Temp_top = as.numeric(Temp_top), 
         Temp_mid = as.numeric(Temp_mid), 
         Temp_bot = as.numeric(Temp_bot)) |> 
  
  # add month name
  mutate(month_name = as.factor(month.name[month])) |> 
  
  # replace 9999s with NAs
  replace_with_na(replace = list(Temp_bot = 9999, Temp_top = 9999, Temp_mid = 9999)) |> 
  
  # select/reorder desired columns
  select(date_time, year, month, day, month_name, Temp_bot, Temp_mid, Temp_top) 
  
