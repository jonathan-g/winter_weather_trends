library(magrittr)
library(tidyverse)
library(lubridate)
library(rnoaa)

#
# Get an web services token from NOAA at https://www.ncdc.noaa.gov/cdo-web/token
# and paste it into the code below
#

if (is.null(getOption("noaakey")))
  options(noaakey="<your token goes here>")

if (str_detect(getOption("noaakey"), "[^A-Za-z0-9]"))
  stop("You need to configure a NOAA web services token before you can use this code.")


# This is a county-level FIPS code. Default is Davidson County, TN
my_location_code <- fipscodes %>%
  filter(state == "Tennessee", county == "Davidson") %>% head(1) %$% fips %>%
  str_c("FIPS:", .)

#
# Get stations in the selected county, and sort by length of record.
#
stations = ncdc_stations(datasetid = "GHCND", locationid=my_location_code, limit = 100) %$% data %>%
  as.tibble() %>% mutate(mindate = ymd(mindate), maxdate = ymd(maxdate),
                         datespan = maxdate - mindate) %>%
  arrange(desc(datespan), mindate)

#
# The station with the longest record
#
best_station <- head(stations,1)

#
# Lists of available data sets, data categories, and data types
#
data_sets = ncdc_datasets(station_id = best_station$id)
data_cats = ncdc_datacats(datasetid = "GHCND")
data_types = ncdc_datatypes(datasetid = "GHCND", datacategoryid = "TEMP")


#
# Get a given type of data for a station for a specified year
#
get_one_year <- function(stationid, datasetid, datatypeid, year, limit = 366) {
  offset = 1
  records = 0
  goal = 366
  data <- tibble()
  start_date = str_c(year, "-01-01")
  end_date = str_c(year, "-12-31")
  while(records < goal) {
    start.time = now()
    message("Getting records ", offset, "-", offset + limit - 1)
    new_data <- ncdc(stationid = stationid, datasetid = datasetid, datatypeid = datatypeid,
                     startdate = start_date, enddate = end_date,
                     offset = offset, limit = limit)
    if (is.null(new_data$meta$totalCount)) {
      break
    } else {
      goal = new_data$meta$totalCount
      offset = new_data$meta %$% {offset + pageCount}
      data = bind_rows(data, new_data$data)
      records <- nrow(data)
      delta.time = now() - start.time
      if(delta.time < 1 && records < goal) {
        message("Sleeping for ", round(1.0 - delta.time, 3), " seconds")
        Sys.sleep(1.0 - delta.time)
      }
    }
  }
  data
}

#
# Get data for the full record of the station.
#
read_station_data <- function(station, datasetid, datatypeid) {
  data <- tibble()
  years <- seq(year(min(station$mindate)), year(max(station$maxdate)))
  for (year in years) {
    message("Getting ", datatypeid, " data for ", station$name, " for year ", year)
    this_year <- get_one_year(station$id, datasetid, datatypeid, year)
    data <- data %>% bind_rows(this_year)
    if (year < max(years)) Sys.sleep(1)
  }
  data
}

#
# Count up days per winter with minimum temperature below a threshold.
# Assign winters to the year of the beginning (e.g., 1950-1951 winter is
# assigned to 1950).
#
get_cold_days <- function(temp_record, threshold_f = 0.0) {
  winters <- temp_record %>%
    mutate(date = as_datetime(date), month = month(date),
           year = year(date) - ifelse(month <= 6, 1, 0),
           tmin = (9.0 / 5.0) * (value / 10.0) + 32.0) %>%
    group_by(year) %>% summarize(cold_days = sum(tmin < threshold_f)) %>%
    ungroup() %>%
    filter(year > min(year))
  winters
}

#
# Add up cumulative snowfall for each winter.
# Assign winters to the year of the beginning (e.g., 1950-1951 winter is
# assigned to 1950).
#
get_winter_snow <- function(snow_record) {
  winters <- snow_record %>%
    mutate(date = as_datetime(date), month = month(date),
           year = year(date) - ifelse(month <= 6, 1, 0),
           snow = value / 25.4) %>%
    group_by(year) %>% summarize(snow = sum(snow)) %>%
    ungroup() %>%
    filter(year > min(year))
    winters
}

plot_cold_days <- function(temp_record, threshold_f = 0.0,
                           baseline = FALSE, location_name = NULL) {
  plot_color = "#3A5FCD"
  cold_days <- get_cold_days(temp_record, threshold_f)
  plot_title = ifelse(is.null(location_name),
                      "Cold ways in winters",
                      str_c("Cold days in ", location_name, " winters"))

  baseline_level = cold_days %>% filter(year >= 1949 & year < 1979) %>%
    summarize(cold_days = mean(cold_days))

    p <- ggplot(cold_days, aes(x = year, y = cold_days)) +
    geom_col(color = plot_color, fill = plot_color)

  if (baseline)
    p <- p + geom_hline(color = plot_color, yintercept = baseline_level$cold_days)

  p <- p +
    scale_x_continuous(expand=c(0,0), breaks = seq(1800, 2050, 10)) +
    scale_y_continuous(expand = c(0,0), limits = c(0,max(cold_days$cold_days) + 0.5)) +
    labs(x = "Year",
         y = bquote("# days below " * .(round(threshold_f, 1)) * degree * F),
         title = plot_title)
  p
}

plot_snow <- function(snow_record, baseline = FALSE, location_name = NULL) {
  plot_color = "#3A5FCD"
  snow <- get_winter_snow(snow_record)
  plot_title = ifelse(is.null(location_name),
                      "Snow fall in winters",
                      str_c("Snow fall in ", location_name, " winters"))


  baseline_level = snow %>% filter(year >= 1949 & year < 1979) %>%
    summarize(snow = mean(snow))

  p <- ggplot(snow, aes(x = year, y = snow)) +
    geom_col(color = plot_color, fill = plot_color)

  if (baseline)
    p <- p + geom_hline(color = plot_color, yintercept = baseline_level$snow)

  p <- p +
    scale_x_continuous(expand=c(0,0), breaks = seq(1800, 2050, 10)) +
    scale_y_continuous(expand = c(0,0), limits = c(0,max(snow$snow) + 1)) +
    labs(x = "Year", y = "Inches of snow",
         title = plot_title)
  p
}

get_data <- function() {
  temp_data <<- read_station_data(best_station, "GHCND", "TMIN")
  snow_data <<- read_station_data(best_station, "GHCND", "SNOW")
  theme_set(theme_bw(base_size = 12))
}
