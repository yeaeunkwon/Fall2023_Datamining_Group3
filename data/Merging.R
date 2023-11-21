library(readr)
data1 <- read_csv("Arizona 1990-01-01 to 1992-09-26.csv")
data2 <- read_csv("Arizona 1992-09-27 to 1992-09-30.csv")
data3 <- read_csv("Arizona 1992-10-01 to 1994-05-31.csv")
data4 <- read_csv("Arizona 1994-06-01 to 1997-02-24.csv")
data5 <- read_csv("Arizona 1997-02-25 to 1997-03-02.csv")
data6 <- read_csv("Arizona 1997-03-03 to 1999-11-27.csv")
data7 <- read_csv("Arizona 1999-11-28 to 1999-12-31.csv")
data8 <- read_csv("Arizona 2000-01-01 to 2001-12-31.csv")
data9 <- read_csv("Arizona 2002-01-01 to 2003-12-31.csv")
data10 <- read_csv("Arizona 2004-01-01 to 2005-12-31.csv")
data11 <- read_csv("Arizona 2006-01-01 to 2007-12-31.csv")
data12 <- read_csv("Arizona 2008-01-01 to 2009-12-31.csv")
data13 <- read_csv("Arizona 2010-01-01 to 2010-01-31.csv")
data14 <- read_csv("Arizona 2010-02-01 to 2012-10-27.csv")
data15 <- read_csv("Arizona 2012-10-28 to 2012-12-31.csv")
data16 <- read_csv("Arizona 2013-01-01 to 2014-12-31.csv")
data17 <- read_csv("Arizona 2015-01-01 to 2015-10-06.csv")
data18 <- read_csv("Arizona 2015-10-07 to 2018-07-01.csv")
data19 <- read_csv("Arizona 2018-07-02 to 2020-10-31.csv")
data20 <- read_csv("Arizona 2020-11-01 to 2023-07-25.csv")
data21 <- read_csv("Arizona 2023-07-26 to 2023-11-15.csv")

library(tidyverse)

# merge all data
data = rbind(data1, data2, data3, data4, data5, data6, data7, data8, data9, data10, data11, data12, data13, data14, 
             data15, data16, data17, data18, data19, data20, data21)


# monthly data 
# datetime 
library(lubridate)
data$year = year(data$datetime)
data$month = month(data$datetime)
data$sunrise_time = format(data$sunrise, "%H:%M:%S")
data$sunset_time = format(data$sunset, "%H:%M:%S")

data %>%
  mutate(
    sunrise_time = as.numeric(hms(sunrise_time)),
    sunset_time = as.numeric(hms(sunset_time))
  ) -> data

data |> 
  dplyr::select(-c(datetime, sunrise, sunset)) |> 
  group_by(year, month) |>
  summarise(across(where(is.double), ~ mean(.x, na.rm=T))) |>
  mutate(
    sunrise_time = sunrise_time / 3600 # hour unit 
  ) -> data_monthly

data |> 
  group_by(year, month) |>
  count(conditions) |>
  pivot_wider(names_from = conditions, values_from=n) -> for_merge_wide

data_monthly |>
  left_join(for_merge_wide) -> data_monthly

data_monthly |>
  ungroup() |>
  select(
    temp, humidity, windspeed, cloudcover, sealevelpressure,
    precip, solarradiation, uvindex, Clear:Rain) -> maindata


write.csv(data, file="merged.csv")
write.csv(data_monthly, file="data_monthly.csv")
write.csv(maindata, file="maindata.csv")
