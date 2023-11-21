# Fall2023_Datamining_Group3

**merged.csv**: This file represents the data merged from each data file that everyone has downloaded.
From 01-01-1990 to 11-15-2023.

**data_monthly.csv**: This file contains data averaged on a monthly basis from the merged data.
The `conditions` column has nominal values, but when aggregating the data on a monthly basis, the frequencies of each nominal category are calculated.

**maindata.csv**: This file contains only selected features from 'data_monthly.csv', including temp, humidity, windspeed, cloudcover, sealevelpressure, precip, solarradiation, uvindex, Clear:Rain(conditions)
