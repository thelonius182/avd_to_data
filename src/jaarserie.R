library(lubridate)
library(dplyr)

year_series = seq.Date(from = ymd("2023-01-07"), by = 1, length.out = 365) %>% as_tibble() %>% rename(this_ymd = value)
sat5th = year_series %>% mutate(day_name = weekdays(this_ymd),
                                day_number = day(this_ymd)) %>% 
  filter(day_name == 'Saturday' & day_number %in% c(29, 30, 31))
