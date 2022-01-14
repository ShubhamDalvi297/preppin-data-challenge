
#2022: Week 2 The Prep School - Birthday Cakes

#load libraries
library(dplyr)
library(lubridate)

#import file & fix date column data type
import_file <- read.csv("PD 2022 Wk 1 Input - Input.csv") %>%
  mutate("Date of Birth" = parse_date_time(Date.of.Birth, orders = c("%m-%d-%Y", "%m/%d/%Y")))

#create column Pupil's Name as Last Name, First Name
add_pupil_name <- import_file %>%
  mutate("Pupil Name" = paste(pupil.first.name,pupil.last.name))

#create column pupil's birthday in calendar year 2022
add_this_year_birthday <- add_pupil_name %>%
  mutate("This Year's Birthday" = update(`Date of Birth`, year = year(today())))

#create column what day of the week the pupil's birthday falls on
#if the birthday falls on a Saturday or Sunday, we need to change the weekday to Friday
add_cake_needed_on <- add_this_year_birthday %>%
  mutate("Cake Needed On" = ifelse(weekdays(`This Year's Birthday`) == "Saturday" | 
                                     weekdays(`This Year's Birthday`) == "Sunday", "Friday", weekdays(`This Year's Birthday`)))

#create column what month the pupil's birthday falls within
add_bd_month <- add_cake_needed_on %>%
  mutate("Month" = months(`Date of Birth`))

#Count how many birthdays there are on each weekday in each month
#Also create a join key
add_count_summarize <- add_bd_month %>% count(Month, `Cake Needed On`) %>% 
  mutate(join_key = paste(Month, `Cake Needed On`))

#create a join key on origin table
add_bd_month_key <- add_bd_month %>%
  mutate(join_key = paste(Month, `Cake Needed On`))

#join tables to repeat how many birthdays there are on each weekday in each month
add_per_wd_mon <- add_bd_month_key %>%
  inner_join(add_count_summarize, by = "join_key") %>%
  rename("BDs per Weekday and Month" = n, "Month" = Month.x, "Cake Needed On" = `Cake Needed On.x`)

#select only new created fields and output the data
cleaned_output <- add_per_wd_mon %>%
  select("Pupil Name", "Date of Birth", 
         "This Year's Birthday", "Month", "Cake Needed On", "BDs per Weekday and Month") %>%
  write.csv(file = "PD 2022 Wk 2 Output.csv", row.names = FALSE)

#clear the objects from the environment
rm(list = ls())
