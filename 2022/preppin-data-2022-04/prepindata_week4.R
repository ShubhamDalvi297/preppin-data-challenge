
#2022: Week 4 The Prep School - Travel Plans

#load libraries ----
library(dplyr)
library(tidyr)
library(fuzzyjoin)

#Input the data sets ----
input_file1 <- read.csv("PD 2022 Wk 1 Input - Input.csv")

input_file2 <- read.csv("PD 2021 WK 1 to 4 ideas - Preferences of Travel.csv")

#prep the data ----
#Join the data sets together based on their common field
join_data <- input_file1 %>%
  inner_join(input_file2, by = c("id" = "Student.ID"))

#select only required fields
select_fields <- join_data %>%
                    select(id, M, Tu, W, Th, F)

#Change the weekdays from separate columns to one column of weekdays and one of the pupil's travel choice
pivot_data <- select_fields %>%
                pivot_longer(c(M, Tu, W, Th, F))

#Rename the pivoted fields to Weekday & Method of Travel
rename_fields <- pivot_data %>%
                  rename(Weekday = name, "Method of Travel" = value)

#Group the travel choices together to remove spelling mistakes
#create a data set with actual values to be retained
values_to_keep <- data.frame("Method of Travel" = c("Aeroplane", "Bicycle", "Car", "Dad's Shoulders",
                                                    "Helicopter", "Jumped", "Mum's Shoulders", "Scooter",
                                                    "Skipped", "Van", "Walk", "Hopped")) %>%
  rename("Method of Travel" = `Method.of.Travel`)

#using fuzzy join to fix typos in student's response
final_result <- stringdist_join(values_to_keep, rename_fields, by = "Method of Travel", mode = "left",
                                ignore_case = TRUE, method = "soundex")

#Create a Sustainable (non-motorized) vs Non-Sustainable (motorized) data field 
#Scooters are the child type rather than the motorized type
is_sustainable <- final_result %>%
  mutate("Sustainable?" = if_else(`Method of Travel.x` %in% c("Walk", "Mum's Shoulders",
                                                            "Bicycle", "Scooter", "Dad's Shoulders",
                                                            "Jumped", "Skipped", "Hopped"),
                                  "Sustainable", "Non-Sustainable"))

#Total up the number of pupil's travelling by each method of travel
trips_count <- is_sustainable %>%
  group_by(`Method of Travel.x`, Weekday) %>%
  count() %>%
  rename("Number of Trips" = n) %>%
  mutate(join_key = paste(Weekday, `Method of Travel.x`))

is_sustainable <- is_sustainable %>%
  mutate(join_key = paste(Weekday, `Method of Travel.x`))

make_data <- trips_count %>%
  inner_join(is_sustainable, by = "join_key") %>%
  select(`Method of Travel.x.x`, Weekday.x, `Number of Trips`, `Sustainable?`) %>%
  distinct(`Method of Travel.x.x`, Weekday.x, `Number of Trips`, `Sustainable?`)

#Work out the % of trips taken by each method of travel each day
#Round to 2 decimal places
number_of_trips <- make_data %>%
  group_by(Weekday.x) %>%
  mutate("Trips per day" = sum(`Number of Trips`)) %>%
  mutate("% of trips per day" = round(`Number of Trips`/ `Trips per day`, 2))

#Remove any unnecessary fields and output the data ----
cleaned_output <- number_of_trips %>%
  rename("Method of Travel" = `Method of Travel.x.x`, "Weekday" = Weekday.x) %>%
  relocate("Sustainable?", "% of trips per day", "Trips per day",
           "Number of Trips", "Weekday", "Method of Travel") %>%
  write.csv(file = "PD 2022 Wk 4 Output.csv", row.names = FALSE)

#clear the objects from the environment ----
rm(list = ls())
