
#2022: Week 1 The Prep School - Parental Contact Details

#load library
library(dplyr)

#import file & fix date column data type
import_file <- read.csv("PD 2022 Wk 1 Input - Input.csv") %>%
  mutate(DOB = as.Date(Date.of.Birth, format = "%m/%d/%Y"))

#create column Pupil's Name as Last Name, First Name
add_pupil_name <- import_file %>%
  mutate("Pupil's Name" = paste(pupil.last.name, pupil.first.name, sep = ", "))

#create column Parental Name as Last Name, First Name 
add_parental_first_name <- add_pupil_name %>%
  mutate("parental.first.name" = 
           ifelse(Parental.Contact < 2, Parental.Contact.Name_1, Parental.Contact.Name_2)
         ) %>%
  mutate("Parental Contact Full Name" = paste(pupil.last.name, parental.first.name, sep = ", "))

#create column email address as Parent First Name.Parent Last Name@Employer.com
add_email_addr <- add_parental_first_name %>%
  mutate("Parental Contact Email Address" = paste0(parental.first.name, ".", pupil.last.name, "@", 
                                                  Preferred.Contact.Employer, ".com"))

#create column Academic Year
add_academic_year <- add_email_addr %>%
  mutate("Academic Year" = ifelse(DOB >= "2011-09-01" & DOB <= "2012-08-31", 4,
                           ifelse(DOB >= "2012-09-01" & DOB <= "2013-08-31", 3,
                                  ifelse(DOB >= "2013-09-01" & DOB <= "2014-08-31", 2, 1))))


#select only new created fields and output the data
cleaned_output <- add_academic_year %>%
  select("Academic Year", "Pupil's Name", "Parental Contact Full Name", 
                         "Parental Contact Email Address") %>%
  write.csv(file = "PD 2022 Wk 1 Output.csv", row.names = FALSE)

#clear the objects from the environment
rm(list = ls())
