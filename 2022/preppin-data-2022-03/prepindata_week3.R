
#2022: Week 3 The Prep School - Passing Grades

#load libraries ----
library(dplyr)
library(lubridate)
library(tidyr)

#Input both data sets ----

#import file 1 & fix date column data type
input_file1 <- read.csv("PD 2022 Wk 1 Input - Input.csv") %>%
  mutate("Date of Birth" = parse_date_time(Date.of.Birth, orders = c("%m-%d-%Y", "%m/%d/%Y")))

#import file 2
input_file2 <- read.csv("PD 2022 WK 3 Grades.csv")

#Join the data sets together to give us the grades per student
join_data <- input_file1 %>%
  inner_join(input_file2, by = c("id" = "Student.ID"))

#Remove the parental data fields, they aren't needed for the challenge this week
select_fields <- join_data %>%
                    select(id, gender, Maths, English, Spanish, Science, Art, History, Geography)

#Pivot the data to create one row of data per student and subject
pivot_data <- select_fields %>%
                pivot_longer(c(Maths, English, Spanish, Science, Art, History, Geography))

#Rename the pivoted fields to Subject and Score 
rename_fields <- pivot_data %>%
                  rename(Subject = name, Score = value)

#Create an average score per student based on all of their grades
#Round the average score per student to one decimal place
score_per_student <- rename_fields %>%
                        group_by(id) %>%
                        summarise("Student's Avg Score" = round(mean(Score), 1))

make_data <- rename_fields %>%
              inner_join(score_per_student, by = "id")

#Create a field that records whether the student passed each subject
#Pass mark is 75 and above in all subjects
#Aggregate the data per student to count how many subjects each student passed
add_flag <- make_data %>%
              mutate(flag_pass = if_else(Score >= 75, 1, 0)) %>%
              group_by(id) %>%
              summarise("Passed Subjects" = sum(flag_pass))

make_data2 <- add_flag %>%
              inner_join(make_data, by = "id")

#Remove any unnecessary fields and output the data ----
cleaned_output <- make_data2 %>%
  select(-Subject, - Score) %>%
  distinct(id, `Passed Subjects`, `Student's Avg Score`, gender) %>%
  rename("Student ID" = id, "Gender" = gender) %>%
  relocate("Passed Subjects", "Student's Avg Score", "Student ID", "Gender") %>%
  write.csv(file = "PD 2022 Wk 3 Output.csv", row.names = FALSE)

#clear the objects from the environment ----
rm(list = ls())
