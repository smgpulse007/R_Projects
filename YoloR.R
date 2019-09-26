 rm(list =ls())
 library(plyr)
 library(dplyr)
 library(lubridate)

d.in <- read.csv("R Projects/patient_dataset.csv")
head(d.in)
# 
# #convert DOB into date-time using lubridate package 
# #CMD+SHIFT+C commands a block of slected text
# 
# df <- mutate(df, dob = mdy(dob), hosp_admission = mdy(hosp_admission))
# df <- mutate(df, age_at_admit = interval(dob, hosp_admission)/ dyears(1))
# head(df)
# 
# #Filter
# 
# df.males <- filter(df, gender == "M")
# 
# #select
# #selects columns
# df.cohort <- select(df, c("age_at_admit", "had_cardiac_arrests"))

#Pipe operator

d.cohort <- d.in %>%
  mutate(d.in, dob = mdy(dob), hosp_admission = mdy(hosp_admission)) %>% 
   mutate(d.in, age_at_admit = interval(dob, hosp_admission)/ dyears(1)) %>%
            select(d.in, c("age_at_admit", "had_cardiac_arrests"))

