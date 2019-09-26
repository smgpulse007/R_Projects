library(readr)
library(dplyr)
library(lubridate)
library(signal)
library(data.table)

myfilter = butter(4, 0.1, type = 'low', plane='z')  
gfilter = butter(4, 0.3, type = 'high', plane='z')  
detach("package:signal", unload=TRUE)



# for (i in 2:7) {
#   `110501_2019-06-21`[,i] <- signal::filter(myfilter, `110501_2019-06-21`[,i])
# }
# flt <- function(x){
#   filter(myfilter, x)
# }

# #Stores cumulative metrics as a list of data frames
# fn_cum <- list.files("~/Downloads/m18-918_study_archive 2/", pattern="*cumulative_metrics.csv", full.names=TRUE, include.dirs = TRUE, recursive = TRUE)
# ldf_cum <- lapply(fn_cum, read_csv)
# #res_daily <- lapply(ldf, summary)
# #names(res_daily) <-  paste0("d.cum", substr(fn_cum, 74, 79))

#Stores daily metrics as a list of data frames
fn_daily <- list.files("~/Downloads/m18-918_study_archive 2/", pattern="[0-9].*[0-9].csv", full.names=TRUE, include.dirs = TRUE, recursive = TRUE)
ldf_daily <- lapply(fn_daily, fread)
#res_daily <- lapply(ldf_daily, summary)
names(ldf_daily) <-  paste0(substr(fn_daily,73, 79), substr(fn_daily,102, 111))

#Stores accel data as a list of data frames
fn_accel <- list.files("~/Downloads/BioStamp918", pattern="*accel.csv", full.names=TRUE, include.dirs = TRUE, recursive = TRUE)
#ldf_accel <- lapply(fn_accel, fread)
gsub("left", "right", fn_accel) -> fn_accel
#res_accel <- lapply(ldf_accel, summary)
#names(ldf_accel) <-  paste0(substr(fn_accel,53,58), substr(fn_accel,105, 115))
accel_list <-  paste0(substr(fn_accel,53,58), substr(fn_accel,105, 115))
fn_accel <- list.files("~/Downloads/BioStamp918", pattern="*accel.csv", full.names=TRUE, include.dirs = TRUE, recursive = TRUE)


#Stores gyro data as a list of data frames
fn_gyro <- list.files("~/Downloads/BioStamp918", pattern="*gyro.csv", full.names=TRUE, include.dirs = TRUE, recursive = TRUE)
#ldf_gyro <- lapply(fn_gyro, fread)
gsub("left", "right", fn_gyro) -> fn_gyro
#res_gyro <- lapply(ldf_gyro, summary)
#names(ldf_gyro) <-  paste0(substr(fn_gyro,53,58), substr(fn_gyro,105, 115))
gyro_list <-  paste0(substr(fn_gyro,53,58), substr(fn_gyro,105, 115))
fn_gyro <- list.files("~/Downloads/BioStamp918", pattern="*gyro.csv", full.names=TRUE, include.dirs = TRUE, recursive = TRUE)

# Extracts every row that has "MOVING:WALKING" and the row that immediately follows it for futher extraction of the time interval.
for (i in 1:length(ldf_daily)) {
  ind2 <- which(ldf_daily[[i]]$ACTIVITY=="MOVING:WALKING")
  ind3 <- which(ldf_daily[[i]]$ACTIVITY=="MOVING:WALKING") + 1
  ldf_daily[[i]][c(ind2, ind3),2:3] -> ldf_daily[[i]] #Selects only the timestamp and the activity column
}

Filter(function(x) dim(x)[[1]] > 0, ldf_daily) -> ldf_daily #Removes null dataframes from daily metrics list

#Orders/Sorts the data by timestamp in ascending order, which will help for futher extraction of the time interval.
for (z in 1:length(ldf_daily)) {
  ldf_daily[[z]] <- ldf_daily[[z]][order(ldf_daily[[z]]$`UTC Timestamp (utc_milliseconds)`),] 
  #sort(ldf_daily[[z]]$`UTC Timestamp (utc_milliseconds)`) -> ldf_daily[[z]]$`UTC Timestamp (utc_milliseconds)`
}

#Merges accel and gyro data together in a list of dataframes containing daily data named in the format "{Subject_ID}_{YYYY-MM-DD}"
#Reduces timestamps from e15 to e12 (in epochs)
#Adds the ID as a column for each subject respectively
# df1= list()
# 
# for (i in 1:length(ldf_accel)) {
#   for (j in 1:length(ldf_gyro)) {
#     if (names(ldf_accel)[i] == names(ldf_gyro)[j]) {
#       merge(ldf_accel[i],ldf_gyro[j]) -> df[[j]] # read each csv, merge and continue as normal
#       df[[j]] %>% dplyr::mutate(id = substr(names(ldf_accel)[i], 1, 6)) -> df[[j]]
#       df[[j]][1] = df[[j]][1]/1000
#       #assign(paste(substr(names(ldf_accel)[i], 1, 18)), names(df[j]))
#       names(df)[j] <-  paste(substr(names(ldf_gyro)[j], 1, 18))
#     }
#   }
# }  
df =data_frame()
df2=data.frame()
df3=data.frame()
#names(df3) <- c("Timestamp", "A_x", "A_y", "A_z", "G_x", "G_y", "G_z", "ID")

#######BUGGED CODE########
#Filters merged data from list "df" to get data when the subject is walking
#This is acheived by extracting the data between "p" row containing "MOVING:WALKING" and "p+1" row. 
#This works perfectly for one dataframe `110501_2019-06-21` as the outtput matches what we expect to see.
#However the code doesn't seem to iterate over the rest of the data to find matches as one only dataframe is outputted without errors.
for(m in 1:length(accel_list)){  # loop through num subjects
  for (k in 1:length(ldf_daily)) {  #also loops through subjects
    if (names(ldf_daily)[k] == accel_list[m]) {
      walking <- 0; min_p <- 0; # establish baseline variables
      for (p in 1:(nrow(ldf_daily[[k]])-1)) {
        if (ldf_daily[[k]][p,2]$ACTIVITY=="MOVING:WALKING") {  # walking
          if (walking == 0) {min_p <- p}  # switch to walking, store index at start
          walking <- 1
        } else {  # not walking
          if (walking == 1) {  # switch to not walking, store index at end, add values to dataframe
            print(c('p_range', min_p,p))
            # read in accel and gyro files and merge to make dataframe and then subset as normal below
            accel <- fread(fn_accel[m])
            gyro <- fread(fn_gyro[m])
            df <- merge(accel, gyro)
            df %>% dplyr::mutate(id = substr(accel_list[m], 1, 6)) -> df
            df[1] = df[1]/1000
            #df[8] <- as.numeric(as.character(df[8]))
            print("Don with operations on df")
            
            #df[] <- lapply(df, function(x) as.numeric(as.character(x)))
            df2 <- rbind(df2, df %>% filter(df$`Timestamp (microseconds)` >= ldf_daily[[k]]$`UTC Timestamp (utc_milliseconds)`[min_p] & df$`Timestamp (microseconds)` <= ldf_daily[[k]]$`UTC Timestamp (utc_milliseconds)`[p]))
            #print(c("range", dim(df[[m]] %>% filter(df[[m]][1] >= ldf_daily[[k]]$`UTC Timestamp (utc_milliseconds)`[min_p] & df[[m]][1] <= ldf_daily[[k]]$`UTC Timestamp (utc_milliseconds)`[p]))))
          }
          walking <- 0;   
        }
      }
      #Apply low-pass 4th order butterworth filter
      #df2[2:7] <- lapply(df2[2:7], filter(myfilter, ))
      for (z in 2:4) {
        if (is.data.frame(df2) && nrow(df2)!=0) {
         # df2[,z] <- signal::filter(myfilter, df2[,z])
          df2[,z] <- as.numeric(signal::filter(myfilter, df2[,z]))
        }
      }

      for (f in 5:7) {
        if (is.data.frame(df2) && nrow(df2)!=0) {
          # df2[,z] <- signal::filter(myfilter, df2[,z])
          df2[,f] <- as.numeric(signal::filter(gfilter, df2[,f]))
        }
      }
      if (is.data.frame(df2) && nrow(df2)!=0) {
      names(df2) <- c("Timestamp", "A_x", "A_y", "A_z", "G_x", "G_y", "G_z", "ID")
      }
      
      df3 <- rbind(df3, df2)
        
   
      assign(paste(substr(accel_list[m], 1, 18)), df2)
      print(c("df2", dim(df2)))
      df2 <- data.frame()
    }
  }
}

write_csv(df3, "~/AbbVie/918_Legit.csv")

