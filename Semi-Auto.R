library(readr)
library(dplyr)
library(lubridate)

#Stores cumulative metrics as a list of data frames
fn_cum <- list.files("~/Downloads/m14-397_study_archive (2)", pattern="*cumulative_metrics.csv", full.names=TRUE, include.dirs = TRUE, recursive = TRUE)
ldf_cum <- lapply(fn_cum, read_csv)
#res_daily <- lapply(ldf, summary)
names(res_daily) <-  paste0("d.cum", substr(fn_cum, 74, 79))

#Stores daily metrics as a list of data frames
fn_daily <- list.files("~/Downloads/m14-397_study_archive (2)", pattern="[0-9].*[0-9].csv", full.names=TRUE, include.dirs = TRUE, recursive = TRUE)
ldf_daily <- lapply(fn_daily, read_csv)
#res_daily <- lapply(ldf_daily, summary)
names(ldf_daily) <-  paste0(substr(fn_daily,74, 79), substr(fn_daily,102, 112) )

#Stores accel data as a list of data frames
fn_accel <- list.files("~/Downloads/BioStamp/", pattern="*accel.csv", full.names=TRUE, include.dirs = TRUE, recursive = TRUE)
ldf_accel <- lapply(fn_accel, read_csv)
gsub("left", "right", fn_accel) -> fn_accel
#res_accel <- lapply(ldf_accel, summary)
names(ldf_accel) <-  paste0(substr(fn_accel,51,56), substr(fn_accel,103, 113))

#Stores gyro data as a list of data frames
fn_gyro <- list.files("~/Downloads/BioStamp/", pattern="*gyro.csv", full.names=TRUE, include.dirs = TRUE, recursive = TRUE)
ldf_gyro <- lapply(fn_gyro, read_csv)
gsub("left", "right", fn_gyro) -> fn_gyro
#res_gyro <- lapply(ldf_gyro, summary)
names(ldf_gyro) <-  paste0(substr(fn_gyro,51,56), substr(fn_gyro,103, 113))


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
df1= list()
df=list()
for (i in 1:length(ldf_accel)) {
  for (j in 1:length(ldf_gyro)) {
    if (names(ldf_accel)[i] == names(ldf_gyro)[j]) {
      merge(ldf_accel[i],ldf_gyro[j]) -> df[[j]]
      df[[j]] %>% dplyr::mutate(id = substr(names(ldf_accel)[i], 1, 6)) -> df[[j]]
      df[[j]][1] = df[[j]][1]/1000
      #assign(paste(substr(names(ldf_accel)[i], 1, 18)), names(df[j]))
      names(df)[j] <-  paste(substr(names(ldf_gyro)[j], 1, 18))
    }
  }
}  

df2=data.frame()

#######BUGGED CODE########
#Filters merged data from list "df" to get data when the subject is walking
#This is acheived by extracting the data between "p" row containing "MOVING:WALKING" and "p+1" row. 
#This works perfectly for one dataframe `110501_2019-06-21` as the outtput matches what we expect to see.
#However the code doesn't seem to iterate over the rest of the data to find matches as one only dataframe is outputted without errors.

for(m in 1:length(df)){
  for (k in 1:length(ldf_daily)) {
    if (names(ldf_daily)[k] == names(df)[m]) {
      for (p in 1:(nrow(ldf_daily[[k]])-1)) {
        if (ldf_daily[[k]][p,2]$ACTIVITY=="MOVING:WALKING") {
          df[[m]] %>% filter(df[[m]][1] >= ldf_daily[[k]]$`UTC Timestamp (utc_milliseconds)`[p] & df[[m]][1] <= ldf_daily[[k]]$`UTC Timestamp (utc_milliseconds)`[p+1]) -> df1[[p]]
          
          
          df2 <- do.call(rbind.data.frame, df1)
          #append(df1, df1[[p]])
          assign(paste(substr(names(df)[m], 1, 18)), df2)
          
        }
      }
    }else{
      break()
    }
  }
}

for (p in 1:(nrow(dait)-1)) {
  if (dait[p,2]$ACTIVITY=="MOVING:WALKING") {

    df %>% filter(df[1] >= dait$`UTC Timestamp (utc_milliseconds)`[p] & df[1] <= dait$`UTC Timestamp (utc_milliseconds)`[p+1]) -> df1[[p]]
    df2 <- plyr::ldply (df1, data.frame)
  }
}

for (i in seq_along(letters)) {
  print(letters[i])
  
}
# d.ts501 %>% dplyr::mutate(A_xf = signal:::filter(myfilter, d.ts501$`Accel X (g)`)) %>%
#   dplyr::mutate(A_yf = signal:::filter(myfilter, d.ts501$`Accel Y (g)`)) %>%
#   dplyr::mutate(A_zf = signal:::filter(myfilter, d.ts501$`Accel Z (g)`)) %>%
#   dplyr::mutate(G_xf = signal:::filter(myfilter, d.ts501$`Gyro X (°/s)`))%>%
#   dplyr::mutate(G_yf = signal:::filter(myfilter, d.ts501$`Gyro Y (°/s)`))%>%
#   dplyr::mutate(G_zf = signal:::filter(myfilter, d.ts501$`Gyro Z (°/s)`))%>%
#   dplyr::select(-c(`Accel X (g)`, `Accel Y (g)`, `Accel Z (g)`, `Gyro X (°/s)`, `Gyro Y (°/s)`, `Gyro Z (°/s)`)) -> d.ts501f
# 
# colnames(d.ts501f)[which(names(d.ts501f) == "Timestamp (microseconds)")] <- "time"