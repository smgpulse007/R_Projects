library(readr)
library(dplyr)

fn_cum <- list.files("~/Downloads/m14-397_study_archive (2)", pattern="*cumulative_metrics.csv", full.names=TRUE, include.dirs = TRUE, recursive = TRUE)
ldf_cum <- lapply(fn_cum, read_csv)
#res_daily <- lapply(ldf, summary)
names(res_daily) <-  paste0("d.cum", substr(fn_cum, 74, 79))


fn_daily <- list.files("~/Downloads/m14-397_study_archive (2)", pattern="[0-9].*[0-9].csv", full.names=TRUE, include.dirs = TRUE, recursive = TRUE)
ldf_daily <- lapply(fn_daily, read_csv)
#res_daily <- lapply(ldf_daily, summary)
names(ldf_daily) <-  paste0(substr(fn_daily,74, 79), substr(fn_daily,102, 112) )

fn_accel <- list.files("~/Downloads/BioStamp/", pattern="*accel.csv", full.names=TRUE, include.dirs = TRUE, recursive = TRUE)
ldf_accel <- lapply(fn_accel, read_csv)
#res_accel <- lapply(ldf_accel, summary)
names(ldf_accel) <-  paste0(substr(fn_accel,51,56), substr(fn_accel,103, 113))

fn_gyro <- list.files("~/Downloads/BioStamp/", pattern="*gyro.csv", full.names=TRUE, include.dirs = TRUE, recursive = TRUE)
ldf_gyro <- lapply(fn_gyro, read_csv)
#res_gyro <- lapply(ldf_gyro, summary)
names(ldf_gyro) <-  paste0(substr(fn_gyro,51,56), substr(fn_gyro,103, 113))


for (i in 1:length(ldf_daily)) {
  ind2 <- which(ldf_daily[[i]]$ACTIVITY=="MOVING:WALKING")
  ind3 <- which(ldf_daily[[i]]$ACTIVITY=="MOVING:WALKING") + 1
  ldf_daily[[i]][c(ind2, ind3),] -> ldf_daily[[i]]
  
  
}
Filter(function(x) dim(x)[[1]] > 0, ldf_daily) -> ldf_daily


for (z in 1:length(ldf_daily)) {
  order(ldf_daily[[z]]$`UTC Timestamp (utc_milliseconds)`) 
}


for (i in 1:length(ldf_accel)) {
  for (j in 1:length(ldf_gyro)) {
    if (names(ldf_accel)[i] == names(ldf_gyro)[j]) {
      merge(ldf_accel[i],ldf_gyro[j]) -> df
      df %>% dplyr::mutate(id = substr(names(ldf_accel)[i], 1, 6)) -> df
      df[1] = df[1]/1000
      assign(paste(substr(names(ldf_accel)[i], 1, 18)), df)
      for (k in 1:length(ldf_daily)) {
        if (names(ldf_daily)[k] == names(ldf_accel)[i]) {
          for (p in 1:(nrow(ldf_daily[[k]])-1)) {
            df1 = list()
            filter(df, df[1] >= ldf_daily[[k]]$`UTC Timestamp (utc_milliseconds)`[p] & df[1] <= ldf_daily[[k]]$`UTC Timestamp (utc_milliseconds)`[p+1]) -> df1[[p]]
            df2 <- do.call(rbind, df1)
            assign(paste(substr(names(ldf_accel)[i], 1, 18)), df2)
         
          
    
        
    }}}}}}

#if timestamp between 1: range(time[1], time[last])

#if timestamp is not in the full date, remove it

#min max 
while()
  flag=1 if walking
flip switch 0-1, strore the timestamp
flip it back; store the timestamp
extract

for (k in 1:length(ldf_daily)) {
  if (names(ldf_daily)[k] == names(ldf_accel)[i]) {
    df %>% dplyr::filter(df[i] >= min(ldf_daily[[k]]$`UTC Timestamp (utc_milliseconds)`) & df[i][1] <=  max(ldf_daily[[k]]$`UTC Timestamp (utc_milliseconds)`)) -> df[i]
  }
}

Create or replace temporary view as somedf
case when activity="Walking" then "include"
case when activity= notwalking then 'astop'
difference between stop and start 












ldf_daily[[i]] %>% dplyr::filter(ACTIVITY == "MOVING:WALKING") %>% dplyr::select(c(`UTC Timestamp (utc_milliseconds)`,ACTIVITY))-> ldf_daily[[i]]

#name <- function(,f2) {
#  d.merge.501 <- merge(accel_daily, d.501.621.gy)
#  d.merge.501 %>% dplyr::mutate(id = names(f1)) -> d.merge.501
#  d.merge.501 %>% dplyr::mutate(`Timestamp (microseconds)` = `Timestamp (microseconds)`/1000) -> d.merge.501
# d.501.621 %>% dplyr::filter(ACTIVITY == "MOVING:WALKING") -> d.501.621
# d.merge.501 %>% dplyr::filter(`Timestamp (microseconds)` >= d.501.621$`UTC Timestamp (utc_milliseconds)`[1] & `Timestamp (microseconds)` <=  d.501.621$`UTC Timestamp (utc_milliseconds)`[8]) -> d.walk501
# d.ts501  <- d.walk501 %>%  dplyr::select(id, everything())
# 
# 
# d.ts501 %>% dplyr::mutate(A_xf = signal:::filter(myfilter, d.ts501$`Accel X (g)`)) %>% 
#   dplyr::mutate(A_yf = signal:::filter(myfilter, d.ts501$`Accel Y (g)`)) %>% 
#   dplyr::mutate(A_zf = signal:::filter(myfilter, d.ts501$`Accel Z (g)`)) %>%
#   dplyr::mutate(G_xf = signal:::filter(myfilter, d.ts501$`Gyro X (°/s)`))%>%
#   dplyr::mutate(G_yf = signal:::filter(myfilter, d.ts501$`Gyro Y (°/s)`))%>%
#   dplyr::mutate(G_zf = signal:::filter(myfilter, d.ts501$`Gyro Z (°/s)`))%>%
#   dplyr::select(-c(`Accel X (g)`, `Accel Y (g)`, `Accel Z (g)`, `Gyro X (°/s)`, `Gyro Y (°/s)`, `Gyro Z (°/s)`)) -> d.ts501f
# 
# colnames(d.ts501f)[which(names(d.ts501f) == "Timestamp (microseconds)")] <- "time"
  
  
#}




/*
  Create or replace temporary view vm_na_ts as
Select user_id, ts ,
case
when rpt_desc = "jnfkjasd" then "fejnfnep" --start
when rpt_desc= "bh" then "dsjndj"-- end
end as time_frame
from vm_na
where rpt_desc in (
  "jnfkjasd", --- walking
  "bh")-- stop 
*/
  
library(dplyr)
df <- list()

for (p in 1:(nrow(dait)-1)) {
    
   filter(`1105012019-06-21t`, `1105012019-06-21t`[1] >= dait$`UTC Timestamp (utc_milliseconds)`[p] & `1105012019-06-21t`[1] <= dait$`UTC Timestamp (utc_milliseconds)`[p+1]) -> df[[p]]
   df1 <- do.call(rbind, df)
}
p=1
rbind(df, do.call(filter, `1105012019-06-21t`, `1105012019-06-21t`[1] >= dait$`UTC Timestamp (utc_milliseconds)`[p] & `1105012019-06-21t`[1] <= dait$`UTC Timestamp (utc_milliseconds)`[p+1]))

#do.call(filter, list(`1105012019-06-21t`,.dots=list(`1105012019-06-21t`[1] >= dait$`UTC Timestamp (utc_milliseconds)`[p], `1105012019-06-21t`[1] <= dait$`UTC Timestamp (utc_milliseconds)`[p+1]))) -> df
p=4
filter_criterion2 <- `1105012019-06-21t`[1] >= dait$`UTC Timestamp (utc_milliseconds)`[p]
filter_criterion1 <- `1105012019-06-21t`[1] <= dait$`UTC Timestamp (utc_milliseconds)`[p+1]
#do.call(filter_, list(`1105012019-06-21t`,paste(filter_criterion2, filter_criterion1, sep="&"))) -> df

rbind(df, do.call(filter_, list(`1105012019-06-21t`,.dots=list(filter_criterion2, filter_criterion1))))
