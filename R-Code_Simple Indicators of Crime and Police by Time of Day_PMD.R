## This code computes temporal summary statistics for crime and police event data
## The code can be adapted to any type of event data
## Data required: Event data with time stamp (date and time) and event type
## Based on the framework developed by Felson & Poulsen (2003) [https://www.sciencedirect.com/science/article/pii/S0169207003000931]
## Code developed by Philipp M. Dau (Ghent University, philippmartin.dau@ugent.be)
## Date: 02.07.2021

###################################################################################################################################

#clean global environment
rm(list = ls())

#load required libraries
library(tidyverse)
library(lubridate)
library(data.table)

#suppress scientific notation 
options("scipen" = 13)

#set working directory
setwd("C:/R Analyses/Temporal Analysis of Crime")

#import raw event data, select appropriate encoding for the available data
Crime.base = read.csv("C:/R Data/Crime Events/Crime_Antwerp_2019&2020.csv", stringsAsFactors = F, encoding = "UTF-8")

#filter raw data and create proper time fields
Crime.df = Crime.base %>% 
  #drop all non criminal related events (or crime types that are not of interest)
  filter(Crime_Type != "No Crime"
         ) %>%
  
  #transform timestamps to POSIXct format
  #in the creators data set both start (s_full) and end (e_full) time of the events were provided
  mutate(
    s_full = as_datetime(s_full),
    e_full = as_datetime(e_full),
  ) %>%
  
  #select relevant columns of dataset
  select(1:4,20,21,25,32) %>%
  mutate(
    
    #create factor for hour variable and reorder (5 a.m. to 4.59 a.m.)
    hour =  factor(lubridate::hour(s_full), levels = c("5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","0","1","2","3","4")),
    
    #create day and time variables
    day  =  lubridate::wday(s_full, label = T),
    time =  gsub("(:[^:]+):.*", "\\1", hms::as_hms(s_full)),
    
    #create variable for time in decimal format
    tdec =  lubridate::hour(s_full) + lubridate::minute(s_full) / 60,
    
    #create variable for new day and assign night hours to appropriate days.
    day_5 = as.character(day),
    day_5 = ifelse(day == "Sun" & tdec < 5, "Sat", day_5),
    day_5 = ifelse(day == "Mon" & tdec < 5, "Sun", day_5),
    day_5 = ifelse(day == "Tue" & tdec < 5, "Mon", day_5),
    day_5 = ifelse(day == "Wed" & tdec < 5, "Tue", day_5),
    day_5 = ifelse(day == "Thu" & tdec < 5, "Wed", day_5),
    day_5 = ifelse(day == "Fri" & tdec < 5, "Thu", day_5),
    day_5 = ifelse(day == "Sat" & tdec < 5, "Fri", day_5),
    day_5 = factor(day_5, levels = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))
  )

#select period of interest, here year 2019 is selected
Crime.df = Crime.df %>% filter(s_full < ymd_hms("2020-01-01 00:00:00"))

#order all events based on hour and timestamp variable
Crime.df = Crime.df[order(Crime.df$hour, Crime.df$tdec),]

#adapted function to calculate median minute
median.t <- function(x) if(length(x) %% 2 != 0){
  y = x[(length(x)+1)/2]
  return(y)
}else{
  y = (x[(length(x)/2)] + x[(length(x)/2)+1])/2
  return(y)
}

#save median minute
medmin = median.t(Crime.df$tdec)

#adapted function to calculate first quartile minute
fq.t <- function(x) if(length(x) %% 2 == 0){
  z = x[1:(length(x)/2)]
  if(length(z) %% 2 != 0){
    y = z[(length(z)+1)/2]
    return(y)
  }else{
    y = (z[(length(z)/2)] + z[(length(z)/2)+1])/2
    return(y)
  }
}else{
  z = x[1:(floor(length(x)/2))]
  if(length(z) %% 2 != 0){
    y = z[(length(z)+1)/2]
    return(y)
  }else{
    y = (z[(length(z)/2)] + x[(length(z)/2)+1])/2
    return(y)
  }  
}

#save first quartile minute
fq = fq.t(Crime.df$tdec)  

#Caution: for event data with small sample sizes, issues might be encountered.
#This stems from few events being clustered around midnight and leads to errors in the time format:
#(e.g., third quartile minute between 23:45 and 00:45, w/o conversion: 12.25, w/ conversion: 24.25).

#conversion of median minute, if needed
if(medmin < fq & medmin > 4.99){
  medmin = medmin + 12 
}else{
  medmin = medmin
}

#conversion of first quartile minute, if needed
if(medmin < fq & fq < 12){
  fq = fq  + 12 
}else{
  fq = fq
}

#format median minute into correct time value
medmin.format = paste(floor(medmin), 
                      if(signif((medmin-floor(medmin))*60, digits = 2) != 0)
                      {if(nchar(round(signif((medmin-floor(medmin))*60, digits = 2)))==1){
                        paste("0",round(signif((medmin-floor(medmin))*60, digits = 2)), sep = "")}
                        else{
                          round(signif((medmin-floor(medmin))*60, digits = 2))
                        }
                      }else{
                        "00"
                      }, sep = ":")

#format first quartile minute into correct time value
fq.format = paste(floor(fq), 
                  if(signif((fq-floor(fq))*60, digits = 2) != 0)
                  {if(nchar(round(signif((fq-floor(fq))*60, digits = 2)))==1){
                    paste("0",round(signif((fq-floor(fq))*60, digits = 2)), sep = "")}
                    else{
                      round(signif((fq-floor(fq))*60, digits = 2))
                    }
                  }else{
                    "00"
                  }, sep = ":")


#adapted function to calculate third quartile minute
tq.t <- function(x) if(length(x) %% 2 == 0){
  x = x[((length(x)/2)+1):length(x)]
  if(length(x) %% 2 != 0){
    y = x[(length(x)+1)/2]
    return(y)
  }else{
    y = (x[(length(x)/2)] + x[(length(x)/2)+1])/2
    return(y)
  }
}else{
  x = x[(((length(x)+1)/2)+1):length(x)]
  if(length(x) %% 2 != 0){
    y = x[(length(x)+1)/2]
    return(y)
  }else{
    y = (x[(length(x)/2)] + x[(length(x)/2)+1])/2
    return(y)
  }  
}

#save third quaritle minute to variable
tq = tq.t(Crime.df$tdec)

#conversion of third quartile minute, if needed
if(tq < medmin & tq > 4.99){
  tq = tq + 12 
}else{
  tq = tq
}

#format first third minute into correct time value
tq.format = paste(floor(tq), 
                  if(signif((tq-floor(tq))*60, digits = 2) != 0)
                  {if(nchar(round(signif((tq-floor(tq))*60, digits = 2)))==1){
                    paste("0",round(signif((tq-floor(tq))*60, digits = 2)), sep = "")}
                    else{
                      round(signif((tq-floor(tq))*60, digits = 2))
                    }
                  }else{
                    "00"
                  }, sep = ":")

#calculate and save daily timespan
dt = if(tq>fq){
  (tq-fq) * 60
}else{
  ((24-fq) + tq)*60
}

#calculate and save five-to-five share
fto5 = sum(Crime.df$tdec >= 5 & Crime.df$tdec < 17) / length(Crime.df$tdec)

#create dataframe for summary of simple indicators (calculated above)
indicator.overall = as.data.frame(matrix(ncol = 7, nrow = length(unique(Crime.df$Crime))+1))

#name columns in output dataframe
names(indicator.overall) = c("Type", "FQ", "Median","TQ","DT","5t5","n")

#store all calcualted values in the output dataframe
indicator.overall[1,1] =  "All"
indicator.overall[1,2] =  fq.format
indicator.overall[1,3] =  medmin.format
indicator.overall[1,4] =  tq.format
indicator.overall[1,5] =  dt
indicator.overall[1,6] =  fto5
indicator.overall[1,7] =  length(Crime.df$tdec)

#export results into .csv files
write.csv(indicator.overall, file = "Temporal Summary_All Crime.csv", row.names = F)
 
#### Worked example on how to loop through different crime types, reporting types, and days of the week ####
#Note, reporting types hold information whether an event was reported, for example, by police officers or citizens


#create dataframe for summary of simple indicators for categorized events
#The data analzed in the paper Dau et al. 2021 - Simple indicators of crime and police by time of day: Using temporal summary statistics to investigate big data sets of police presence and reported crime
#included 20 crime types, two reporting gategories and was then calucalted for each day of the week, thus the dataframe has 280 entries. The exapmle code gives an idea on how to create the output based on
# avaiable data (also other than crime or police data)

indicator.wday.oia = as.data.frame(matrix(ncol = 9, nrow = length(unique(Crime.df$Crime_Type))*length(unique(Crime.df$OIA))*7))

#name columns in output dataframe
names(indicator.overall) = c("Reporting Type", "Day", "Crime Type" ,"FQ", "Median","TQ","DT","5t5","n")

#loop through different reporting types (e.g.m officer vs citizen-reported)
for (o in unique(Crime.df$OIA)){
  #loop through each crime type
  for(k in unique(Crime.df$Crime_Type)){
    #loop through each day of the week
    for(d in unique(Crime.df$day_5)){
      
      #filter the data for current selection of reporting type (o), crime type (k), and day of the week (d)
      temp.df = Crime.df %>% filter(OIA == o)
      temp.df = temp.df %>% filter(Crime_Type == k)
      temp.df = temp.df %>% filter(day_5 == d)
      #reorder the data by hour and decimal time
      temp.df = temp.df[order(temp.df$hour, temp.df$tdec),]
      
      #if sample size of selected subset is below 2, store predefined output as summaries are not computable
      if(nrow(temp.df) < 2){
        indicator.wday.oia[w,1] =  o
        indicator.wday.oia[w,2] =  d
        indicator.wday.oia[w,3] =  k
        indicator.wday.oia[w,4] =  "Sample Size too small"
        indicator.wday.oia[w,5] =  "Sample Size too small"
        indicator.wday.oia[w,6] =  "Sample Size too small"
        indicator.wday.oia[w,7] =  "Sample Size too small"
        indicator.wday.oia[w,8] =  "Sample Size too small"
        indicator.wday.oia[w,9] =  length(temp.df$tdec)
        w=w+1
        
      }else{
        
        #calculate summary statistics (same as above)
        medmin.temp = median.t(temp.df$tdec)
        fq.temp = fq.t(temp.df$tdec) 
        
        if(medmin.temp < fq.temp & medmin.temp > 4.99){
          medmin.temp = medmin.temp + 12 
        }else{
          medmin.temp = medmin.temp
        }
        
        
        
        medmin.format.temp = paste(floor(medmin.temp), 
                                   if(signif((medmin.temp-floor(medmin.temp))*60, digits = 2) != 0)
                                   {if(nchar(round(signif((medmin.temp-floor(medmin.temp))*60, digits = 2)))==1){
                                     paste("0",round(signif((medmin.temp-floor(medmin.temp))*60, digits = 2)), sep = "")}
                                     else{
                                       round(signif((medmin.temp-floor(medmin.temp))*60, digits = 2))
                                     }
                                   }else{
                                     "00"
                                   }, sep = ":")
        
        fq.temp = fq.t(temp.df$tdec)  
        
        if(medmin.temp < fq.temp & fq.temp < 12){
          fq.temp = fq.temp + 12 
        }else{
          fq.temp = fq.temp
        }
        
        fq.format.temp = paste(floor(fq.temp), 
                               if(signif((fq.temp-floor(fq.temp))*60, digits = 2) != 0)
                               {if(nchar(round(signif((fq.temp-floor(fq.temp))*60, digits = 2)))==1){
                                 paste("0",round(signif((fq.temp-floor(fq.temp))*60, digits = 2)), sep = "")}
                                 else{
                                   round(signif((fq.temp-floor(fq.temp))*60, digits = 2))
                                 }
                               }else{
                                 "00"
                               }, sep = ":")
        
        tq.temp = tq.t(temp.df$tdec)
        if(tq.temp < medmin.temp & tq.temp > 4.99){
          tq.temp = tq.temp + 12 
        }else{
          tq.temp = tq.temp
        }
        
        tq.format.temp = paste(floor(tq.temp), 
                               if(signif((tq.temp-floor(tq.temp))*60, digits = 2) != 0)
                               {if(nchar(round(signif((tq.temp-floor(tq.temp))*60, digits = 2)))==1){
                                 paste("0",round(signif((tq.temp-floor(tq.temp))*60, digits = 2)), sep = "")}
                                 else{
                                   round(signif((tq.temp-floor(tq.temp))*60, digits = 2))
                                 }
                               }else{
                                 "00"
                               }, sep = ":")
        
        dt.temp = if(tq.temp>fq.temp){
          (tq.temp-fq.temp) * 60
        }else{
          ((24-fq.temp) + tq.temp)*60
        }
        
        fto5.temp = sum(temp.df$tdec >= 5 & temp.df$tdec < 17) / length(temp.df$tdec)
        
        #store values in the output data frame
        indicator.wday.oia[w,1] =  o
        indicator.wday.oia[w,2] =  d
        indicator.wday.oia[w,3] =  k
        indicator.wday.oia[w,4] =  fq.format.temp
        indicator.wday.oia[w,5] =  medmin.format.temp
        indicator.wday.oia[w,6] =  tq.format.temp
        indicator.wday.oia[w,7] =  dt.temp
        indicator.wday.oia[w,8] =  fto5.temp
        indicator.wday.oia[w,9] =  length(temp.df$tdec)
        w=w+1
      }
    }
  }
}

#order the dataframe by crime type and day of the week
indicator.wday.oia = indicator.wday.oia[order(indicator.wday.oia$Type, indicator.wday.oia$Day),]

#export results into .csv files
write.csv(indicator.wday.oia, file = "Temporal Summary_Crime_Report_Day.csv", row.names = F)

