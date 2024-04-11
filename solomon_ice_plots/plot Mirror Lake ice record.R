#Plot Mirror Lake ice record
#CTS 21 April 2023

library(dplyr)
library(lubridate)
library(ggplot2)

#---- Load and manipulate data ----

#Read data, simplify to one ice-on and one ice-off per year
d <- read.csv('iceDataMirrorLake.csv') %>% 
  as_tibble() %>% #just easier to absorb when printing
  group_by(waterYear) %>% 
  mutate(iceInDate = as.Date(iceInDate),
         iceOutDate = as.Date(iceOutDate)) %>% 
  summarize(iceInDate = min(iceInDate),
            iceOutDate = max(iceOutDate, na.rm = FALSE),
            .groups = 'drop')
  
#Examine structure of data
head(d)
str(d)

#Change class of a couple columns
# d$iceInDate <- as.Date(d$iceInDate)
# d$iceOutDate <- as.Date(d$iceOutDate)

# #Calculate iceInDate from iceInDOY
# #The spreadsheet that I got from Tammy did not include the iceInDate. Calculating it and 
# #writing back out to the csv for clarity.
# #Supplying a number (like doy) to as.Date tells it to calculate date as that number of
# #days since origin. So need to use 31 Dec of previous year as origin. 
# d$iceInDate <- as.Date(d$iceInDOY,origin=make_date(year=(d$waterYear-1),month=12,day=31))
# head(d,15)
# tail(d,15)

# #Note that in some cases the calculated iceInDate does not match what is indicated in the
# #comment flag that Brenda left on the .xlsx.
# #Make a temporary version of d to try to sort this out
# dTemp <- select(d,waterYear,iceInDOY,iceInDate_Calculated=iceInDate,iceOutDOY,iceOutDate)
# #Manually enter Brenda's notes on iceInDate from the .xlsx
# brendaNotes <- data.frame(waterYear=c(1976,1989,2006,2015,2019,2020,2021),
#                           iceInDate_BrendaNote=as.Date(c('1976-11-22','1989-11-22','2006-12-30','2015-12-30','2019-12-01','2020-12-16','2021-12-09')))
# #Merge
# dTemp <- left_join(dTemp,brendaNotes,by='waterYear')
# #Reorganize columns
# dTemp <- select(dTemp,waterYear,iceInDOY,iceInDate_Calculated,iceInDate_BrendaNote,iceOutDOY,iceOutDate)
# #Examine result
# dTemp
# #Looking like the only mismatch is in waterYear 1976. Assume that the DOY is correct and
# #Brenda's note is a mistake.

#Write out a new version of the .csv for recording data moving forward
#write.csv(cbind(select(d,waterYear,iceInDate,iceOutDate),comments=NA),file='new file to record data/iceDataMirrorLake.csv',row.names=FALSE)

#Add columns for DOWY (day of water year)

hbef_dowy_mapper <- function(date){
  
  if(is.na(date)) return(NA_Date_)
  
  doy <- yday(date)
  
  if(doy >= 152){ #after june 1 (non leap year)
    dowy <- doy - 151
  } else {
    dowy <- doy + 214
  }
  
  return(dowy)
}
  
# d$iceInDOY <- yday(d$iceInDate)
# d$iceOutDOY <- yday(d$iceOutDate)
d$iceInDOWY <- sapply(d$iceInDate, hbef_dowy_mapper)
d$iceOutDOWY <- sapply(d$iceOutDate, hbef_dowy_mapper)

#Add columns for month-day
# d$iceOutMD <- format(as.Date(d$iceOutDOY, origin = '1970-01-01'), '%m-%d')
# d$iceInMD <- format(as.Date(d$iceInDOY, origin = '1970-01-01'), '%m-%d')

#---- Plot data ----

yrange <- range(select(d, ends_with('DOWY')), na.rm = TRUE)
month_starts <- seq(as.Date('1999-01-01'), as.Date('1999-12-31'), by = 'month')
yaxis <- sapply(month_starts, hbef_dowy_mapper)

jpeg('Mirror Lake ice record.jpg',width=6,height=6,units='in',res=200,quality=90)

par(mar = c(4, 4, 1, 5))

plot(d$waterYear, d$iceOutDOWY,
     col = 'red', pch = 19,
     ylim = rev(yrange),
     xlab = 'Water Year', ylab = 'Month', main = '',
     yaxt = 'n')
points(d$waterYear, d$iceInDOWY, col = 'blue', pch = 19)
axis(2, at = yaxis, labels = month.abb, las = 1)
legend(x = max(d$waterYear) + 2, y = mean(yrange),
       legend = c('ice in', 'ice out'),
       col = c('blue', 'red'),
       pch = 19, xpd = NA, bty = 'n')

dev.off()

#---- Plot with connecting lines for periods ----

jpeg('mirror_lake_ice_record_periods.jpg',width=6,height=6,units='in',res=200,quality=90)

boxplot_stats <- fivenum(c(d$iceOutDOWY, d$iceInDOWY), na.rm = TRUE)
boxplot_xloc <- max(d$waterYear) + 1

par(mar = c(4, 4, 1, 5))

plot(d$waterYear, d$iceOutDOWY,
     type = 'n',
     ylim = rev(yrange),
     xlab = 'Water Year', ylab = 'Month', main = '',
     yaxt = 'n')
segments(d$waterYear, d$iceOutDOWY, d$waterYear, d$iceInDOWY, col = 'gray50')
points(d$waterYear, d$iceOutDOWY, col = 'red', pch = 19)
points(d$waterYear, d$iceInDOWY, col = 'blue', pch = 19)
# points(boxplot_xloc,
#        boxplot_stats[3],
#        col = 'purple', pch = 19)
# segments(boxplot_xloc, boxplot_stats[1], boxplot_xloc, boxplot_stats[2],
#          col = 'purple')
# segments(boxplot_xloc, boxplot_stats[4], boxplot_xloc, boxplot_stats[5],
#          col = 'purple')
axis(2, at = yaxis, labels = month.abb, las = 1)
legend(x = max(d$waterYear) + 2, y = mean(yrange),
       legend = c('ice in', 'ice out'),
       col = c('blue', 'red'),
       pch = 19, xpd = NA, bty = 'n')

dev.off()

# ggplot(d) +
#   geom_point(mapping=aes(x=waterYear,y=iceOutDOY,col='iceOut')) +
#   geom_point(mapping=aes(x=waterYear,y=iceInDOY,col='iceIn')) +
#   scale_y_reverse() +
#   scale_color_manual(values=c('iceOut'='red','iceIn'='blue')) +
#   labs(y='Day of year')

