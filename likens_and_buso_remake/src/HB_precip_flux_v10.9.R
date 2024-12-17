# This program calculates HB precip fluxes (g/Ha) using a comma delimited
# chemistry concentration file and a daily watershed flow (mm) file.
# written by John Campbell and Bob Smith

# edited by Nina Lany 2023-08-17

#Install packages
#install.packages("zoo")
#install.packages("plyr")

#Use the libraries
library("zoo")
library("plyr")

# remove all objects from the current workspace
rm(list=ls())

# set working directory to current directory
current_path <-
    '~/git/hbef/hbef_misc/likens_and_buso_remake/data_in/official_method/'
  # "C:/Users/nlany/Box/External-HBEF-DATA/Fluxes/precip_chem/HB_Precip_flux_R_program"
  #"D:/hubbard_brook_data/HB fluxes/HB_Precip_flux_R_program"

setwd(current_path)

######Change these prior to running the program#################################

#Manually set end date
end_date <- "2022-06-01"

#Manually select watershed
watershed <- "6"

#Loop through all watersheds (bracket at the end of the program also)
#for (ws in c(1,2,3,4,5,6,7,8,9)){
#  watershed <- paste(ws)

#Automatically set begin date to match stream chemistry data
if(watershed==7){
  begin_date <- as.Date("1964-12-28")
} else {
  if(watershed==8){
    begin_date <- as.Date("1968-12-30")
} else {
  if(watershed==9){
    begin_date <- as.Date("1994-12-26")
} else {
  if(watershed==5 || watershed==6){
    begin_date <- as.Date("1963-12-16")
} else {begin_date <- as.Date("1963-7-10")
}}}}

# Package ID: knb-lter-hbr.14.16 Cataloging System:https://pasta.edirepository.org.
# Data set title: Hubbard Brook Experimental Forest: Total Daily Precipitation by Watershed, 1956 - present.
# Data set creator:    - USDA Forest Service, Northern Research Station
# Contact:    -  Hubbard Brook Ecosystem Study  - hbr-im@lternet.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu

# inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/14/16/c606bfe2f2deb3fa3eabf692ae15f02d"
# infile1 <- tempfile()
# try(download.file(inUrl1,infile1,method="curl"))
# if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

infile1 <- 'ws6_precip.csv'
dt1 <-read.csv(infile1,header=F
               ,skip=1
               ,sep=","
               , col.names=c(
                 "DATE",
                 "watershed",
                 "Precip"    ), check.names=TRUE)
#
# unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

# attempting to convert dt1$DATE dateTime string to R date structure (date or POSIXct)
tmpDateFormat<-"%Y-%m-%d"
tmp1DATE<-as.Date(dt1$DATE,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1DATE) == length(tmp1DATE[!is.na(tmp1DATE)])){dt1$DATE <- tmp1DATE } else {print("Date conversion failed for dt1$DATE. Please inspect the data and do the date conversion yourself.")}
rm(tmpDateFormat,tmp1DATE)
if (class(dt1$watershed)!="factor") dt1$watershed<- as.factor(dt1$watershed)
if (class(dt1$Precip)=="factor") dt1$Precip <-as.numeric(levels(dt1$Precip))[as.integer(dt1$Precip) ]
if (class(dt1$Precip)=="character") dt1$Precip <-as.numeric(dt1$Precip)

# Convert Missing Values to NA for non-dates

# Here is the structure of the input data frame:
str(dt1)
attach(dt1)
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.

summary(DATE)
summary(watershed)
summary(Precip)
# Get more details on character variables

summary(as.factor(dt1$watershed))
detach(dt1)

precip_vol_in <- dt1
colnames(precip_vol_in) <- c("DATE","WS","precip_mm")

#subset by watershed
precip_vol_in <- precip_vol_in[(precip_vol_in$WS==paste("W",watershed, sep="")),]

begin_date

precip_vol_in <- precip_vol_in[(precip_vol_in$DATE >= paste(begin_date) &
                                  precip_vol_in$DATE <= paste(end_date)),]

precip_vol_in <- precip_vol_in[, !(colnames(precip_vol_in) %in% c("WS"))]

write.csv(precip_vol_in, file=paste("ws",watershed,"_precip.csv", sep=""), row.names=FALSE)

ifelse(watershed < "7", chem_file <- "ws1_6_precip_chem.csv", chem_file <- "ws7_9_precip_chem.csv")

precip_chem <- read.table(file=paste(chem_file, sep = ""),header=TRUE,sep=",",
colClasses=c("numeric","character","character","character","character",
             "numeric","numeric","numeric","numeric","numeric","numeric",
             "numeric","numeric","numeric","numeric","numeric","numeric",
             "numeric","numeric","numeric","numeric","numeric","numeric",
             "numeric","numeric","numeric","numeric","numeric","numeric",
             "numeric","numeric","numeric","character","character","numeric",
             "numeric","numeric","character"))

tail(precip_chem,5)

precip_chem$DATE <- as.Date(precip_chem$Date,"%m/%d/%Y")

precip_chem <- subset(precip_chem, select = -c(RG,Date,Year,Month,Day,EST))

fill_NAs <- function(fill_NA) {
    # fill in missing NAs with appropriate chemistry
    lenData <- nrow(fill_NA)
    firstNA <- TRUE
    lastNA <- FALSE
    # this for loop is for concentration columns numbers
    # and has to be changed if addition or deletion of future columns
    for (l in 2:25) {
        # This for loop is for rows in the data set
        for (i in 1:(lenData-1)) {
            #looking for NA values
            if (is.na(fill_NA[i,l])) {
                if(firstNA) {
                    first <- i
                    # next statement handles cases where a run of NAs consists of one
                    if (!is.na(fill_NA[i+1,l]))
                    {
                        last <- first
                        if (fill_NA[first-1,l] < 0 && fill_NA[last+1,l] < 0)
                            fill_NA[i,l] <- fill_NA[last+1,l]
                        else if (fill_NA[first-1,l] < 0 && fill_NA[last+1,l] >= 0)
                            fill_NA[i,l] <- fill_NA[last+1,l]
                        else if (fill_NA[first-1,l] >= 0 && fill_NA[last+1,l] < 0)
                            fill_NA[i,l] <- fill_NA[last+1,l]
                        else
                            fill_NA[i,l] <- fill_NA[last+1,l]
                    }
                    else {
                        firstNA <- FALSE
                        lastNA <- TRUE
                    }
                }
                else if (lastNA) {
                    # looking for last NA in a run of NAs by finding next good chem data
                    if (is.na(fill_NA[i,l]) && !is.na(fill_NA[i+1,l])) {
                        last <- i
                        for (rep in first:last) {
                            print(rep)
                            if (fill_NA[first-1,l] < 0 && fill_NA[last+1,l] < 0)
                                fill_NA[rep,l] <- fill_NA[last+1,l]
                            else if (fill_NA[first-1,l] < 0 && fill_NA[last+1,l] >= 0)
                                fill_NA[rep,l] <- fill_NA[last+1,l]
                            else if (fill_NA[first-1,l] >= 0 && fill_NA[last+1,l] < 0)
                                fill_NA[rep,l] <- fill_NA[last+1,l]
                            else
                                fill_NA[rep,l] <- fill_NA[last+1,l]
                        }
                        firstNA <-TRUE
                        lastNA <- FALSE
                    }
                }
            }
        }
    }
    return (fill_NA)
}

# makeYearMonth adds variables Year and Month to precip_monthly_flux_gHa
# These two variables are constructed from the chararcter varialble Year_Month
# which was the aggregating varible used to contruct the stream_monthly_flux_gHa
# dataset. The function returns the updated version of stream_monthly_flux_gHa
makeYearMonth <- function(monthly_data)
{
  temp_Date <- monthly_data$Year_Month
  tYear <- substr(temp_Date,1,4)
  tMonth <-  substr(temp_Date,6,7)
  Year <- as.numeric(tYear)
  Month <- as.numeric(tMonth)

  Year <- as.matrix(Year)
  Month <- as.matrix(tMonth)

  monthly_data <- cbind(monthly_data,Year,Month)

  monthly_data <-
    monthly_data[c("Year","Month","Year_Month","precip_mm","Ca_flux","Mg_flux",
                   "K_flux","Na_flux","Al_Ferron_flux","TMAl_flux","OMAl_flux",
                   "Al_ICP_flux","NH4_flux","H_flux","SO4_flux","NO3_flux",
                   "Cl_flux","PO4_flux","DOC_flux","TDN_flux","DON_flux",
                   "SiO2_flux","Mn_flux","Fe_flux","F_flux","SpecCond_flux",
                   "TheoryCond_flux")]

  return (monthly_data)
}

# missingData assigns NA to concentrations equal to -999 missing data
missingData <- function(precip_chem)
{
  len <- nrow(precip_chem)
  for (i in 1:len)
  {
    if (!is.na(precip_chem$Ca[i]) && precip_chem$Ca[i] < -900)
      precip_chem$Ca[i] <- NA
    if (!is.na(precip_chem$Mg[i]) && precip_chem$Mg[i] < -900)
      precip_chem$Mg[i] <- NA
    if (!is.na(precip_chem$K[i]) && precip_chem$K[i] < -900)
      precip_chem$K[i] <- NA
    if (!is.na(precip_chem$Na[i]) && precip_chem$Na[i] < -900)
      precip_chem$Na[i] <- NA
    if (!is.na(precip_chem$Al_Ferron[i]) && precip_chem$Al_Ferron[i] < -900)
      precip_chem$Al_Ferron[i] <- NA
    if (!is.na(precip_chem$TMAl[i]) && precip_chem$TMAl[i] < -900)
      precip_chem$TMAl[i] <- NA
    if (!is.na(precip_chem$OMAl[i]) && precip_chem$OMAl[i] < -900)
      precip_chem$OMAl[i] <- NA
    if (!is.na(precip_chem$Al_ICP[i]) && precip_chem$Al_ICP[i] < -900)
      precip_chem$Al_ICP[i] <- NA
    if (!is.na(precip_chem$NH4[i]) && precip_chem$NH4[i] < -900)
      precip_chem$NH4[i] <- NA
    if (!is.na(precip_chem$pH[i]) && precip_chem$pH[i] < -900)
      precip_chem$pH[i] <- NA
    if (!is.na(precip_chem$SO4[i]) && precip_chem$SO4[i] < -900)
      precip_chem$SO4[i] <- NA
    if (!is.na(precip_chem$NO3[i]) && precip_chem$NO3[i] < -900)
      precip_chem$NO3[i] <- NA
    if (!is.na(precip_chem$Cl[i]) && precip_chem$Cl[i] < -900)
      precip_chem$Cl[i] <- NA
    if (!is.na(precip_chem$PO4[i]) && precip_chem$PO4[i] < -900)
      precip_chem$PO4[i] <- NA
    if (!is.na(precip_chem$DOC[i]) && precip_chem$DOC[i] < -900)
      precip_chem$DOC[i] <- NA
    if (!is.na(precip_chem$TDN[i]) && precip_chem$TDN[i] < -900)
      precip_chem$TDN[i] <- NA
    if (!is.na(precip_chem$DON[i]) && precip_chem$DON[i] < -900)
      precip_chem$DON[i] <- NA
    if (!is.na(precip_chem$SiO2[i]) && precip_chem$SiO2[i] < -900)
      precip_chem$SiO2[i] <- NA
    if (!is.na(precip_chem$Mn[i]) && precip_chem$Mn[i] < -900)
      precip_chem$Mn[i] <- NA
    if (!is.na(precip_chem$Fe[i]) && precip_chem$Fe[i] < -900)
      precip_chem$Fe[i] <- NA
    if (!is.na(precip_chem$F[i]) && precip_chem$F[i] < -900)
      precip_chem$F[i] <- NA
    if (!is.na(precip_chem$SpecCond[i]) && precip_chem$SpecCond[i] < -900)
      precip_chem$SpecCond[i] <- NA
    if (!is.na(precip_chem$TheoryCond[i]) && precip_chem$TheoryCond[i] < -900)
      precip_chem$TheoryCond[i] <- NA     }
  return (precip_chem)
}

# this function returns a missing -888 value with the appropriate number of
# decimals places for any NA value for either monthly
# volwt conc or WY volwt conc. The function matches column names using the
# column number from function fix_NA.
missingNum <- function(df,c) {
  if (colnames(df)[c] == "volwt_Al_Ferron" | colnames(df)[c] ==
      "volwt_TMAl" | colnames(df)[c] == "volwt_OMAl" | colnames(df)[c] ==
      "volwt_NH4" | colnames(df)[c] == "volwt_PO4"  | colnames(df)[c] ==
      "volwt_SpecCond" | colnames(df)[c] == "volwt_SpecCond")
    return(-888.888)
  else if (colnames(df)[c] == "volwt_H")
    return(-888.8888)
  else
    return(-888.88)
}

# This function loops through row and columns looking for NA values in the
# volwt conc files for month and WY data. The function calls
# function missingNum to fill in NA with -888 and appropriate decimal places.
fix_NA <- function(testdf) {
  rowLen <- nrow(testdf)
  colLen <- ncol(testdf)
  for (r in 1:rowLen) {
    for (c in 1:colLen) {
      if (is.na(testdf[r,c])) {
        if (is.numeric(testdf[r,c]))
          testdf[r,c] <- missingNum(testdf,c)
      }
    }
  }
  return(testdf)
}

precip_chem_passed <- subset(precip_chem,precip_chem$Pass == 'T')

precip_chem_miss_passed <- missingData(precip_chem_passed)

precip_chem_miss_passed <-
  precip_chem_miss_passed[c("DATE","Ca","Mg","K","Na","Al_Ferron","TMAl","OMAl",
                            "Al_ICP","NH4","pH","SO4","NO3","Cl","PO4","DOC",
                            "TDN","DON","SiO2","Mn","Fe","F","Cation","Anion",
                            "SpecCond","TheoryCond","FieldCode","VarCode",
                            "IonBal","IonErr","Qcode","Pass")]

convert_pH <- function(precip_chem)
{
  #converts pH units to H+ mg/l
  temp <- -(precip_chem$pH)
  H <- 10**(temp)*1000
  return (H)
}

#create pH from H+ concentration in mg/l
precip_calc_pH <- function(pH,H)
{
  # converts H mg/l to pH to fill in missing (NA) pH
  missing <- is.na(pH)

  if (missing)
  {
    temp <- log10(H/1000)*-1
    pH <- temp
    return (pH)
  }
  else # pH not missing just return pH
    return (pH)
}

daily_precip_calc_pH <- function(pH,H)
{
  # the daily conc file averages the daily pH Data by sample dates.
  # It needs to be recalculated using the daily H+ concentration. -888 for H+
  # returns a pH value of -888.88 which indicates that the data wasn't sampled
  if (H < 0)
    pH <- -888.88
  else
  { temp <- log10(H/1000)*-1
  pH <-temp }

  return (pH)
}

# filling in missing chem conc with average of good before & after values
# can't average pH must convert to H+
precip_chem_miss_passed$H <-
  ifelse(is.na(precip_chem_miss_passed$pH),NA,
         ifelse((precip_chem_miss_passed$pH < 0),
                -888.88,convert_pH(precip_chem_miss_passed)))
precip_chem_miss_passed$H <-
  ifelse(is.na(precip_chem_miss_passed$H),
         c((na.locf(precip_chem_miss_passed$H) +
              rev(na.locf(rev(precip_chem_miss_passed$H))))/2),
         precip_chem_miss_passed$H)
precip_chem_miss_passed$Ca <-
  c((na.locf(precip_chem_miss_passed$Ca) +
       rev(na.locf(rev(precip_chem_miss_passed$Ca))))/2)
precip_chem_miss_passed$Mg <-
  c((na.locf(precip_chem_miss_passed$Mg) +
       rev(na.locf(rev(precip_chem_miss_passed$Mg))))/2)
precip_chem_miss_passed$K <-
  c((na.locf(precip_chem_miss_passed$K) +
       rev(na.locf(rev(precip_chem_miss_passed$K))))/2)
precip_chem_miss_passed$Na <-
  c((na.locf(precip_chem_miss_passed$Na) +
       rev(na.locf(rev(precip_chem_miss_passed$Na))))/2)
precip_chem_miss_passed$Al_Ferron <-
  c((na.locf(precip_chem_miss_passed$Al_Ferron) +
       rev(na.locf(rev(precip_chem_miss_passed$Al_Ferron))))/2)
precip_chem_miss_passed$TMAl <-
  c((na.locf(precip_chem_miss_passed$TMAl) +
       rev(na.locf(rev(precip_chem_miss_passed$TMAl))))/2)
precip_chem_miss_passed$OMAl <-
  c((na.locf(precip_chem_miss_passed$OMAl) +
       rev(na.locf(rev(precip_chem_miss_passed$OMAl))))/2)
precip_chem_miss_passed$Al_ICP <-
  c((na.locf(precip_chem_miss_passed$Al_ICP) +
       rev(na.locf(rev(precip_chem_miss_passed$Al_ICP))))/2)
precip_chem_miss_passed$NH4 <-
  c((na.locf(precip_chem_miss_passed$NH4) +
       rev(na.locf(rev(precip_chem_miss_passed$NH4))))/2)
precip_chem_miss_passed$SO4 <-
  c((na.locf(precip_chem_miss_passed$SO4) +
       rev(na.locf(rev(precip_chem_miss_passed$SO4))))/2)
precip_chem_miss_passed$NO3 <-
  c((na.locf(precip_chem_miss_passed$NO3) +
       rev(na.locf(rev(precip_chem_miss_passed$NO3))))/2)
precip_chem_miss_passed$Cl <-
  c((na.locf(precip_chem_miss_passed$Cl) +
       rev(na.locf(rev(precip_chem_miss_passed$Cl))))/2)
precip_chem_miss_passed$PO4 <-
  c((na.locf(precip_chem_miss_passed$PO4) +
       rev(na.locf(rev(precip_chem_miss_passed$PO4))))/2)
precip_chem_miss_passed$DOC <-
  c((na.locf(precip_chem_miss_passed$DOC) +
       rev(na.locf(rev(precip_chem_miss_passed$DOC))))/2)
precip_chem_miss_passed$TDN <-
  c((na.locf(precip_chem_miss_passed$TDN) +
       rev(na.locf(rev(precip_chem_miss_passed$TDN))))/2)
precip_chem_miss_passed$DON <-
  c((na.locf(precip_chem_miss_passed$DON) +
       rev(na.locf(rev(precip_chem_miss_passed$DON))))/2)
precip_chem_miss_passed$SiO2 <-
  c((na.locf(precip_chem_miss_passed$SiO2) +
       rev(na.locf(rev(precip_chem_miss_passed$SiO2))))/2)
precip_chem_miss_passed$Mn <-
  c((na.locf(precip_chem_miss_passed$Mn) +
       rev(na.locf(rev(precip_chem_miss_passed$Mn))))/2)
precip_chem_miss_passed$Fe <-
  c((na.locf(precip_chem_miss_passed$Fe) +
       rev(na.locf(rev(precip_chem_miss_passed$Fe))))/2)
precip_chem_miss_passed$F <-
  c((na.locf(precip_chem_miss_passed$F) +
       rev(na.locf(rev(precip_chem_miss_passed$F))))/2)
precip_chem_miss_passed$SpecCond <-
  c((na.locf(precip_chem_miss_passed$SpecCond) +
       rev(na.locf(rev(precip_chem_miss_passed$SpecCond))))/2)
precip_chem_miss_passed$TheoryCond <-
  c((na.locf(precip_chem_miss_passed$TheoryCond) +
       rev(na.locf(rev(precip_chem_miss_passed$TheoryCond))))/2)

len_Conc <- nrow(precip_chem_miss_passed)

#fill in missing pH from H+ mg/l
for (i in 1:len_Conc)
{
  precip_chem_miss_passed$pH[i] <-
    precip_calc_pH(precip_chem_miss_passed$pH[i],precip_chem_miss_passed$H[i])
}

#reorder columns
precip_chem_miss_new <-
  precip_chem_miss_passed[c("DATE","Ca","Mg","K","Na","Al_Ferron","TMAl","OMAl",
                            "Al_ICP","NH4","pH","H","SO4","NO3","Cl","PO4",
                            "DOC","TDN","DON","SiO2","Mn","Fe","F","SpecCond",
                            "TheoryCond")]

ws_precip <- read.table(file=paste("ws",watershed,"_precip.csv", sep=""),
                        header=TRUE,sep=",", colClasses=c("character","numeric"))

ws_precip$DATE <- as.Date(ws_precip$DATE,"%Y-%m-%d")

temp_date <- ws_precip$DATE
Year <- substr(temp_date,1,4)
Year <- as.matrix(Year)
Month <-  substr(temp_date,6,7)
Month <- as.matrix(Month)
Year_Month <- paste(Year,Month,sep="-")
Year_Month <- as.matrix(Year_Month)

#create water year variable
water_year <- function(Year,Month)
{
  # water year June 1st - May 31st
  # assign water year depending if month < 6
  tYear <- as.numeric(Year)
  tMon <- as.numeric(Month)
  WYear <- ifelse(tMon<6,tYear-1,tYear)
  return (WYear)
}

WY <- water_year(Year,Month)
WY <- as.matrix(WY)

ws_precip <- cbind(ws_precip,Year,Month,Year_Month,WY)
ws_precip <-
  ws_precip[c("DATE","Year","Month","Year_Month","WY","precip_mm")]

ws_precip$DATE <- as.Date(ws_precip$DATE, "%Y-%m-%d")

precip_chem_miss_passed <-
  precip_chem_miss_passed[(precip_chem_miss_passed$DATE >=
                                 paste(begin_date) &
                             precip_chem_miss_passed$DATE <=
                                 paste(end_date)),]

precip_chem_miss_passed <-
  subset(precip_chem_miss_passed, select =
           -c(Cation,Anion,FieldCode,VarCode,IonBal,IonErr,Qcode,Pass))

date_range <- as.data.frame(seq(as.Date("1954-01-01"), by = "day", length.out = 382044))
colnames(date_range) <- c("DATE")
date_range <- as.data.frame(date_range[(date_range$DATE >= paste(begin_date) &
                            date_range$DATE <= paste(end_date)),])
colnames(date_range) <- c("DATE")

head(date_range)
tail(date_range)

precip_chem_miss_passed <-
  merge(date_range, precip_chem_miss_passed, by='DATE', all=TRUE)

head(precip_chem_miss_passed)
tail(precip_chem_miss_passed)

# next convert chemistry between sample dates to appropriate values
precip_chem_miss_passed_filled <- imputeTS::na_locf(precip_chem_miss_passed, option = 'nocb')
precip_chem_miss_passed_filled <- fill_NAs(precip_chem_miss_passed)

precip_dailyChemConc_filled <-
  merge(ws_precip, precip_chem_miss_passed_filled, by='DATE')

precip_dailyChemConc_filled$Ca <-
  ifelse(precip_dailyChemConc_filled$Ca < 0,NA,
         precip_dailyChemConc_filled$Ca)
precip_dailyChemConc_filled$Mg <-
  ifelse(precip_dailyChemConc_filled$Mg < 0,NA,
         precip_dailyChemConc_filled$Mg)
precip_dailyChemConc_filled$K <-
  ifelse(precip_dailyChemConc_filled$K < 0,NA,
         precip_dailyChemConc_filled$K)
precip_dailyChemConc_filled$Na <-
  ifelse(precip_dailyChemConc_filled$Na < 0,NA,
         precip_dailyChemConc_filled$Na)
precip_dailyChemConc_filled$Al_Ferron <-
  ifelse(precip_dailyChemConc_filled$Al_Ferron < 0,NA,
         precip_dailyChemConc_filled$Al_Ferron)
precip_dailyChemConc_filled$TMAl <-
  ifelse(precip_dailyChemConc_filled$TMAl < 0,NA,
         precip_dailyChemConc_filled$TMAl)
precip_dailyChemConc_filled$OMAl <-
  ifelse(precip_dailyChemConc_filled$OMAl < 0,NA,
         precip_dailyChemConc_filled$OMAl)
precip_dailyChemConc_filled$Al_ICP <-
  ifelse(precip_dailyChemConc_filled$Al_ICP < 0,NA,
         precip_dailyChemConc_filled$Al_ICP)
precip_dailyChemConc_filled$NH4 <-
  ifelse(precip_dailyChemConc_filled$NH4 < 0,NA,
         precip_dailyChemConc_filled$NH4)
precip_dailyChemConc_filled$SO4 <-
  ifelse(precip_dailyChemConc_filled$SO4 < 0,NA,
         precip_dailyChemConc_filled$SO4)
precip_dailyChemConc_filled$NO3 <-
  ifelse(precip_dailyChemConc_filled$NO3 < 0,NA,
         precip_dailyChemConc_filled$NO3)
precip_dailyChemConc_filled$Cl <-
  ifelse(precip_dailyChemConc_filled$Cl < 0,NA,
         precip_dailyChemConc_filled$Cl)
precip_dailyChemConc_filled$PO4 <-
  ifelse(precip_dailyChemConc_filled$PO4 < 0,NA,
         precip_dailyChemConc_filled$PO4)
precip_dailyChemConc_filled$DOC <-
  ifelse(precip_dailyChemConc_filled$DOC < 0,NA,
         precip_dailyChemConc_filled$DOC)
precip_dailyChemConc_filled$TDN <-
  ifelse(precip_dailyChemConc_filled$TDN < 0,NA,
         precip_dailyChemConc_filled$TDN)
precip_dailyChemConc_filled$DON <-
  ifelse(precip_dailyChemConc_filled$DON < 0,NA,
         precip_dailyChemConc_filled$DON)
precip_dailyChemConc_filled$SiO2 <-
  ifelse(precip_dailyChemConc_filled$SiO2 < 0,NA,
         precip_dailyChemConc_filled$SiO2)
precip_dailyChemConc_filled$Mn <-
  ifelse(precip_dailyChemConc_filled$Mn < 0,NA,
         precip_dailyChemConc_filled$Mn)
precip_dailyChemConc_filled$Fe <-
  ifelse(precip_dailyChemConc_filled$Fe < 0,NA,
         precip_dailyChemConc_filled$Fe)
precip_dailyChemConc_filled$F <-
  ifelse(precip_dailyChemConc_filled$F < 0,NA,
         precip_dailyChemConc_filled$F)
precip_dailyChemConc_filled$H <-
  ifelse(precip_dailyChemConc_filled$pH < 0,NA,
         precip_dailyChemConc_filled$H)
precip_dailyChemConc_filled$pH <-
  ifelse(precip_dailyChemConc_filled$pH < 0,NA,
         precip_dailyChemConc_filled$pH)
precip_dailyChemConc_filled$SpecCond <-
  ifelse(precip_dailyChemConc_filled$SpecCond < 0,NA,
         precip_dailyChemConc_filled$SpecCond)
precip_dailyChemConc_filled$TheoryCond <-
  ifelse(precip_dailyChemConc_filled$TheoryCond < 0,NA,
         precip_dailyChemConc_filled$TheoryCond)

precip_daily_flux_gHa <- precip_dailyChemConc_filled

#create flux columns and assign NA values

precip_daily_flux_gHa[c("Ca_flux","Mg_flux","K_flux","Na_flux","Al_Ferron_flux",
                        "TMAl_flux","OMAl_flux","Al_ICP_flux","NH4_flux",
                        "H_flux","SO4_flux","NO3_flux","Cl_flux","PO4_flux",
                        "DOC_flux","TDN_flux","DON_flux","SiO2_flux","Mn_flux",
                        "Fe_flux","F_flux","SpecCond_flux",
                        "TheoryCond_flux")] <-NA

precip_daily_flux_gHa$Ca_flux <-
  ifelse(is.na(precip_daily_flux_gHa$Ca),NA,
         precip_daily_flux_gHa$Ca*precip_daily_flux_gHa$precip_mm*10)
precip_daily_flux_gHa$Mg_flux <-
  ifelse(is.na(precip_daily_flux_gHa$Mg),NA,
         precip_daily_flux_gHa$Mg*precip_daily_flux_gHa$precip_mm*10)
precip_daily_flux_gHa$K_flux <-
  ifelse(is.na(precip_daily_flux_gHa$K),NA,
         precip_daily_flux_gHa$K*precip_daily_flux_gHa$precip_mm*10)
precip_daily_flux_gHa$Na_flux <-
  ifelse(is.na(precip_daily_flux_gHa$Na),NA,
         precip_daily_flux_gHa$Na*precip_daily_flux_gHa$precip_mm*10)
precip_daily_flux_gHa$Al_Ferron_flux <-
  ifelse(is.na(precip_daily_flux_gHa$Al_Ferron),NA,
         precip_daily_flux_gHa$Al_Ferron*precip_daily_flux_gHa$precip_mm*10)
precip_daily_flux_gHa$TMAl_flux <-
  ifelse(is.na(precip_daily_flux_gHa$TMAl),NA,
         precip_daily_flux_gHa$TMAl*precip_daily_flux_gHa$precip_mm*10)
precip_daily_flux_gHa$OMAl_flux <-
  ifelse(is.na(precip_daily_flux_gHa$OMAl),NA,
         precip_daily_flux_gHa$OMAl*precip_daily_flux_gHa$precip_mm*10)
precip_daily_flux_gHa$Al_ICP_flux <-
  ifelse(is.na(precip_daily_flux_gHa$Al_ICP),NA,
         precip_daily_flux_gHa$Al_ICP*precip_daily_flux_gHa$precip_mm*10)
precip_daily_flux_gHa$NH4_flux <-
  ifelse(is.na(precip_daily_flux_gHa$NH4),NA,
         precip_daily_flux_gHa$NH4*precip_daily_flux_gHa$precip_mm*10)
precip_daily_flux_gHa$SO4_flux <-
  ifelse(is.na(precip_daily_flux_gHa$SO4),NA,
         precip_daily_flux_gHa$SO4*precip_daily_flux_gHa$precip_mm*10)
precip_daily_flux_gHa$NO3_flux <-
  ifelse(is.na(precip_daily_flux_gHa$NO3),NA,
         precip_daily_flux_gHa$NO3*precip_daily_flux_gHa$precip_mm*10)
precip_daily_flux_gHa$Cl_flux <-
  ifelse(is.na(precip_daily_flux_gHa$Cl),NA,
         precip_daily_flux_gHa$Cl*precip_daily_flux_gHa$precip_mm*10)
precip_daily_flux_gHa$PO4_flux <-
  ifelse(is.na(precip_daily_flux_gHa$PO4),NA,
         precip_daily_flux_gHa$PO4*precip_daily_flux_gHa$precip_mm*10)
precip_daily_flux_gHa$DOC_flux <-
  ifelse(is.na(precip_daily_flux_gHa$DOC),NA,
         precip_daily_flux_gHa$DOC*precip_daily_flux_gHa$precip_mm*10)
precip_daily_flux_gHa$TDN_flux <-
  ifelse(is.na(precip_daily_flux_gHa$TDN),NA,
         precip_daily_flux_gHa$TDN*precip_daily_flux_gHa$precip_mm*10)
precip_daily_flux_gHa$DON_flux <-
  ifelse(is.na(precip_daily_flux_gHa$DON),NA,
         precip_daily_flux_gHa$DON*precip_daily_flux_gHa$precip_mm*10)
precip_daily_flux_gHa$SiO2_flux <-
  ifelse(is.na(precip_daily_flux_gHa$SiO2),NA,
         precip_daily_flux_gHa$SiO2*precip_daily_flux_gHa$precip_mm*10)
precip_daily_flux_gHa$Mn_flux <-
  ifelse(is.na(precip_daily_flux_gHa$Mn),NA,
         precip_daily_flux_gHa$Mn*precip_daily_flux_gHa$precip_mm*10)
precip_daily_flux_gHa$Fe_flux <-
  ifelse(is.na(precip_daily_flux_gHa$Fe),NA,
         precip_daily_flux_gHa$Fe*precip_daily_flux_gHa$precip_mm*10)
precip_daily_flux_gHa$F_flux <-
  ifelse(is.na(precip_daily_flux_gHa$F),NA,
         precip_daily_flux_gHa$F*precip_daily_flux_gHa$precip_mm*10)
precip_daily_flux_gHa$H_flux <-
  ifelse(is.na(precip_daily_flux_gHa$H),NA,
         precip_daily_flux_gHa$H*precip_daily_flux_gHa$precip_mm*10)
precip_daily_flux_gHa$SpecCond_flux <-
  ifelse(is.na(precip_daily_flux_gHa$SpecCond),NA,
         precip_daily_flux_gHa$SpecCond*precip_daily_flux_gHa$precip_mm*10)
precip_daily_flux_gHa$TheoryCond_flux <-
  ifelse(is.na(precip_daily_flux_gHa$TheoryCond),NA,
         precip_daily_flux_gHa$TheoryCond*precip_daily_flux_gHa$precip_mm*10)

precip_daily_flux_gHa_new <-
  precip_daily_flux_gHa[c("DATE","Year","Month","Year_Month","WY","precip_mm",
                          "Ca_flux","Mg_flux","K_flux","Na_flux",
                          "Al_Ferron_flux","TMAl_flux","OMAl_flux",
                          "Al_ICP_flux","NH4_flux","H_flux","SO4_flux",
                          "NO3_flux","Cl_flux","PO4_flux","DOC_flux","TDN_flux",
                          "DON_flux","SiO2_flux","Mn_flux","Fe_flux","F_flux",
                          "SpecCond_flux","TheoryCond_flux")]

#This section deletes the last month if there partial daily data
precip_daily_flux_gHa_new$Year <-
  as.numeric(as.character(precip_daily_flux_gHa_new$Year))
min_year <- min(precip_daily_flux_gHa_new$Year)
max_year <- max(precip_daily_flux_gHa_new$Year)
precip_daily_flux_gHa_new$Month <-
  as.numeric(as.character(precip_daily_flux_gHa_new$Month))

Jan_min<-length(which(precip_daily_flux_gHa_new$Year==min_year &
                        precip_daily_flux_gHa_new$Month==1))
Feb_min<-length(which(precip_daily_flux_gHa_new$Year==min_year &
                        precip_daily_flux_gHa_new$Month==2))
Mar_min<-length(which(precip_daily_flux_gHa_new$Year==min_year &
                        precip_daily_flux_gHa_new$Month==3))
Apr_min<-length(which(precip_daily_flux_gHa_new$Year==min_year &
                        precip_daily_flux_gHa_new$Month==4))
May_min<-length(which(precip_daily_flux_gHa_new$Year==min_year &
                        precip_daily_flux_gHa_new$Month==5))
Jun_min<-length(which(precip_daily_flux_gHa_new$Year==min_year &
                        precip_daily_flux_gHa_new$Month==6))
Jul_min<-length(which(precip_daily_flux_gHa_new$Year==min_year &
                        precip_daily_flux_gHa_new$Month==7))
Aug_min<-length(which(precip_daily_flux_gHa_new$Year==min_year &
                        precip_daily_flux_gHa_new$Month==8))
Sep_min<-length(which(precip_daily_flux_gHa_new$Year==min_year &
                        precip_daily_flux_gHa_new$Month==9))
Oct_min<-length(which(precip_daily_flux_gHa_new$Year==min_year &
                        precip_daily_flux_gHa_new$Month==10))
Nov_min<-length(which(precip_daily_flux_gHa_new$Year==min_year &
                        precip_daily_flux_gHa_new$Month==11))
Dec_min<-length(which(precip_daily_flux_gHa_new$Year==min_year &
                        precip_daily_flux_gHa_new$Month==12))

Jan_max<-length(which(precip_daily_flux_gHa_new$Year==max_year &
                        precip_daily_flux_gHa_new$Month==1))
Feb_max<-length(which(precip_daily_flux_gHa_new$Year==max_year &
                        precip_daily_flux_gHa_new$Month==2))
Mar_max<-length(which(precip_daily_flux_gHa_new$Year==max_year &
                        precip_daily_flux_gHa_new$Month==3))
Apr_max<-length(which(precip_daily_flux_gHa_new$Year==max_year &
                        precip_daily_flux_gHa_new$Month==4))
May_max<-length(which(precip_daily_flux_gHa_new$Year==max_year &
                        precip_daily_flux_gHa_new$Month==5))
Jun_max<-length(which(precip_daily_flux_gHa_new$Year==max_year &
                        precip_daily_flux_gHa_new$Month==6))
Jul_max<-length(which(precip_daily_flux_gHa_new$Year==max_year &
                        precip_daily_flux_gHa_new$Month==7))
Aug_max<-length(which(precip_daily_flux_gHa_new$Year==max_year &
                        precip_daily_flux_gHa_new$Month==8))
Sep_max<-length(which(precip_daily_flux_gHa_new$Year==max_year &
                        precip_daily_flux_gHa_new$Month==9))
Oct_max<-length(which(precip_daily_flux_gHa_new$Year==max_year &
                        precip_daily_flux_gHa_new$Month==10))
Nov_max<-length(which(precip_daily_flux_gHa_new$Year==max_year &
                        precip_daily_flux_gHa_new$Month==11))
Dec_max<-length(which(precip_daily_flux_gHa_new$Year==max_year &
                        precip_daily_flux_gHa_new$Month==12))

precip_daily_flux_gHa_new <- subset(precip_daily_flux_gHa_new,
                                    !(Year==min_year & Month==1 & Jan_min<31))
precip_daily_flux_gHa_new <- subset(precip_daily_flux_gHa_new,
                                    !(Year==min_year & Month==2 & Feb_min<28))
precip_daily_flux_gHa_new <- subset(precip_daily_flux_gHa_new,
                                    !(Year==min_year & Month==3 & Mar_min<31))
precip_daily_flux_gHa_new <- subset(precip_daily_flux_gHa_new,
                                    !(Year==min_year & Month==4 & Apr_min<30))
precip_daily_flux_gHa_new <- subset(precip_daily_flux_gHa_new,
                                    !(Year==min_year & Month==5 & May_min<31))
precip_daily_flux_gHa_new <- subset(precip_daily_flux_gHa_new,
                                    !(Year==min_year & Month==6 & Jun_min<30))
precip_daily_flux_gHa_new <- subset(precip_daily_flux_gHa_new,
                                    !(Year==min_year & Month==7 & Jul_min<31))
precip_daily_flux_gHa_new <- subset(precip_daily_flux_gHa_new,
                                    !(Year==min_year & Month==8 & Aug_min<31))
precip_daily_flux_gHa_new <- subset(precip_daily_flux_gHa_new,
                                    !(Year==min_year & Month==9 & Sep_min<30))
precip_daily_flux_gHa_new <- subset(precip_daily_flux_gHa_new,
                                    !(Year==min_year & Month==10 & Oct_min<31))
precip_daily_flux_gHa_new <- subset(precip_daily_flux_gHa_new,
                                    !(Year==min_year & Month==11 & Nov_min<30))
precip_daily_flux_gHa_new <- subset(precip_daily_flux_gHa_new,
                                    !(Year==min_year & Month==12 & Dec_min<31))

precip_daily_flux_gHa_new <- subset(precip_daily_flux_gHa_new,
                                    !(Year==max_year & Month==1 & Jan_max<31))
precip_daily_flux_gHa_new <- subset(precip_daily_flux_gHa_new,
                                    !(Year==max_year & Month==2 & Feb_max<28))
precip_daily_flux_gHa_new <- subset(precip_daily_flux_gHa_new,
                                    !(Year==max_year & Month==3 & Mar_max<31))
precip_daily_flux_gHa_new <- subset(precip_daily_flux_gHa_new,
                                    !(Year==max_year & Month==4 & Apr_max<30))
precip_daily_flux_gHa_new <- subset(precip_daily_flux_gHa_new,
                                    !(Year==max_year & Month==5 & May_max<31))
precip_daily_flux_gHa_new <- subset(precip_daily_flux_gHa_new,
                                    !(Year==max_year & Month==6 & Jun_max<30))
precip_daily_flux_gHa_new <- subset(precip_daily_flux_gHa_new,
                                    !(Year==max_year & Month==7 & Jul_max<31))
precip_daily_flux_gHa_new <- subset(precip_daily_flux_gHa_new,
                                    !(Year==max_year & Month==8 & Aug_max<31))
precip_daily_flux_gHa_new <- subset(precip_daily_flux_gHa_new,
                                    !(Year==max_year & Month==9 & Sep_max<30))
precip_daily_flux_gHa_new <- subset(precip_daily_flux_gHa_new,
                                    !(Year==max_year & Month==10 & Oct_max<31))
precip_daily_flux_gHa_new <- subset(precip_daily_flux_gHa_new,
                                    !(Year==max_year & Month==11 & Nov_max<30))
precip_daily_flux_gHa_new <- subset(precip_daily_flux_gHa_new,
                                    !(Year==max_year & Month==12 & Dec_max<31))

precip_monthly_flux_gHa <-
  aggregate(precip_daily_flux_gHa_new[c("precip_mm","Ca_flux","Mg_flux",
                                        "K_flux","Na_flux","Al_Ferron_flux",
                                        "TMAl_flux","OMAl_flux","Al_ICP_flux",
                                        "NH4_flux","H_flux","SO4_flux",
                                        "NO3_flux","Cl_flux","PO4_flux",
                                        "DOC_flux","TDN_flux","DON_flux",
                                        "SiO2_flux","Mn_flux","Fe_flux",
                                        "F_flux","SpecCond_flux",
                                        "TheoryCond_flux")],
            by=list(precip_daily_flux_gHa_new$Year_Month),sum)

colnames(precip_monthly_flux_gHa) <- c("Year_Month","precip_mm","Ca_flux",
                                       "Mg_flux","K_flux","Na_flux",
                                       "Al_Ferron_flux","TMAl_flux",
                                       "OMAl_flux","Al_ICP_flux","NH4_flux",
                                       "H_flux","SO4_flux","NO3_flux","Cl_flux",
                                       "PO4_flux","DOC_flux","TDN_flux",
                                       "DON_flux","SiO2_flux","Mn_flux",
                                       "Fe_flux","F_flux","SpecCond_flux",
                                       "TheoryCond_flux")

precip_monthly_flux_gHa <- makeYearMonth(precip_monthly_flux_gHa)
precip_monthly_volwt_conc <- precip_monthly_flux_gHa

precip_monthly_flux_gHa <- fix_NA(precip_monthly_flux_gHa)

#creates a subdirectory called results if it doesn't already exist
ifelse(!dir.exists(paste(current_path,"/results",sep = "")),
       dir.create(paste(current_path,"/results",sep = "")), FALSE)

#strips out SpecCond and TheoryCond from the monthly flux table dataframe
precip_monthly_flux_gHa <- subset(precip_monthly_flux_gHa,
                                  select = -c(SpecCond_flux,TheoryCond_flux))

precip_monthly_flux_gHa$H_flux <-
  round(precip_monthly_flux_gHa$H_flux,digits = 4)



precip_monthly_volwt_conc$volwt_Ca <-
  ifelse(is.na(precip_monthly_volwt_conc$Ca_flux),NA,
         precip_monthly_volwt_conc$Ca_flux/
           (precip_monthly_volwt_conc$precip_mm*10))
precip_monthly_volwt_conc$volwt_Mg <-
  ifelse(is.na(precip_monthly_volwt_conc$Mg_flux),NA,
         precip_monthly_volwt_conc$Mg_flux/
           (precip_monthly_volwt_conc$precip_mm*10))
precip_monthly_volwt_conc$volwt_K <-
  ifelse(is.na(precip_monthly_volwt_conc$K_flux),NA,
         precip_monthly_volwt_conc$K_flux/
           (precip_monthly_volwt_conc$precip_mm*10))
precip_monthly_volwt_conc$volwt_Na <-
  ifelse(is.na(precip_monthly_volwt_conc$Na_flux),NA,
         precip_monthly_volwt_conc$Na_flux/
           (precip_monthly_volwt_conc$precip_mm*10))
precip_monthly_volwt_conc$volwt_Al_Ferron <-
  ifelse(is.na(precip_monthly_volwt_conc$Al_Ferron_flux),NA,
         precip_monthly_volwt_conc$Al_Ferron_flux/
           (precip_monthly_volwt_conc$precip_mm*10))
precip_monthly_volwt_conc$volwt_TMAl <-
  ifelse(is.na(precip_monthly_volwt_conc$TMAl_flux),NA,
         precip_monthly_volwt_conc$TMAl_flux/
           (precip_monthly_volwt_conc$precip_mm*10))
precip_monthly_volwt_conc$volwt_OMAl <-
  ifelse(is.na(precip_monthly_volwt_conc$OMAl_flux),NA,
         precip_monthly_volwt_conc$OMAl_flux/
           (precip_monthly_volwt_conc$precip_mm*10))
precip_monthly_volwt_conc$volwt_Al_ICP <-
  ifelse(is.na(precip_monthly_volwt_conc$Al_ICP_flux),
         NA,precip_monthly_volwt_conc$Al_ICP_flux/
           (precip_monthly_volwt_conc$precip_mm*10))
precip_monthly_volwt_conc$volwt_NH4 <-
  ifelse(is.na(precip_monthly_volwt_conc$NH4_flux),NA,
         precip_monthly_volwt_conc$NH4_flux/
           (precip_monthly_volwt_conc$precip_mm*10))
precip_monthly_volwt_conc$volwt_SO4 <-
  ifelse(is.na(precip_monthly_volwt_conc$SO4_flux),NA,
         precip_monthly_volwt_conc$SO4_flux/
           (precip_monthly_volwt_conc$precip_mm*10))
precip_monthly_volwt_conc$volwt_NO3 <-
  ifelse(is.na(precip_monthly_volwt_conc$NO3_flux),NA,
         precip_monthly_volwt_conc$NO3_flux/
           (precip_monthly_volwt_conc$precip_mm*10))
precip_monthly_volwt_conc$volwt_Cl <-
  ifelse(is.na(precip_monthly_volwt_conc$Cl_flux),NA,
         precip_monthly_volwt_conc$Cl_flux/
           (precip_monthly_volwt_conc$precip_mm*10))
precip_monthly_volwt_conc$volwt_PO4 <-
  ifelse(is.na(precip_monthly_volwt_conc$PO4_flux),NA,
         precip_monthly_volwt_conc$PO4_flux/
           (precip_monthly_volwt_conc$precip_mm*10))
precip_monthly_volwt_conc$volwt_DOC <-
  ifelse(is.na(precip_monthly_volwt_conc$DOC_flux),NA,
         precip_monthly_volwt_conc$DOC_flux/
           (precip_monthly_volwt_conc$precip_mm*10))
precip_monthly_volwt_conc$volwt_TDN <-
  ifelse(is.na(precip_monthly_volwt_conc$TDN_flux),NA,
         precip_monthly_volwt_conc$TDN_flux/
           (precip_monthly_volwt_conc$precip_mm*10))
precip_monthly_volwt_conc$volwt_DON <-
  ifelse(is.na(precip_monthly_volwt_conc$DON_flux),NA,
         precip_monthly_volwt_conc$DON_flux/
           (precip_monthly_volwt_conc$precip_mm*10))
precip_monthly_volwt_conc$volwt_SiO2 <-
  ifelse(is.na(precip_monthly_volwt_conc$SiO2_flux),NA,
         precip_monthly_volwt_conc$SiO2_flux/
           (precip_monthly_volwt_conc$precip_mm*10))
precip_monthly_volwt_conc$volwt_Mn <-
  ifelse(is.na(precip_monthly_volwt_conc$Mn_flux),NA,
         precip_monthly_volwt_conc$Mn_flux/
           (precip_monthly_volwt_conc$precip_mm*10))
precip_monthly_volwt_conc$volwt_Fe <-
  ifelse(is.na(precip_monthly_volwt_conc$Fe_flux),NA,
         precip_monthly_volwt_conc$Fe_flux/
           (precip_monthly_volwt_conc$precip_mm*10))
precip_monthly_volwt_conc$volwt_F <-
  ifelse(is.na(precip_monthly_volwt_conc$F_flux),NA,
         precip_monthly_volwt_conc$F_flux/
           (precip_monthly_volwt_conc$precip_mm*10))
precip_monthly_volwt_conc$volwt_H <-
  ifelse(is.na(precip_monthly_volwt_conc$H_flux),NA,
         precip_monthly_volwt_conc$H_flux/
           (precip_monthly_volwt_conc$precip_mm*10))
precip_monthly_volwt_conc$volwt_SpecCond <-
  ifelse(is.na(precip_monthly_volwt_conc$SpecCond_flux),NA,
         precip_monthly_volwt_conc$SpecCond_flux/
           (precip_monthly_volwt_conc$precip_mm*10))
precip_monthly_volwt_conc$volwt_TheoryCond <-
  ifelse(is.na(precip_monthly_volwt_conc$TheoryCond_flux),NA,
         precip_monthly_volwt_conc$TheoryCond_flux/
           (precip_monthly_volwt_conc$precip_mm*10))

precip_monthly_volwt_conc$volwt_pH <- NA

len_Conc <- nrow(precip_monthly_volwt_conc)

#fill in missing pH from H+ mg/l
for (i in 1:len_Conc)
{
  precip_monthly_volwt_conc$volwt_pH[i] <-
    precip_calc_pH(precip_monthly_volwt_conc$volwt_pH[i],
                   precip_monthly_volwt_conc$volwt_H[i])
}

volwt_Ca <- round(precip_monthly_volwt_conc$volwt_Ca,digits = 2)
volwt_Mg <- round(precip_monthly_volwt_conc$volwt_Mg,digits = 2)
volwt_K <- round(precip_monthly_volwt_conc$volwt_K,digits = 2)
volwt_Na <- round(precip_monthly_volwt_conc$volwt_Na,digits = 2)
volwt_Al_Ferron <- round(precip_monthly_volwt_conc$volwt_Al_Ferron,digits = 3)
volwt_TMAl <- round(precip_monthly_volwt_conc$volwt_TMAl,digits = 3)
volwt_OMAl <- round(precip_monthly_volwt_conc$volwt_OMAl,digits = 3)
volwt_Al_ICP <- round(precip_monthly_volwt_conc$volwt_Al_ICP,digits = 2)
volwt_NH4 <- round(precip_monthly_volwt_conc$volwt_NH4,digits = 3)
volwt_SO4 <- round(precip_monthly_volwt_conc$volwt_SO4,digits = 2)
volwt_NO3 <- round(precip_monthly_volwt_conc$volwt_NO3,digits = 2)
volwt_Cl <- round(precip_monthly_volwt_conc$volwt_Cl,digits = 2)
volwt_PO4 <- round(precip_monthly_volwt_conc$volwt_PO4,digits = 3)
volwt_DOC <- round(precip_monthly_volwt_conc$volwt_DOC,digits = 2)
volwt_TDN <- round(precip_monthly_volwt_conc$volwt_TDN,digits = 2)
volwt_DON <- round(precip_monthly_volwt_conc$volwt_DON,digits = 2)
volwt_SiO2 <- round(precip_monthly_volwt_conc$volwt_SiO2,digits = 2)
volwt_Mn <- round(precip_monthly_volwt_conc$volwt_Mn,digits = 2)
volwt_Fe <- round(precip_monthly_volwt_conc$volwt_Fe,digits = 2)
volwt_F <- round(precip_monthly_volwt_conc$volwt_F,digits = 2)
volwt_H <- round(precip_monthly_volwt_conc$volwt_H,digits = 4)
volwt_pH <- round(precip_monthly_volwt_conc$volwt_pH,digits = 2)
volwt_SpecCond <- round(precip_monthly_volwt_conc$volwt_SpecCond,digits = 3)
volwt_TheoryCond <- round(precip_monthly_volwt_conc$volwt_TheoryCond,digits = 3)

precip_monthly_volwt_conc$volwt_Ca <- volwt_Ca
precip_monthly_volwt_conc$volwt_Mg <- volwt_Mg
precip_monthly_volwt_conc$volwt_K <- volwt_K
precip_monthly_volwt_conc$volwt_Na <- volwt_Na
precip_monthly_volwt_conc$volwt_Al_Ferron <- volwt_Al_Ferron
precip_monthly_volwt_conc$volwt_TMAl <- volwt_TMAl
precip_monthly_volwt_conc$volwt_OMAl <- volwt_OMAl
precip_monthly_volwt_conc$volwt_Al_ICP <- volwt_Al_ICP
precip_monthly_volwt_conc$volwt_NH4 <- volwt_NH4
precip_monthly_volwt_conc$volwt_SO4 <- volwt_SO4
precip_monthly_volwt_conc$volwt_NO3 <- volwt_NO3
precip_monthly_volwt_conc$volwt_Cl <- volwt_Cl
precip_monthly_volwt_conc$volwt_PO4 <- volwt_PO4
precip_monthly_volwt_conc$volwt_DOC <- volwt_DOC
precip_monthly_volwt_conc$volwt_TDN <- volwt_TDN
precip_monthly_volwt_conc$volwt_DON <- volwt_DON
precip_monthly_volwt_conc$volwt_SiO2 <- volwt_SiO2
precip_monthly_volwt_conc$volwt_Mn <- volwt_Mn
precip_monthly_volwt_conc$volwt_Fe <- volwt_Fe
precip_monthly_volwt_conc$volwt_F <- volwt_F
precip_monthly_volwt_conc$volwt_H <- volwt_H
precip_monthly_volwt_conc$volwt_pH <- volwt_pH
precip_monthly_volwt_conc$volwt_SpecCond <- volwt_SpecCond

precip_monthly_volwt_conc <-
  subset(precip_monthly_volwt_conc, select = -c(Ca_flux,Mg_flux,K_flux,Na_flux,
                                                Al_Ferron_flux,TMAl_flux,
                                                OMAl_flux,Al_ICP_flux,NH4_flux,
                                                H_flux,SO4_flux,NO3_flux,
                                                Cl_flux,PO4_flux,DOC_flux,
                                                TDN_flux,DON_flux,SiO2_flux,
                                                Mn_flux,Fe_flux,F_flux,
                                                SpecCond_flux,TheoryCond_flux))

precip_monthly_volwt_conc <- fix_NA(precip_monthly_volwt_conc)

#strips out TheoryCond from the monthly volwt table dataframe
precip_monthly_volwt_conc <- subset(precip_monthly_volwt_conc,
                                    select = -c(volwt_TheoryCond))

#reorders column names
precip_monthly_volwt_conc <-
  precip_monthly_volwt_conc[c("Year","Month","Year_Month","precip_mm",
                              "volwt_Ca","volwt_Mg","volwt_K","volwt_Na",
                              "volwt_Al_Ferron","volwt_TMAl","volwt_OMAl",
                              "volwt_Al_ICP","volwt_NH4","volwt_SO4",
                              "volwt_NO3","volwt_Cl","volwt_PO4","volwt_DOC",
                              "volwt_TDN","volwt_DON","volwt_SiO2","volwt_Mn",
                              "volwt_Fe","volwt_F","volwt_H","volwt_pH",
                              "volwt_SpecCond")]

colnames(precip_monthly_volwt_conc) <-
  c("Year","Month","Year_Month","precip_mm","Ca_volwt","Mg_volwt","K_volwt",
    "Na_volwt","Al_Ferron_volwt","TMAl_volwt","OMAl_volwt","Al_ICP_volwt",
    "NH4_volwt","SO4_volwt","NO3_volwt","Cl_volwt","PO4_volwt","DOC_volwt",
    "TDN_volwt","DON_volwt","SiO2_volwt","Mn_volwt","Fe_volwt",
    "F_volwt","H_volwt","pH_volwt","SpecCond_volwt")

#adds pH, ANC, and specific conductance to flux file
precip_monthly_flux_gHa <- merge(x = precip_monthly_flux_gHa, y = precip_monthly_volwt_conc[ , c("Year_Month","pH_volwt","SpecCond_volwt")], by = "Year_Month")

head(precip_monthly_flux_gHa,5)

#reorders column names
precip_monthly_flux_gHa <-
  precip_monthly_flux_gHa[c("Year","Month","Year_Month","precip_mm",
                              "Ca_flux","Mg_flux","K_flux","Na_flux",
                              "Al_Ferron_flux","TMAl_flux","OMAl_flux",
                              "Al_ICP_flux","NH4_flux","SO4_flux",
                              "NO3_flux","Cl_flux","PO4_flux","DOC_flux",
                              "TDN_flux","DON_flux","SiO2_flux","Mn_flux",
                              "Fe_flux","F_flux","H_flux","pH_volwt",
                              "SpecCond_volwt")]

out_monthly_flux_gHa_Name <-
  paste("results/ws",watershed,"_precip_monthly_flux_gHa.csv",sep="")
out_monthly_volwt_Name <-
  paste("results/ws",watershed,"_precip_monthly_volwt_conc.csv",sep="")

write.csv(precip_monthly_flux_gHa,
          file = out_monthly_flux_gHa_Name,row.names=FALSE)
write.csv(precip_monthly_volwt_conc,
          file = out_monthly_volwt_Name,row.names=FALSE)

#Strips the last WY off if it is incomplete
WY_day_count <- count(precip_daily_flux_gHa_new, 'WY')
WY_day_count_head <- head(WY_day_count,1)
WY_day_count_tail <- tail(WY_day_count,1)
first_year<-ifelse(WY_day_count_head$freq <= 364,
                   WY_day_count_head$WY+1,WY_day_count_head$WY)
last_year<-ifelse(WY_day_count_tail$freq <=
                    364,WY_day_count_tail$WY-1,WY_day_count_tail$WY)
precip_daily_flux_gHa_new <- subset(precip_daily_flux_gHa_new,
                                    !(precip_daily_flux_gHa_new$WY<first_year))
precip_daily_flux_gHa_new <- subset(precip_daily_flux_gHa_new,
                                    !(precip_daily_flux_gHa_new$WY>last_year))

precip_WY_flux_gHa <-
  aggregate(precip_daily_flux_gHa_new[c("precip_mm","Ca_flux","Mg_flux",
                                        "K_flux","Na_flux","Al_Ferron_flux",
                                        "TMAl_flux","OMAl_flux","Al_ICP_flux",
                                        "NH4_flux","H_flux","SO4_flux",
                                        "NO3_flux","Cl_flux","PO4_flux",
                                        "DOC_flux","TDN_flux","DON_flux",
                                        "SiO2_flux","Mn_flux","Fe_flux",
                                        "F_flux","SpecCond_flux",
                                        "TheoryCond_flux")],
            by=list(precip_daily_flux_gHa_new$WY),sum)

colnames(precip_WY_flux_gHa) <- c("WaterYear","precip_mm","Ca_flux","Mg_flux",
                                  "K_flux","Na_flux","Al_Ferron_flux",
                                  "TMAl_flux","OMAl_flux","Al_ICP_flux",
                                  "NH4_flux","H_flux","SO4_flux","NO3_flux",
                                  "Cl_flux","PO4_flux","DOC_flux","TDN_flux",
                                  "DON_flux","SiO2_flux","Mn_flux","Fe_flux",
                                  "F_flux","SpecCond_flux","TheoryCond_flux")

precip_WY_volwt_conc <- precip_WY_flux_gHa
precip_WY_flux_gHa <- fix_NA(precip_WY_flux_gHa)

precip_WY_volwt_conc$volwt_Ca <-
  ifelse(is.na(precip_WY_volwt_conc$Ca_flux),NA,
         precip_WY_volwt_conc$Ca_flux/
           (precip_WY_volwt_conc$precip_mm*10))
precip_WY_volwt_conc$volwt_Mg <-
  ifelse(is.na(precip_WY_volwt_conc$Mg_flux),NA,
         precip_WY_volwt_conc$Mg_flux/
           (precip_WY_volwt_conc$precip_mm*10))
precip_WY_volwt_conc$volwt_K <-
  ifelse(is.na(precip_WY_volwt_conc$K_flux),NA,
         precip_WY_volwt_conc$K_flux/
           (precip_WY_volwt_conc$precip_mm*10))
precip_WY_volwt_conc$volwt_Na <-
  ifelse(is.na(precip_WY_volwt_conc$Na_flux),NA,
         precip_WY_volwt_conc$Na_flux/
           (precip_WY_volwt_conc$precip_mm*10))
precip_WY_volwt_conc$volwt_Al_Ferron <-
  ifelse(is.na(precip_WY_volwt_conc$Al_Ferron_flux),NA,
         precip_WY_volwt_conc$Al_Ferron_flux/
           (precip_WY_volwt_conc$precip_mm*10))
precip_WY_volwt_conc$volwt_TMAl <-
  ifelse(is.na(precip_WY_volwt_conc$TMAl_flux),NA,
         precip_WY_volwt_conc$TMAl_flux/
           (precip_WY_volwt_conc$precip_mm*10))
precip_WY_volwt_conc$volwt_OMAl <-
  ifelse(is.na(precip_WY_volwt_conc$OMAl_flux),NA,
         precip_WY_volwt_conc$OMAl_flux/
           (precip_WY_volwt_conc$precip_mm*10))
precip_WY_volwt_conc$volwt_Al_ICP <-
  ifelse(is.na(precip_WY_volwt_conc$Al_ICP_flux),NA,
         precip_WY_volwt_conc$Al_ICP_flux/
           (precip_WY_volwt_conc$precip_mm*10))
precip_WY_volwt_conc$volwt_NH4 <-
  ifelse(is.na(precip_WY_volwt_conc$NH4_flux),NA,
         precip_WY_volwt_conc$NH4_flux/
           (precip_WY_volwt_conc$precip_mm*10))
precip_WY_volwt_conc$volwt_SO4 <-
  ifelse(is.na(precip_WY_volwt_conc$SO4_flux),NA,
         precip_WY_volwt_conc$SO4_flux/
           (precip_WY_volwt_conc$precip_mm*10))
precip_WY_volwt_conc$volwt_NO3 <-
  ifelse(is.na(precip_WY_volwt_conc$NO3_flux),NA,
         precip_WY_volwt_conc$NO3_flux/
           (precip_WY_volwt_conc$precip_mm*10))
precip_WY_volwt_conc$volwt_Cl <-
  ifelse(is.na(precip_WY_volwt_conc$Cl_flux),NA,
         precip_WY_volwt_conc$Cl_flux/
           (precip_WY_volwt_conc$precip_mm*10))
precip_WY_volwt_conc$volwt_PO4 <-
  ifelse(is.na(precip_WY_volwt_conc$PO4_flux),NA,
         precip_WY_volwt_conc$PO4_flux/
           (precip_WY_volwt_conc$precip_mm*10))
precip_WY_volwt_conc$volwt_DOC <-
  ifelse(is.na(precip_WY_volwt_conc$DOC_flux),NA,
         precip_WY_volwt_conc$DOC_flux/
           (precip_WY_volwt_conc$precip_mm*10))
precip_WY_volwt_conc$volwt_TDN <-
  ifelse(is.na(precip_WY_volwt_conc$TDN_flux),NA,
         precip_WY_volwt_conc$TDN_flux/
           (precip_WY_volwt_conc$precip_mm*10))
precip_WY_volwt_conc$volwt_DON <-
  ifelse(is.na(precip_WY_volwt_conc$DON_flux),NA,
         precip_WY_volwt_conc$DON_flux/
           (precip_WY_volwt_conc$precip_mm*10))
precip_WY_volwt_conc$volwt_SiO2 <-
  ifelse(is.na(precip_WY_volwt_conc$SiO2_flux),NA,
         precip_WY_volwt_conc$SiO2_flux/
           (precip_WY_volwt_conc$precip_mm*10))
precip_WY_volwt_conc$volwt_Mn <-
  ifelse(is.na(precip_WY_volwt_conc$Mn_flux),NA,
         precip_WY_volwt_conc$Mn_flux/
           (precip_WY_volwt_conc$precip_mm*10))
precip_WY_volwt_conc$volwt_Fe <-
  ifelse(is.na(precip_WY_volwt_conc$Fe_flux),NA,
         precip_WY_volwt_conc$Fe_flux/
           (precip_WY_volwt_conc$precip_mm*10))
precip_WY_volwt_conc$volwt_F <-
  ifelse(is.na(precip_WY_volwt_conc$F_flux),NA,
         precip_WY_volwt_conc$F_flux/
           (precip_WY_volwt_conc$precip_mm*10))
precip_WY_volwt_conc$volwt_H <-
  ifelse(is.na(precip_WY_volwt_conc$H_flux),NA,
         precip_WY_volwt_conc$H_flux/
           (precip_WY_volwt_conc$precip_mm*10))
precip_WY_volwt_conc$volwt_SpecCond <-
  ifelse(is.na(precip_WY_volwt_conc$SpecCond_flux),NA,
         precip_WY_volwt_conc$SpecCond_flux/
           (precip_WY_volwt_conc$precip_mm*10))
precip_WY_volwt_conc$volwt_TheoryCond <-
  ifelse(is.na(precip_WY_volwt_conc$TheoryCond_flux),NA,
         precip_WY_volwt_conc$TheoryCond_flux/
           (precip_WY_volwt_conc$precip_mm*10))

precip_WY_volwt_conc$volwt_pH <- NA

len_Conc <- nrow(precip_WY_volwt_conc)

#fill in missing pH from H+ mg/l
for (i in 1:len_Conc)
{
  precip_WY_volwt_conc$volwt_pH[i] <-
    precip_calc_pH(precip_WY_volwt_conc$volwt_pH[i],
                   precip_WY_volwt_conc$volwt_H[i])
}

volwt_Ca <- round(precip_WY_volwt_conc$volwt_Ca,digits = 2)
volwt_Mg <- round(precip_WY_volwt_conc$volwt_Mg,digits = 2)
volwt_K <- round(precip_WY_volwt_conc$volwt_K,digits = 2)
volwt_Na <- round(precip_WY_volwt_conc$volwt_Na,digits = 2)
volwt_Al_Ferron <- round(precip_WY_volwt_conc$volwt_Al_Ferron,digits = 3)
volwt_TMAl <- round(precip_WY_volwt_conc$volwt_TMAl,digits = 3)
volwt_OMAl <- round(precip_WY_volwt_conc$volwt_OMAl,digits = 3)
volwt_Al_ICP <- round(precip_WY_volwt_conc$volwt_Al_ICP,digits = 2)
#volwt_NH4 <- round(precip_WY_volwt_conc$volwt_NH4,digits = 3)
volwt_NH4 <- round(precip_WY_volwt_conc$volwt_NH4,digits = 3)
volwt_SO4 <- round(precip_WY_volwt_conc$volwt_SO4,digits = 2)
volwt_NO3 <- round(precip_WY_volwt_conc$volwt_NO3,digits = 2)
volwt_Cl <- round(precip_WY_volwt_conc$volwt_Cl,digits = 2)
volwt_PO4 <- round(precip_WY_volwt_conc$volwt_PO4,digits = 3)
volwt_DOC <- round(precip_WY_volwt_conc$volwt_DOC,digits = 2)
volwt_TDN <- round(precip_WY_volwt_conc$volwt_TDN,digits = 2)
volwt_DON <- round(precip_WY_volwt_conc$volwt_DON,digits = 2)
volwt_SiO2 <- round(precip_WY_volwt_conc$volwt_SiO2,digits = 2)
volwt_Mn <- round(precip_WY_volwt_conc$volwt_Mn,digits = 2)
volwt_Fe <- round(precip_WY_volwt_conc$volwt_Fe,digits = 2)
volwt_F <- round(precip_WY_volwt_conc$volwt_F,digits = 2)
volwt_H <- round(precip_WY_volwt_conc$volwt_H,digits = 4)
volwt_pH <- round(precip_WY_volwt_conc$volwt_pH,digits = 2)
volwt_SpecCond <- round(precip_WY_volwt_conc$volwt_SpecCond,digits = 3)
volwt_TheoryCond <- round(precip_WY_volwt_conc$volwt_TheoryCond,digits = 3)

precip_WY_volwt_conc$volwt_Ca <- volwt_Ca
precip_WY_volwt_conc$volwt_Mg <- volwt_Mg
precip_WY_volwt_conc$volwt_K <- volwt_K
precip_WY_volwt_conc$volwt_Na <- volwt_Na
precip_WY_volwt_conc$volwt_Al_Ferron <- volwt_Al_Ferron
precip_WY_volwt_conc$volwt_TMAl <- volwt_TMAl
precip_WY_volwt_conc$volwt_OMAl <- volwt_OMAl
precip_WY_volwt_conc$volwt_Al_ICP <- volwt_Al_ICP
precip_WY_volwt_conc$volwt_NH4 <- volwt_NH4
precip_WY_volwt_conc$volwt_SO4 <- volwt_SO4
precip_WY_volwt_conc$volwt_NO3 <- volwt_NO3
precip_WY_volwt_conc$volwt_Cl <- volwt_Cl
precip_WY_volwt_conc$volwt_PO4 <- volwt_PO4
precip_WY_volwt_conc$volwt_DOC <- volwt_DOC
precip_WY_volwt_conc$volwt_TDN <- volwt_TDN
precip_WY_volwt_conc$volwt_DON <- volwt_DON
precip_WY_volwt_conc$volwt_SiO2 <- volwt_SiO2
precip_WY_volwt_conc$volwt_Mn <- volwt_Mn
precip_WY_volwt_conc$volwt_Fe <- volwt_Fe
precip_WY_volwt_conc$volwt_F <- volwt_F
precip_WY_volwt_conc$volwt_H <- volwt_H
precip_WY_volwt_conc$volwt_pH <- volwt_pH
precip_WY_volwt_conc$volwt_SpecCond <- volwt_SpecCond
precip_WY_volwt_conc$volwt_TheoryCond <- volwt_TheoryCond

precip_WY_volwt_conc <-
  subset(precip_WY_volwt_conc, select = -c(Ca_flux,Mg_flux,K_flux,Na_flux,
                                           Al_Ferron_flux,TMAl_flux,OMAl_flux,
                                           Al_ICP_flux,NH4_flux,H_flux,SO4_flux,
                                           NO3_flux,Cl_flux,PO4_flux,DOC_flux,
                                           TDN_flux,DON_flux,SiO2_flux,Mn_flux,
                                           Fe_flux,F_flux,SpecCond_flux,
                                           TheoryCond_flux))

precip_WY_volwt_conc <- fix_NA(precip_WY_volwt_conc)

#strips out SpecCond and TheoryCond from the WY flux table dataframe
precip_WY_flux_gHa <- subset(precip_WY_flux_gHa,
                             select = -c(SpecCond_flux,TheoryCond_flux))

#strips out TheoryCond from the WY volume-weighted concentration dataframe
precip_WY_volwt_conc <- subset(precip_WY_volwt_conc,
                               select = -c(volwt_TheoryCond))

out_WY_volwt_Name <-
  paste("results/ws",watershed,"_precip_WY_volwt_conc.csv",sep="")
out_WY_Name <-
  paste("results/ws",watershed,"_precip_WY_flux_gHa.csv",sep="")

precip_WY_flux_gHa$H_flux <- round(precip_WY_flux_gHa$H_flux,digits = 4)

#volwt_s columns
precip_WY_volwt_conc <-
  precip_WY_volwt_conc[c("WaterYear","precip_mm","volwt_Ca","volwt_Mg",
                         "volwt_K","volwt_Na","volwt_Al_Ferron","volwt_TMAl",
                         "volwt_OMAl","volwt_Al_ICP","volwt_NH4","volwt_SO4",
                         "volwt_NO3","volwt_Cl","volwt_PO4","volwt_DOC",
                         "volwt_TDN","volwt_DON","volwt_SiO2","volwt_Mn",
                         "volwt_Fe","volwt_F","volwt_H","volwt_pH",
                         "volwt_SpecCond")]

colnames(precip_WY_volwt_conc) <-
  c("WaterYear","precip_mm","Ca_volwt","Mg_volwt","K_volwt",
    "Na_volwt","Al_Ferron_volwt","TMAl_volwt","OMAl_volwt","Al_ICP_volwt",
    "NH4_volwt","SO4_volwt","NO3_volwt","Cl_volwt","PO4_volwt","DOC_volwt",
    "TDN_volwt","DON_volwt","SiO2_volwt","Mn_volwt","Fe_volwt",
    "F_volwt","H_volwt","pH_volwt","SpecCond_volwt")

#adds pH, ANC, and specific conductance to flux file
precip_WY_flux_gHa <- merge(x = precip_WY_flux_gHa, y = precip_WY_volwt_conc[ , c("WaterYear","pH_volwt","SpecCond_volwt")], by = "WaterYear")

#reorders column names
precip_WY_flux_gHa <-
  precip_WY_flux_gHa[c("WaterYear","precip_mm","Ca_flux","Mg_flux","K_flux","Na_flux",
                            "Al_Ferron_flux","TMAl_flux","OMAl_flux",
                            "Al_ICP_flux","NH4_flux","SO4_flux",
                            "NO3_flux","Cl_flux","PO4_flux","DOC_flux",
                            "TDN_flux","DON_flux","SiO2_flux","Mn_flux",
                            "Fe_flux","F_flux","H_flux","pH_volwt",
                            "SpecCond_volwt")]

head(precip_WY_volwt_conc,5)
head(precip_WY_flux_gHa,5)

write.csv(precip_WY_flux_gHa, file = out_WY_Name,row.names=FALSE)
write.csv(precip_WY_volwt_conc, file = out_WY_volwt_Name,row.names = FALSE)

# counts the number of days per year as a check for duplicates
# in the chemistry or stream file
WY_day_count <- count(precip_daily_flux_gHa_new, 'WY')
print(WY_day_count)

# delete the precip file that was generated
file.remove(paste0("ws",watershed,"_precip.csv"))

#}
