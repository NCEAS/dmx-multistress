#########################################################################
##### GOA Dynamics Working Group                                    #####
##### Benthic Nearshore Group - Data Assembly script                #####
##### Created by Rachael Blake on Sept. 21, 2015                    #####
#########################################################################

## load packages (order matters)
library(httr)
library(plyr)
library(dplyr)
library(XML)
library(curl)
library(rvest)
library(tidyr)
library(stringr)

# create empty data frame
BenNear <- data.frame('Year'=c(1975:2015))

# merge in data columns generated by data cleaning scripts
BenNear <- merge(BenNear,ENSO_annual,all.x=T)    # ENSO annual
BenNear <- merge(BenNear,pdo_annual,all.x=T)     # PDO annual
BenNear <- merge(BenNear,npgo_annual,all.x=T)    # NPGO annual
BenNear <- merge(BenNear,upanom,all.x=T)         # Upwelling anomalies annual
BenNear <- merge(BenNear,Phy,all.x=T)            # Phytoplankton - Seward Line, spring
BenNear <- merge(BenNear,SatChl_df,all.x=T)      # Chla - Satellite annual
BenNear <- merge(BenNear,SST,all.x=T)            # SST - Seward Line
  


###############################################################################################
###  Multivariate ENSO Index (MEI): 
URL_enso <- "http://www.esrl.noaa.gov/psd/enso/mei/table.html"
enso_pre <- xpathSApply(content(GET(URL_enso)),"/html/body/pre", xmlValue)
enso_cols <- scan(textConnection(enso_pre), skip=10, nlines=1, what=character()) # get header row
enso <- read.csv(file=textConnection(enso_pre), skip=11, stringsAsFactors=F, sep="\t", 
                 header=FALSE, col.names=enso_cols)
enso_df <- enso[1:66,]  # removes the text at bottom of file
#
ENSO_annual <- enso_df %>%
                    rename(Year=YEAR) %>% # rename data columns
                    filter(Year %in% c(1975:2015)) %>% # selects years 1975 - 2015
                    gather(Months, ENSO, -Year) %>% # reshapes data to be column-wise
                    filter(!is.na(ENSO)) %>% # remove NA values
                    group_by(Year) %>%
                    summarise(ENSO_anul_mn=mean(ENSO)) %>% # get annual means
                    ungroup()  # 


##############################################################################################
###  Pacific Decadal Oscillation Index (PDO): 
URL_pdo <- "http://jisao.washington.edu/pdo/PDO.latest"
pdo_raw <- html(URL_pdo)
pdo_pre <- pdo_raw %>% 
               html_node("p") %>%
               html_text()
pdo_cols <- scan(textConnection(pdo_pre), skip=29, nlines=1, what=character())# Get header row
pdo_df <- read.table(file=textConnection(pdo_pre), skip=30, nrows=116, stringsAsFactors=F, sep="", 
                  header=FALSE, col.names=pdo_cols, strip.white=TRUE, fill=TRUE)
pdo_df$YEAR <- substr(pdo_df$YEAR, 1, 4)  # removes asterisks from years 2002-2015

pdo_annual <- pdo_df %>% 
              rename(Year=YEAR) %>% # rename data columns         
              filter(Year %in% c(1975:2015)) %>% # selects years 1975 - 2015
              gather(Month, PDO, -Year) %>% # reshapes data to be column-wise
              group_by(Year) %>%
              summarise(PDO_anul_mn=mean(as.numeric(as.character(PDO)), na.rm = TRUE)) %>% # get annual means
              ungroup() 

###############################################################################################
### North Pacific Gyre Oscillation Index (NPGO): 
URL_npgo <- "http://www.o3d.org/npgo/npgo.php"
npgo_pre <- xpathSApply(content(GET(URL_npgo)),"/html/body/pre", xmlValue)
npgo_cols <- scan(textConnection(npgo_pre), skip=25, nlines=1, what=character())# Get header row
npgo_cols <- npgo_cols[2:4] # select column names
npgo_df <- read.csv(file=textConnection(npgo_pre), skip=26, stringsAsFactors=F, sep="", 
                 header=FALSE, col.names=npgo_cols, strip.white=TRUE)

npgo_annual <- npgo_df %>% 
               rename(Year=YEAR) %>% # rename data columns         
               filter(Year %in% c(1975:2015)) %>% # selects years 1975 - 2015
               group_by(Year) %>%
               summarise(NPGO_anul_mn=mean(NPGO)) %>% # get annual means
               ungroup()  # 

##############################################################################################
### Upwelling Anomalies: 
URL_upanom <- "http://www.pfeg.noaa.gov/products/PFELData/upwell/monthly/upanoms.mon"
upanom_raw <- html(URL_upanom)
upanom_pre <- upanom_raw %>% 
              html_node("p") %>%
              html_text()
upanom_cols <- scan(textConnection(upanom_pre), skip=2, nlines=1, what=character())# Get header row
upanom_cols <- c("Lat", "Long", upanom_cols[-1])# split position into lat and long 
upanom_df <- read.csv(file=textConnection(upanom_pre), skip=4, stringsAsFactors=F, sep="", 
                   header=FALSE, col.names=upanom_cols, strip.white=TRUE)
#
upanom <- upanom_df %>% 
          rename(Year=YEAR) %>% # rename data columns   
          filter(Year %in% c(1975:2015)) %>% # selects years 1975 - 2015
          gather(Month, UpwelAnom,-Year,-Lat,-Long) %>% # reshapes data to be column-wise
          group_by(Year) %>%
          summarise(UpWelAnom_anul_mn=mean(UpwelAnom, na.rm = TRUE)) %>% # get annual means
          ungroup() 

###############################################################################################
### Phytoplankton (annual spring mean): (from Seward Line dataset)
# Get 1998-2010 data 
URL_Chl <- "http://gulfwatch.nceas.ucsb.edu/goa/d1/mn/v1/object/df35b.41.3"
ChlGet <- GET(URL_Chl)
Chl1 <- content(ChlGet, as='text')
Chl_df <- read.csv(file=textConnection(Chl1),stringsAsFactors=FALSE)
head(Chl_df)
#################
### NTOE: Have Jessica correct the dates for 2007 (swapped Month and Day)
### in the data sheet on the portal.  
#################
#
Phy <- Chl_df %>%
       arrange(dateTime) %>%     
       mutate(Year=substring(dateTime,1,4),
              Month=substring(dateTime,6,7)) %>%  
       filter(Month %in% c("05")) %>%  # selects just the May samples for all years
       group_by(Year) %>%
       summarise(ChlA_micgL_AnnSpMn=mean(chloropyllA),
                 TotChl_micgL_AnnSpMn=mean(totalChl)) %>% # get annual means
       ungroup() %>%
       mutate(TotChlA_micgL_AnnSpMn=rowSums(.[2:3],na.rm=T)) %>%
       select(Year,TotChlA_micgL_AnnSpMn)

###############################################################################################
# Mean annual Chl a anomalies (mg/m3) for Gulf of Alaska
# From Waite & Mueter 2013, Fig 11 Annual
# Waite, J.N. and Mueter, F.J. 2013. Spatial and temporal variability of chlorophyll-a concentrations 
# in the coastal Gulf of Alaska, 1998-2011, using cloud-free reconstructions of SeaWiFS and MODIS-Aqua data.
# Prog. Oceanogr. 116, 179-192.
#
URL_SatChl <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7uRHdOTGQtSVBQOE0"
SatChlGet <- GET(URL_SatChl)
SatChl1 <- content(SatChlGet, as='text')
SatChl_df <- read.csv(file=textConnection(SatChl1),stringsAsFactors=FALSE)
head(SatChl_df)

################################################################################################
###  Water Temperature (SST): 
URL_T <- "http://gulfwatch.nceas.ucsb.edu/goa/d1/mn/v1/object/df35b.31.1"
TGet <- GET(URL_T)
T1 <- content(TGet, as='text')
Tmps <- read.csv(file=textConnection(T1),stringsAsFactors=FALSE,strip.white=TRUE)
head(Tmps)

URL_Ts <- "http://gulfwatch.nceas.ucsb.edu/goa/d1/mn/v1/object/df35b.32.2"
TsGet <- GET(URL_Ts)
Ts1 <- content(TsGet, as='text')
TmpSams <- read.csv(file=textConnection(Ts1),stringsAsFactors=FALSE,strip.white=TRUE)
head(TmpSams)
#
Temps <- merge(Tmps, TmpSams, all.x=TRUE)  # merge sample information with data values
Temps$Date <- sapply((strsplit(as.character(Temps$dateTime), split=" ")), function(x) x[1]) # split date out
head(Temps)

############################################
### NOTE : Need to deal with missing sample info for cruiseID TXS09, consecStationNum 3
############################################
# missing_date <- filter(Temps, is.na(dateTime))  # selects data with missing dates
# miss_cID <- unique(missing_date$cruiseID) # selects the cruise IDs for which sample info is missing


SST <- Temps %>%
             mutate(Year=sapply((strsplit(as.character(Date), split="/")), 
                                function(x) x[3])) %>%   # creates Year column
             arrange(dateTime) %>%
             rename(WTemp_C=temp) %>%
             group_by(Year) %>%
             summarise(WTemp_C_AnnMn=mean(WTemp_C)) %>% # get annual means
             ungroup() %>%
             select(Year, WTemp_C_AnnMn)  # selects columns wanted


##############################################################################################









