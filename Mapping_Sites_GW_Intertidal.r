###########################################################
##### Mapping for GoA Data        June 2015           #####
#####  Modified from Jessica Couture's script by REB  #####
###########################################################

# Nearshore Intertidal Sites - Gulf Watch
setwd("C:/Users/rblake/Documents/NCEAS/GoA Dynamics WG/GW_Nearshore Intertidal Data")


sti <- read.csv('GW_Nearshore_Sites.csv',header=T,stringsAsFactors=T) ## 
str(sti)


library(dplyr)
Locs <- sti %>%
    mutate(Site='Sitename') %>%
    mutate(lat=Longitude) %>%
    mutate(lon=Latitude) %>%
    select(lat,lon,Site)

# PWS Oceanic Drivers - Gulf Watch
setwd("C:/Users/rblake/Desktop ")

sti2 <- read.csv('Copy of LTM2013-01.csv',header=T)
head(sti2)

#library(dplyr)
Locs2 <- sti2 %>%
    mutate(Site='Station') %>%
    mutate(lat=Lat.) %>%
    mutate(lon=Long.) %>%
    select(lat,lon,Site)


### Plotting ###
library(rworldmap)
library(rworldxtra)
library(rgdal)
library(ggplot2)

  
world=getMap('low',projection=NA)
worldB=world[!is.na(world$continent),]
world2=worldB[worldB$continent=='North America' & worldB$LON<0,]
fWorld=fortify(world2)

### ZOOOOOM in:
setwd("C:/Users/rblake/Documents/NCEAS/GoA Portfolio Effects WG/Hydrocarbon data/")

state <- readOGR(dsn="GIS",layer="statep010")
stateDf <- fortify(state)

# The palette:
colMap <- c("dimgrey","black")

colors <- c("blue3","turquoise2","deepskyblue","royalblue1","violet","thistle1",
            "darkseagreen","springgreen2","greenyellow","olivedrab3",
            "coral","tomato3","orangered4","rosybrown1","hotpink1",
            "yellow","goldenrod1","tan2")

# The plot:
 ggplot(data=stateDf, aes(y=lat, x=lon)) +
   geom_map(map=stateDf,aes(x=long,y=lat,map_id=id)) +
   coord_map(xlim = c(-150, -144),ylim = c(59, 62)) + 
   scale_fill_manual(values=colMap) +
   geom_point(data=sti, aes(x=as.numeric(Longitude), y=as.numeric(Latitude),
                            colour=Sitename), size=5, shape=18, colour="blue") + 
   geom_text(data=sti, hjust=0.5, vjust=-0.5, aes(x=Longitude, y=Latitude, label=Sitename), 
             colour="gold2", size=4 ) +
   geom_point(data=sti2, aes(x=as.numeric(Long.), y=as.numeric(Lat.),
                            colour=Station), size=5, shape=20, colour="red") + 
   geom_text(data=sti2, hjust=0.5, vjust=-0.5, aes(x=Long., y=Lat., label=Station), 
             colour="gold2", size=4 ) +  
   scale_colour_manual(values=c('Station'='red', 'Sitename'='blue'), guide='legend') +
   #ggtitle('Gulf Watch PWS Intertidal Stations') +
   #ggtitle('Gulf Watch PWS Oceanic Drivers Stations') +
   theme(axis.line=element_line('black'),
         panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(),
         panel.border=element_blank(),
         panel.background=element_blank(),
         legend.key = element_blank(),
         axis.text=element_text(size=14),
         title=element_text(size=16,face="bold")) 
  
#################################
### Sea Otters from NPPSDB    ###
#################################

sti <- SOTT ## SOTT comes from the Data Manip script, and is the extracted Sea Otter observations.
str(sti)

library(dplyr)
Locs <- sti %>%
    mutate(Site='Master.Key') %>%
    mutate(lat=Lat) %>%
    mutate(lon=Lon) %>%
    select(lat,lon,Site)

### Plotting ###
library(rworldmap)
library(rworldxtra)
library(rgdal)
library(ggplot2)

setwd("C:/Users/rblake/Documents/NCEAS/GoA Portfolio Effects WG/Hydrocarbon data/")

state <- readOGR(dsn="GIS",layer="statep010")
stateDf <- fortify(state)
  
# The palette:
colMap <- c("dimgrey","black")

colors <- c("blue3","turquoise2","deepskyblue","royalblue1","violet","thistle1",
            "darkseagreen","springgreen2","greenyellow","olivedrab3",
            "coral","tomato3","orangered4","rosybrown1","hotpink1",
            "yellow","goldenrod1","tan2")

# The plot:
 ggplot(data=stateDf, aes(y=lat, x=long)) +
   geom_map(map=stateDf,aes(x=long,y=lat,map_id=id)) +
   coord_map(xlim = c(-150, -144),ylim = c(59, 62)) + 
   scale_fill_manual(values=colMap) +
   geom_point(data=sti, aes(x=as.numeric(Lon), y=as.numeric(Lat),
                            colour=Master.Key), size=5, shape=18, colour="blue") + 
   #geom_text(data=sti, hjust=0.5, vjust=-0.5, aes(x=Lon, y=Lat, label=Master.Key), 
   #          colour="gold2", size=4 ) +  
   #scale_colour_manual(values=c('Master.Key'='blue'), guide='legend') +
   theme(axis.line=element_line('black'),
         panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(),
         panel.border=element_blank(),
         panel.background=element_blank(),
         legend.key = element_blank(),
         axis.text=element_text(size=14),
         title=element_text(size=16,face="bold")) 
  



