#################################################################
##### Gulf Watch Intertidal Data Assembly and Manipulation  #####
#####   Script by Rachael Blake,  June 2015                 #####
#####                                                       #####
#################################################################

setwd("C:/Users/rblake/Documents/NCEAS/GoA Dynamics WG/GW_Nearshore Intertidal Data")

# Structured blank data sheet
blnk <- read.csv('GW_Nearshore_Data.csv',header=T) ## 
head(blnk)
str(blnk)

# Creating vector of Sitenames to use later
PWS_Sites <- unique(blnk$Site_Name)
PWS_Sites_Codes <- unique(blnk[,2:3]) ; names(PWS_Sites_Codes)[names(PWS_Sites_Codes)=="SiteID"] <- "Site_Code"

# load necessary packages
library(httr)
library(tidyr)
library(plyr)
library(dplyr)

###################################
# Get data from Gulf Watch portal #
###################################
# Mussels - Getting 2008-2012 data from Gulf of Alaska AOOS Axiom portal
URL_M <- "https://workspace.aoos.org/published/file/52265bf0e4b0f364fbbb22b5/NearshoreBenthicSystemsInGOA_BenthicMusselData_2008_2012.csv"
MGet <- GET(URL_M)
Mus1 <- content(MGet, as='text')
Mus <- read.csv(file=textConnection(Mus1), header=T)
head(Mus) 
# Cleaning up data
Mus$Area.sampled..m2. <- as.numeric(as.character(Mus$Area.sampled..m2.))  # convert from factor to numeric
Mus$Size..mm. <- as.numeric(as.character(Mus$Size..mm.))                  # convert from factor to numeric
head(Mus) ; str(Mus)

Mus_12_PWS <- Mus %>%
           filter(Sitename %in% PWS_Sites) %>%  # extract PWS samples
           select(-X, -X.1) %>%                # remove blank columns
           filter(!Area.sampled..m2. == ".") %>%  # remove invisible "." 
           filter(!Size..mm. == ".") %>%          # remove invisible "." 
           rename(Sample_Year=Sampling.Date, Quad_Num=Quad, AreaSampled_m2=Area.sampled..m2.,
                  Size_mm=Size..mm., Site_Name=Sitename)  %>%
           arrange(Site_Name, Sample_Year, Quad_Num)
head(Mus_12_PWS)

# 2014 Mussel Data
URL_M2 <- "https://workspace.aoos.org/published/file/ed831409-9b1d-4292-b943-01075ca35ae6/NearshoreBenthicSystemsInGOA_SOP09_Mussel_2014MusselsGreaterThan20mm_Data_20150108.csv"
M2Get <- GET(URL_M2)
Mus2c <- content(M2Get, as='text')
Mus2 <- read.csv(file=textConnection(Mus2c))
head(Mus2) ; str(Mus2)
# Cleaning mussel numbers
Mus2a <- Mus2 %>%
         filter(Region =="PWS") %>% 
         rename(Size_mm=size, vert_tx_num=vert.tx..)  %>%
         mutate(Site_Name = ifelse((Site=="Galena"),'Galena Bay',  # must rename Sites to be uniform before anything can be done
                            ifelse((Site=="Fidalgo"),'Port Fidalgo',
                            ifelse((Site=="Olsen"),'Olsen Bay',
                            ifelse((Site=="Simpson"),'Simpson Bay',
                            ifelse((Site=="Observation"),'Observation Island',
                            ifelse((Site=="Hogan"),'Hogan Bay',
                            ifelse((Site=="Iktua"),'Iktua Bay',
                            ifelse((Site=="Whale"),'Whale Bay',
                            ifelse((Site=="Johnson"),'Johnson Bay',
                            ifelse((Site=="Herring"),'Herring Bay',""))))))))))
                     )  
#
URL_Sinfo <- "https://workspace.aoos.org/published/file/76f67c6c-a84b-4110-9db3-b7c45e4be754/NearshoreBenthicSystemsInGOA_SOP09_2014MusselSiteLayout_Data_20150108.csv"
SinfoGet <- GET(URL_Sinfo)
Sinfo1 <- content(SinfoGet, as='text')
Sinfo <- read.csv(file=textConnection(Sinfo1))
head(Sinfo) ; str(Sinfo)
# Cleaning Site info
Sinfo2 <- Sinfo %>%
          filter(Region =="PWS") %>% 
          rename(AreaSampled_m2=area.of.quad..m2., vert_tx_num=vertical.transect..)  %>%
          mutate(Site_Name = ifelse((Site=="Galena"),'Galena Bay',  # must rename Sites to be uniform before anything can be done
                             ifelse((Site=="Fidalgo"),'Port Fidalgo',
                             ifelse((Site=="Olsen"),'Olsen Bay',
                             ifelse((Site=="Simpson"),'Simpson Bay',
                             ifelse((Site=="Observation"),'Observation Island',
                             ifelse((Site=="Hogan Bay"),'Hogan Bay',
                             ifelse((Site=="Iktua"),'Iktua Bay',
                             ifelse((Site=="Whale Bay"),'Whale Bay',
                             ifelse((Site=="Johnson Bay"),'Johnson Bay',
                             ifelse((Site=="Herring Bay"),'Herring Bay',""))))))))))
                      )
#
# put these two datasheets together
Mus14 <- merge(Mus2a[,-c(3,4)], Sinfo2[,c(1:2,5,10,18,22)], 
               by=c("Region","Block","Site_Name","Sampling.Date","vert_tx_num"), all=TRUE)
head(Mus14)
# Cleaning combined datasheets
MYMD <- strsplit(as.character(Mus14$Sampling.Date), split="/") # split the Sample_Date column to extract year
Mus14$Sample_Year <- sapply(MYMD, function(x) x[3]) # create Sample Year column
Mus14_2 <- Mus14 %>%
           filter(!vert_tx_num == ".")  # remove invisible "." 
Mus14_PWS <- Mus14_2[,-c(4)]
head(Mus14_PWS)
###
### add rows from 2014 to dataframe for 2010-2012
names(Mus14_PWS)[names(Mus14_PWS)=="vert_tx_num"] <- "Quad_Num"# need to rename "vert_tx_num" to match "Quad_Num" column
Mus_PWS <- rbind(Mus_12_PWS[,-c(3)], Mus14_PWS)
Mus_PWS$Sample_Year <- as.numeric(Mus_PWS$Sample_Year)  # convert numbers from characters to numbers
Mus_PWS$Quad_Num <- as.numeric(Mus_PWS$Quad_Num)
Mus_PWS$AreaSampled_m2 <- as.numeric(Mus_PWS$AreaSampled_m2)
Mus_PWS$Size_mm <- as.numeric(Mus_PWS$Size_mm)
head(Mus_PWS) ; tail(Mus_PWS)

# Standardize to number per m2
Mus_PWS_s <- Mus_PWS %>%
             count(Site_Name, Sample_Year, Quad_Num, AreaSampled_m2) %>%
             mutate(n_m2=n/AreaSampled_m2) %>%
             group_by(Site_Name, Sample_Year) %>%  # group by sites and years
             summarize(Mus_Mn_n_m2=mean(n_m2))    # take the mean size
head(Mus_PWS_s)
# Averaging per site per year
Mus_PWS_a <- Mus_PWS %>%      
             group_by(Site_Name, Sample_Year) %>%  # group by sites and years
             summarize(Mus_Mn_Size_mm=mean(Size_mm))  # take the mean size
head(Mus_PWS_a)

#write.csv(Mus_PWS, "C:/Users/rblake/Desktop/Mus_PWS.csv", row.names=F)
###############################
# Limpets - Abundance
URL_Ld <- "https://workspace.aoos.org/published/file/5204fccde4b067e4402e6d00/Rocky_Limpet_Density_RawData_2010thru_2012_29Jul2013.csv"
LdGet <- GET(URL_Ld)
Limd1 <- content(LdGet, as='text')
Limd <- read.csv(file=textConnection(Limd1))
head(Limd)
# Cleaning up Data - abundance/density
Limd_PWS <- Limd %>% 
            filter(Site_Name %in% PWS_Sites) %>% #extract PWS samples
            rename(Limpets_n_m2=Density..individuals.per.sq.m., Sample_Day=Sample_Date,
                   Num_Limp=Number_Limpets)  %>%  # rename columns
            arrange(Site_Name, Sample_Year, Rep_Number) 
head(Limd_PWS)
# Averaging per site per year
Limd_PWS_a <- Limd_PWS %>%
              group_by(Site_Name, Sample_Year) %>%
              summarise(Limp_Mn_n_m2=mean(Limpets_n_m2)) %>%
              ungroup() 
head(Limd_PWS_a)

### Limpets - Size
URL_Ls <- "https://workspace.aoos.org/published/file/5204fce2e4b067e4402e6d03/Rocky_Limpet_Size_RawData_2006thru_2012_29Jul2013.csv"
LsGet <- GET(URL_Ls)
Lims1 <- content(LsGet, as='text')
Lims <- read.csv(file=textConnection(Lims1))
head(Lims)
# Cleaning, filtering, and summarizing data - size
Lims_PWS <- Lims %>%
            filter(Site_Name %in% PWS_Sites) %>%  # extract PWS samples
            group_by(Site_Name, Sample_Year) %>%
            summarise(Limp_MnSize_m2=mean(Limpet_Size_mm)) %>% # averaging per site per year
            ungroup() %>%
            filter(Sample_Year != 2007)  %>% # taking out year 2007 right now
            arrange(Site_Name, Sample_Year)
head(Lims_PWS)

####################################
# Whelks & Chitons
URL_NK <- "https://workspace.aoos.org/published/file/52065f52e4b0f364fbbb186e/NearshoreBenthicSystemsinGOA_RockyNucellaKatharinaRawData_2006thru2013.csv"
NKGet <- GET(URL_NK)
WCH1 <- content(NKGet, as='text')
WCH <- read.csv(file=textConnection(WCH1))
head(WCH)
# Cleaning, filtering, - WHELKS ONLY
Wlk_PWS <- WCH %>%
           filter(Site_Name %in% PWS_Sites) %>%  # extract PWS samples
           mutate(Species_Name = revalue(Species_Name, c("Nucella sp."="Nucella sp"))) %>% # remove "." from Nucella
           filter(Species_Name == "Nucella sp") %>% # extract only the Nucella rows
           filter(Sample_Year %in% c(2010, 2011, 2012, 2013, 2014))  %>% # taking out years before 2010
           rename(Quad_Size_m = Quadrat_Size..m.)  %>%  # rename columns
           mutate(Whelk_n_m2 = Density..individuals.per.2.square.m./2) %>% # getting n per m2
           group_by(Site_Name, Sample_Year) %>%
           summarise(Whelk_Mean_n_m2=mean(Whelk_n_m2)) %>%
           ungroup() %>%
           arrange(Site_Name, Sample_Year)

Wlk_PWS
##
# Cleaning, filtering, - CHITONS ONLY
Chi_PWS <- WCH %>%
           filter(Site_Name %in% PWS_Sites) %>%  # extract PWS samples
           filter(Species_Name == "Katharina tunicata") %>% # extract only the Katharina rows
           filter(Sample_Year %in% c(2010, 2011, 2012, 2013, 2014))  %>% # taking out years before 2010
           rename(Quad_Size_m = Quadrat_Size..m.)  %>%  # rename columns
           mutate(Chiton_n_m2 = Density..individuals.per.2.square.m./2) %>% # getting n per m2
           group_by(Site_Name, Sample_Year) %>%
           summarise(Chiton_Mean_n_m2=mean(Chiton_n_m2)) %>%
           ungroup() %>%
           arrange(Site_Name, Sample_Year)
Chi_PWS

########################################
# Sea Stars
URL_SS <-"https://workspace.aoos.org/published/file/5204f963e4b067e4402e6ccd/BenthicNearshoreSystemsInGOA__SeaStars_Data_2006_2012.csv"
SSGet <- GET(URL_SS)
SS1 <- content(SSGet, as='text')
SS <- read.csv(file=textConnection(SS1))
head(SS)
# Cleaning, filtering, etc
SS_PWS <- SS %>%
          filter(Site_Name %in% PWS_Sites) %>%  # extract PWS samples
          filter(Sample_Year %in% c(2010, 2011, 2012, 2013, 2014))  %>% # taking out years before 2010
          mutate(SS_n_m2 = Density..individuals_per_100_sq_m./100) %>% # getting n per m2
          # STOP here if you want species-level data
          group_by(Site_Name, Sample_Year) %>%
          summarise(SS_Mean_n_m2=mean(SS_n_m2)) %>%
          ungroup() %>%
          arrange(Site_Name, Sample_Year)
SS_PWS

##########################################
# Oyster Catchers
URL_OysC <- "https://workspace.aoos.org/published/file/dfa87109-392b-4da4-b083-42f96e27a2ea/NearshoreBenthicSystemsInGOA_SOP07_BLOY_2014NestDensity_Data_20150105.csv"
OysCGet <- GET(URL_OysC)
OysC1 <- content(OysCGet, as='text')
OysC <- read.csv(file=textConnection(OysC1))
head(OysC)
# Cleaning, filtering, etc. 
OysC[OysC == "."] <- NA   # replace "." with NA in the entire data frame
MDY <- strsplit(as.character(OysC$DATE), split="/") # split the Sample_Date column to extract year
OysC$Sample_Year <- sapply(MDY, function(x) x[3]) # create Sample Year column
OysC$Sample_Year[OysC$Sample_Year == "2014 - 6"] <- "2014" # replace weird first date "2014 - 6" with "2014"


OyC_PWS <- OysC %>%
           select(-X) %>%  # remove weird blank column
           filter(REGION == "PWS") %>% # extract PWS samples
           rename(Nest_Site=NEST_SITE.., Adults_Num=X._ADULTS, Eggs_Num=X._EGGS, Chicks_Num=X._CHICKS,
                  Prey_Collected=PREY_COLL.) %>%
           mutate(Site_Code = ifelse((SITE=="RI-01"),'PWS_B08_RI_01',
                              ifelse((SITE=="RI-02"),'PWS_B08_RI_02',
                              ifelse((SITE=="RI-03"),'PWS_B08_RI_03',
                              ifelse((SITE=="RI-04"),'PWS_B08_RI_04',
                              ifelse((SITE=="RI-05"),'PWS_B08_RI_05',"")))))
                  ) %>%
           inner_join(PWS_Sites_Codes, by="Site_Code") %>%
           group_by(Site_Name, Sample_Year) %>%
           summarise(Adult_Mean_n=mean(Adults_Num), Egg_Mean_n=mean(Eggs_Num), 
                     Chick_Mean_n=mean(Chicks_Num)) %>%
           ungroup() %>%
           arrange(Site_Name, Sample_Year)
head(OyC_PWS)

##########################################
# Eelgrass
ElG <- read.csv("C:\\Users\\rblake\\Documents\\NCEAS\\GoA Dynamics WG\\GW_Nearshore Intertidal Data\\Eelgrass pct cover summary 2010 thru 2014.csv")
head(ElG)

##########################################
# Inverts & Algae Percent Cover
URL_IA <- "https://workspace.aoos.org/published/file/744b4b95-e596-41cc-9419-881165da2864/BenthicNearshoreSystemsInGOA_SOP04_RockyCover_2006to2014_Data_20141015.csv"
IAGet <- GET(URL_IA)
IA1 <- content(IAGet, as='text')
IA <- read.csv(file=textConnection(IA1))
head(IA)
# Cleaning
# define common categories of species for larger aggregation of data
anemone <-  c("Anthopleura elegantissima","Anthopleura xanthogrammica","Epiactis sp.",
              "Metridium senile","unidentified anemone","Urticina crassicornis")
barnacle <- c("Balanus / Semibalanus sp.","Balanus glandula","barnacle","barnacle spat",
              "Chthamalus dalli","Semibalanus balanoides","Semibalanus cariosus")
brown_alga <- c("Alaria marginata","Analipus japonicus","Chordaria flagelliformis",
                "Coilodesme bulligera","Desmarestia aculeata","Dictyosiphon foeniculaceus",
                "Ectocarpus sp.","Elachista fucicola","Elachista sp.","Eudesme virescens",
                "Fucus distichus","Leathesia marina","Melanosiphon / Scytosiphon sp.",
                "Melanosiphon intestinalis","Petalonia fascia","Ralfsia fungiformis",
                "Ralfsia sp.","Saccharina latissima","Saccharina sessilis",
                "Scytosiphon lomentaria","Soranthera ulvoidea","unidentified brown algae")
bryazoan <- c("encrusting bryozoan","Eurystomella bilabiata","foliose bryozoan",
              "Stomachetosella cruenta")
chiton <- c("Cryptochiton stelleri")
clam <- c("Hiatella arctica","Mya truncata")
coralline_alga <- c("Corallina sp.","encrusting coralline algae","foliose coralline algae")
encrusting_red_alga <- c("Hildenbrandia sp.","non-coralline algal crust")
filamentous_brown <- c("Pylaiella littoralis")
filamentous_green <- c("Chaetomorpha melagonium","Chaetomorpha sp.","Cladophora / Chaetomorpha sp.",
                       "Ulothrix flacca")
green_alga <- c("Acrosiphonia sp.","Blidingia minima var. minima","Ulva / Monostroma sp.",
                "Ulva sp.","unidentified green algae")
hydroid <- c("unidentified hydroid")
jingle_shell <- c("Pododesmus macroschisma")
mussel <- c("Modiolus modiolus","Musculus sp","Mytilus trossulus")
red_alga <- c("Ahnfeltia fastigiata","Antithamnionella pacifica",
              "Boreophyllum / Pyropia / Wildemania sp.","Callithamnion pikeanum",
              "Ceramium pacificum","Constantinea subulifera","Cryptopleura ruprechtiana",
              "Cryptosiphonia woodii","Dumontia alaskana","Endocladia muricata",
              "Gloiopeltis furcata","Gracilaria pacifica","Halosaccion glandiforme",
              "Mastocarpus sp.","Mazzaella parksii","Mazzaella phyllocarpa","Mazzaella sp.",
              "Microcladia borealis","Nemalion elminthoides","Neoptilota / Ptilota sp.",
              "Neorhodomela larix","Neorhodomela oregona","Odonthalia / Neorhodomela sp.",
              "Odonthalia floccosa","Palmaria callophylloides","Palmaria hecatensis",
              "Palmaria hecatensis/mollis","Palmaria mollis","Palmaria sp.",
              "Phycodrys / Tokidadendron sp.","Phycodrys fimbriata","Pleonosporium vancouverianum",
              "Plocamium pacificum","Polysiphonia sp.","Pterosiphonia / Polysiphonia sp.",
              "Pterosiphonia bipinnata","Ptilota sp.","Rhodochorton purpureum",
              "Tokidadendron bullatum","unidentified filamentous red algae")
sponge <- c("unidentified sponge")
tunicate <- c("unidentified tunicate")
worm <- c("spirorbidae","unidentified worm")

IA_PWS <- IA %>% 
          filter(Site_Name %in% PWS_Sites) %>%  # extract PWS samples
          filter(Sample_Year %in% c(2010, 2011, 2012, 2013, 2014)) %>% # taking out years before 2010
          mutate(Common_Cat = ifelse((Species_Name %in% anemone),'anemone',
                              ifelse((Species_Name %in% barnacle),'barnacle',
                              ifelse((Species_Name %in% brown_alga),'brown_alga',
                              ifelse((Species_Name %in% bryazoan),'bryazoan',
                              ifelse((Species_Name %in% chiton),'chiton',
                              ifelse((Species_Name %in% clam),'clam',
                              ifelse((Species_Name %in% coralline_alga),'coralline_alga',
                              ifelse((Species_Name %in% encrusting_red_alga),'encrusting_red_alga',
                              ifelse((Species_Name %in% filamentous_brown),'filamentous_brown',
                              ifelse((Species_Name %in% filamentous_green),'filamentous_green',
                              ifelse((Species_Name %in% green_alga),'green_alga',
                              ifelse((Species_Name %in% hydroid),'hydroid',
                              ifelse((Species_Name %in% jingle_shell),'jingle_shell',
                              ifelse((Species_Name %in% mussel),'mussel',
                              ifelse((Species_Name %in% red_alga),'red_alga',
                              ifelse((Species_Name %in% sponge),'sponge',
                              ifelse((Species_Name %in% tunicate),'tunicate',
                              ifelse((Species_Name %in% worm),'worm',""))))))))))))))))))
                 )     # add new column with common category
head(IA_PWS) ; IA_PWS[45:90,]

#####
##### 
##### 
# FUNCTION for getting Percent Cover for all intertidal inverts and algae
PerCovCalc <- function(df, new_column_name) { 
              df %>%
              count(Site_Code, Site_Name, Sample_Year, Elevation_Position, Quadrat_Num) %>%
              mutate(Per_Cov = (n/25)*100) %>%   # calculates the percent cover per quadrat
              group_by(Site_Name, Sample_Year) %>%
              summarise_(.dots = setNames(list(~mean(Per_Cov)), new_column_name)) %>%
              ungroup()
              }
#####
#####
#####

# Bare Substrate 
# Filter out the species/entry of interest
BS_IA_PWS1 <- filter(IA_PWS, Species_Name=="Bare Substrate", Layer_Num=="1") # only in layer 1 according to Tom Dean
# call the function
BS_IA_PWS <- PerCovCalc(BS_IA_PWS1, "Bare_Sub_Mn_Per_Cov")    ;   BS_IA_PWS

###########  Common Category level analysis
#####
# Anemone
a_IA_PWS1 <- filter(IA_PWS, Common_Cat=="anemone")   # Filter out the species/entry of interest
a_IA_PWS <- PerCovCalc(a_IA_PWS1, "anemone_Mn_Per_Cov") ; a_IA_PWS  # call the function
#####
# Barnacle
b_IA_PWS1 <- filter(IA_PWS, Common_Cat=="barnacle")   # Filter out the species/entry of interest
b_IA_PWS <- PerCovCalc(b_IA_PWS1, "barnacle_Mn_Per_Cov") ; b_IA_PWS  # call the function
#####
# Brown Alga
ba_IA_PWS1 <- filter(IA_PWS, Common_Cat=="brown_alga")   # Filter out the species/entry of interest
ba_IA_PWS <- PerCovCalc(ba_IA_PWS1, "brwn_algae_Mn_Per_Cov") ; ba_IA_PWS  # call the function
#####
# Bryazoan
bz_IA_PWS1 <- filter(IA_PWS, Common_Cat=="bryazoan")   # Filter out the species/entry of interest
bz_IA_PWS <- PerCovCalc(bz_IA_PWS1, "bryzn_Mn_Per_Cov") ; bz_IA_PWS  # call the function
#####
# Chiton
cn_IA_PWS1 <- filter(IA_PWS, Common_Cat=="chiton")   # Filter out the species/entry of interest
cn_IA_PWS <- PerCovCalc(cn_IA_PWS1, "chiton_Mn_Per_Cov") ; cn_IA_PWS  # call the function
#####
# Clam
cm_IA_PWS1 <- filter(IA_PWS, Common_Cat=="clam")   # Filter out the species/entry of interest
cm_IA_PWS <- PerCovCalc(cm_IA_PWS1, "clam_Mn_Per_Cov") ; cm_IA_PWS  # call the function
#####
# Coralline Alga
ca_IA_PWS1 <- filter(IA_PWS, Common_Cat=="coralline_alga")   # Filter out the species/entry of interest
ca_IA_PWS <- PerCovCalc(ca_IA_PWS1, "cor_alga_Mn_Per_Cov") ; ca_IA_PWS  # call the function
#####
# Encrusting Red Alga
er_IA_PWS1 <- filter(IA_PWS, Common_Cat=="encrusting_red_alga")   # Filter out the species/entry of interest
er_IA_PWS <- PerCovCalc(er_IA_PWS1, "enc_alga_Mn_Per_Cov") ; er_IA_PWS  # call the function
#####
# Filamentous Brown
fb_IA_PWS1 <- filter(IA_PWS, Common_Cat=="filamentous_brown")   # Filter out the species/entry of interest
fb_IA_PWS <- PerCovCalc(fb_IA_PWS1, "fil_brn_alga_Mn_Per_Cov") ; fb_IA_PWS  # call the function
#####
# Filamentous Green
fg_IA_PWS1 <- filter(IA_PWS, Common_Cat=="filamentous_green")   # Filter out the species/entry of interest
fg_IA_PWS <- PerCovCalc(fg_IA_PWS1, "fil_grn_alga_Mn_Per_Cov") ; fg_IA_PWS  # call the function
#####
# Green Alga
ga_IA_PWS1 <- filter(IA_PWS, Common_Cat=="green_alga")   # Filter out the species/entry of interest
ga_IA_PWS <- PerCovCalc(ga_IA_PWS1, "grn_alga_Mn_Per_Cov") ; ga_IA_PWS  # call the function
#####
# Hydroid
hy_IA_PWS1 <- filter(IA_PWS, Common_Cat=="hydroid")   # Filter out the species/entry of interest
hy_IA_PWS <- PerCovCalc(hy_IA_PWS1, "hydroid_Mn_Per_Cov") ; hy_IA_PWS  # call the function
#####
# Jingle Shell
js_IA_PWS1 <- filter(IA_PWS, Common_Cat=="jingle_shell")   # Filter out the species/entry of interest
js_IA_PWS <- PerCovCalc(js_IA_PWS1, "Jngl_shl_Mn_Per_Cov") ; js_IA_PWS  # call the function
#####
# Mussel
ms_IA_PWS1 <- filter(IA_PWS, Common_Cat=="mussel")   # Filter out the species/entry of interest
ms_IA_PWS <- PerCovCalc(ms_IA_PWS1, "mussel_Mn_Per_Cov") ; ms_IA_PWS  # call the function
#####
# Red Alga
ra_IA_PWS1 <- filter(IA_PWS, Common_Cat=="red_alga")   # Filter out the species/entry of interest
ra_IA_PWS <- PerCovCalc(ra_IA_PWS1, "red_alga_Mn_Per_Cov") ; ra_IA_PWS  # call the function
#####
# Sponge
sp_IA_PWS1 <- filter(IA_PWS, Common_Cat=="sponge")   # Filter out the species/entry of interest
sp_IA_PWS <- PerCovCalc(sp_IA_PWS1, "sponge_Mn_Per_Cov") ; sp_IA_PWS  # call the function
#####
# Tunicate
tn_IA_PWS1 <- filter(IA_PWS, Common_Cat=="tunicate")   # Filter out the species/entry of interest
tn_IA_PWS <- PerCovCalc(tn_IA_PWS1, "tunicate_Mn_Per_Cov") ; tn_IA_PWS  # call the function
#####
# Worm
wm_IA_PWS1 <- filter(IA_PWS, Common_Cat=="worm")   # Filter out the species/entry of interest
wm_IA_PWS <- PerCovCalc(wm_IA_PWS1, "worm_Mn_Per_Cov") ; wm_IA_PWS  # call the function
#####
# Sum the percent cover for all the invert and algae columns to see if they add to 100 percent!
blnk_8r$Sum_Per_Cov <- rowSums(blnk_8r[, c(17:35)], na.rm = TRUE) ; blnk_8r[45:90,] 
### WHY DO THESE ROWS NOT SUM TO 100% ?  Because there were two transects at each site!?!

########## Species or Genus-level analysis
#####
# Fucus distichus
Fd_IA_PWS1 <- filter(IA_PWS, Species_Name=="Fucus distichus")   # Filter out the species/entry of interest
Fd_IA_PWS <- PerCovCalc(Fd_IA_PWS1, "Fuc_dist_Mn_Per_Cov") ; Fd_IA_PWS  # call the function
#####
# Pylaiella littoralis
Pl_IA_PWS1 <- filter(IA_PWS, Species_Name=="Pylaiella littoralis")   # Filter out the species/entry of interest
Pl_IA_PWS <- PerCovCalc(Pl_IA_PWS1, "Pyl_litt_Mn_Per_Cov") ; Pl_IA_PWS  # call the function
#####
# Halosaccion glandiforme
Hg_IA_PWS1 <- filter(IA_PWS, Species_Name=="Halosaccion glandiforme")   # Filter out the species/entry of interest
Hg_IA_PWS <- PerCovCalc(Hg_IA_PWS1, "Hal_glan_Mn_Per_Cov") ; Hg_IA_PWS  # call the function
#####
# Neorhodomela sp
Neo_sp <- c("Neorhodomela oregona","Neorhodomela larix","Odonthalia / Neorhodomela sp.","Odonthalia floccosa")
No_IA_PWS1 <- filter(IA_PWS, Species_Name %in% Neo_sp)   # Filter out the species/entry of interest
No_IA_PWS <- PerCovCalc(No_IA_PWS1, "Neo_sp_Mn_Per_Cov") ; No_IA_PWS  # call the function
#####
# Acrosiphonia sp
As_IA_PWS1 <- filter(IA_PWS, Species_Name=="Acrosiphonia sp.")   # Filter out the species/entry of interest
As_IA_PWS <- PerCovCalc(As_IA_PWS1, "Acr_sp_Mn_Per_Cov") ; As_IA_PWS  # call the function
#####
# Ahnfeltia fastigiata
Af_IA_PWS1 <- filter(IA_PWS, Species_Name=="Ahnfeltia fastigiata")   # Filter out the species/entry of interest
Af_IA_PWS <- PerCovCalc(Af_IA_PWS1, "Ahn_fast_Mn_Per_Cov") ; Af_IA_PWS  # call the function
#####
# Alaria marginata
Am_IA_PWS1 <- filter(IA_PWS, Species_Name=="Alaria marginata")   # Filter out the species/entry of interest
Am_IA_PWS <- PerCovCalc(Am_IA_PWS1, "Ala_marg_Mn_Per_Cov") ; Am_IA_PWS  # call the function
#####
# Analipus japonicus
Aj_IA_PWS1 <- filter(IA_PWS, Species_Name=="Analipus japonicus")   # Filter out the species/entry of interest
Aj_IA_PWS <- PerCovCalc(Aj_IA_PWS1, "Ana_japo_Mn_Per_Cov") ; Aj_IA_PWS  # call the function
#####
# Anthopleura sp
Anth_sp <- c("Anthopleura elegantissima","Anthopleura xanthogrammica")
Ans_IA_PWS1 <- filter(IA_PWS, Species_Name %in% Anth_sp)   # Filter out the species/entry of interest
Ans_IA_PWS <- PerCovCalc(Ans_IA_PWS1, "Anth_sp_Mn_Per_Cov") ; Ans_IA_PWS  # call the function
#####
# Antithamnionella pacifica
Ap_IA_PWS1 <- filter(IA_PWS, Species_Name=="Antithamnionella pacifica")   # Filter out the species/entry of interest
Ap_IA_PWS <- PerCovCalc(Ap_IA_PWS1, "Ant_paci_Mn_Per_Cov") ; Ap_IA_PWS  # call the function
#####
# Balanus / Semibalanus sp.
barnacle_sp <- c("barnacle","barnacle spat","Balanus / Semibalanus sp.","Balanus glandula","Semibalanus balanoides",
                 "Semibalanus cariosus")
BSs_IA_PWS1 <- filter(IA_PWS, Species_Name %in% barnacle_sp)   # Filter out the species/entry of interest
BSs_IA_PWS <- PerCovCalc(BSs_IA_PWS1, "Bal_Semibal_sp_Mn_Per_Cov") ; BSs_IA_PWS  # call the function
#####
# Blidingia minima
Bm_IA_PWS1 <- filter(IA_PWS, Species_Name=="Blidingia minima var. minima")   # Filter out the species/entry of interest
Bm_IA_PWS <- PerCovCalc(Bm_IA_PWS1, "Bli_mini_Mn_Per_Cov") ; Bm_IA_PWS  # call the function
#####
# Boreophyllum / Pyropia / Wildemania sp.
BPW_IA_PWS1 <- filter(IA_PWS, Species_Name=="Boreophyllum / Pyropia / Wildemania sp.")   # Filter out the species/entry of interest
BPW_IA_PWS <- PerCovCalc(BPW_IA_PWS1, "Bore_Pyro_Wild_Mn_Per_Cov") ; BPW_IA_PWS  # call the function
#####
# Callithamnion pikeanum
Cp_IA_PWS1 <- filter(IA_PWS, Species_Name=="Callithamnion pikeanum")   # Filter out the species/entry of interest
Cp_IA_PWS <- PerCovCalc(Cp_IA_PWS1, "Cal_pike_Mn_Per_Cov") ; Cp_IA_PWS  # call the function
#####
# Ceramium pacificum
Crp_IA_PWS1 <- filter(IA_PWS, Species_Name=="Ceramium pacificum")   # Filter out the species/entry of interest
Crp_IA_PWS <- PerCovCalc(Crp_IA_PWS1, "Cer_paci_Mn_Per_Cov") ; Crp_IA_PWS  # call the function
#####
# Chaetomorpha / Cladophora 
Ch_Cl_sp <- c("Chaetomorpha melagonium","Chaetomorpha sp.","Cladophora / Chaetomorpha sp.")
Chl_IA_PWS1 <- filter(IA_PWS, Species_Name %in% Ch_Cl_sp)   # Filter out the species/entry of interest
Chl_IA_PWS <- PerCovCalc(Chl_IA_PWS1, "Chaet_Clad_sp_Mn_Per_Cov") ; Chl_IA_PWS  # call the function
#####
# Chordaria flagelliformis
Cf_IA_PWS1 <- filter(IA_PWS, Species_Name=="Chordaria flagelliformis")   # Filter out the species/entry of interest
Cf_IA_PWS <- PerCovCalc(Cf_IA_PWS1, "Cho_flag_Mn_Per_Cov") ; Cf_IA_PWS  # call the function
#####
# Chthamalus dalli
Cd_IA_PWS1 <- filter(IA_PWS, Species_Name=="Chthamalus dalli")   # Filter out the species/entry of interest
Cd_IA_PWS <- PerCovCalc(Cd_IA_PWS1, "Cht_dall_Mn_Per_Cov") ; Cd_IA_PWS  # call the function
#####
# Coilodesme bulligera
Cb_IA_PWS1 <- filter(IA_PWS, Species_Name=="Coilodesme bulligera")   # Filter out the species/entry of interest
Cb_IA_PWS <- PerCovCalc(Cb_IA_PWS1, "Coi_bull_Mn_Per_Cov") ; Cb_IA_PWS  # call the function
#####
# Constantinea subulifera
Cs_IA_PWS1 <- filter(IA_PWS, Species_Name=="Constantinea subulifera")   # Filter out the species/entry of interest
Cs_IA_PWS <- PerCovCalc(Cs_IA_PWS1, "Con_subu_Mn_Per_Cov") ; Cs_IA_PWS  # call the function
#####
# Corallina sp.
Csp_IA_PWS1 <- filter(IA_PWS, Species_Name=="Corallina sp.")   # Filter out the species/entry of interest
Csp_IA_PWS <- PerCovCalc(Csp_IA_PWS1, "Corallina_sp_Mn_Per_Cov") ; Csp_IA_PWS  # call the function
#####
# Cryptochiton stelleri
Crs_IA_PWS1 <- filter(IA_PWS, Species_Name=="Cryptochiton stelleri")   # Filter out the species/entry of interest
Crs_IA_PWS <- PerCovCalc(Crs_IA_PWS1, "Cryp_stell_Mn_Per_Cov") ; Crs_IA_PWS  # call the function
#####
# Cryptopleura ruprechtiana
Cr_IA_PWS1 <- filter(IA_PWS, Species_Name=="Cryptopleura ruprechtiana")   # Filter out the species/entry of interest
Cr_IA_PWS <- PerCovCalc(Cr_IA_PWS1, "Cryp_rupr_Mn_Per_Cov") ; Cr_IA_PWS  # call the function
#####
# Cryptosiphonia woodii
Cw_IA_PWS1 <- filter(IA_PWS, Species_Name=="Cryptosiphonia woodii")   # Filter out the species/entry of interest
Cw_IA_PWS <- PerCovCalc(Cw_IA_PWS1, "Cryp_wood_Mn_Per_Cov") ; Cw_IA_PWS  # call the function
#####
# Desmarestia aculeata
Da_IA_PWS1 <- filter(IA_PWS, Species_Name=="Desmarestia aculeata")   # Filter out the species/entry of interest
Da_IA_PWS <- PerCovCalc(Da_IA_PWS1, "Des_acul_Mn_Per_Cov") ; Da_IA_PWS  # call the function
#####
# Dictyosiphon foeniculaceus
Df_IA_PWS1 <- filter(IA_PWS, Species_Name=="Dictyosiphon foeniculaceus")   # Filter out the species/entry of interest
Df_IA_PWS <- PerCovCalc(Df_IA_PWS1, "Dict_foen_Mn_Per_Cov") ; Df_IA_PWS  # call the function
#####
# Dumontia alaskana
Da_IA_PWS1 <- filter(IA_PWS, Species_Name=="Dumontia alaskana")   # Filter out the species/entry of interest
Da_IA_PWS <- PerCovCalc(Da_IA_PWS1, "Dum_alas_Mn_Per_Cov") ; Da_IA_PWS  # call the function
#####
# Ectocarpus sp.
Es_IA_PWS1 <- filter(IA_PWS, Species_Name=="Ectocarpus sp.")   # Filter out the species/entry of interest
Es_IA_PWS <- PerCovCalc(Es_IA_PWS1, "Ecto_sp_Mn_Per_Cov") ; Es_IA_PWS  # call the function
#####
# Elachista sp.
El_sp <- c("Elachista fucicola","Elachista sp.")
Els_IA_PWS1 <- filter(IA_PWS, Species_Name %in% El_sp)   # Filter out the species/entry of interest
Els_IA_PWS <- PerCovCalc(Els_IA_PWS1, "Elach_sp_sp_Mn_Per_Cov") ; Els_IA_PWS  # call the function
#####
# encrusting bryozoan
bz_IA_PWS1 <- filter(IA_PWS, Species_Name=="encrusting bryozoan")   # Filter out the species/entry of interest
bz_IA_PWS <- PerCovCalc(bz_IA_PWS1, "enc_bryz_Mn_Per_Cov") ; bz_IA_PWS  # call the function
#####
# encrusting coralline algae
eca_IA_PWS1 <- filter(IA_PWS, Species_Name=="encrusting coralline algae")   # Filter out the species/entry of interest
eca_IA_PWS <- PerCovCalc(eca_IA_PWS1, "enc_corall_Mn_Per_Cov") ; eca_IA_PWS  # call the function
#####
# Endocladia muricata
Em_IA_PWS1 <- filter(IA_PWS, Species_Name=="Endocladia muricata")   # Filter out the species/entry of interest
Em_IA_PWS <- PerCovCalc(Em_IA_PWS1, "Endo_muri_Mn_Per_Cov") ; Em_IA_PWS  # call the function
#####
# Epiactis sp.
Epi_IA_PWS1 <- filter(IA_PWS, Species_Name=="Epiactis sp.")   # Filter out the species/entry of interest
Epi_IA_PWS <- PerCovCalc(Em_IA_PWS1, "Epi_sp_Mn_Per_Cov") ; Epi_IA_PWS  # call the function
#####
# Eudesme virescens
Ev_IA_PWS1 <- filter(IA_PWS, Species_Name=="Eudesme virescens")   # Filter out the species/entry of interest
Ev_IA_PWS <- PerCovCalc(Ev_IA_PWS1, "Eud_vire_Mn_Per_Cov") ; Ev_IA_PWS  # call the function

#####
# Eurystomella bilabiata
Eb_IA_PWS1 <- filter(IA_PWS, Species_Name=="Eurystomella bilabiata")   # Filter out the species/entry of interest
Eb_IA_PWS <- PerCovCalc(Eb_IA_PWS1, "Eury_bila_Mn_Per_Cov") ; Eb_IA_PWS  # call the function
#####
# foliose bryozoan
fb_IA_PWS1 <- filter(IA_PWS, Species_Name=="foliose bryozoan")   # Filter out the species/entry of interest
fb_IA_PWS <- PerCovCalc(fb_IA_PWS1, "fol_bryz_Mn_Per_Cov") ; fb_IA_PWS  # call the function
#####
# foliose coralline algae
fca_IA_PWS1 <- filter(IA_PWS, Species_Name=="foliose coralline algae")   # Filter out the species/entry of interest
fca_IA_PWS <- PerCovCalc(fca_IA_PWS1, "fol_cor_alg_Mn_Per_Cov") ; fca_IA_PWS  # call the function
#####
# Gloiopeltis furcata
Gf_IA_PWS1 <- filter(IA_PWS, Species_Name=="Gloiopeltis furcata")   # Filter out the species/entry of interest
Gf_IA_PWS <- PerCovCalc(Gf_IA_PWS1, "Glo_furc_Mn_Per_Cov") ; Gf_IA_PWS  # call the function
#####
# Gracilaria pacifica
Gp_IA_PWS1 <- filter(IA_PWS, Species_Name=="Gracilaria pacifica")   # Filter out the species/entry of interest
Gp_IA_PWS <- PerCovCalc(Gp_IA_PWS1, "Grac_pac_Mn_Per_Cov") ; Gp_IA_PWS  # call the function
#####
# Hiatella arctica
Ha_IA_PWS1 <- filter(IA_PWS, Species_Name=="Hiatella arctica")   # Filter out the species/entry of interest
Ha_IA_PWS <- PerCovCalc(Ha_IA_PWS1, "Hia_arct_Mn_Per_Cov") ; Ha_IA_PWS  # call the function
#####
# Hildenbrandia sp.
Hsp_IA_PWS1 <- filter(IA_PWS, Species_Name=="Hildenbrandia sp.")   # Filter out the species/entry of interest
Hsp_IA_PWS <- PerCovCalc(Hsp_IA_PWS1, "Hild_sp_Mn_Per_Cov") ; Hsp_IA_PWS  # call the function
#####
# Leathesia marina
Lm_IA_PWS1 <- filter(IA_PWS, Species_Name=="Leathesia marina")   # Filter out the species/entry of interest
Lm_IA_PWS <- PerCovCalc(Lm_IA_PWS1, "Lea_mar_Mn_Per_Cov") ; Lm_IA_PWS  # call the function
#####
# Mastocarpus sp.
Msp_IA_PWS1 <- filter(IA_PWS, Species_Name=="Mastocarpus sp.")   # Filter out the species/entry of interest
Msp_IA_PWS <- PerCovCalc(Msp_IA_PWS1, "Masto_sp_Mn_Per_Cov") ; Msp_IA_PWS  # call the function
#####
# Mazzaella sp.
Mz_sp <- c("Mazzaella parksii","Mazzaella phyllocarpa","Mazzaella sp.")
Mzs_IA_PWS1 <- filter(IA_PWS, Species_Name %in% Mz_sp)   # Filter out the species/entry of interest
Mzs_IA_PWS <- PerCovCalc(Mzs_IA_PWS1, "Mazz_sp_sp_Mn_Per_Cov") ; Mzs_IA_PWS  # call the function
#####
# Melanosiphon / Scytosiphon sp.
MS_sp <- c("Melanosiphon / Scytosiphon sp.","Melanosiphon intestinalis","Scytosiphon lomentaria")
MS_IA_PWS1 <- filter(IA_PWS, Species_Name %in% MS_sp)   # Filter out the species/entry of interest
MS_IA_PWS <- PerCovCalc(MS_IA_PWS1, "Mel_Scyt_sp_Mn_Per_Cov") ; MS_IA_PWS  # call the function
#####
# Metridium senile
Met_IA_PWS1 <- filter(IA_PWS, Species_Name=="Metridium senile")   # Filter out the species/entry of interest
Met_IA_PWS <- PerCovCalc(Met_IA_PWS1, "Met_sen_Mn_Per_Cov") ; Met_IA_PWS  # call the function
#####
# Microcladia borealis
Mic_IA_PWS1 <- filter(IA_PWS, Species_Name=="Microcladia borealis")   # Filter out the species/entry of interest
Mic_IA_PWS <- PerCovCalc(Mic_IA_PWS1, "Mic_bore_Mn_Per_Cov") ; Mic_IA_PWS  # call the function
#####
# Modiolus modiolus
Mod_IA_PWS1 <- filter(IA_PWS, Species_Name=="Modiolus modiolus")   # Filter out the species/entry of interest
Mod_IA_PWS <- PerCovCalc(Mod_IA_PWS1, "Mod_mod_Mn_Per_Cov") ; Mod_IA_PWS  # call the function
#####
# Musculus sp
Mus_IA_PWS1 <- filter(IA_PWS, Species_Name=="Musculus sp")   # Filter out the species/entry of interest
Mus_IA_PWS <- PerCovCalc(Mus_IA_PWS1, "Mus_sp_Mn_Per_Cov") ; Mus_IA_PWS  # call the function
######
# Mya truncata
MYA_IA_PWS1 <- filter(IA_PWS, Species_Name=="Mya truncata")   # Filter out the species/entry of interest
MYA_IA_PWS <- PerCovCalc(MYA_IA_PWS1, "Mya_trun_Mn_Per_Cov") ; MYA_IA_PWS  # call the function
#####
# Mytilus trossulus
MYt_IA_PWS1 <- filter(IA_PWS, Species_Name=="Mytilus trossulus")   # Filter out the species/entry of interest
MYt_IA_PWS <- PerCovCalc(MYt_IA_PWS1, "Myt_tros_Mn_Per_Cov") ; MYt_IA_PWS  # call the function
#####
# Nemalion elminthoides
Ne_IA_PWS1 <- filter(IA_PWS, Species_Name=="Nemalion elminthoides")   # Filter out the species/entry of interest
Ne_IA_PWS <- PerCovCalc(Ne_IA_PWS1, "Nem_elmi_Mn_Per_Cov") ; Ne_IA_PWS  # call the function
#####
# Neoptilota / Ptilota sp.
NP_IA_PWS1 <- filter(IA_PWS, Species_Name=="Neoptilota / Ptilota sp.")   # Filter out the species/entry of interest
NP_IA_PWS <- PerCovCalc(NP_IA_PWS1, "Neo_Ptil_sp_Mn_Per_Cov") ; NP_IA_PWS  # call the function
#####
# non-coralline algal crust
nac_IA_PWS1 <- filter(IA_PWS, Species_Name=="non-coralline algal crust")   # Filter out the species/entry of interest
nac_IA_PWS <- PerCovCalc(nac_IA_PWS1, "nonc_alg_crust_Mn_Per_Cov") ; nac_IA_PWS  # call the function
#####
# Palmaria sp.
Ps_sp <- c("Palmaria callophylloides","Palmaria hecatensis","Palmaria hecatensis/mollis",
           "Palmaria mollis","Palmaria sp.")
Ps_IA_PWS1 <- filter(IA_PWS, Species_Name %in% Ps_sp)   # Filter out the species/entry of interest
Ps_IA_PWS <- PerCovCalc(Ps_IA_PWS1, "Pal_sp_Mn_Per_Cov") ; Ps_IA_PWS  # call the function
#####
# Petalonia fascia
Pet_IA_PWS1 <- filter(IA_PWS, Species_Name=="Petalonia fascia")   # Filter out the species/entry of interest
Pet_IA_PWS <- PerCovCalc(Pet_IA_PWS1, "Pet_fasc_crust_Mn_Per_Cov") ; Pet_IA_PWS  # call the function
#####
# Phycodrys / Tokidadendron sp.
Phy_sp <- c("Phycodrys / Tokidadendron sp.","Phycodrys fimbriata","Tokidadendron bullatum")
Phy_IA_PWS1 <- filter(IA_PWS, Species_Name %in% Phy_sp)   # Filter out the species/entry of interest
Phy_IA_PWS <- PerCovCalc(Phy_IA_PWS1, "Phy_Tok_sp_Mn_Per_Cov") ; Phy_IA_PWS  # call the function
#####
# Pleonosporium vancouverianum
Ple_IA_PWS1 <- filter(IA_PWS, Species_Name=="Pleonosporium vancouverianum")   # Filter out the species/entry of interest
Ple_IA_PWS <- PerCovCalc(Ple_IA_PWS1, "Ple_vanc_Mn_Per_Cov") ; Ple_IA_PWS  # call the function
#####
# Plocamium pacificum
Plp_IA_PWS1 <- filter(IA_PWS, Species_Name=="Plocamium pacificum")   # Filter out the species/entry of interest
Plp_IA_PWS <- PerCovCalc(Plp_IA_PWS1, "Plo_pac_Mn_Per_Cov") ; Plp_IA_PWS  # call the function
#####
# Pododesmus macroschisma
Pm_IA_PWS1 <- filter(IA_PWS, Species_Name=="Pododesmus macroschisma")   # Filter out the species/entry of interest
Pm_IA_PWS <- PerCovCalc(Pm_IA_PWS1, "Pod_mac_Mn_Per_Cov") ; Pm_IA_PWS  # call the function
#####
# Pterosiphonia / Polysiphonia sp.
Pte_sp <- c("Polysiphonia sp.","Pterosiphonia / Polysiphonia sp.","Pterosiphonia bipinnata")
Pte_IA_PWS1 <- filter(IA_PWS, Species_Name %in% Pte_sp)   # Filter out the species/entry of interest
Pte_IA_PWS <- PerCovCalc(Pte_IA_PWS1, "Pte_Poly_sp_Mn_Per_Cov") ; Pte_IA_PWS  # call the function
#####
# Ptilota sp.
Pts_IA_PWS1 <- filter(IA_PWS, Species_Name=="Ptilota sp.")   # Filter out the species/entry of interest
Pts_IA_PWS <- PerCovCalc(Pts_IA_PWS1, "Ptil_sp_Mn_Per_Cov") ; Pts_IA_PWS  # call the function
#####
# Ralfsia sp.
Rlf_sp <- c("Ralfsia sp.","Ralfsia fungiformis")
Rlf_IA_PWS1 <- filter(IA_PWS, Species_Name %in% Rlf_sp)   # Filter out the species/entry of interest
Rlf_IA_PWS <- PerCovCalc(Rlf_IA_PWS1, "Ralf_sp_Mn_Per_Cov") ; Rlf_IA_PWS  # call the function
#####
# Rhodochorton purpureum
Rho_IA_PWS1 <- filter(IA_PWS, Species_Name=="Rhodochorton purpureum")   # Filter out the species/entry of interest
Rho_IA_PWS <- PerCovCalc(Rho_IA_PWS1, "Rho_purp_Mn_Per_Cov") ; Rho_IA_PWS  # call the function
#####
# Saccharina sp.
Sacc_sp <- c("Saccharina latissima","Saccharina sessilis")
Sacc_IA_PWS1 <- filter(IA_PWS, Species_Name %in% Sacc_sp)   # Filter out the species/entry of interest
Sacc_IA_PWS <- PerCovCalc(Sacc_IA_PWS1, "Sacc_sp_Mn_Per_Cov") ; Sacc_IA_PWS  # call the function
#####
# Soranthera ulvoidea
Sor_IA_PWS1 <- filter(IA_PWS, Species_Name=="Soranthera ulvoidea")   # Filter out the species/entry of interest
Sor_IA_PWS <- PerCovCalc(Sor_IA_PWS1, "Sor_ulv_Mn_Per_Cov") ; Sor_IA_PWS  # call the function
#####
# spirorbidae
spi_IA_PWS1 <- filter(IA_PWS, Species_Name=="spirorbidae")   # Filter out the species/entry of interest
spi_IA_PWS <- PerCovCalc(spi_IA_PWS1, "spiror_Mn_Per_Cov") ; spi_IA_PWS  # call the function
#####
#  Stomachetosella cruenta
Sto_IA_PWS1 <- filter(IA_PWS, Species_Name=="Stomachetosella cruenta")   # Filter out the species/entry of interest
Sto_IA_PWS <- PerCovCalc(Sto_IA_PWS1, "Sto_crue_Mn_Per_Cov") ; Sto_IA_PWS  # call the function
#####
# Ulothrix flacca
Ulo_IA_PWS1 <- filter(IA_PWS, Species_Name=="Ulothrix flacca")   # Filter out the species/entry of interest
Ulo_IA_PWS <- PerCovCalc(Ulo_IA_PWS1, "Ulo_flac_Mn_Per_Cov") ; Ulo_IA_PWS  # call the function
#####
# Ulva / Monostroma sp.
Ulv_sp <- c("Ulva / Monostroma sp.","Ulva sp.")
Ulv_IA_PWS1 <- filter(IA_PWS, Species_Name %in% Ulv_sp)   # Filter out the species/entry of interest
Ulv_IA_PWS <- PerCovCalc(Ulv_IA_PWS1, "Ulv_Mono_sp_Mn_Per_Cov") ; Ulv_IA_PWS  # call the function
#####
# Urticina crassicornis
Urt_IA_PWS1 <- filter(IA_PWS, Species_Name=="Urticina crassicornis")   # Filter out the species/entry of interest
Urt_IA_PWS <- PerCovCalc(Urt_IA_PWS1, "Urt_crass_Mn_Per_Cov") ; Urt_IA_PWS  # call the function
#####
# unidentified anemone
ua_IA_PWS1 <- filter(IA_PWS, Species_Name=="unidentified anemone")   # Filter out the species/entry of interest
ua_IA_PWS <- PerCovCalc(ua_IA_PWS1, "unid_anem_Mn_Per_Cov") ; ua_IA_PWS  # call the function
#####
# unidentified brown algae
uba_IA_PWS1 <- filter(IA_PWS, Species_Name=="unidentified brown algae")   # Filter out the species/entry of interest
uba_IA_PWS <- PerCovCalc(uba_IA_PWS1, "unid_brn_alg_Mn_Per_Cov") ; uba_IA_PWS  # call the function
#####
# unidentified filamentous red algae
ufra_IA_PWS1 <- filter(IA_PWS, Species_Name=="unidentified filamentous red algae")   # Filter out the species/entry of interest
ufra_IA_PWS <- PerCovCalc(ufra_IA_PWS1, "unid_fil_red_alg_Mn_Per_Cov") ; ufra_IA_PWS  # call the function
#####
# unidentified green algae
uga_IA_PWS1 <- filter(IA_PWS, Species_Name=="unidentified green algae")   # Filter out the species/entry of interest
uga_IA_PWS <- PerCovCalc(uga_IA_PWS1, "unid_grn_alg_Mn_Per_Cov") ; uga_IA_PWS  # call the function
#####
# unidentified hydroid
uh_IA_PWS1 <- filter(IA_PWS, Species_Name=="unidentified hydroid")   # Filter out the species/entry of interest
uh_IA_PWS <- PerCovCalc(uh_IA_PWS1, "unid_hydroid_Mn_Per_Cov") ; uh_IA_PWS  # call the function
#####
# unidentified sponge
sp_IA_PWS1 <- filter(IA_PWS, Species_Name=="unidentified sponge")   # Filter out the species/entry of interest
sp_IA_PWS <- PerCovCalc(sp_IA_PWS1, "unid_sponge_Mn_Per_Cov") ; sp_IA_PWS  # call the function
#####
# unidentified tunicate
tun_IA_PWS1 <- filter(IA_PWS, Species_Name=="unidentified tunicate")   # Filter out the species/entry of interest
tun_IA_PWS <- PerCovCalc(tun_IA_PWS1, "unid_tunic_Mn_Per_Cov") ; tun_IA_PWS  # call the function
#####
# unidentified worm
wm_IA_PWS1 <- filter(IA_PWS, Species_Name=="unidentified worm")   # Filter out the species/entry of interest
wm_IA_PWS <- PerCovCalc(wm_IA_PWS1, "unid_worm_Mn_Per_Cov") ; wm_IA_PWS  # call the function
#####

###########################################
# Temperature (NOTE: These data are not yet QAQC'd, and are downloaded from the Gulf Watch Workspace)
setwd("C:/Users/rblake/Documents/NCEAS/GoA Dynamics WG/GW_Nearshore Intertidal Data/Temp Data")

IkT <- read.csv('Iktua_TempData.csv',header=T) ; head(IkT) ; str(IkT)
CdT <- read.csv('Cedar_temp_2012thru_ 2013_28Jan2014.csv') ; head(CdT)
HrT <- read.csv('Herring_temp_ 2010thru_2013_28Jan014.csv') ; head(HrT)
HoT <- read.csv('Hogan_temp_ 2010thru_2013_28Jan2014.csv') ; head(HoT)
JnT <- read.csv('Johnson_temp_2010thru_2013_27Jan2014.csv') ; head(JnT)
PyT <- read.csv('Perry_temp_2012thru_2013_27Jan2014.csv') ; head(PyT)
WhT <- read.csv('Whale_temp_2010thru_2013_27Jan2014.csv') ; head(PyT)

# Add a Site Name column
IkT$Site_Name <- "Iktua Bay"
CdT$Site_Name <- "Cedar Bay"
HrT$Site_Name <- "Herring Bay"
HoT$Site_Name <- "Hogan Bay"
JnT$Site_Name <- "Johnson Bay"
PyT$Site_Name <- "Perry Island"
WhT$Site_Name <- "Whale Bay"

# make Time/Date columns all named the same
names(CdT)[names(CdT)=="Time..GMT.08.00"] <- "Date.Time..GMT.08.00"
names(PyT)[names(PyT)=="Time..GMT.08.00"] <- "Date.Time..GMT.08.00"
names(WhT)[names(WhT)=="Time..GMT.08.00"] <- "Date.Time..GMT.08.00"

head(CdT) ; head(PyT) ; head(WhT)

# merge all the seperate files into one
dfs <- list(IkT,CdT,HrT,HoT,JnT,PyT,WhT)
Temps <- do.call("rbind", dfs)

# rename Temp column
names(Temps)[names(Temps)=="Temp_.deg_C."] <- "Temp_deg_C"

# Split off hours into a column
Tm <- strsplit(as.character(Temps$Date.Time..GMT.08.00), split=" ") # split the column to extract 
Tm2<- sapply(Tm, function(x) x[2]) # create column
T_Tm <- strsplit(as.character(Tm2), split=":") # split the column to extract 
Temps$Sample_Hour_8GMT <- sapply(T_Tm, function(x) x[1]) # create column

# Split off Years and Months into columns
MnDyYr <- sapply(Tm, function(x) x[1]) # split the column 
MnDyYr2 <- strsplit(as.character(MnDyYr), split="/")
Temps$Sample_Year <- sapply(MnDyYr2, function(x) x[3]) # create Sample Year column
Temps$Sample_Month <- sapply(MnDyYr2, function(x) x[1]) # create Sample Month column

# Summarize data
Temp_PWS <- Temps %>% 
            group_by(Site_Name, Sample_Year) %>%
            summarise(Temp_Mean_C=mean(Temp_deg_C),Temp_Max_C=max(Temp_deg_C),
                      Temp_Min_C=min(Temp_deg_C),Temp_Var_C=var(Temp_deg_C)) %>%
            ungroup()
head(Temp_PWS)

###############################################
# for loop to add columns to blank datasheet
varnames <- list(Mus_PWS_s,Mus_PWS_a,Limd_PWS_a,Lims_PWS,Wlk_PWS,SS_PWS,OyC_PWS,ElG,BS_IA_PWS,
                 a_IA_PWS,b_IA_PWS,ba_IA_PWS,bz_IA_PWS,cn_IA_PWS,cm_IA_PWS,ca_IA_PWS,er_IA_PWS,
                 fb_IA_PWS,fg_IA_PWS,ga_IA_PWS,hy_IA_PWS,js_IA_PWS,ms_IA_PWS,ra_IA_PWS,sp_IA_PWS,
                 tn_IA_PWS,wm_IA_PWS,Fd_IA_PWS,Pl_IA_PWS,Hg_IA_PWS,No_IA_PWS,As_IA_PWS,Af_IA_PWS,
                 Am_IA_PWS,Aj_IA_PWS,Ans_IA_PWS,Ap_IA_PWS,BSs_IA_PWS,Bm_IA_PWS,BPW_IA_PWS,
                 Cp_IA_PWS,Crp_IA_PWS,Chl_IA_PWS,Cf_IA_PWS,Cd_IA_PWS,Cb_IA_PWS,Cs_IA_PWS,
                 Csp_IA_PWS,Crs_IA_PWS,Cr_IA_PWS,Cw_IA_PWS,Da_IA_PWS,Df_IA_PWS,Da_IA_PWS,
                 Es_IA_PWS,Els_IA_PWS,bz_IA_PWS,eca_IA_PWS,Em_IA_PWS,Epi_IA_PWS,Ev_IA_PWS,
                 Eb_IA_PWS,fb_IA_PWS,fca_IA_PWS,Gf_IA_PWS,Gp_IA_PWS,Ha_IA_PWS,Hsp_IA_PWS,
                 Lm_IA_PWS,Msp_IA_PWS,Mzs_IA_PWS,MS_IA_PWS,Met_IA_PWS,Mic_IA_PWS,Mod_IA_PWS,
                 Mus_IA_PWS,MYA_IA_PWS,MYt_IA_PWS,Ne_IA_PWS,NP_IA_PWS,nac_IA_PWS,Ps_IA_PWS,
                 Pet_IA_PWS,Phy_IA_PWS,Ple_IA_PWS,Plp_IA_PWS,Pm_IA_PWS,Pte_IA_PWS,Pts_IA_PWS,
                 Rlf_IA_PWS,Rho_IA_PWS,Sacc_IA_PWS,Sor_IA_PWS,spi_IA_PWS,Sto_IA_PWS,Ulo_IA_PWS,
                 Ulv_IA_PWS,Urt_IA_PWS,ua_IA_PWS,uba_IA_PWS,ufra_IA_PWS,uga_IA_PWS,uh_IA_PWS,
                 sp_IA_PWS,tun_IA_PWS,wm_IA_PWS,Temp_PWS)

blnk_full<-blnk # must initialize the final blank dataframe first
###
for(i in 1:length(varnames)){ 
    df <- varnames[i]
    blnk_full <- merge(blnk_full, df, by=c("Site_Name","Sample_Year"), all=TRUE)
    }
###
blnk_full[1:35,]
#write.csv(blnk_full, "C:/Users/rblake/Desktop/blnk_full.csv", row.names=F)






###############################################
# Sea Otters  (from the North Pacific Seabird Database)
setwd("C:/Users/rblake/Documents/NCEAS/nppsd")

DAT <- read.csv('tbl_DATA_OBS.csv',header=T) ##  Observations
head(DAT) ; str(DAT)

STE <- read.csv('tbl_LOCATION.csv',header=T) ## Sample info
head(STE) ; str(STE)

SOT <- filter(DAT, Common.Name =="Sea Otter")   # select otter observations

SOTT <- SOT %>%
        merge(STE, by=c("Master.Key"))  %>%   # add in the sample info
        rename(Sample_Year=Year) %>%         # rename column
        filter(Sample_Year %in% c(2010,2011,2012,2013,2014))   #  select years of interest
head(SOTT)

#write.csv(SOTT, "C:/Users/rblake/Desktop/SOTT.csv", row.names=F)

# from ArcMap, MasterKeys for sea otter observation in PWS 2010-2012 within 24 km of Gulf Watch sites
# 24 km was chosen based on foraging range in this doc: 
#                    http://www.fws.gov/alaska/fisheries/mmm/stock/Revised_April_2014_Southcentral_Alaska_Sea_Otter_SAR.pdf

Ott_list <- c("1052012-05-07 16:59:17",
              "732010-09-18 12:40:28",
              "732010-09-18 14:01:42",
              "732010-09-18 14:11:24",
              "732010-09-18 14:20:46",
              "732010-09-18 14:29:48",
              "732010-09-18 14:29:48",
              "732010-09-18 14:38:32")

SOTT_24km <- filter(SOTT, Master.Key %in% Ott_list)
SOTT_24km


##########################################
# Substrate
URL_Sub <- "https://workspace.aoos.org/published/file/a73727a4-7ae6-4ab3-b893-03247860e9e8/NearshoreBenthicSystemsInGOA_SOP4_Rocky_2014QuadratSubstrateData_FINAL_20140806.csv"
SubGet <- GET(URL_Sub)
Sub1 <- content(SubGet, as='text')
Sub0 <- read.csv(file=textConnection(Sub1))
head(Sub0)
# Cleaning, filtering, etc. 
Sub <- merge(Sub0, PWS_Sites_Codes, by=c("Site_Code"), all=T) # adding in site names
Sub <- Sub[which(!is.na(Sub$Sample_Date)),] # exclude rows that are all NA
YMD <- strsplit(as.character(Sub$Sample_Date), split="/") # split the Sample_Date column to extract year
Sub$Sample_Year <- sapply(YMD, function(x) x[3]) # create Sample Year column
head(Sub)

#Sub$Sample_Date <- as.Date(Sub$Sample_Date, format="%m/%d/%Y") # convert dates to Date


Sub_PWS <- Sub %>%
           filter(grepl("PWS", Site_Code)) %>% # extract PWS samples
           group_by(Site_Name, Sample_Year) %>%
  
    
           mutate(Pri_Sub_Typ_Count = count(Primary_Substrate_Type)) %>% # count the number of quadrats (rows) 
           ungroup() 
head(Sub_PWS)






#write.csv(blnk_17, "C:/Users/rblake/Desktop/blnk_17.csv", row.names=F)


