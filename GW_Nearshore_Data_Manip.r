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


library(httr)
library(tidyr)
library(plyr)
library(dplyr)

###################################
# Get data from Gulf Watch portal #
###################################
# Mussels - Getting data from Gulf of Alaska AOOS Axiom portal
URL_M <- "https://workspace.aoos.org/published/file/52265bf0e4b0f364fbbb22b5/NearshoreBenthicSystemsInGOA_BenthicMusselData_2008_2012.csv"
MGet <- GET(URL_M)
Mus1 <- content(MGet, as='text')
Mus <- read.csv(file=textConnection(Mus1), header=T)
head(Mus) 
# Cleaning up data
Mus$Area.sampled..m2. <- as.numeric(as.character(Mus$Area.sampled..m2.))  # convert from factor to numeric
Mus$Size..mm. <- as.numeric(as.character(Mus$Size..mm.))                  # convert from factor to numeric
head(Mus) ; str(Mus)

Mus_PWS <- Mus %>%
           filter(Sitename %in% PWS_Sites) %>%  # extract PWS samples
           select(-X, -X.1) %>%                # remove blank columns
           filter(!Area.sampled..m2. == ".") %>%  # remove invisible "." 
           filter(!Size..mm. == ".") %>%          # remove invisible "." 
           rename(Sample_Year=Sampling.Date, Quad_Num=Quad, AreaSampled_m2=Area.sampled..m2.,
                  Size_mm=Size..mm., Site_Name=Sitename)  %>%
           arrange(Site_Name, Sample_Year, Quad_Num)
head(Mus_PWS)
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
# 2014 Mussel Data
#URL_M2 <- "https://workspace.aoos.org/published/file/ed831409-9b1d-4292-b943-01075ca35ae6/NearshoreBenthicSystemsInGOA_SOP09_Mussel_2014MusselsGreaterThan20mm_Data_20150108.csv"
#M2Get <- GET(URL_M2)
#Mus2c <- content(M2Get, as='text')
#Mus2 <- read.csv(file=textConnection(Mus2c))
#head(Mus2) ; str(Mus2)
#
#URL_Sinfo <- "https://workspace.aoos.org/published/file/76f67c6c-a84b-4110-9db3-b7c45e4be754/NearshoreBenthicSystemsInGOA_SOP09_2014MusselSiteLayout_Data_20150108.csv"
#SinfoGet <- GET(URL_Sinfo)
#Sinfo1 <- content(SinfoGet, as='text')
#Sinfo <- read.csv(file=textConnection(Sinfo1))
#head(Sinfo) ; str(Sinfo)
# put these two datasheets together
#names(Mus2)[names(Mus2)=="vert.tx.."] <- "vert_tx_num"# rename column
#names(Sinfo)[names(Sinfo)=="vertical.transect.."] <- "vert_tx_num"# rename column
#Mus14 <- merge(Mus2, Sinfo[,c(1:5,10,18)], by=c("Region","SiteID", "Site", "Sampling.Date",
#                                             "vert_tx_num"), all=TRUE)
#head(Mus14)
# Cleaning
#MYMD <- strsplit(as.character(Mus14$Sampling.Date), split="/") # split the Sample_Date column to extract year
#Mus14$Sample_Year <- sapply(MYMD, function(x) x[3]) # create Sample Year column

#Mus14_PWS <- Mus14 %>%
#             rename(Size_mm=size, AreaSampled_m2=area.of.quad..m2.)  %>%
#             filter(Region =="PWS") %>% 
#             mutate(Site_Name = ifelse((Site=="Galena"),'Galena Bay',
 #                               ifelse((Site=="Fidalgo"),'Port Fidalgo',
#                                ifelse((Site=="Olsen"),'Olsen Bay',
#                                ifelse((Site=="Simpson"),'Simpson Bay',
#                                ifelse((Site=="Observation"),'Observation Island',
#                                ifelse((Site=="Hogan"),'Hogan Bay',
#                                ifelse((Site=="Iktua"),'Iktua Bay',
#                                ifelse((Site=="Whale"),'Whale Bay',
#                                ifelse((Site=="Johnson"),'Johnson Bay',
#                                ifelse((Site=="Herring"),'Herring Bay',""))))))))))
#                    ) %>%
#            
#head(Mus14_PWS)






# Add columns to blank datasheet
blnk_M <- merge(blnk, Mus_PWS_s, by=c("Site_Name","Sample_Year"), all=TRUE)
blnk_M    
blnk_2 <- merge(blnk_M, Mus_PWS_a, by=c("Site_Name","Sample_Year"), all=TRUE)
blnk_2

#write.csv(Mus, "C:/Users/rblake/Desktop/Mus.csv")
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
# Add columns to blank datasheet
blnk_L <- merge(blnk_2, Limd_PWS_a, by=c("Site_Name","Sample_Year"), all=TRUE)
blnk_L[15:45,]
blnk_3 <- merge(blnk_L, Lims_PWS, by=c("Site_Name","Sample_Year"), all=TRUE)
blnk_3[15:45,]

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
# Add columns to blank datasheet - NOTE: Only adding Whelks, because there were no Chitons
blnk_4 <- merge(blnk_3, Wlk_PWS, by=c("Site_Name","Sample_Year"), all=TRUE)
blnk_4[15:45,]
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
# Add columns to blank datasheet - NOTE: Only adding Whelks, because there were no Chitons
blnk_5 <- merge(blnk_4, SS_PWS, by=c("Site_Name","Sample_Year"), all=TRUE)
blnk_5[15:45,]
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
# Add columns to blank datasheet
blnk_6 <- merge(blnk_5, OyC_PWS, by=c("Site_Name","Sample_Year"), all=TRUE)
blnk_6[15:45,]

##########################################
# Eelgrass
ElG <- read.csv("C:\\Users\\rblake\\Documents\\NCEAS\\GoA Dynamics WG\\GW_Nearshore Intertidal Data\\Eelgrass pct cover summary 2010 thru 2014.csv")
head(ElG)
# add columns to blank datasheet
blnk_7 <- merge(blnk_6, ElG, by=c("Site_Name","Sample_Year"), all=TRUE)
blnk_7[15:45,]
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
              mutate(Per_Cov = (n/25)*100) %>%   # calculates the percent cover of bare substrate per quadrat
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
# Merge wtih the larger data frame
blnk_8 <- merge(blnk_7, BS_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE)
blnk_8[45:90,]

###########  Common Category level analysis
#####
# Anemone
a_IA_PWS1 <- filter(IA_PWS, Common_Cat=="anemone")   # Filter out the species/entry of interest
a_IA_PWS <- PerCovCalc(a_IA_PWS1, "anemone_Mn_Per_Cov") ; a_IA_PWS  # call the function
blnk_8a <- merge(blnk_8, a_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE) # Merge wtih the larger data frame
blnk_8a[45:90,]
#####
# Barnacle
b_IA_PWS1 <- filter(IA_PWS, Common_Cat=="barnacle")   # Filter out the species/entry of interest
b_IA_PWS <- PerCovCalc(b_IA_PWS1, "barnacle_Mn_Per_Cov") ; b_IA_PWS  # call the function
blnk_8b <- merge(blnk_8a, b_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE) # Merge wtih the larger data frame
blnk_8b[45:90,]
#####
# Brown Alga
ba_IA_PWS1 <- filter(IA_PWS, Common_Cat=="brown_alga")   # Filter out the species/entry of interest
ba_IA_PWS <- PerCovCalc(ba_IA_PWS1, "brwn_algae_Mn_Per_Cov") ; ba_IA_PWS  # call the function
blnk_8c <- merge(blnk_8b, ba_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE) # Merge wtih the larger data frame
blnk_8c[45:90,]
#####
# Bryazoan
bz_IA_PWS1 <- filter(IA_PWS, Common_Cat=="bryazoan")   # Filter out the species/entry of interest
bz_IA_PWS <- PerCovCalc(bz_IA_PWS1, "bryzn_Mn_Per_Cov") ; bz_IA_PWS  # call the function
blnk_8d <- merge(blnk_8c, bz_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE) # Merge wtih the larger data frame
blnk_8d[45:90,]
#####
# Chiton
cn_IA_PWS1 <- filter(IA_PWS, Common_Cat=="chiton")   # Filter out the species/entry of interest
cn_IA_PWS <- PerCovCalc(cn_IA_PWS1, "chiton_Mn_Per_Cov") ; cn_IA_PWS  # call the function
blnk_8e <- merge(blnk_8d, cn_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE) # Merge wtih the larger data frame
blnk_8e[45:90,]
#####
# Clam
cm_IA_PWS1 <- filter(IA_PWS, Common_Cat=="clam")   # Filter out the species/entry of interest
cm_IA_PWS <- PerCovCalc(cm_IA_PWS1, "clam_Mn_Per_Cov") ; cm_IA_PWS  # call the function
blnk_8f <- merge(blnk_8e, cm_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE) # Merge wtih the larger data frame
blnk_8f[45:90,]
#####
# Coralline Alga
ca_IA_PWS1 <- filter(IA_PWS, Common_Cat=="coralline_alga")   # Filter out the species/entry of interest
ca_IA_PWS <- PerCovCalc(ca_IA_PWS1, "cor_alga_Mn_Per_Cov") ; ca_IA_PWS  # call the function
blnk_8g <- merge(blnk_8f, ca_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE) # Merge wtih the larger data frame
blnk_8g[45:90,]
#####
# Encrusting Red Alga
er_IA_PWS1 <- filter(IA_PWS, Common_Cat=="encrusting_red_alga")   # Filter out the species/entry of interest
er_IA_PWS <- PerCovCalc(er_IA_PWS1, "enc_alga_Mn_Per_Cov") ; er_IA_PWS  # call the function
blnk_8h <- merge(blnk_8g, er_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE) # Merge wtih the larger data frame
blnk_8h[45:90,]
#####
# Filamentous Brown
fb_IA_PWS1 <- filter(IA_PWS, Common_Cat=="filamentous_brown")   # Filter out the species/entry of interest
fb_IA_PWS <- PerCovCalc(fb_IA_PWS1, "fil_brn_alga_Mn_Per_Cov") ; fb_IA_PWS  # call the function
blnk_8i <- merge(blnk_8h, fb_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE) # Merge wtih the larger data frame
blnk_8i[45:90,]
#####
# Filamentous Green
fg_IA_PWS1 <- filter(IA_PWS, Common_Cat=="filamentous_green")   # Filter out the species/entry of interest
fg_IA_PWS <- PerCovCalc(fg_IA_PWS1, "fil_grn_alga_Mn_Per_Cov") ; fg_IA_PWS  # call the function
blnk_8j <- merge(blnk_8i, fg_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE) # Merge wtih the larger data frame
blnk_8j[45:90,]
#####
# Green Alga
ga_IA_PWS1 <- filter(IA_PWS, Common_Cat=="green_alga")   # Filter out the species/entry of interest
ga_IA_PWS <- PerCovCalc(ga_IA_PWS1, "grn_alga_Mn_Per_Cov") ; ga_IA_PWS  # call the function
blnk_8k <- merge(blnk_8j, ga_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE) # Merge wtih the larger data frame
blnk_8k[45:90,]
#####
# Hydroid
hy_IA_PWS1 <- filter(IA_PWS, Common_Cat=="hydroid")   # Filter out the species/entry of interest
hy_IA_PWS <- PerCovCalc(hy_IA_PWS1, "hydroid_Mn_Per_Cov") ; hy_IA_PWS  # call the function
blnk_8l <- merge(blnk_8k, hy_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE) # Merge wtih the larger data frame
blnk_8l[45:90,]
#####
# Jingle Shell
js_IA_PWS1 <- filter(IA_PWS, Common_Cat=="jingle_shell")   # Filter out the species/entry of interest
js_IA_PWS <- PerCovCalc(js_IA_PWS1, "Jngl_shl_Mn_Per_Cov") ; js_IA_PWS  # call the function
blnk_8m <- merge(blnk_8l, js_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE) # Merge wtih the larger data frame
blnk_8m[45:90,]
#####
# Mussel
ms_IA_PWS1 <- filter(IA_PWS, Common_Cat=="mussel")   # Filter out the species/entry of interest
ms_IA_PWS <- PerCovCalc(ms_IA_PWS1, "mussel_Mn_Per_Cov") ; ms_IA_PWS  # call the function
blnk_8n <- merge(blnk_8m, ms_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE) # Merge wtih the larger data frame
blnk_8n[45:90,]
#####
# Red Alga
ra_IA_PWS1 <- filter(IA_PWS, Common_Cat=="red_alga")   # Filter out the species/entry of interest
ra_IA_PWS <- PerCovCalc(ra_IA_PWS1, "red_alga_Mn_Per_Cov") ; ra_IA_PWS  # call the function
blnk_8o <- merge(blnk_8n, ra_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE) # Merge wtih the larger data frame
blnk_8o[45:90,]
#####
# Sponge
sp_IA_PWS1 <- filter(IA_PWS, Common_Cat=="sponge")   # Filter out the species/entry of interest
sp_IA_PWS <- PerCovCalc(sp_IA_PWS1, "sponge_Mn_Per_Cov") ; sp_IA_PWS  # call the function
blnk_8p <- merge(blnk_8o, sp_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE) # Merge wtih the larger data frame
blnk_8p[45:90,]
#####
# Tunicate
tn_IA_PWS1 <- filter(IA_PWS, Common_Cat=="tunicate")   # Filter out the species/entry of interest
tn_IA_PWS <- PerCovCalc(tn_IA_PWS1, "tunicate_Mn_Per_Cov") ; tn_IA_PWS  # call the function
blnk_8q <- merge(blnk_8p, tn_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE) # Merge wtih the larger data frame
blnk_8q[45:90,]
#####
# Worm
wm_IA_PWS1 <- filter(IA_PWS, Common_Cat=="worm")   # Filter out the species/entry of interest
wm_IA_PWS <- PerCovCalc(wm_IA_PWS1, "worm_Mn_Per_Cov") ; wm_IA_PWS  # call the function
blnk_8r <- merge(blnk_8q, wm_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE) # Merge wtih the larger data frame
blnk_8r[45:90,]
#####
# Sum the percent cover for all the invert and algae columns to see if they add to 100 percent!
blnk_8r$Sum_Per_Cov <- rowSums(blnk_8r[, c(17:35)], na.rm = TRUE) ; blnk_8r[45:90,] 
### WHY DO THESE ROWS NOT SUM TO 100% ?  Because there were two transects at each site!?!

########## Species or Genus-level analysis
#####
# Fucus distichus
Fd_IA_PWS1 <- filter(IA_PWS, Species_Name=="Fucus distichus")   # Filter out the species/entry of interest
Fd_IA_PWS <- PerCovCalc(Fd_IA_PWS1, "Fuc_dist_Mn_Per_Cov") ; Fd_IA_PWS  # call the function
blnk_9 <- merge(blnk_8r, Fd_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE) # Merge wtih the larger data frame
blnk_9[45:90,]
#####
# Pylaiella littoralis
Pl_IA_PWS1 <- filter(IA_PWS, Species_Name=="Pylaiella littoralis")   # Filter out the species/entry of interest
Pl_IA_PWS <- PerCovCalc(Pl_IA_PWS1, "Pyl_litt_Mn_Per_Cov") ; Pl_IA_PWS  # call the function
blnk_10 <- merge(blnk_9, Pl_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE)  # Merge wtih the larger data frame
blnk_10[45:90,]
#####
# Halosaccion glandiforme
Hg_IA_PWS1 <- filter(IA_PWS, Species_Name=="Halosaccion glandiforme")   # Filter out the species/entry of interest
Hg_IA_PWS <- PerCovCalc(Hg_IA_PWS1, "Hal_glan_Mn_Per_Cov") ; Hg_IA_PWS  # call the function
blnk_11 <- merge(blnk_10, Hg_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE)  # Merge wtih the larger data frame
blnk_11[45:90,]
#####
# Neorhodomela sp
Neo_sp <- c("Neorhodomela oregona","Neorhodomela larix","Odonthalia / Neorhodomela sp.","Odonthalia floccosa")
No_IA_PWS1 <- filter(IA_PWS, Species_Name %in% Neo_sp)   # Filter out the species/entry of interest
No_IA_PWS <- PerCovCalc(No_IA_PWS1, "Neo_sp_Mn_Per_Cov") ; No_IA_PWS  # call the function
blnk_12 <- merge(blnk_11, No_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE)  # Merge wtih the larger data frame
blnk_12[45:90,]
#####
# Acrosiphonia sp
As_IA_PWS1 <- filter(IA_PWS, Species_Name=="Acrosiphonia sp.")   # Filter out the species/entry of interest
As_IA_PWS <- PerCovCalc(As_IA_PWS1, "Acr_sp_Mn_Per_Cov") ; As_IA_PWS  # call the function
blnk_13 <- merge(blnk_12, As_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE)  # Merge wtih the larger data frame
blnk_13[45:90,]
#####
# Ahnfeltia fastigiata
Af_IA_PWS1 <- filter(IA_PWS, Species_Name=="Ahnfeltia fastigiata")   # Filter out the species/entry of interest
Af_IA_PWS <- PerCovCalc(Af_IA_PWS1, "Ahn_fast_Mn_Per_Cov") ; Af_IA_PWS  # call the function
blnk_14 <- merge(blnk_13, Af_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE)  # Merge wtih the larger data frame
blnk_14[45:90,]
#####
# Alaria marginata
Am_IA_PWS1 <- filter(IA_PWS, Species_Name=="Alaria marginata")   # Filter out the species/entry of interest
Am_IA_PWS <- PerCovCalc(Am_IA_PWS1, "Ala_marg_Mn_Per_Cov") ; Am_IA_PWS  # call the function
blnk_15 <- merge(blnk_14, Am_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE)  # Merge wtih the larger data frame
blnk_15[45:90,]
#####
# Analipus japonicus
Aj_IA_PWS1 <- filter(IA_PWS, Species_Name=="Analipus japonicus")   # Filter out the species/entry of interest
Aj_IA_PWS <- PerCovCalc(Aj_IA_PWS1, "Ana_japo_Mn_Per_Cov") ; Aj_IA_PWS  # call the function
blnk_16 <- merge(blnk_15, Aj_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE)  # Merge wtih the larger data frame
blnk_16[45:90,]
#####
# Anthopleura sp
Anth_sp <- c("Anthopleura elegantissima","Anthopleura xanthogrammica")
Ans_IA_PWS1 <- filter(IA_PWS, Species_Name %in% Anth_sp)   # Filter out the species/entry of interest
Ans_IA_PWS <- PerCovCalc(Ans_IA_PWS1, "Anth_sp_Mn_Per_Cov") ; Ans_IA_PWS  # call the function
blnk_17 <- merge(blnk_16, Ans_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE)  # Merge wtih the larger data frame
blnk_17[45:90,]
#####
# Antithamnionella pacifica
Ap_IA_PWS1 <- filter(IA_PWS, Species_Name=="Antithamnionella pacifica")   # Filter out the species/entry of interest
Ap_IA_PWS <- PerCovCalc(Ap_IA_PWS1, "Ant_paci_Mn_Per_Cov") ; Ap_IA_PWS  # call the function
blnk_18 <- merge(blnk_17, Ap_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE)  # Merge wtih the larger data frame
blnk_18[45:90,]
#####
# Balanus / Semibalanus sp.
barnacle_sp <- c("barnacle","barnacle spat","Balanus / Semibalanus sp.","Balanus glandula","Semibalanus balanoides",
                 "Semibalanus cariosus")
BSs_IA_PWS1 <- filter(IA_PWS, Species_Name %in% barnacle_sp)   # Filter out the species/entry of interest
BSs_IA_PWS <- PerCovCalc(BSs_IA_PWS1, "Bal_Semibal_sp_Mn_Per_Cov") ; BSs_IA_PWS  # call the function
blnk_19 <- merge(blnk_18, BSs_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE)  # Merge wtih the larger data frame
blnk_19[45:90,]
#####
# Blidingia minima
Bm_IA_PWS1 <- filter(IA_PWS, Species_Name=="Blidingia minima var. minima")   # Filter out the species/entry of interest
Bm_IA_PWS <- PerCovCalc(Bm_IA_PWS1, "Bli_mini_Mn_Per_Cov") ; Bm_IA_PWS  # call the function
blnk_20 <- merge(blnk_19, Bm_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE)  # Merge wtih the larger data frame
blnk_20[45:90,]
#####
# Boreophyllum / Pyropia / Wildemania sp.
BPW_IA_PWS1 <- filter(IA_PWS, Species_Name=="Boreophyllum / Pyropia / Wildemania sp.")   # Filter out the species/entry of interest
BPW_IA_PWS <- PerCovCalc(BPW_IA_PWS1, "Bore_Pyro_Wild_Mn_Per_Cov") ; BPW_IA_PWS  # call the function
blnk_21 <- merge(blnk_20, BPW_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE)  # Merge wtih the larger data frame
blnk_21[45:90,]
#####
# Callithamnion pikeanum
Cp_IA_PWS1 <- filter(IA_PWS, Species_Name=="Callithamnion pikeanum")   # Filter out the species/entry of interest
Cp_IA_PWS <- PerCovCalc(Cp_IA_PWS1, "Cal_pike_Mn_Per_Cov") ; Cp_IA_PWS  # call the function
blnk_22 <- merge(blnk_21, Cp_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE)  # Merge wtih the larger data frame
blnk_22[45:90,]
#####
# Ceramium pacificum
Crp_IA_PWS1 <- filter(IA_PWS, Species_Name=="Ceramium pacificum")   # Filter out the species/entry of interest
Crp_IA_PWS <- PerCovCalc(Crp_IA_PWS1, "Cer_paci_Mn_Per_Cov") ; Crp_IA_PWS  # call the function
blnk_23 <- merge(blnk_2, Crp_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE)  # Merge wtih the larger data frame
blnk_23[45:90,]
#####
# Chaetomorpha / Cladophora 
Ch_Cl_sp <- c("Chaetomorpha melagonium","Chaetomorpha sp.","Cladophora / Chaetomorpha sp.")
Chl_IA_PWS1 <- filter(IA_PWS, Species_Name %in% Ch_Cl_sp)   # Filter out the species/entry of interest
Chl_IA_PWS <- PerCovCalc(Chl_IA_PWS1, "Chaet_Clad_sp_Mn_Per_Cov") ; Chl_IA_PWS  # call the function
blnk_24 <- merge(blnk_23, Chl_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE)  # Merge wtih the larger data frame
blnk_24[45:90,]
#####
# Chordaria flagelliformis
Cf_IA_PWS1 <- filter(IA_PWS, Species_Name=="Chordaria flagelliformis")   # Filter out the species/entry of interest
Cf_IA_PWS <- PerCovCalc(Cf_IA_PWS1, "Cho_flag_Mn_Per_Cov") ; Cf_IA_PWS  # call the function
blnk_25 <- merge(blnk_24, Cf_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE)  # Merge wtih the larger data frame
blnk_25[45:90,]
#####
# Chthamalus dalli
Cd_IA_PWS1 <- filter(IA_PWS, Species_Name=="Chthamalus dalli")   # Filter out the species/entry of interest
Cd_IA_PWS <- PerCovCalc(Cd_IA_PWS1, "Cht_dall_Mn_Per_Cov") ; Cd_IA_PWS  # call the function
blnk_26 <- merge(blnk_25, Cd_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE)  # Merge wtih the larger data frame
blnk_26[45:90,]
#####
# Coilodesme bulligera
Cb_IA_PWS1 <- filter(IA_PWS, Species_Name=="Coilodesme bulligera")   # Filter out the species/entry of interest
Cb_IA_PWS <- PerCovCalc(Cb_IA_PWS1, "Coi_bull_Mn_Per_Cov") ; Cb_IA_PWS  # call the function
blnk_27 <- merge(blnk_26, Cb_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE)  # Merge wtih the larger data frame
blnk_27[45:90,]
#####
# Constantinea subulifera
Cs_IA_PWS1 <- filter(IA_PWS, Species_Name=="Constantinea subulifera")   # Filter out the species/entry of interest
Cs_IA_PWS <- PerCovCalc(Cs_IA_PWS1, "Con_subu_Mn_Per_Cov") ; Cs_IA_PWS  # call the function
blnk_28 <- merge(blnk_27, Cs_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE)  # Merge wtih the larger data frame
blnk_28[45:90,]
#####
# Corallina sp.
Csp_IA_PWS1 <- filter(IA_PWS, Species_Name=="Corallina sp.")   # Filter out the species/entry of interest
Csp_IA_PWS <- PerCovCalc(Csp_IA_PWS1, "Corallina_sp_Mn_Per_Cov") ; Csp_IA_PWS  # call the function
blnk_29 <- merge(blnk_28, Csp_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE)  # Merge wtih the larger data frame
blnk_29[45:90,]
#####
# Cryptochiton stelleri
Crs_IA_PWS1 <- filter(IA_PWS, Species_Name=="Cryptochiton stelleri")   # Filter out the species/entry of interest
Crs_IA_PWS <- PerCovCalc(Crs_IA_PWS1, "Cryp_stell_Mn_Per_Cov") ; Crs_IA_PWS  # call the function
blnk_30 <- merge(blnk_29, Crs_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE)  # Merge wtih the larger data frame
blnk_30[45:90,]
#####
# Cryptopleura ruprechtiana
Cr_IA_PWS1 <- filter(IA_PWS, Species_Name=="Cryptopleura ruprechtiana")   # Filter out the species/entry of interest
Cr_IA_PWS <- PerCovCalc(Cr_IA_PWS1, "Cryp_rupr_Mn_Per_Cov") ; Cr_IA_PWS  # call the function
blnk_31 <- merge(blnk_30, Cr_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE)  # Merge wtih the larger data frame
blnk_31[45:90,]
#####
# Cryptosiphonia woodii
Cw_IA_PWS1 <- filter(IA_PWS, Species_Name=="Cryptosiphonia woodii")   # Filter out the species/entry of interest
Cw_IA_PWS <- PerCovCalc(Cw_IA_PWS1, "Cryp_wood_Mn_Per_Cov") ; Cw_IA_PWS  # call the function
blnk_32 <- merge(blnk_31, Cw_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE)  # Merge wtih the larger data frame
blnk_32[45:90,]
#####
# Desmarestia aculeata
Da_IA_PWS1 <- filter(IA_PWS, Species_Name=="Desmarestia aculeata")   # Filter out the species/entry of interest
Da_IA_PWS <- PerCovCalc(Da_IA_PWS1, "Des_acul_Mn_Per_Cov") ; Da_IA_PWS  # call the function
blnk_33 <- merge(blnk_32, Da_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE)  # Merge wtih the larger data frame
blnk_33[45:90,]
#####
# Dictyosiphon foeniculaceus
Df_IA_PWS1 <- filter(IA_PWS, Species_Name=="Dictyosiphon foeniculaceus")   # Filter out the species/entry of interest
Df_IA_PWS <- PerCovCalc(Df_IA_PWS1, "Dict_foen_Mn_Per_Cov") ; Df_IA_PWS  # call the function
blnk_34 <- merge(blnk_33, Df_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE)  # Merge wtih the larger data frame
blnk_34[45:90,]
#####
# Dumontia alaskana
Da_IA_PWS1 <- filter(IA_PWS, Species_Name=="Dumontia alaskana")   # Filter out the species/entry of interest
Da_IA_PWS <- PerCovCalc(Da_IA_PWS1, "Dum_alas_Mn_Per_Cov") ; Da_IA_PWS  # call the function
blnk_35 <- merge(blnk_34, Da_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE)  # Merge wtih the larger data frame
blnk_35[45:90,]
#####
# Ectocarpus sp.
Es_IA_PWS1 <- filter(IA_PWS, Species_Name=="Ectocarpus sp.")   # Filter out the species/entry of interest
Es_IA_PWS <- PerCovCalc(Es_IA_PWS1, "Ecto_sp_Mn_Per_Cov") ; Es_IA_PWS  # call the function
blnk_36 <- merge(blnk_35, Es_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE)  # Merge wtih the larger data frame
blnk_36[45:90,]
#####
# Elachista sp.
El_sp <- c("Elachista fucicola","Elachista sp.")
Els_IA_PWS1 <- filter(IA_PWS, Species_Name %in% El_sp)   # Filter out the species/entry of interest
Els_IA_PWS <- PerCovCalc(Els_IA_PWS1, "Elach_sp_sp_Mn_Per_Cov") ; Els_IA_PWS  # call the function
blnk_37 <- merge(blnk_36, Els_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE)  # Merge wtih the larger data frame
blnk_37[45:90,]
#####
# encrusting bryozoan
bz_IA_PWS1 <- filter(IA_PWS, Species_Name=="encrusting bryozoan")   # Filter out the species/entry of interest
bz_IA_PWS <- PerCovCalc(bz_IA_PWS1, "enc_bryz_Mn_Per_Cov") ; bz_IA_PWS  # call the function
blnk_38 <- merge(blnk_37, bz_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE)  # Merge wtih the larger data frame
blnk_38[45:90,]
#####
# encrusting coralline algae
eca_IA_PWS1 <- filter(IA_PWS, Species_Name=="encrusting coralline algae")   # Filter out the species/entry of interest
eca_IA_PWS <- PerCovCalc(eca_IA_PWS1, "enc_corall_Mn_Per_Cov") ; eca_IA_PWS  # call the function
blnk_39 <- merge(blnk_38, eca_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE)  # Merge wtih the larger data frame
blnk_39[45:90,]
#####
# Endocladia muricata
Em_IA_PWS1 <- filter(IA_PWS, Species_Name=="Endocladia muricata")   # Filter out the species/entry of interest
Em_IA_PWS <- PerCovCalc(Em_IA_PWS1, "Endo_corall_Mn_Per_Cov") ; eca_IA_PWS  # call the function
blnk_39 <- merge(blnk_38, eca_IA_PWS, by=c("Site_Name","Sample_Year"), all=TRUE)  # Merge wtih the larger data frame
blnk_39[45:90,]











# Fucus distichus
#Fd_IA_PWS <- IA_PWS %>%
#             filter(Species_Name=="Fucus distichus") %>%  # selecting out only species of interest
#             count(Site_Code, Site_Name, Sample_Year, Elevation_Position, Quadrat_Num) %>%
#             mutate(Per_Cov = (n/25)*100) %>% # calculates the percent cover of bare substrate per quadrat
#             group_by(Site_Name, Sample_Year) %>%
#             summarise(Fuc_dist_Mn_Per_Cov=mean(Per_Cov)) %>%
#             ungroup()
#Fd_IA_PWS




## Sum the percent cover for all the invert and algae columns to see if they add to 100 percent!


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


