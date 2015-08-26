#####################################################################
### Temperature and Mussel data for potential cast study analysis ###
###  script by R.E.Blake    Aug 2015                              ###
#####################################################################
# Selecting Temperature and Mussel columns from output of GW_Nearshore_Data_Manip script
TMuss <- blnk_full %>%
         select(Site_Name,Sample_Year,Region,SiteID,Longitude,Latitude,Mus_Mn_n_m2,Mus_Mn_Size_mm,
                Mus_IA_PWS,WTemp_Mean_C,WTemp_Max_C,WTemp_Min_C,WTemp_Var_C) %>%
         filter(WTemp_Mean_C != "NA")
names(TMuss)    
#######################################
library(psych)
pairs.panels(TMuss[,c(7:13)],smooth=F,density=T,ellipses=F,lm=T,digits=3,scale=T)


#######################################
library(ggplot2) ;  library(reshape2); library(grid); library(plyr); library(proto)
#######################################
### MAKE MY OWN THEME TO SAVE LINES OF CODE
theme_boxplot <- function(base_size = 12){
 theme_bw(base_size) %+replace%
   theme(legend.key.size=unit(15,"points"),
         legend.text=element_text(size=I(13)),
         legend.key=element_blank(),
         legend.title=element_blank(),
         legend.position="none",
         plot.margin=unit(c(0.25,2,0.25,2), "lines"), # respectively: top, right, bottom, left; refers to margin *outside* labels; default is c(1,1,0.5,0.5)
         panel.border=element_rect(colour='black', fill = NA),
         panel.margin=unit(0,"lines"),
         axis.ticks.length=unit(1,"mm"),
         axis.ticks.margin = unit(0, "lines"),
         axis.text=element_text(size=15),
         axis.title.x=element_text(hjust=.5, vjust=-.01, size=17),
         axis.title.y=element_text(size=17, angle=90, hjust=0.5, vjust=1),
         panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(),
         strip.text.x=element_text(size=14),
         strip.background=element_rect(colour='black', fill='white'))
}  
##########################################
# Below is from: https://stat.ethz.ch/pipermail/r-help/2011-November/295230.html
# Fit a linear model to the data and save the model object:
mod <- lm(Mus_Mn_n_m2~Temp_Min_C, data=TMuss)
# Create a list of character strings - the first component
# produces the fitted model, the second produces a
# string to compute R^2, but in plotmath syntax.
rout <- list(paste('Fitted model:',round(coef(mod)[1],3),' + ',
                   round(coef(mod)[2],3), 'x',sep = ''),
             paste('R^2 == ',round(summary(mod)[['r.squared']],3),
                   sep=''))
TMin_NMus <- ggplot(TMuss, aes(Temp_Min_C,Mus_Mn_n_m2)) + geom_point() +
               #xlab("PhiCO2") + ylab("PhiPS2") +
               geom_smooth(method=lm) + theme_boxplot() + 
               geom_text(aes(x=-8, y=900, label=rout[[2]]), 
                         hjust=0, parse=TRUE)
TMin_NMus
#####
mod1 <- lm(Mus_Mn_n_m2~Temp_Mean_C, data=TMuss)
rout1 <- list(paste('Fitted model:',round(coef(mod1)[1],3),' + ',
                   round(coef(mod1)[2],3), 'x',sep = ''),
              paste('R^2 == ',round(summary(mod1)[['r.squared']],3),
                    sep=''))
TMn_NMus <- ggplot(TMuss, aes(Temp_Mean_C,Mus_Mn_n_m2)) + geom_point() +
              geom_smooth(method=lm) + theme_boxplot() + 
              geom_text(aes(x=9, y=24, label=rout1[[2]]), 
                        hjust=0, parse=TRUE)
TMn_NMus
#####
mod2 <- lm(Mus_Mn_Size_mm~Temp_Min_C, data=TMuss)
rout2 <- list(paste('Fitted model:',round(coef(mod2)[1],3),' + ',
                   round(coef(mod2)[2],3), 'x',sep = ''),
              paste('R^2 == ',round(summary(mod2)[['r.squared']],3),
                    sep=''))
TMin_SzMus2 <- ggplot(TMuss, aes(Temp_Min_C,Mus_Mn_Size_mm)) + geom_point() +
               geom_smooth(method=lm) + theme_boxplot() + 
               geom_text(aes(x=-5, y=25, label=rout2[[2]]), 
                         hjust=0, parse=TRUE)
TMin_SzMus2 
#####
mod3 <- lm(Mus_Mn_Size_mm~Temp_Mean_C, data=TMuss)
rout3 <- list(paste('Fitted model:',round(coef(mod3)[1],3),' + ',
                   round(coef(mod3)[2],3), 'x',sep = ''),
              paste('R^2 == ',round(summary(mod3)[['r.squared']],3),
                    sep=''))
TMn_SzMus2 <- ggplot(TMuss, aes(Temp_Mean_C,Mus_Mn_Size_mm)) + geom_point() +
               geom_smooth(method=lm) + theme_boxplot() + 
               geom_text(aes(x=5, y=23, label=rout3[[2]]), 
                         hjust=0, parse=TRUE)
TMn_SzMus2 
#####
