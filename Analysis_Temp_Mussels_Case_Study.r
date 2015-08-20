#####################################################################
### Temperature and Mussel data for potential cast study analysis ###
###  script by R.E.Blake    Aug 2015                              ###
#####################################################################
# Selecting Temperature and Mussel columns from output of GW_Nearshore_Data_Manip script
TMuss <- blnk_full %>%
         select(Site_Name,Sample_Year,Region,SiteID,Longitude,Latitude,Mus_Mn_n_m2,Mus_Mn_Size_mm,
                mussel_Mn_Per_Cov,Temp_Mean_C,Temp_Max_C,Temp_Min_C,Temp_Var_C) %>%
         filter(Temp_Mean_C != "NA")
      




