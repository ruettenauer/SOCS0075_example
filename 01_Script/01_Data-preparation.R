#### Data Prep          ####
#### Tobias Ruettenauer ####
#### 2020/ 01 / 31      ####

# Packages
library(WDI)

# Working Directory
setwd("C:/work/Lehre/Dissertation_module/SOCS0075_2023/Example_project/02_Data")




################
### Data API ###
################

# Search GDP per capita (log-transformed)
WDIsearch("CO2.*capita")

# Define countries, indicators form above, and time period
wd.df <- WDI(country = "all", 
             indicator = c('population' = "SP.POP.TOTL", 
                           'gdp_pc' = "NY.GDP.PCAP.KD", 
                           'co2_pc' = "EN.ATM.CO2E.PC",
                           'nox_pc' = "EN.ATM.NOXE.PC",
                           'pm25_mean' = "EN.ATM.PM25.MC.M3",
                           'parliament_women_pt' = "SG.GEN.PARL.ZS",
                           'gini'  = "SI.POV.GINI", 
                           'fossil_fuel_pt' = "EG.USE.COMM.FO.ZS", 
                           'taxes_total' = "IC.TAX.TOTL.CP.ZS"),
             extra = TRUE,
             start = 2000, end = 2019)

# Drop all country aggregates
wd.df <- wd.df[which(wd.df$region != "Aggregates"), ]

# Save data
save(wd.df, file = "WDI_short.RData")