
library(dplyr)
library(leaflet)
library(readr)
library(lubridate)

# load data

# Ozone data (8-hr)--------------------------------------------------------------------
# dir_O3 <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/Daily_O3"
# dir_O3 <- "E:/MASDAR_FK/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/Daily_O3"
dir_O3 <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/hourly_FK_new/hourly_data/test_FK/Daily_mean/Daily_O3"

EAD_O3_2013 <- read.csv(paste0(dir_O3, "/","database_EAD_2013_O3_daily.csv"))
EAD_O3_2014 <- read.csv(paste0(dir_O3, "/","database_EAD_2014_O3_daily.csv"))
EAD_O3_2015 <- read.csv(paste0(dir_O3, "/","database_EAD_2015_O3_daily.csv"))
EAD_O3_2016 <- read.csv(paste0(dir_O3, "/","database_EAD_2016_O3_daily.csv"))

DM_O3_2013 <- read.csv(paste0(dir_O3, "/","database_DM_2013_O3_daily.csv"))
DM_O3_2014 <- read.csv(paste0(dir_O3, "/","database_DM_2014_O3_daily.csv"))
DM_O3_2015 <- read.csv(paste0(dir_O3, "/","database_DM_2015_O3_daily.csv"))
DM_O3_2016 <- read.csv(paste0(dir_O3, "/","database_DM_2016_O3_daily.csv"))

NCMS_O3_2013 <- read.csv(paste0(dir_O3, "/","database_NCMS_2013_O3_daily.csv"))
NCMS_O3_2014 <- read.csv(paste0(dir_O3, "/","database_NCMS_2014_O3_daily.csv"))
NCMS_O3_2015 <- read.csv(paste0(dir_O3, "/","database_NCMS_2015_O3_daily.csv"))
NCMS_O3_2016 <- read.csv(paste0(dir_O3, "/","database_NCMS_2016_O3_daily.csv"))

# bind data together
UAE_O3 <- rbind(EAD_O3_2013, EAD_O3_2014, EAD_O3_2015, EAD_O3_2016,
                DM_O3_2013, DM_O3_2014, DM_O3_2015, DM_O3_2016,
                NCMS_O3_2013, NCMS_O3_2014, NCMS_O3_2015, NCMS_O3_2016)


# remove lines wtih NA in the Mean_8hour column
 UAE_O3 <- UAE_O3[!is.na(UAE_O3$Mean_8hour),]

# UAE_O3 <- UAE_O3 %>%
#   mutate(DateTime = ymd_hms(Date))

# # add date field only
# UAE_O3 <- UAE_O3 %>%
#   mutate(Date = date(DateTime))

# conversion from ug/m3 to ppb (WHO conversion factor)
 UAE_O3$MAX_8hour <- (UAE_O3$MAX_8hour) /1.96 

UAE_O3$Site <- as.character(UAE_O3$Site)

str(UAE_O3)



EAD <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQI/Stations_EAD_info_2.csv")
DM <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQI/Stations_DM_info_2.csv")
NCMS <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQI/Stations_NCMS_info_2.csv")

station_info <- rbind(EAD, DM, NCMS)

UAE_O3$Pollutant <- "O3"

# attach infos to ozone data
UAE_O3 <- UAE_O3 %>%
  left_join(station_info, c("Site", "Pollutant"))


names(UAE_O3)[names(UAE_O3) == 'Site.Type'] <- 'Site_Type'
# names(UAE_O3)[names(UAE_O3) == 'Value'] <- 'Max_O3_8hr'
UAE_O3$Site <- as.character(UAE_O3$Site)
names(UAE_O3)[names(UAE_O3) == 'Pollutant'] <- 'Pollutant_O3'
names(UAE_O3)[names(UAE_O3) == 'MAX_8hour'] <- 'Max_O3_8hr'


# str(UAE_O3)

UAE_O3 <- UAE_O3 %>%
  select(Date,
         Site,
         Pollutant_O3,
         Site_Type,
         Latitude,
         Longitude,
         Max_O3_8hr)

# create a field for the date & hour
UAE_O3 <- UAE_O3 %>%
  mutate(Date = date(Date))

# write_csv(UAE_O3, "D:/AQI/UAE_O3_8h_Max.csv")


# CO data (8-hr)--------------------------------------------------------------------
dir_CO <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/Daily_CO"
# dir_CO <- "E:/MASDAR_FK/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/Daily_CO"


EAD_CO_2013 <- read.csv(paste0(dir_CO, "/","database_EAD_2013_CO_daily.csv"))
EAD_CO_2014 <- read.csv(paste0(dir_CO, "/","database_EAD_2014_CO_daily.csv"))
EAD_CO_2015 <- read.csv(paste0(dir_CO, "/","database_EAD_2015_CO_daily.csv"))
EAD_CO_2016 <- read.csv(paste0(dir_CO, "/","database_EAD_2016_CO_daily.csv"))

DM_CO_2013 <- read.csv(paste0(dir_CO, "/","database_DM_2013_CO_daily.csv"))
DM_CO_2014 <- read.csv(paste0(dir_CO, "/","database_DM_2014_CO_daily.csv"))
DM_CO_2015 <- read.csv(paste0(dir_CO, "/","database_DM_2015_CO_daily.csv"))
DM_CO_2016 <- read.csv(paste0(dir_CO, "/","database_DM_2016_CO_daily.csv"))

NCMS_CO_2013 <- read.csv(paste0(dir_CO, "/","database_NCMS_2013_CO_daily.csv"))
NCMS_CO_2014 <- read.csv(paste0(dir_CO, "/","database_NCMS_2014_CO_daily.csv"))
NCMS_CO_2015 <- read.csv(paste0(dir_CO, "/","database_NCMS_2015_CO_daily.csv"))
NCMS_CO_2016 <- read.csv(paste0(dir_CO, "/","database_NCMS_2016_CO_daily.csv"))

# bind data together
UAE_CO <- rbind(EAD_CO_2013, EAD_CO_2014, EAD_CO_2015, EAD_CO_2016,
                DM_CO_2013, DM_CO_2014, DM_CO_2015, DM_CO_2016,
                NCMS_CO_2013, NCMS_CO_2014, NCMS_CO_2015, NCMS_CO_2016)

# UAE_CO <- UAE_CO %>%
#   mutate(DateTime = ymd_hms(Date))


# remove lines wtih NA in the Mean_8hour column
UAE_CO <- UAE_CO[!is.na(UAE_CO$Mean_8hour),]


# conversion from mg/m3 to ppm (WHO conversion factor)
UAE_CO$MAX_8hour <- (UAE_CO$MAX_8hour) /1.15 


UAE_CO$Site <- as.character(UAE_CO$Site)
str(UAE_CO)




# load stations infos
EAD <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQI/Stations_EAD_info_2.csv") 
DM <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQI/Stations_DM_info_2.csv") 
NCMS <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQI/Stations_NCMS_info_2.csv") 
station_info <- rbind(EAD, DM, NCMS)

UAE_CO$Pollutant <- "CO"

# station_info <- station_info %>%
#   filter(Pollutant =="CO")

# attach infos to ozone data
UAE_CO <- UAE_CO %>%
  left_join(station_info, c("Site", "Pollutant"))


names(UAE_CO)[names(UAE_CO) == 'Site.Type'] <- 'Site_Type'
# names(UAE_O3)[names(UAE_O3) == 'Value'] <- 'Max_O3_8hr'
UAE_CO$Site <- as.character(UAE_CO$Site)
names(UAE_CO)[names(UAE_CO) == 'Pollutant'] <- 'Pollutant_CO'
names(UAE_CO)[names(UAE_CO) == 'MAX_8hour'] <- 'Max_CO_8hr'



UAE_CO <- UAE_CO %>%
  select(Date,
         Site,
         Pollutant_CO,
         Site_Type,
         Latitude,
         Longitude,
         Max_CO_8hr)


# create a field for the date & hour
UAE_CO <- UAE_CO %>%
  mutate(Date = date(Date))

# write_csv(UAE_CO, "D:/AQI/UAE_CO_8h_Max.csv")

# remove stations with outliers
UAE_CO <- UAE_CO %>%
  filter(!Site == "Bain Aljesrain") %>%
  filter(!Site == "Karama") %>%
  filter(!Site == "Safa") %>%
  filter(!Site == "Zabeel")



##################################################################################
# AQ data (24-hr daily averages, filtered from outliers 4-box plots)-------------

dir_AQ <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/Daily filtered with 4 boxplot"
# dir_AQ <- "E:/MASDAR_FK/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/daily moved/daily_filtered_4_box"
dir_AQ <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/hourly_FK_new/hourly_data/test_FK/Daily_mean/daily_filtered_4_box"


EAD_AQ_2013 <- read.csv(paste0(dir_AQ, "/","database_EAD_ 2013 _daily_filtered.csv"))
EAD_AQ_2014 <- read.csv(paste0(dir_AQ, "/","database_EAD_ 2014 _daily_filtered.csv"))
EAD_AQ_2015 <- read.csv(paste0(dir_AQ, "/","database_EAD_ 2015 _daily_filtered.csv"))
EAD_AQ_2016 <- read.csv(paste0(dir_AQ, "/","database_EAD_ 2016 _daily_filtered.csv"))

DM_AQ_2013 <- read.csv(paste0(dir_AQ, "/","database_DM_ 2013 _daily_filtered.csv"))
DM_AQ_2014 <- read.csv(paste0(dir_AQ, "/","database_DM_ 2014 _daily_filtered.csv"))
DM_AQ_2015 <- read.csv(paste0(dir_AQ, "/","database_DM_ 2015 _daily_filtered.csv"))
DM_AQ_2016 <- read.csv(paste0(dir_AQ, "/","database_DM_ 2016 _daily_filtered.csv"))

NCMS_AQ_2013 <- read.csv(paste0(dir_AQ, "/","database_DM_ 2013 _daily_filtered.csv"))
NCMS_AQ_2014 <- read.csv(paste0(dir_AQ, "/","database_DM_ 2014 _daily_filtered.csv"))
NCMS_AQ_2015 <- read.csv(paste0(dir_AQ, "/","database_DM_ 2015 _daily_filtered.csv"))
NCMS_AQ_2016 <- read.csv(paste0(dir_AQ, "/","database_DM_ 2016 _daily_filtered.csv"))

# bind data together
UAE_AQ <- rbind(EAD_AQ_2013, EAD_AQ_2014, EAD_AQ_2015, EAD_AQ_2016,
                DM_AQ_2013, DM_AQ_2014, DM_AQ_2015, DM_AQ_2016,
                NCMS_AQ_2013, NCMS_AQ_2014, NCMS_AQ_2015, NCMS_AQ_2016)

UAE_AQ$Site <- as.character(UAE_AQ$Site)
UAE_AQ$Site_Type <- as.character(UAE_AQ$Site_Type)
UAE_AQ$Pollutant <- as.character(UAE_AQ$Pollutant)
 str(UAE_AQ)



# names(UAE_AQ)[names(UAE_AQ) == 'Daily_mean'] <- 'Value'
# UAE_AQ$Date <- as.POSIXct(as.Date(UAE_AQ$Date, "%Y-%m-%d")) 
UAE_AQ$Date <- as.Date(UAE_AQ$Date)
str(UAE_AQ)

UAE_AQ <- UAE_AQ %>%
  select(Date,
         Site,
         Pollutant,
         Site_Type,
         Latitude,
         Longitude,
         Daily_mean)

str(UAE_AQ)

# select only PM25
UAE_PM25 <- UAE_AQ %>%
  filter(Pollutant == c("PM2.5"))

names(UAE_PM25)[names(UAE_PM25) == 'Pollutant'] <- 'Pollutant_PM25'

str(UAE_PM25)

# select only PM10
UAE_PM10 <- UAE_AQ %>%
  filter(Pollutant == c("PM10"))

names(UAE_PM10)[names(UAE_PM10) == 'Pollutant'] <- 'Pollutant_PM10'

names(UAE_PM25)[names(UAE_PM25) == 'Daily_mean'] <- 'PM25_24hr'
names(UAE_PM10)[names(UAE_PM10) == 'Daily_mean'] <- 'PM10_24hr'


str(UAE_PM25)


# select only NO2
UAE_NO2 <- UAE_AQ %>%
  filter(Pollutant == c("NO2"))

names(UAE_NO2)[names(UAE_NO2) == 'Pollutant'] <- 'Pollutant_NO2'

str(UAE_NO2)


# select only SO2
UAE_SO2 <- UAE_AQ %>%
  filter(Pollutant == c("SO2"))

names(UAE_SO2)[names(UAE_SO2) == 'Pollutant'] <- 'Pollutant_SO2'

str(UAE_SO2)




# # SO2 and NO2 (1-hr)---------------------------------------------------------------------
# # for EAD use filtered data (4 boxplot)
# dir_SO2_NO2 <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/R files/filtered_4_box" 
# # dir_SO2_NO2 <- "E:/MASDAR_FK/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/R files/filtered_4_box" 
# 
# EAD_SO2_NO2_2013 <- read_csv(paste0(dir_SO2_NO2, "/","database_EAD_ 2013 _hourly_filtered.csv"))
# EAD_SO2_NO2_2014 <- read_csv(paste0(dir_SO2_NO2, "/","database_EAD_ 2014 _hourly_filtered.csv"))
# EAD_SO2_NO2_2015 <- read_csv(paste0(dir_SO2_NO2, "/","database_EAD_ 2015 _hourly_filtered.csv"))
# EAD_SO2_NO2_2015$DateTime <- EAD_SO2_NO2_2015$DateTime +3
# EAD_SO2_NO2_2016 <- read_csv(paste0(dir_SO2_NO2, "/","database_EAD_ 2016 _hourly_filtered.csv"))
# EAD_SO2_NO2_2016$DateTime <- EAD_SO2_NO2_2016$DateTime +3
# 
# DM_SO2_NO2_2013 <- read_csv(paste0(dir_SO2_NO2, "/","database_DM_ 2013 _hourly_filtered.csv"))
# DM_SO2_NO2_2013$DateTime <- DM_SO2_NO2_2013$DateTime +3
# DM_SO2_NO2_2014 <- read_csv(paste0(dir_SO2_NO2, "/","database_DM_ 2014 _hourly_filtered.csv"))
# DM_SO2_NO2_2014$DateTime <- DM_SO2_NO2_2014$DateTime +3
# DM_SO2_NO2_2015 <- read_csv(paste0(dir_SO2_NO2, "/","database_DM_ 2015 _hourly_filtered.csv"))
# DM_SO2_NO2_2015$DateTime <- DM_SO2_NO2_2015$DateTime +3
# DM_SO2_NO2_2016 <- read_csv(paste0(dir_SO2_NO2, "/","database_DM_ 2016 _hourly_filtered.csv"))
# DM_SO2_NO2_2016$DateTime <- DM_SO2_NO2_2016$DateTime +3
# 
# NCMS_SO2_NO2_2013 <- read_csv(paste0(dir_SO2_NO2, "/","database_NCMS_ 2013 _hourly_filtered.csv"))
# NCMS_SO2_NO2_2014 <- read_csv(paste0(dir_SO2_NO2, "/","database_NCMS_ 2014 _hourly_filtered.csv"))
# NCMS_SO2_NO2_2015 <- read_csv(paste0(dir_SO2_NO2, "/","database_NCMS_ 2015 _hourly_filtered.csv"))
# NCMS_SO2_NO2_2016 <- read_csv(paste0(dir_SO2_NO2, "/","database_NCMS_ 2016 _hourly_filtered.csv"))
# 
# 
# SO2_NO2_all <- rbind(EAD_SO2_NO2_2013, EAD_SO2_NO2_2014, EAD_SO2_NO2_2015, EAD_SO2_NO2_2016,
#                      DM_SO2_NO2_2013, DM_SO2_NO2_2014, DM_SO2_NO2_2015, DM_SO2_NO2_2016,
#                      NCMS_SO2_NO2_2013, NCMS_SO2_NO2_2014, NCMS_SO2_NO2_2015, NCMS_SO2_NO2_2016)
# 
# SO2_NO2_all <- SO2_NO2_all %>%
#   select(DateTime,
#          Site,
#          Pollutant,
#          Site_Type,
#          Latitude,
#          Longitude,
#          Value) 
# 
# str(SO2_NO2_all)
# 
# # change site names....
# SO2_NO2_all$Site  <- ifelse(grepl("ALAinIslamicIns", SO2_NO2_all$Site, ignore.case = TRUE), 
#                        "Al Ain Islamic Ins", SO2_NO2_all$Site)
# SO2_NO2_all$Site  <- ifelse(grepl("ALAinStreet", SO2_NO2_all$Site, ignore.case = TRUE), 
#                        "Al Ain Street", SO2_NO2_all$Site)
# SO2_NO2_all$Site  <- ifelse(grepl("AlMafraq", SO2_NO2_all$Site, ignore.case = TRUE), 
#                        "Al Mafraq", SO2_NO2_all$Site)
# SO2_NO2_all$Site  <- ifelse(grepl("AlQua0x27a", SO2_NO2_all$Site, ignore.case = TRUE), 
#                        "Al Qua'a", SO2_NO2_all$Site)
# SO2_NO2_all$Site  <- ifelse(grepl("AlRuwais", SO2_NO2_all$Site, ignore.case = TRUE), 
#                        "Al Ruwais", SO2_NO2_all$Site)
# SO2_NO2_all$Site  <- ifelse(grepl("AlTawia", SO2_NO2_all$Site, ignore.case = TRUE), 
#                        "Al Tawia", SO2_NO2_all$Site)
# SO2_NO2_all$Site <- ifelse(grepl("Bain Aljesrain", SO2_NO2_all$Site, ignore.case = TRUE), 
#                       "Bain Al Jesrain", SO2_NO2_all$Site)
# SO2_NO2_all$Site <- ifelse(grepl("BainAljesrain", SO2_NO2_all$Site, ignore.case = TRUE), 
#                       "Bain Al Jesrain", SO2_NO2_all$Site)
# SO2_NO2_all$Site  <- ifelse(grepl("baniyasSchool", SO2_NO2_all$Site, ignore.case = TRUE), 
#                        "Baniyas School", SO2_NO2_all$Site)
# SO2_NO2_all$Site  <- ifelse(grepl("BidaZayed", SO2_NO2_all$Site, ignore.case = TRUE), 
#                        "Bida Zayed", SO2_NO2_all$Site)
# SO2_NO2_all$Site  <- ifelse(grepl("E11Road", SO2_NO2_all$Site, ignore.case = TRUE), 
#                        "E11 Road", SO2_NO2_all$Site)
# SO2_NO2_all$Site  <- ifelse(grepl("GayathiSchool", SO2_NO2_all$Site, ignore.case = TRUE), 
#                        "Gayathi School", SO2_NO2_all$Site)
# SO2_NO2_all$Site  <- ifelse(grepl("Habshan", SO2_NO2_all$Site, ignore.case = TRUE), 
#                        "Habshan", SO2_NO2_all$Site)
# SO2_NO2_all$Site  <- ifelse(grepl("HamdanStreet", SO2_NO2_all$Site, ignore.case = TRUE), 
#                        "Hamdan Street",SO2_NO2_all$Site)
# SO2_NO2_all$Site  <- ifelse(grepl("KhadejaPrimarySchool", SO2_NO2_all$Site, ignore.case = TRUE), 
#                        "Khadeja Primary School", SO2_NO2_all$Site)
# SO2_NO2_all$Site  <- ifelse(grepl("KhalifaCityA", SO2_NO2_all$Site, ignore.case = TRUE), 
#                        "Khalifa City A", SO2_NO2_all$Site)
# SO2_NO2_all$Site <- ifelse(grepl("KhalifaHighSchool", SO2_NO2_all$Site, ignore.case = TRUE), 
#                       "Khalifa High School", SO2_NO2_all$Site)
# SO2_NO2_all$Site <- ifelse(grepl("LiwaOasis", SO2_NO2_all$Site, ignore.case = TRUE), 
#                       "Liwa Oasis", SO2_NO2_all$Site)
# SO2_NO2_all$Site <- ifelse(grepl("Mussafah", SO2_NO2_all$Site, ignore.case = TRUE),
#                       "Mussafah", SO2_NO2_all$Site)
# SO2_NO2_all$Site  <- ifelse(grepl("Sweihan", SO2_NO2_all$Site, ignore.case = TRUE), 
#                        "Sweihan", SO2_NO2_all$Site)
# SO2_NO2_all$Site  <- ifelse(grepl("Zakher", SO2_NO2_all$Site, ignore.case = TRUE), 
#                        "Zakher", SO2_NO2_all$Site)
# SO2_NO2_all$Site  <- ifelse(grepl("JEBELALIVILLAGE", SO2_NO2_all$Site, ignore.case = TRUE), 
#                        "JEBEL ALI VILLAGE", SO2_NO2_all$Site)
# SO2_NO2_all$Site  <- ifelse(grepl("EMIRATESHILLS", SO2_NO2_all$Site, ignore.case = TRUE), 
#                        "EMIRATES HILLS", SO2_NO2_all$Site)
# SO2_NO2_all$Site  <- ifelse(grepl("DUBAIAIRPORT", SO2_NO2_all$Site, ignore.case = TRUE), 
#                        "DUBAI AIR PORT", SO2_NO2_all$Site)
# SO2_NO2_all$Site  <- ifelse(grepl("DUBAI AIRPORT", SO2_NO2_all$Site, ignore.case = TRUE), 
#                        "DUBAI AIR PORT", SO2_NO2_all$Site)
# SO2_NO2_all$Site  <- ifelse(grepl("JEBELALIPORT", SO2_NO2_all$Site, ignore.case = TRUE), 
#                        "JEBEL ALI PORT", SO2_NO2_all$Site)
# SO2_NO2_all$Site  <- ifelse(grepl("SHK0x2EZAYEDROAD", SO2_NO2_all$Site, ignore.case = TRUE), 
#                        "SHK. ZAYED ROAD", SO2_NO2_all$Site)
# SO2_NO2_all$Site  <- ifelse(grepl("SHK0x2EMOHD0x2EBINZAYEDROAD", SO2_NO2_all$Site, ignore.case = TRUE), 
#                        "SHK. MOHD. BIN ZAYED ROAD", SO2_NO2_all$Site)
# SO2_NO2_all$Site  <- ifelse(grepl("AlHamriyah", SO2_NO2_all$Site, ignore.case = TRUE), 
#                        "Al Hamriyah", SO2_NO2_all$Site)
# SO2_NO2_all$Site  <- ifelse(grepl("ELdErLyHouse", SO2_NO2_all$Site, ignore.case = TRUE), 
#                        "Elderly House", SO2_NO2_all$Site)
# SO2_NO2_all$Site  <- ifelse(grepl("AlJeer", SO2_NO2_all$Site, ignore.case = TRUE), 
#                        "Al Jeer", SO2_NO2_all$Site)
# SO2_NO2_all$Site  <- ifelse(grepl("AlQasimiyah", SO2_NO2_all$Site, ignore.case = TRUE), 
#                        "Al Qasimiyah ", SO2_NO2_all$Site)
# 
# # select SO2
# UAE_SO2 <- SO2_NO2_all %>%
#   filter(Pollutant == "SO2") 
# 
# # filter our negative values
# UAE_SO2 <- UAE_SO2 %>%
#   filter(Value >= 0 | is.na(Value))
# 
# 
# # create a field for the date & hour
# UAE_SO2 <- UAE_SO2 %>%
#   mutate(Date = date(DateTime),
#          Hour = hour(DateTime))
# 
# 
# 
# # select NO2
# UAE_NO2 <- SO2_NO2_all %>%
#   filter(Pollutant == "NO2") 
# 
# # filter our negative values
# UAE_NO2 <- UAE_NO2 %>%
#   filter(Value >= 0 | is.na(Value))
# 
# # create a field for the date & hour
# UAE_NO2 <- UAE_NO2 %>%
#   mutate(Date = date(DateTime),
#          Hour = hour(DateTime))


# names(SO2_NO2_all)[names(SO2_NO2_all) == 'DateTime'] <- 'Date'



# conversion from ug/m3 to ppb (WHO conversion factor)
# SO2 
UAE_SO2$Daily_mean <- UAE_SO2$Daily_mean /2.62

# NO2 
UAE_NO2$Daily_mean <- UAE_NO2$Daily_mean /1.88

names(UAE_SO2)[names(UAE_SO2) == 'Daily_mean'] <- 'SO2_24hr'
names(UAE_NO2)[names(UAE_NO2) == 'Daily_mean'] <- 'NO2_24hr'

# # filter out ozone data (daily average)
# UAE_AQ <- UAE_AQ %>%
#   filter(Pollutant != "O3")

########################################
### Bind all Data Together #############
########################################

######################## join NO2, SO2,  O3 and CO ######################################################

# QQQ<- cbind(SO2_NO2_all[,1:2], SO2_NO2_all[,4:6])

ZZZ <- UAE_NO2 %>%
  left_join(UAE_SO2, c("Date",   "Site","Site_Type", "Latitude", "Longitude"))


BBB <- ZZZ %>%
  left_join(UAE_O3, by = c("Date", "Site", "Site_Type", "Latitude", "Longitude"))

CCC <- BBB %>%
  left_join(UAE_CO, by = c("Date", "Site", "Site_Type", "Latitude", "Longitude"))

# save intermediate data into an R obejct
# save(CCC, UAE_AQ,UAE_CO,UAE_NO2,UAE_O3,UAE_PM10, UAE_PM25,UAE_SO2, file="D:/AQI/saves.Rdata")

########################################################################################################
# restart R and clean cash memory
# .rs.restartR()
# 
# library(dplyr)
# library(leaflet)
# library(readr)
# library(lubridate)
# 
# load("D:/AQI/saves.Rdata")
# 
# 
# UAE_PM25$Date <- as.POSIXct(as.Date(UAE_PM25$Date, "%Y-%m-%d")) 
# UAE_PM25 <- UAE_PM25 %>%
#   mutate(Date = date(Date))
# str(UAE_PM25)
# 
# unique(UAE_PM25$Site)

UAE_PM <- cbind(UAE_PM10, UAE_PM25$PM25_24hr)

# rm(AAA, BBB, QQQ)


# join PM2.5 and PM10 #########################

AQ_data <- CCC %>%
  left_join(UAE_PM, by = c("Date", "Site", "Latitude", "Longitude"))

head(AQ_data)

AQ_data_clean <- AQ_data %>%
  select(- Site_Type.x,
         - Site_Type.y)

head(AQ_data_clean)

# rename PM25 column
names(AQ_data_clean)[names(AQ_data_clean) == 'UAE_PM25$PM25_24hr'] <- 'PM25_24hr'

head(AQ_data_clean)


# make some cross checks 
ab<- filter( AQ_data, AQ_data$Site == NA)
ab<- filter( AQ_data, AQ_data$Latitude ==  NA)
ab<- filter( AQ_data, AQ_data$Longitude ==  NA)
ab<- filter( AQ_data, AQ_data$Site ==  "Hamdan Street")


 save(AQ_data_clean, file="D:/AQI/AQ_data_all_clean_24h_new.Rdata")
 
 
 
############################################################################# 
########## END ##############################################################
#############################################################################
#############################################################################
