
library(dplyr)
library(plyr)
library(leaflet)
library(readr)
library(lubridate)
library(ggplot2)


# load data

##################################################################################
# AQ data (24-hr daily averages, filtered from outliers 4-box plots)-------------

dir_AQ <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/daily moved/daily_filtered_4_box"
# dir_AQ <- "E:/MASDAR_FK/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/daily moved/daily_filtered_4_box"


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


# change site names....
UAE_AQ$Site  <- ifelse(grepl("ALAinIslamicIns", UAE_AQ$Site, ignore.case = TRUE), 
                       "Al Ain Islamic Ins", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("ALAinStreet", UAE_AQ$Site, ignore.case = TRUE), 
                       "Al Ain Street", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("AlMafraq", UAE_AQ$Site, ignore.case = TRUE), 
                       "Al Mafraq", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("AlQua0x27a", UAE_AQ$Site, ignore.case = TRUE), 
                       "Al Qua'a", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("AlRuwais", UAE_AQ$Site, ignore.case = TRUE), 
                       "Al Ruwais", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("AlTawia", UAE_AQ$Site, ignore.case = TRUE), 
                       "Al Tawia", UAE_AQ$Site)
UAE_AQ$Site <- ifelse(grepl("Bain Aljesrain", UAE_AQ$Site, ignore.case = TRUE), 
                      "Bain Al Jesrain", UAE_AQ$Site)
UAE_AQ$Site <- ifelse(grepl("BainAljesrain", UAE_AQ$Site, ignore.case = TRUE), 
                      "Bain Al Jesrain", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("baniyasSchool", UAE_AQ$Site, ignore.case = TRUE), 
                       "Baniyas School", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("BidaZayed", UAE_AQ$Site, ignore.case = TRUE), 
                       "Bida Zayed", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("E11Road", UAE_AQ$Site, ignore.case = TRUE), 
                       "E11 Road", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("GayathiSchool", UAE_AQ$Site, ignore.case = TRUE), 
                       "Gayathi School", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("Habshan", UAE_AQ$Site, ignore.case = TRUE), 
                       "Habshan", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("HamdanStreet", UAE_AQ$Site, ignore.case = TRUE), 
                       "Hamdan Street",UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("KhadejaPrimarySchool", UAE_AQ$Site, ignore.case = TRUE), 
                       "Khadeja Primary School", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("KhalifaCityA", UAE_AQ$Site, ignore.case = TRUE), 
                       "Khalifa City A", UAE_AQ$Site)
UAE_AQ$Site <- ifelse(grepl("KhalifaHighSchool", UAE_AQ$Site, ignore.case = TRUE), 
                      "Khalifa High School", UAE_AQ$Site)
UAE_AQ$Site <- ifelse(grepl("LiwaOasis", UAE_AQ$Site, ignore.case = TRUE), 
                      "Liwa Oasis", UAE_AQ$Site)
UAE_AQ$Site <- ifelse(grepl("Mussafah", UAE_AQ$Site, ignore.case = TRUE),
                      "Mussafah", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("Sweihan", UAE_AQ$Site, ignore.case = TRUE), 
                       "Sweihan", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("Zakher", UAE_AQ$Site, ignore.case = TRUE), 
                       "Zakher", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("JEBELALIVILLAGE", UAE_AQ$Site, ignore.case = TRUE), 
                       "JEBEL ALI VILLAGE", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("EMIRATESHILLS", UAE_AQ$Site, ignore.case = TRUE), 
                       "EMIRATES HILLS", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("DUBAIAIRPORT", UAE_AQ$Site, ignore.case = TRUE), 
                       "DUBAI AIR PORT", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("DUBAI AIRPORT", UAE_AQ$Site, ignore.case = TRUE), 
                       "DUBAI AIR PORT", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("JEBELALIPORT", UAE_AQ$Site, ignore.case = TRUE), 
                       "JEBEL ALI PORT", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("SHK0x2EZAYEDROAD", UAE_AQ$Site, ignore.case = TRUE), 
                       "SHK. ZAYED ROAD", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("SHK0x2EMOHD0x2EBINZAYEDROAD", UAE_AQ$Site, ignore.case = TRUE), 
                       "SHK. MOHD. BIN ZAYED ROAD", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("AlHamriyah", UAE_AQ$Site, ignore.case = TRUE), 
                       "Al Hamriyah", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("ELdErLyHouse", UAE_AQ$Site, ignore.case = TRUE), 
                       "Elderly House", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("AlJeer", UAE_AQ$Site, ignore.case = TRUE), 
                       "Al Jeer", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("AlQasimiyah", UAE_AQ$Site, ignore.case = TRUE), 
                       "Al Qasimiyah ", UAE_AQ$Site)


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



################################################################################
#### correlation PM25 vs PM10 ##################################################
PM10_PM25 <- cbind(UAE_PM10[1], UAE_PM10$PM10_24hr, UAE_PM25$PM25_24hr)
colnames(PM10_PM25) <- c("Date", "PM10", "PM25")

# remove all lines with NA
PM10_PM25 <- na.omit(PM10_PM25)
PM10_PM25 <- as.data.frame(PM10_PM25)




## get the months of observations
PM10_PM25$month <- factor(format(PM10_PM25$Date, format = "%b"), levels = month.abb)

## Define seasons
PM10_PM25$season <- character(length = nrow(PM10_PM25))
PM10_PM25$season[PM10_PM25$month %in% month.abb[c(1:2)]] <- "winter"
PM10_PM25$season[PM10_PM25$month %in% month.abb[c(12)]] <- "winter"
PM10_PM25$season[PM10_PM25$month %in% month.abb[c(3:5)]] <- "spring"
PM10_PM25$season[PM10_PM25$month %in% month.abb[c(6:8)]] <- "summer"
PM10_PM25$season[PM10_PM25$month %in% month.abb[c(9:11)]] <- "fall"
PM10_PM25$season <- factor(PM10_PM25$season, levels = c("winter","spring","summer","fall"))



#### this funtion FORCE regression to pass through the origin #######################

lm_eqn <- function(df){
  m <- lm(PM25 ~ -1 + PM10, df);
  eq <- substitute(italic(y) == b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(b = format(coef(m)[1], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}
###################################################################################



# plot with regression line-----

# jpeg('E:/MASDAR_FK/Air Quality/Phase 1/Pathflow of Phase I_DG/Sat_AOD_Correlation/PM25_vs_AOD.jpg',
jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQI/PM10_PM25_corr.jpg',    
     quality = 100, bg = "white", res = 200, width = 7, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


# define regression equation for each season
eq <- ddply(PM10_PM25, .(season),lm_eqn)


# ggplot(PM25_AOD, aes(x=Val_AOD, y=Val_PM25, color = season)) +
ggplot(PM10_PM25, aes(x=PM10, y=PM25)) +
  theme_bw() +
  # geom_point(size = 2) +
  geom_jitter(colour=alpha("black",0.15)) +
  facet_grid(season ~ .) +
  theme( strip.text = element_text(size = 18)) + 
  scale_color_manual(values = c("#ff0000", "#0000ff", "#000000", "#ffb732")) + 
  # geom_smooth(method="lm") +  # Add linear regression line
  geom_smooth(method = "lm", formula = y ~ -1 + x) +  # force fit through the origin
  ylab(expression(paste(PM[2.5], " (µg/",m^3, ")"))) +
  xlab(expression(paste(PM[10], " (µg/",m^3, ")"))) +
  ylim(c(0,400)) + 
  xlim(c(0,400)) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=13)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=12),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=13)) +
  
  geom_text(data = eq, aes(x = 320, y = 200, label = V1),
            parse = TRUE, inherit.aes=FALSE, size = 5, color = "blue" ) +
  facet_grid(season ~.)


par(oldpar)
dev.off()

