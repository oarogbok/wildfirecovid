#-----------------------------------
# Covid & Air Pollution Project
# Funmi Arogbokun 
# January 10, 2021
#-------------------------------------  

# tidyverse
library(dplyr)
library(readr)
library(tidyr)
library(magrittr)
# plotting libraries
library(ggplot2)
library(ggmap)
library(gridExtra)
# spatial libraries
install.packages("sf")
install.packages("sp")
install.packages("maptools")
library(sf)
library(sp)
library(rgdal)
library(rgeos)
library(maptools) #found online in github forum - to use fortify function in creating maps https://github.com/tidyverse/ggplot2/issues/1447

    # reading in data and setting working directory
    setwd("/Users/funmiarogbokun/Desktop/CZ Biohub Contractor/Wildfires & Covid Project/EPA Data/Air Pollution - CA WA OR")
    pollution <- read.csv("Arogbokun_daily.csv", stringsAsFactors = F, header = T) 
   
    #subgroups by state
    CA_only <- filter (pollution, State.Name == "California")
    
    #subgroups by pollutant
    CA_NO2 <- filter (CA_only, Parameter.Name == "Nitrogen dioxide (NO2)")
    CA_Ozone <- filter (CA_only, Parameter.Name == "Ozone")
    CA_PM25 <- filter (CA_only, Parameter.Name == "PM2.5 - Local Conditions")
    CA_PM10 <- filter (CA_only, Parameter.Name == "PM10 Total 0-10um STP")
    CA_PM25AQI <- filter (CA_only, Parameter.Name == "Acceptable PM2.5 AQI & Speciation Mass")

################################ covid #
    
setwd("/Users/funmiarogbokun/Desktop/CZ Biohub Contractor/Wildfires & Covid Project/Maps_R/CA_Counties")
#sf is new and easier to use than sp package - UNC R
#reading in the shapefile
CA_counties_sf = st_read("CA_Counties_TIGER2016.shp",stringsAsFactors = F)

str(CA_counties_sf) #shows us this is an sf and a data.frame; geometry column has the polygon info
CA_counties_sf$geometry #is also the same as saying CA_counties_sf %<% geometry
plot(CA_counties_sf$geometry) #is also the same as saying plot(CA_counties_sf %<% geometry) OR this: CA_counties_sf %<% geometry %<% plot

#finding information on the coordinate reference system - want to project the shape file  
st_crs(CA_counties_sf) #the unique ID is --> EPSG = 3857, proj4string = ""PROJ4","+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"
    # Below will assign it the correct projection from spatialreference.org --> doing this allows us to be able to do maps with multiple layers
    CA_counties_sf_projection = CA_counties_sf %>% 
      st_transform(2870, 2871, 2872, 2873, 2874, 2875) #2870 - 2875 are the ESPG numbers for CA zones 1 to 6
    CA_counties_sf_projection_zone1 = CA_counties_sf %>% 
      st_transform(2870)
    CA_counties_sf_projection_zone2 = CA_counties_sf %>% 
      st_transform(2871)
    CA_counties_sf_projection_zone3 = CA_counties_sf %>% 
      st_transform(2872)
    CA_counties_sf_projection_zone4 = CA_counties_sf %>% 
      st_transform(2873)
    CA_counties_sf_projection_zone5 = CA_counties_sf %>% 
      st_transform(2874)
    CA_counties_sf_projection_zone6 = CA_counties_sf %>% 
      st_transform(2875)
    
    #checking the newly assigned projections
    st_crs(CA_counties_sf_projection_zone1)
    st_crs(CA_counties_sf_projection_zone2)
    #plotting the map - all the zones look the same on the coordinate; I'll just plot zone 1
    ggplot(CA_counties_sf_projection_zone1) +
      geom_sf()
      theme_void()

#trying to join the covid data to this map  
setwd("/Users/funmiarogbokun/Desktop/CZ Biohub Contractor/Wildfires & Covid Project/Covid_data")
covid_data <- read.csv("county_data.csv", stringsAsFactors = F, header = T) 

#want to make sure R is reading the date in as a data, not char
covid_data$date <- as.Date(covid_data$date, "%Y-%m-%d")
        
    #want to join to CA shapefile so will filter to CA only
    covid_data_CA <- covid_data[which(covid_data$state=="California"),]

# Creating COVID Datasets: Daily (1 day from March - October) -------------
 
    #Filtering by one day of each month right now for visualizing with pollution data
    
# dates that currently match the air pollution data
    # March, 18, 2020
    covid_CA_Mar1820 <- covid_data_CA %>% filter(date == as.Date("2020-03-18")) #57 obs
    # April, 18, 2020
    covid_CA_Apr1820 <- covid_data_CA %>% filter(date == as.Date("2020-04-18")) #60 obs
    # May, 18, 2020
    covid_CA_May1820 <- covid_data_CA %>% filter(date == as.Date("2020-05-18")) #60 obs
    # June, 18, 2020
    covid_CA_Jun1820 <- covid_data_CA %>% filter(date == as.Date("2020-06-18")) #60 obs
    # July, 18, 2020
    covid_CA_Jul1820 <- covid_data_CA %>% filter(date == as.Date("2020-07-18")) #60 obs
    # Aug, 18, 2020
    covid_CA_Aug1820 <- covid_data_CA %>% filter(date == as.Date("2020-08-18")) #60 obs
    # September, 18, 2020
    covid_CA_Sep1820 <- covid_data_CA %>% filter(date == as.Date("2020-09-18")) #60 obs
    # October, 18, 2020
    covid_CA_Oct1820 <- covid_data_CA %>% filter(date == as.Date("2020-10-18")) #60 obs

#most recent date in covid data    
    # Jan 19, 2021 
    covid_data_CA_Jan1921 <- covid_data_CA %>% filter(date == as.Date("2021-01-19")) #60 obs
    

# Creating Covid Maps  ----------------------------------------------------

#checking the range for covid data
summary (covid_data_CA$cases) #min = 0, max = 28549, mean = 172.5, median=15)
#adding log_cases onto dataset so can check range for log values in CA covid dataset
covid_data_CA$cases_log <- log(covid_data_CA$cases)
summary (covid_data_CA$cases_log) #min = -inf, max = 10.259, mean = -inf, median=2.708)
  
#joining the two datasets (covid cases data and shapefile)
#creating county variable column to match other dataset to prepare for merge
CA_counties_sf_projection_zone1$county = CA_counties_sf_projection_zone1$NAME
    
    #merging shapefile and covid data for each date & creating raw and log transformed maps
   
# March, 18, 2020 
    CA_covid_map_Mar1820 <- merge(CA_counties_sf_projection_zone1,covid_CA_Mar1820,by="county")
    
      #raw
      CA_covid_march_raw <- ggplot(CA_covid_map_Mar1820, 
             aes(fill = cases %>% as.numeric)) +
        geom_sf() + 
        scale_fill_gradient(low = "darkred", high = "yellow", limits=c(0, 28550)) +
        labs(title="Number of New Covid Cases in California on March 18, 2020") + 
        labs(fill = "New Cases") 
      #log-transformed
      CA_covid_march_log <- ggplot(CA_covid_map_Mar1820, 
                                   aes(fill = log(cases) %>% as.numeric)) +
        geom_sf() + 
        scale_fill_gradient(low = "darkred", high = "yellow", limits=c(0, 10.3)) +
        labs(title="Log-transformed Number of New Covid Cases in California on March 18, 2020") + 
        labs(fill = "Ln(New Cases)") 
      
# APRIL, 18, 2020 
      CA_covid_map_Apr1820 <- merge(CA_counties_sf_projection_zone1,covid_CA_Apr1820,by="county") #change df & second dataname
      
        #raw #change the 2 dataset names in first row & map title
        CA_covid_april_raw <- ggplot(CA_covid_map_Apr1820, 
                                     aes(fill = cases %>% as.numeric)) +
          geom_sf() + 
          scale_fill_gradient(low = "darkred", high = "yellow", limits=c(0, 28550)) +
          labs(title="Number of New Covid Cases in California on April 18, 2020") + 
          labs(fill = "New Cases") 
        #log-transformed
        CA_covid_april_log <- ggplot(CA_covid_map_Apr1820, 
                                     aes(fill = log(cases) %>% as.numeric)) +
          geom_sf() + 
          scale_fill_gradient(low = "darkred", high = "yellow", limits=c(0, 10.3)) +
          labs(title="Log-transformed Number of New Covid Cases in California on April 18, 2020") + 
          labs(fill = "Ln(New Cases)") 
        
# May, 18, 2020 
        CA_covid_map_May1820 <- merge(CA_counties_sf_projection_zone1,covid_CA_May1820,by="county") #change second dataname
        
        #raw #change the 2 dataset names in first row & map title
        CA_covid_may_raw <- ggplot(CA_covid_map_May1820, 
                                     aes(fill = cases %>% as.numeric)) +
          geom_sf() + 
          scale_fill_gradient(low = "darkred", high = "yellow", limits=c(0, 28550)) +
          labs(title="Number of New Covid Cases in California on May 18, 2020") + 
          labs(fill = "New Cases") 
        #log-transformed
        CA_covid_may_log <- ggplot(CA_covid_map_May1820, 
                                     aes(fill = log(cases) %>% as.numeric)) +
          geom_sf() + 
          scale_fill_gradient(low = "darkred", high = "yellow", limits=c(0, 10.3)) +
          labs(title="Log-transformed Number of New Covid Cases in California on May 18, 2020") + 
          labs(fill = "Ln(New Cases)") 
        
# June, 18, 2020 
        CA_covid_map_Jun1820 <- merge(CA_counties_sf_projection_zone1,covid_CA_Jun1820,by="county") #change second dataname
        
        #raw #change the 2 dataset names in first row & map title
        CA_covid_june_raw <- ggplot(CA_covid_map_Jun1820, 
                                     aes(fill = cases %>% as.numeric)) +
          geom_sf() + 
          scale_fill_gradient(low = "darkred", high = "yellow", limits=c(0, 28550)) +
          labs(title="Number of New Covid Cases in California on June 18, 2020") + 
          labs(fill = "New Cases") 
        #log-transformed
        CA_covid_june_log <- ggplot(CA_covid_map_Jun1820, 
                                     aes(fill = log(cases) %>% as.numeric)) +
          geom_sf() + 
          scale_fill_gradient(low = "darkred", high = "yellow", limits=c(0, 10.3)) +
          labs(title="Log-transformed Number of New Covid Cases in California on June 18, 2020") + 
          labs(fill = "Ln(New Cases)") 
        
# July, 18, 2020 
        CA_covid_map_Jul1820 <- merge(CA_counties_sf_projection_zone1,covid_CA_Jul1820,by="county") #change second dataname
        
        #raw #change the 2 dataset names in first row & map title
        CA_covid_july_raw <- ggplot(CA_covid_map_Jul1820, 
                                     aes(fill = cases %>% as.numeric)) +
          geom_sf() + 
          scale_fill_gradient(low = "darkred", high = "yellow", limits=c(0, 28550)) +
          labs(title="Number of New Covid Cases in California on July 18, 2020") + 
          labs(fill = "New Cases") 
        #log-transformed
        CA_covid_july_log <- ggplot(CA_covid_map_Jul1820, 
                                     aes(fill = log(cases) %>% as.numeric)) +
          geom_sf() + 
          scale_fill_gradient(low = "darkred", high = "yellow", limits=c(0, 10.3)) +
          labs(title="Log-transformed Number of New Covid Cases in California on July 18, 2020") + 
          labs(fill = "Ln(New Cases)") 
        
# Aug, 18, 2020 
        CA_covid_map_Aug1820 <- merge(CA_counties_sf_projection_zone1,covid_CA_Aug1820,by="county") #change second dataname
        
        #raw #change the 2 dataset names in first row & map title
        CA_covid_aug_raw <- ggplot(CA_covid_map_Aug1820, 
                                     aes(fill = cases %>% as.numeric)) +
          geom_sf() + 
          scale_fill_gradient(low = "darkred", high = "yellow", limits=c(0, 28550)) +
          labs(title="Number of New Covid Cases in California on Aug 18, 2020") + 
          labs(fill = "New Cases") 
        #log-transformed
        CA_covid_aug_log <- ggplot(CA_covid_map_Aug1820, 
                                     aes(fill = log(cases) %>% as.numeric)) +
          geom_sf() + 
          scale_fill_gradient(low = "darkred", high = "yellow", limits=c(0, 10.3)) +
          labs(title="Log-transformed Number of New Covid Cases in California on Aug 18, 2020") + 
          labs(fill = "Ln(New Cases)") 
        
# Sept, 18, 2020 
        CA_covid_map_Sep1820 <- merge(CA_counties_sf_projection_zone1,covid_CA_Sep1820,by="county") #change second dataname
        
        #raw #change the 2 dataset names in first row & map title
        CA_covid_sept_raw <- ggplot(CA_covid_map_Sep1820, 
                                     aes(fill = cases %>% as.numeric)) +
          geom_sf() + 
          scale_fill_gradient(low = "darkred", high = "yellow", limits=c(0, 28550)) +
          labs(title="Number of New Covid Cases in California on Sept 18, 2020") + 
          labs(fill = "New Cases") 
        #log-transformed
        CA_covid_sept_log <- ggplot(CA_covid_map_Sep1820, 
                                     aes(fill = log(cases) %>% as.numeric)) +
          geom_sf() + 
          scale_fill_gradient(low = "darkred", high = "yellow", limits=c(0, 10.3)) +
          labs(title="Log-transformed Number of New Covid Cases in California on Sept 18, 2020") + 
          labs(fill = "Ln(New Cases)") 
        
# Oct, 18, 2020 
        CA_covid_map_Oct1820 <- merge(CA_counties_sf_projection_zone1,covid_CA_Oct1820,by="county") #change second dataname
        
        #raw #change the 2 dataset names in first row & map title
        CA_covid_oct_raw <- ggplot(CA_covid_map_Oct1820, 
                                     aes(fill = cases %>% as.numeric)) +
          geom_sf() + 
          scale_fill_gradient(low = "darkred", high = "yellow", limits=c(0, 28550)) +
          labs(title="Number of New Covid Cases in California on Oct 18, 2020") + 
          labs(fill = "New Cases") 
        #log-transformed
        CA_covid_oct_log <- ggplot(CA_covid_map_Oct1820, 
                                     aes(fill = log(cases) %>% as.numeric)) +
          geom_sf() + 
          scale_fill_gradient(low = "darkred", high = "yellow", limits=c(0, 10.3)) +
          labs(title="Log-transformed Number of New Covid Cases in California on Oct 18, 2020") + 
          labs(fill = "Ln(New Cases)") 
        
# Air Pollution -----------------------------------------------------------
    
    # reading in data and setting working directory
    setwd("/Users/funmiarogbokun/Desktop/CZ Biohub Contractor/Wildfires & Covid Project/EPA Data/Air Pollution - CA WA OR")
    pollution <- read.csv("Arogbokun_daily.csv", stringsAsFactors = F, header = T) 
     
    #making sure R reads dates as such
    pollution$Date.Local <- as.Date(pollution$Date.Local, "%Y-%m-%d")
    pollution$Date.of.Last.Change <- as.Date(pollution$Date.of.Last.Change, "%Y-%m-%d")
    
    #subgroups by state
    pollution_CA <- filter (pollution, State.Name == "California")
    
    #subgroups by pollutant
    CA_NO2 <- filter (pollution_CA, Parameter.Name == "Nitrogen dioxide (NO2)")
    CA_Ozone <- filter (pollution_CA, Parameter.Name == "Ozone")
    CA_PM25 <- filter (pollution_CA, Parameter.Name == "PM2.5 - Local Conditions")
    CA_PM10 <- filter (pollution_CA, Parameter.Name == "PM10 Total 0-10um STP")
    #not sure what the pollutants shown below would be used for
    CA_PM25AQI <- filter (pollution_CA, Parameter.Name == "Acceptable PM2.5 AQI & Speciation Mass")
    
    #seperating out PM2.5 in 1 hour avg vs. 24hr avg
    CA_PM25_onehr <- filter (CA_PM25, Sample.Duration == "1 HOUR")
    CA_PM25_24hr <- filter (CA_PM25, Sample.Duration == "24-HR BLK AVG"| Sample.Duration == "24 HOUR")
    
    #seperating out PM10 in 1 hour avg vs. 24hr avg
    CA_PM10_onehr <- filter (CA_PM10, Sample.Duration == "1 HOUR")
    CA_PM10_24hr <- filter (CA_PM10, Sample.Duration == "24-HR BLK AVG" | Sample.Duration == "24 HOUR")
    

# Filtering out Single Date for each Pollutant ----------------------------

#NO2
    # March, 18, 2020
    NO2_CA_Mar1820 <- CA_NO2 %>% filter(Date.Local == as.Date("2020-03-18")) #75 obs
    # April, 18, 2020
    NO2_CA_Apr1820 <- CA_NO2 %>% filter(Date.Local == as.Date("2020-04-18")) #98 obs
    # May, 18, 2020
    NO2_CA_May1820 <- CA_NO2 %>% filter(Date.Local == as.Date("2020-05-18")) #100 obs
    # June, 18, 2020
    NO2_CA_Jun1820 <- CA_NO2 %>% filter(Date.Local == as.Date("2020-06-18")) #97 obs
    # July, 18, 2020
    NO2_CA_Jul1820 <- CA_NO2 %>% filter(Date.Local == as.Date("2020-07-18")) #54 obs
    # Aug, 18, 2020
    NO2_CA_Aug1820 <- CA_NO2 %>% filter(Date.Local == as.Date("2020-08-18")) #28 obs
    # September, 18, 2020
    NO2_CA_Sep1820 <- CA_NO2 %>% filter(Date.Local == as.Date("2020-09-18")) #7 obs
    # October, 18, 2020
    NO2_CA_Oct1820 <- CA_NO2 %>% filter(Date.Local == as.Date("2020-10-18")) #0 obs
    
#Ozone
    ozone_CA_Mar1820 <- CA_Ozone %>% filter(Date.Local == as.Date("2020-03-18")) #126 obs
    # April, 18, 2020
    ozone_CA_Apr1820 <- CA_Ozone %>% filter(Date.Local == as.Date("2020-04-18")) #148 obs
    # May, 18, 2020
    ozone_CA_May1820 <- CA_Ozone %>% filter(Date.Local == as.Date("2020-05-18")) #150 obs
    # June, 18, 2020
    ozone_CA_Jun1820 <- CA_Ozone %>% filter(Date.Local == as.Date("2020-06-18")) #154 obs
    # July, 18, 2020
    ozone_CA_Jul1820 <- CA_Ozone %>% filter(Date.Local == as.Date("2020-07-18")) #97 obs
    # Aug, 18, 2020
    ozone_CA_Aug1820 <- CA_Ozone %>% filter(Date.Local == as.Date("2020-08-18")) #55 obs
    # September, 18, 2020
    ozone_CA_Sep1820 <- CA_Ozone %>% filter(Date.Local == as.Date("2020-09-18")) #29 obs
    # October, 18, 2020
    ozone_CA_Oct1820 <- CA_Ozone %>% filter(Date.Local == as.Date("2020-10-18")) #4 obs
    
#PM2.5 - 24hr avg
    # March, 18, 2020
    PM25day_CA_Mar1820 <- CA_PM25_24hr %>% filter(Date.Local == as.Date("2020-03-18")) #70 obs
    # April, 18, 2020
    PM25day_CA_Apr1820 <- CA_PM25_24hr %>% filter(Date.Local == as.Date("2020-04-18")) #115 obs
    # May, 18, 2020
    PM25day_CA_May1820 <- CA_PM25_24hr %>% filter(Date.Local == as.Date("2020-05-18")) #100 obs
    # June, 18, 2020
    PM25day_CA_Jun1820 <- CA_PM25_24hr %>% filter(Date.Local == as.Date("2020-06-18")) #81 obs
    # July, 18, 2020
    PM25day_CA_Jul1820 <- CA_PM25_24hr %>% filter(Date.Local == as.Date("2020-07-18")) #56 obs
    # Aug, 18, 2020
    PM25day_CA_Aug1820 <- CA_PM25_24hr %>% filter(Date.Local == as.Date("2020-08-18")) #39 obs
    # September, 18, 2020
    PM25day_CA_Sep1820 <- CA_PM25_24hr %>% filter(Date.Local == as.Date("2020-09-18")) #17 obs
    # October, 18, 2020
    PM25day_CA_Oct1820 <- CA_PM25_24hr %>% filter(Date.Local == as.Date("2020-10-18")) #4 obs
    
    
#PM10 - 24hr avg   
    # March, 18, 2020
    PM10day_CA_Mar1820 <- CA_PM10_24hr %>% filter(Date.Local == as.Date("2020-03-18")) #77 obs
    # April, 18, 2020
    PM10day_CA_Apr1820 <- CA_PM10_24hr %>% filter(Date.Local == as.Date("2020-04-18")) #78 obs
    # May, 18, 2020
    PM10day_CA_May1820 <- CA_PM10_24hr %>% filter(Date.Local == as.Date("2020-05-18")) #78 obs
    # June, 18, 2020
    PM10day_CA_Jun1820 <- CA_PM10_24hr %>% filter(Date.Local == as.Date("2020-06-18")) #75 obs
    # July, 18, 2020
    PM10day_CA_Jul1820 <- CA_PM10_24hr %>% filter(Date.Local == as.Date("2020-07-18")) #40 obs
    # Aug, 18, 2020
    PM10day_CA_Aug1820 <- CA_PM10_24hr %>% filter(Date.Local == as.Date("2020-08-18")) #25 obs
    # September, 18, 2020
    PM10day_CA_Sep1820 <- CA_PM10_24hr %>% filter(Date.Local == as.Date("2020-09-18")) #18 obs
    # October, 18, 2020
    PM10day_CA_Oct1820 <- CA_PM10_24hr %>% filter(Date.Local == as.Date("2020-10-18")) #9 obs


# Preping Pollution Plots ---------------------------------------------------------

######## Finding Maximum Values (so can make the range from 0 to the max for each plot)
    #NO2
    summary(CA_NO2$Arithmetic.Mean) # scale_color_continuous(limits=c(-2, 55)) #SET TO ENCOMPASS MIN AND MAX FOR NO2
    summary(log(CA_NO2$Arithmetic.Mean)) #range is (0, 4.1)
    #Ozone
    summary(CA_Ozone$Arithmetic.Mean) #min = 0.00000; max = 0.10518 (make range from 0 to 0.11)
    summary(log(CA_Ozone$Arithmetic.Mean)) #range is (-4, -2)
    #PM2.5 - 24 hrs
    summary(CA_PM25_24hr$Arithmetic.Mean) #min = -1.700; max = 202.200 (make range from -2 to 203)
    summary(log(CA_PM25_24hr$Arithmetic.Mean)) #range is (0, 5.4)
    #PM10 - 24hrs
    summary(CA_PM10_24hr$Arithmetic.Mean) #min = -1.00; max = 2032.00 (make range from -1 to 2033)
    summary(log(CA_PM10_24hr$Arithmetic.Mean)) #range is (0, 7.7)
                 
####### Transforming CA shapefile so lat and long can be added - prep for merge with air pollution data
    CA_shapefile_counties <- st_transform(CA_counties_sf_projection_zone1, CRS("+proj=longlat +datum=WGS84"))

# NO2 Plots for each month/day --------------------------------------------

    #change the data in geom_point
    #change the title in each
    #change the range in each

#March 18, 2020
    #raw  
    CA_NO2_march_raw <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=NO2_CA_Mar1820, aes(x=Longitude, y=Latitude, color=Arithmetic.Mean)) +
      labs(title="Average 1-hour level of NO2 in California on March 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(-2, 55)) 
    #log-transformed 
    CA_NO2_march_log <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=NO2_CA_Mar1820, aes(x=Longitude, y=Latitude, color=log(Arithmetic.Mean))) +
      labs(title="Log-transformed Average 1-hour level of NO2 in California on March 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(0, 4.1))  
      
#April 18, 2020
    #raw  
    CA_NO2_april_raw <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=NO2_CA_Apr1820, aes(x=Longitude, y=Latitude, color=Arithmetic.Mean)) +
      labs(title="Average 1-hour level of NO2 in California on April 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(-2, 55)) 
    #log-transformed 
    CA_NO2_april_log <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=NO2_CA_Apr1820, aes(x=Longitude, y=Latitude, color=log(Arithmetic.Mean))) +
      labs(title="Log-transformed Average 1-hour level of NO2 in California on April 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(0, 4.1))     

#May 18, 2020
    #raw  
    CA_NO2_may_raw <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=NO2_CA_May1820, aes(x=Longitude, y=Latitude, color=Arithmetic.Mean)) +
      labs(title="Average 1-hour level of NO2 in California on May 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(-2, 55)) 
    #log-transformed 
    CA_NO2_may_log <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=NO2_CA_May1820, aes(x=Longitude, y=Latitude, color=log(Arithmetic.Mean))) +
      labs(title="Log-transformed Average 1-hour level of NO2 in California on May 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(0, 4.1))     

#June 18, 2020
    #raw  
    CA_NO2_june_raw <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=NO2_CA_Jun1820, aes(x=Longitude, y=Latitude, color=Arithmetic.Mean)) +
      labs(title="Average 1-hour level of NO2 in California on June 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(-2, 55)) 
    #log-transformed 
    CA_NO2_june_log <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=NO2_CA_Jun1820, aes(x=Longitude, y=Latitude, color=log(Arithmetic.Mean))) +
      labs(title="Log-transformed Average 1-hour level of NO2 in California on June 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(0, 4.1))     

#July 18, 2020
    #raw  
    CA_NO2_july_raw <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=NO2_CA_Jul1820, aes(x=Longitude, y=Latitude, color=Arithmetic.Mean)) +
      labs(title="Average 1-hour level of NO2 in California on July 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(-2, 55)) 
    #log-transformed 
    CA_NO2_july_log <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=NO2_CA_Jul1820, aes(x=Longitude, y=Latitude, color=log(Arithmetic.Mean))) +
      labs(title="Log-transformed Average 1-hour level of NO2 in California on July 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(0, 4.1))     

#Aug 18, 2020
    #raw  
    CA_NO2_aug_raw <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=NO2_CA_Aug1820, aes(x=Longitude, y=Latitude, color=Arithmetic.Mean)) +
      labs(title="Average 1-hour level of NO2 in California on Aug 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(-2, 55)) 
    #log-transformed 
    CA_NO2_aug_log <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=NO2_CA_Aug1820, aes(x=Longitude, y=Latitude, color=log(Arithmetic.Mean))) +
      labs(title="Log-transformed Average 1-hour level of NO2 in California on Aug 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(0, 4.1))     

#Sept 18, 2020
    #raw  
    CA_NO2_sept_raw <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=NO2_CA_Sep1820, aes(x=Longitude, y=Latitude, color=Arithmetic.Mean)) +
      labs(title="Average 1-hour level of NO2 in California on Sept 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(-2, 55)) 
    #log-transformed 
    CA_NO2_sept_log <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=NO2_CA_Sep1820, aes(x=Longitude, y=Latitude, color=log(Arithmetic.Mean))) +
      labs(title="Log-transformed Average 1-hour level of NO2 in California on Sept 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(0, 4.1))     

#Oct 18, 2020
    #raw  
    CA_NO2_oct_raw <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=NO2_CA_Oct1820, aes(x=Longitude, y=Latitude, color=Arithmetic.Mean)) +
      labs(title="Average 1-hour level of NO2 in California on Oct 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(-2, 55)) 
    #log-transformed 
    CA_NO2_oct_log <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=NO2_CA_Oct1820, aes(x=Longitude, y=Latitude, color=log(Arithmetic.Mean))) +
      labs(title="Log-transformed Average 1-hour level of NO2 in California on Oct 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(0, 4.1))     


# Ozone Plots for each month/day ------------------------------------------

    #change the data in geom_point
    #change the title in each (pollutant and hour duration)
    #change the range in each
    
    #March 18, 2020
    #raw  
    CA_ozone_march_raw <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=ozone_CA_Mar1820, aes(x=Longitude, y=Latitude, color=Arithmetic.Mean)) +
      labs(title="Average 8-hour level of Ozone in California on March 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(0, 0.11)) 
    #log-transformed 
    CA_ozone_march_log <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=ozone_CA_Mar1820, aes(x=Longitude, y=Latitude, color=log(Arithmetic.Mean))) +
      labs(title="Log-transformed Average 8-hour level of Ozone in California on March 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(-4, -2))  
    
    #April 18, 2020
    #raw  
    CA_ozone_april_raw <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=ozone_CA_Apr1820, aes(x=Longitude, y=Latitude, color=Arithmetic.Mean)) +
      labs(title="Average 8-hour level of Ozone in California on April 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(0, 0.11)) 
    #log-transformed 
    CA_ozone_april_log <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=ozone_CA_Apr1820, aes(x=Longitude, y=Latitude, color=log(Arithmetic.Mean))) +
      labs(title="Log-transformed Average 8-hour level of Ozone in California on April 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(-4, -2))     
    
    #May 18, 2020
    #raw  
    CA_ozone_may_raw <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=ozone_CA_May1820, aes(x=Longitude, y=Latitude, color=Arithmetic.Mean)) +
      labs(title="Average 8-hour level of Ozone in California on May 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(0, 0.11)) 
    #log-transformed 
    CA_ozone_may_log <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=ozone_CA_May1820, aes(x=Longitude, y=Latitude, color=log(Arithmetic.Mean))) +
      labs(title="Log-transformed Average 8-hour level of Ozone in California on May 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(-4, -2))     
    
    #June 18, 2020
    #raw  
    CA_ozone_june_raw <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=ozone_CA_Jun1820, aes(x=Longitude, y=Latitude, color=Arithmetic.Mean)) +
      labs(title="Average 8-hour level of Ozone in California on June 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(0, 0.11)) 
    #log-transformed 
    CA_ozone_june_log <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=ozone_CA_Jun1820, aes(x=Longitude, y=Latitude, color=log(Arithmetic.Mean))) +
      labs(title="Log-transformed Average 8-hour level of Ozone in California on June 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(-4, -2))     
    
    #July 18, 2020
    #raw  
    CA_ozone_july_raw <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=ozone_CA_Jul1820, aes(x=Longitude, y=Latitude, color=Arithmetic.Mean)) +
      labs(title="Average 8-hour level of Ozone in California on July 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(0, 0.11)) 
    #log-transformed 
    CA_ozone_july_log <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=ozone_CA_Jul1820, aes(x=Longitude, y=Latitude, color=log(Arithmetic.Mean))) +
      labs(title="Log-transformed Average 8-hour level of Ozone in California on July 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(-4, -2))     
    
    #Aug 18, 2020
    #raw  
    CA_ozone_aug_raw <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=ozone_CA_Aug1820, aes(x=Longitude, y=Latitude, color=Arithmetic.Mean)) +
      labs(title="Average 8-hour level of Ozone in California on Aug 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(0, 0.11)) 
    #log-transformed 
    CA_ozone_aug_log <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=ozone_CA_Aug1820, aes(x=Longitude, y=Latitude, color=log(Arithmetic.Mean))) +
      labs(title="Log-transformed Average 8-hour level of Ozone in California on Aug 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(-4, -2))     
    
    #Sept 18, 2020
    #raw  
    CA_ozone_sept_raw <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=ozone_CA_Sep1820, aes(x=Longitude, y=Latitude, color=Arithmetic.Mean)) +
      labs(title="Average 8-hour level of Ozone in California on Sept 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(0, 0.11)) 
    #log-transformed 
    CA_ozone_sept_log <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=ozone_CA_Sep1820, aes(x=Longitude, y=Latitude, color=log(Arithmetic.Mean))) +
      labs(title="Log-transformed Average 8-hour level of Ozone in California on Sept 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(-4, -2))     
    
    #Oct 18, 2020
    #raw  
    CA_ozone_oct_raw <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=ozone_CA_Oct1820, aes(x=Longitude, y=Latitude, color=Arithmetic.Mean)) +
      labs(title="Average 8-hour level of Ozone2 in California on Oct 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(0, 0.11)) 
    #log-transformed 
    CA_ozone_oct_log <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=ozone_CA_Oct1820, aes(x=Longitude, y=Latitude, color=log(Arithmetic.Mean))) +
      labs(title="Log-transformed Average 8-hour level of Ozone in California on Oct 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(-4, -2))     

# PM 2.5 (24hrs) Plots for each month/day ---------------------------------

    #March 18, 2020
    #raw  
    CA_pm25day_march_raw <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=PM25day_CA_Mar1820, aes(x=Longitude, y=Latitude, color=Arithmetic.Mean)) +
      labs(title="Average 24-hour level of PM 2.5 in California on March 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(-2, 203)) 
    #log-transformed 
    CA_pm25day_march_log <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=PM25day_CA_Mar1820, aes(x=Longitude, y=Latitude, color=log(Arithmetic.Mean))) +
      labs(title="Log-transformed Average 24-hour level of PM 2.5 in California on March 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(0, 5.4))  
    
    #April 18, 2020
    #raw  
    CA_pm25day_april_raw <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=PM25day_CA_Apr1820, aes(x=Longitude, y=Latitude, color=Arithmetic.Mean)) +
      labs(title="Average 24-hour level of PM 2.5 in California on April 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(-2, 203)) 
    #log-transformed 
    CA_pm25day_april_log <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=PM25day_CA_Apr1820, aes(x=Longitude, y=Latitude, color=log(Arithmetic.Mean))) +
      labs(title="Log-transformed Average 24-hour level of PM 2.5 in California on April 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(0, 5.4))     
    
    #May 18, 2020
    #raw  
    CA_pm25day_may_raw <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=PM25day_CA_May1820, aes(x=Longitude, y=Latitude, color=Arithmetic.Mean)) +
      labs(title="Average 24-hour level of PM 2.5 in California on May 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(-2, 203)) 
    #log-transformed 
    CA_pm25day_may_log <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=PM25day_CA_May1820, aes(x=Longitude, y=Latitude, color=log(Arithmetic.Mean))) +
      labs(title="Log-transformed Average 24-hour level of PM 2.5 in California on May 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(0, 5.4))     
    
    #June 18, 2020
    #raw  
    CA_pm25day_june_raw <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=PM25day_CA_Jun1820, aes(x=Longitude, y=Latitude, color=Arithmetic.Mean)) +
      labs(title="Average 24-hour level of PM 2.5 in California on June 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(-2, 203)) 
    #log-transformed 
    CA_pm25day_june_log <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=PM25day_CA_Jun1820, aes(x=Longitude, y=Latitude, color=log(Arithmetic.Mean))) +
      labs(title="Log-transformed Average 24-hour level of PM 2.5 in California on June 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(0, 5.4))     
    
    #July 18, 2020
    #raw  
    CA_pm25day_july_raw <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=PM25day_CA_Jul1820, aes(x=Longitude, y=Latitude, color=Arithmetic.Mean)) +
      labs(title="Average 24-hour level of PM 2.5 in California on July 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(-2, 203)) 
    #log-transformed 
    CA_pm25day_july_log <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=PM25day_CA_Jul1820, aes(x=Longitude, y=Latitude, color=log(Arithmetic.Mean))) +
      labs(title="Log-transformed Average 24-hour level of PM 2.5 in California on July 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(0, 5.4))     
    
    #Aug 18, 2020
    #raw  
    CA_pm25day_aug_raw <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=PM25day_CA_Aug1820, aes(x=Longitude, y=Latitude, color=Arithmetic.Mean)) +
      labs(title="Average 24-hour level of PM 2.5 in California on Aug 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(-2, 203)) 
    #log-transformed 
    CA_pm25day_aug_log <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=PM25day_CA_Aug1820, aes(x=Longitude, y=Latitude, color=log(Arithmetic.Mean))) +
      labs(title="Log-transformed Average 24-hour level of PM 2.5 in California on Aug 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(0, 5.4))     
    
    #Sept 18, 2020
    #raw  
    CA_pm25day_sept_raw <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=PM25day_CA_Sep1820, aes(x=Longitude, y=Latitude, color=Arithmetic.Mean)) +
      labs(title="Average 24-hour level of PM 2.5 in California on Sept 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(-2, 203)) 
    #log-transformed 
    CA_pm25day_sept_log <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=PM25day_CA_Sep1820, aes(x=Longitude, y=Latitude, color=log(Arithmetic.Mean))) +
      labs(title="Log-transformed Average 24-hour level of PM 2.5 in California on Sept 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(0, 5.4))     
    
    #Oct 18, 2020
    #raw  
    CA_pm25day_oct_raw <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=PM25day_CA_Oct1820, aes(x=Longitude, y=Latitude, color=Arithmetic.Mean)) +
      labs(title="Average 24-hour level of PM 2.5 in California on Oct 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(-2, 203)) 
    #log-transformed 
    CA_pm25day_oct_log <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=PM25day_CA_Oct1820, aes(x=Longitude, y=Latitude, color=log(Arithmetic.Mean))) +
      labs(title="Log-transformed Average 24-hour level of PM 2.5 in California on Oct 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(0, 5.4))     
    

# PM 10 (24hrs) Plots for each month/day ----------------------------------

    #March 18, 2020
    #raw  
    CA_pm10day_march_raw <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=PM10day_CA_Mar1820, aes(x=Longitude, y=Latitude, color=Arithmetic.Mean)) +
      labs(title="Average 24-hour level of PM 10 in California on March 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(-1, 2033)) 
    #log-transformed 
    CA_pm10day_march_log <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=PM10day_CA_Mar1820, aes(x=Longitude, y=Latitude, color=log(Arithmetic.Mean))) +
      labs(title="Log-transformed Average 24-hour level of PM 10 in California on March 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(0, 7.7))  
    
    #April 18, 2020
    #raw  
    CA_pm10day_april_raw <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=PM10day_CA_Apr1820, aes(x=Longitude, y=Latitude, color=Arithmetic.Mean)) +
      labs(title="Average 24-hour level of PM 10 in California on April 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(-1, 2033)) 
    #log-transformed 
    CA_pm10day_april_log <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=PM10day_CA_Apr1820, aes(x=Longitude, y=Latitude, color=log(Arithmetic.Mean))) +
      labs(title="Log-transformed Average 24-hour level of PM 10 in California on April 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(0, 7.7))     
    
    #May 18, 2020
    #raw  
    CA_pm10day_may_raw <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=PM10day_CA_May1820, aes(x=Longitude, y=Latitude, color=Arithmetic.Mean)) +
      labs(title="Average 24-hour level of PM 10 in California on May 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(-1, 2033)) 
    #log-transformed 
    CA_pm10day_may_log <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=PM10day_CA_May1820, aes(x=Longitude, y=Latitude, color=log(Arithmetic.Mean))) +
      labs(title="Log-transformed Average 24-hour level of PM 10 in California on May 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(0, 7.7))     
    
    #June 18, 2020
    #raw  
    CA_pm10day_june_raw <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=PM10day_CA_Jun1820, aes(x=Longitude, y=Latitude, color=Arithmetic.Mean)) +
      labs(title="Average 24-hour level of PM 10 in California on June 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(-1, 2033)) 
    #log-transformed 
    CA_pm10day_june_log <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=PM10day_CA_Jun1820, aes(x=Longitude, y=Latitude, color=log(Arithmetic.Mean))) +
      labs(title="Log-transformed Average 24-hour level of PM 10 in California on June 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(0, 7.7))     
    
    #July 18, 2020
    #raw  
    CA_pm10day_july_raw <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=PM10day_CA_Jul1820, aes(x=Longitude, y=Latitude, color=Arithmetic.Mean)) +
      labs(title="Average 24-hour level of PM 10 in California on July 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(-1, 2033)) 
    #log-transformed 
    CA_pm10day_july_log <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=PM10day_CA_Jul1820, aes(x=Longitude, y=Latitude, color=log(Arithmetic.Mean))) +
      labs(title="Log-transformed Average 24-hour level of PM 10 in California on July 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(0, 7.7))     
    
    #Aug 18, 2020
    #raw  
    CA_pm10day_aug_raw <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=PM10day_CA_Aug1820, aes(x=Longitude, y=Latitude, color=Arithmetic.Mean)) +
      labs(title="Average 24-hour level of PM 10 in California on Aug 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(-1, 2033)) 
    #log-transformed 
    CA_pm10day_aug_log <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=PM10day_CA_Aug1820, aes(x=Longitude, y=Latitude, color=log(Arithmetic.Mean))) +
      labs(title="Log-transformed Average 24-hour level of PM 10 in California on Aug 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(0, 7.7))     
    
    #Sept 18, 2020
    #raw  
    CA_pm10day_sept_raw <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=PM10day_CA_Sep1820, aes(x=Longitude, y=Latitude, color=Arithmetic.Mean)) +
      labs(title="Average 24-hour level of PM 10 in California on Sept 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(-1, 2033)) 
    #log-transformed 
    CA_pm10day_sept_log <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=PM10day_CA_Sep1820, aes(x=Longitude, y=Latitude, color=log(Arithmetic.Mean))) +
      labs(title="Log-transformed Average 24-hour level of PM 10 in California on Sept 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(0, 7.7))     
    
    #Oct 18, 2020
    #raw  
    CA_pm10day_oct_raw <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=PM10day_CA_Oct1820, aes(x=Longitude, y=Latitude, color=Arithmetic.Mean)) +
      labs(title="Average 24-hour level of PM 10 in California on Oct 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(-1, 2033)) 
    #log-transformed 
    CA_pm10day_oct_log <- ggplot() +
      geom_sf(data=CA_shapefile_counties)+
      geom_point(data=PM10day_CA_Oct1820, aes(x=Longitude, y=Latitude, color=log(Arithmetic.Mean))) +
      labs(title="Log-transformed Average 24-hour level of PM 10 in California on Oct 18, 2020") +
      scale_color_gradient(low = "darkred", high = "yellow", limits=c(0, 7.7))     
    

# Exporting Files - CA Covid & Pollution Maps -----------------------------

#path to new folder for plots
setwd("/Users/funmiarogbokun/Desktop/CZ Biohub Contractor/Wildfires & Covid Project/Joint_plots_covid_epa")
 

# Joint Plots - NO2 -------------------------------------------------------

#March       
    #RAW   
    pdf("NO2_raw_CA_March1820.pdf") 
    grid.arrange(CA_NO2_march_raw, CA_covid_march_raw, nrow = 2)
    dev.off() 
    #Log   
    pdf("NO2_log_CA_March1820.pdf") 
    grid.arrange(CA_NO2_march_log, CA_covid_march_log, nrow = 2)
    dev.off() 

#April       
    #RAW   
    pdf("NO2_raw_CA_Apr1820.pdf") 
    grid.arrange(CA_NO2_april_raw, CA_covid_april_raw, nrow = 2)
    dev.off() 
    #Log   
    pdf("NO2_log_CA_Apr1820.pdf") 
    grid.arrange(CA_NO2_april_log, CA_covid_april_log, nrow = 2)
    dev.off() 
    
#May       
    #RAW   
    pdf("NO2_raw_CA_May1820.pdf") 
    grid.arrange(CA_NO2_may_raw, CA_covid_may_raw, nrow = 2)
    dev.off() 
    #Log   
    pdf("NO2_log_CA_May1820.pdf") 
    grid.arrange(CA_NO2_may_log, CA_covid_may_log, nrow = 2)
    dev.off() 
    
#June       
    #RAW   
    pdf("NO2_raw_CA_June1820.pdf") 
    grid.arrange(CA_NO2_june_raw, CA_covid_june_raw, nrow = 2)
    dev.off() 
    #Log   
    pdf("NO2_log_CA_June1820.pdf") 
    grid.arrange(CA_NO2_june_log, CA_covid_june_log, nrow = 2)
    dev.off() 

#July       
    #RAW   
    pdf("NO2_raw_CA_July1820.pdf") 
    grid.arrange(CA_NO2_july_raw, CA_covid_july_raw, nrow = 2)
    dev.off() 
    #Log   
    pdf("NO2_log_CA_July1820.pdf") 
    grid.arrange(CA_NO2_july_log, CA_covid_july_log, nrow = 2)
    dev.off() 
    
#Aug       
    #RAW   
    pdf("NO2_raw_CA_Aug1820.pdf") 
    grid.arrange(CA_NO2_aug_raw, CA_covid_aug_raw, nrow = 2)
    dev.off() 
    #Log   
    pdf("NO2_log_CA_Aug1820.pdf") 
    grid.arrange(CA_NO2_aug_log, CA_covid_aug_log, nrow = 2)
    dev.off() 
    
#Sept       
    #RAW   
    pdf("NO2_raw_CA_Sept1820.pdf") 
    grid.arrange(CA_NO2_sept_raw, CA_covid_sept_raw, nrow = 2)
    dev.off() 
    #Log   
    pdf("NO2_log_CA_Sept1820.pdf") 
    grid.arrange(CA_NO2_sept_log, CA_covid_sept_log, nrow = 2)
    dev.off() 
 
#Oct       
    #RAW   
    pdf("NO2_raw_CA_Oct1820.pdf") 
    grid.arrange(CA_NO2_oct_raw, CA_covid_oct_raw, nrow = 2)
    dev.off() 
    #Log   
    pdf("NO2_log_CA_Oct1820.pdf") 
    grid.arrange(CA_NO2_oct_log, CA_covid_oct_log, nrow = 2)
    dev.off() 



# Joint Plots - Ozone -----------------------------------------------------
    setwd("/Users/funmiarogbokun/Desktop/CZ Biohub Contractor/Wildfires & Covid Project/Joint_plots_covid_epa/Ozone")
    
    #March       
    #RAW   
    pdf("Ozone_raw_CA_March1820.pdf") 
    grid.arrange(CA_ozone_march_raw, CA_covid_march_raw, nrow = 2)
    dev.off() 
    #Log   
    pdf("Ozone_log_CA_March1820.pdf") 
    grid.arrange(CA_ozone_march_log, CA_covid_march_log, nrow = 2)
    dev.off() 
    
    #April       
    #RAW   
    pdf("Ozone_raw_CA_Apr1820.pdf") 
    grid.arrange(CA_ozone_april_raw, CA_covid_april_raw, nrow = 2)
    dev.off() 
    #Log   
    pdf("Ozone_log_CA_Apr1820.pdf") 
    grid.arrange(CA_ozone_april_log, CA_covid_april_log, nrow = 2)
    dev.off() 
    
    #May       
    #RAW   
    pdf("Ozone_raw_CA_May1820.pdf") 
    grid.arrange(CA_ozone_may_raw, CA_covid_may_raw, nrow = 2)
    dev.off() 
    #Log   
    pdf("Ozone_log_CA_May1820.pdf") 
    grid.arrange(CA_ozone_may_log, CA_covid_may_log, nrow = 2)
    dev.off() 
    
    #June       
    #RAW   
    pdf("Ozone_raw_CA_June1820.pdf") 
    grid.arrange(CA_ozone_june_raw, CA_covid_june_raw, nrow = 2)
    dev.off() 
    #Log   
    pdf("Ozone_log_CA_June1820.pdf") 
    grid.arrange(CA_ozone_june_log, CA_covid_june_log, nrow = 2)
    dev.off() 
    
    #July       
    #RAW   
    pdf("Ozone_raw_CA_July1820.pdf") 
    grid.arrange(CA_ozone_july_raw, CA_covid_july_raw, nrow = 2)
    dev.off() 
    #Log   
    pdf("Ozone_log_CA_July1820.pdf") 
    grid.arrange(CA_ozone_july_log, CA_covid_july_log, nrow = 2)
    dev.off() 
    
    #Aug       
    #RAW   
    pdf("Ozone_raw_CA_Aug1820.pdf") 
    grid.arrange(CA_ozone_aug_raw, CA_covid_aug_raw, nrow = 2)
    dev.off() 
    #Log   
    pdf("Ozone_log_CA_Aug1820.pdf") 
    grid.arrange(CA_ozone_aug_log, CA_covid_aug_log, nrow = 2)
    dev.off() 
    
    #Sept       
    #RAW   
    pdf("Ozone_raw_CA_Sept1820.pdf") 
    grid.arrange(CA_ozone_sept_raw, CA_covid_sept_raw, nrow = 2)
    dev.off() 
    #Log   
    pdf("Ozone_log_CA_Sept1820.pdf") 
    grid.arrange(CA_ozone_sept_log, CA_covid_sept_log, nrow = 2)
    dev.off() 
    
    #Oct       
    #RAW   
    pdf("Ozone_raw_CA_Oct1820.pdf") 
    grid.arrange(CA_ozone_oct_raw, CA_covid_oct_raw, nrow = 2)
    dev.off() 
    #Log   
    pdf("Ozone_log_CA_Oct1820.pdf") 
    grid.arrange(CA_ozone_oct_log, CA_covid_oct_log, nrow = 2)
    dev.off() 

# Joint Plots - PM 2.5 (24 hrs) -------------------------------------------
    setwd("/Users/funmiarogbokun/Desktop/CZ Biohub Contractor/Wildfires & Covid Project/Joint_plots_covid_epa/PM2.5")
    
    #March       
    #RAW   
    pdf("PM25day_raw_CA_March1820.pdf") 
    grid.arrange(CA_pm25day_march_raw, CA_covid_march_raw, nrow = 2)
    dev.off() 
    #Log   
    pdf("PM25day_log_CA_March1820.pdf") 
    grid.arrange(CA_pm25day_march_log, CA_covid_march_log, nrow = 2)
    dev.off() 
    
    #April       
    #RAW   
    pdf("PM25day_raw_CA_Apr1820.pdf") 
    grid.arrange(CA_pm25day_april_raw, CA_covid_april_raw, nrow = 2)
    dev.off() 
    #Log   
    pdf("PM25day_log_CA_Apr1820.pdf") 
    grid.arrange(CA_pm25day_april_log, CA_covid_april_log, nrow = 2)
    dev.off() 
    
    #May       
    #RAW   
    pdf("PM25day_raw_CA_May1820.pdf") 
    grid.arrange(CA_pm25day_may_raw, CA_covid_may_raw, nrow = 2)
    dev.off() 
    #Log   
    pdf("PM25day_log_CA_May1820.pdf") 
    grid.arrange(CA_pm25day_may_log, CA_covid_may_log, nrow = 2)
    dev.off() 
    
    #June       
    #RAW   
    pdf("PM25day_raw_CA_June1820.pdf") 
    grid.arrange(CA_pm25day_june_raw, CA_covid_june_raw, nrow = 2)
    dev.off() 
    #Log   
    pdf("PM25day_log_CA_June1820.pdf") 
    grid.arrange(CA_pm25day_june_log, CA_covid_june_log, nrow = 2)
    dev.off() 
    
    #July       
    #RAW   
    pdf("PM25day_raw_CA_July1820.pdf") 
    grid.arrange(CA_pm25day_july_raw, CA_covid_july_raw, nrow = 2)
    dev.off() 
    #Log   
    pdf("PM25day_log_CA_July1820.pdf") 
    grid.arrange(CA_pm25day_july_log, CA_covid_july_log, nrow = 2)
    dev.off() 
    
    #Aug       
    #RAW   
    pdf("PM25day_raw_CA_Aug1820.pdf") 
    grid.arrange(CA_pm25day_aug_raw, CA_covid_aug_raw, nrow = 2)
    dev.off() 
    #Log   
    pdf("PM25day_log_CA_Aug1820.pdf") 
    grid.arrange(CA_pm25day_aug_log, CA_covid_aug_log, nrow = 2)
    dev.off() 
    
    #Sept       
    #RAW   
    pdf("PM25day_raw_CA_Sept1820.pdf") 
    grid.arrange(CA_pm25day_sept_raw, CA_covid_sept_raw, nrow = 2)
    dev.off() 
    #Log   
    pdf("PM25day_log_CA_Sept1820.pdf") 
    grid.arrange(CA_pm25day_sept_log, CA_covid_sept_log, nrow = 2)
    dev.off() 
    
    #Oct       
    #RAW   
    pdf("PM25day_raw_CA_Oct1820.pdf") 
    grid.arrange(CA_pm25day_oct_raw, CA_covid_oct_raw, nrow = 2)
    dev.off() 
    #Log   
    pdf("PM25day_log_CA_Oct1820.pdf") 
    grid.arrange(CA_pm25day_oct_log, CA_covid_oct_log, nrow = 2)
    dev.off() 
    

# Joint Plots - PM 10 (24hrs) ---------------------------------------------
    setwd("/Users/funmiarogbokun/Desktop/CZ Biohub Contractor/Wildfires & Covid Project/Joint_plots_covid_epa/PM10")
    
    #March       
    #RAW   
    pdf("PM10day_raw_CA_March1820.pdf") 
    grid.arrange(CA_pm10day_march_raw, CA_covid_march_raw, nrow = 2)
    dev.off() 
    #Log   
    pdf("PM10day_log_CA_March1820.pdf") 
    grid.arrange(CA_pm10day_march_log, CA_covid_march_log, nrow = 2)
    dev.off() 
    
    #April       
    #RAW   
    pdf("PM10day_raw_CA_Apr1820.pdf") 
    grid.arrange(CA_pm10day_april_raw, CA_covid_april_raw, nrow = 2)
    dev.off() 
    #Log   
    pdf("PM10day_log_CA_Apr1820.pdf") 
    grid.arrange(CA_pm10day_april_log, CA_covid_april_log, nrow = 2)
    dev.off() 
    
    #May       
    #RAW   
    pdf("PM10day_raw_CA_May1820.pdf") 
    grid.arrange(CA_pm10day_may_raw, CA_covid_may_raw, nrow = 2)
    dev.off() 
    #Log   
    pdf("PM10day_log_CA_May1820.pdf") 
    grid.arrange(CA_pm10day_may_log, CA_covid_may_log, nrow = 2)
    dev.off() 
    
    #June       
    #RAW   
    pdf("PM10day_raw_CA_June1820.pdf") 
    grid.arrange(CA_p10day_june_raw, CA_covid_june_raw, nrow = 2)
    dev.off() 
    #Log   
    pdf("PM10day_log_CA_June1820.pdf") 
    grid.arrange(CA_pm10day_june_log, CA_covid_june_log, nrow = 2)
    dev.off() 
    
    #July       
    #RAW   
    pdf("PM10day_raw_CA_July1820.pdf") 
    grid.arrange(CA_pm10day_july_raw, CA_covid_july_raw, nrow = 2)
    dev.off() 
    #Log   
    pdf("PM10day_log_CA_July1820.pdf") 
    grid.arrange(CA_pm10day_july_log, CA_covid_july_log, nrow = 2)
    dev.off() 
    
    #Aug       
    #RAW   
    pdf("PM10day_raw_CA_Aug1820.pdf") 
    grid.arrange(CA_pm10day_aug_raw, CA_covid_aug_raw, nrow = 2)
    dev.off() 
    #Log   
    pdf("PM10day_log_CA_Aug1820.pdf") 
    grid.arrange(CA_pm10day_aug_log, CA_covid_aug_log, nrow = 2)
    dev.off() 
    
    #Sept       
    #RAW   
    pdf("PM10day_raw_CA_Sept1820.pdf") 
    grid.arrange(CA_pm10day_sept_raw, CA_covid_sept_raw, nrow = 2)
    dev.off() 
    #Log   
    pdf("PM10day_log_CA_Sept1820.pdf") 
    grid.arrange(CA_pm10day_sept_log, CA_covid_sept_log, nrow = 2)
    dev.off() 
    
    #Oct       
    #RAW   
    pdf("PM10day_raw_CA_Oct1820.pdf") 
    grid.arrange(CA_pm10day_oct_raw, CA_covid_oct_raw, nrow = 2)
    dev.off() 
    #Log   
    pdf("PM10day_log_CA_Oct1820.pdf") 
    grid.arrange(CA_pm10day_oct_log, CA_covid_oct_log, nrow = 2)
    dev.off() 
    
# Old Code - creates bar graphs for pollution and other things ------------
    
    #### creates a plot of the air pollution monitors as points on the CA county shapefile (works)
    ggplot() +
      geom_sf(data=CA_counties_sf_projection_zone1)+
      geom_sf(data=CA_NO2, aes(x=Longitude, y=Latitude, color=Arithmetic.Mean))
    
################################ #creating bar graphs of each pollutant (just all of the data for CA but for each of the 5 pollutant options)
    ###################NO2
    #NO2 - Maximum
    CA_NO2_plot_max <-
      ggplot(CA_NO2, aes(Date.Local, First.Maximum.Value)) +
      geom_bar(stat="identity")+ 
      ggtitle("NO2 in Califormia: Highest Maximum 1-hour level of NO2 in California per Day (January - October 2020)")+
      labs(y="Level of NO2 (Parts per billion)", x = "Date")
    
    #Average
    CA_NO2_plot_avg <-
      ggplot(CA_NO2, aes(Date.Local, Arithmetic.Mean)) +
      geom_bar(stat="identity")+ 
      ggtitle("NO2 in Califormia: Average 1-hour level of NO2 in California per Day (January - October 2020)")+
      labs(y="Level of NO2 (Parts per billion)", x = "Date")
    
    grid.arrange(CA_NO2_plot_avg, CA_NO2_plot_max, nrow = 2)
    
    ################### Ozone
    #Ozone - Maximum
    CA_ozone_plot_max <-
      ggplot(CA_Ozone, aes(Date.Local, First.Maximum.Value)) +
      geom_bar(stat="identity")+ 
      ggtitle("Ozone in Califormia: Highest Maximum 8-Hour-Average level of Ozone in California per Day (January - October 2020)")+
      labs(y="Level of Ozone (Parts per million)", x = "Date")
    
    #Average
    CA_ozone_plot_avg <-
      ggplot(CA_Ozone, aes(Date.Local, Arithmetic.Mean)) +
      geom_bar(stat="identity")+ 
      ggtitle("Ozone in Califormia: 8-Hour-Average level of Ozone in California per Day (January - October 2020)")+
      labs(y="Level of Ozone (Parts per million)", x = "Date")
    
    grid.arrange(CA_ozone_plot_avg, CA_ozone_plot_max, nrow = 2)
    
    ################### PM2.5 - ONE HOUR
    #PM2.5 - Maximum
    CA_PM25HOUR_plot_max <-
      ggplot(CA_PM25_onehr, aes(Date.Local, First.Maximum.Value)) +
      geom_bar(stat="identity")+ 
      ggtitle("PM 2.5 in Califormia: Highest Maximum 1-hour level of PM 2.5 in California per Day (January - October 2020)")+
      labs(y="Level of PM 2.5 (Micrograms/cubic meter (LC))", x = "Date")
    
    #Average
    CA_PM25HOUR_plot_avg <-
      ggplot(CA_PM25_onehr, aes(Date.Local, Arithmetic.Mean)) +
      geom_bar(stat="identity")+ 
      ggtitle("PM 2.5 in Califormia: Average 1-hour level of PM 2.5 in California per Day (January - October 2020)")+
      labs(y="Level of PM 2.5 (Micrograms/cubic meter (LC))", x = "Date")
    
    grid.arrange(CA_PM25HOUR_plot_avg, CA_PM25HOUR_plot_max, nrow = 2)
    
    ################### PM10 - ONE HOUR
    #PM10 - Maximum
    CA_PM10HOUR_plot_max <-
      ggplot(CA_PM10_onehr, aes(Date.Local, First.Maximum.Value)) +
      geom_bar(stat="identity")+ 
      ggtitle("PM 10 in Califormia: Highest Maximum 1-hour level of PM 10 in California per Day (January - October 2020)")+
      labs(y="Level of PM 10 (Micrograms/cubic meter (25 C))", x = "Date")
    
    #Average
    CA_PM10HOUR_plot_avg <-
      ggplot(CA_PM10_onehr, aes(Date.Local, Arithmetic.Mean)) +
      geom_bar(stat="identity")+ 
      ggtitle("PM 10 in Califormia: Average 1-hour level of PM 10 in California per Day (January - October 2020)")+
      labs(y="Level of PM 10 (Micrograms/cubic meter (25 C))", x = "Date")
    
    grid.arrange(CA_PM10HOUR_plot_avg, CA_PM10HOUR_plot_max, nrow = 2)
    
    ################### PM2.5 - 24 HOUR
    #PM2.5 
    CA_PM25allday_plot_max <-
      ggplot(CA_PM25_24hr, aes(Date.Local, Arithmetic.Mean)) +
      geom_bar(stat="identity")+ 
      ggtitle("PM 2.5 in Califormia: 24-hour Average Level of PM 2.5 in California per Day (January - October 2020)")+
      labs(y="Level of PM 2.5 (Micrograms/cubic meter (LC))", x = "Date")
    
    
    ################### PM10 - 24 HOUR
    #PM10 - Maximum
    CA_PM10allday_plot_max <-
      ggplot(CA_PM10_24hr, aes(Date.Local, Arithmetic.Mean)) +
      geom_bar(stat="identity")+ 
      ggtitle("PM 10 in Califormia: 24-hour Average Level of PM 10 in California per Day (January - October 2020)")+
      labs(y="Level of PM 10 (Micrograms/cubic meter (25 C))", x = "Date")
    
######### old things #
    
ggplot(CA_counties_sf, 
       aes(fill = ALAND %>% as.numeric)) +
  geom_sf()

vis <- 
  pollution %>%
  select(State.Name, Date.Local, Parameter.Name, AQI) %>%
  group_by(State.Name, Date.Local) %>%
  summarise(Parameter.Name = "Ozone", AQI = mean(AQI)) %>%
  gather(key = class, value = value, -Date.Local, -State.Name) %>%
ggplot(data = vis) +
  geom_path(aes(x = Date.Local, y = value, color = State.Name)) +
  facet_wrap(~ class) +
  theme_bw()

##########
setwd("/Users/funmiarogbokun/Desktop/CZ Biohub Contractor/Wildfires & Covid Project/Covid_data")
CA_covid =  read.csv("california-history.csv", stringsAsFactors = F, header = T) 
CA_covid$date_correct <- as.Date(CA_covid$date, "%m/%d/%Y")

# Ploting hospitalized in CA
ggplot(CA_covid, aes(date_correct, hospitalizedCurrently)) +
  geom_bar(stat="identity")+ 
  ggtitle("Total number of people currently hospitalized in California")+
  labs(y="Total number of people currently hospitalized", x = "Date")

# Ploting ICU hospitalized in CA
ggplot(CA_covid, aes(date_correct, inIcuCurrently)) +
  geom_bar(stat="identity")+ 
  ggtitle("Total number of people currently hospitalized in the ICU in California")+
  labs(y="Total number of people currently hospitalized in the ICU", x = "Date (March 2020 - Nov 2020)")

# Ploting number of new covid cases for each day in CA
ggplot(CA_covid, aes(date_correct, positiveIncrease)) +
  geom_bar(stat="identity")+ 
  ggtitle("Number of New Covid Cases by day in California")+
  labs(y="Number of new daily cases of Covid in CA", x = "Date (March 2020 - Nov 2020)")

####### pollution
pollution$date_formatted <- as.Date(pollution$Date.Local, "%m/%d/%Y")
NO2 <- filter (pollution, Parameter.Name == "Nitrogen dioxide (NO2)")
CA_only_NO2 <- filter (NO2, State.Name == "California")
 
# Ploting number of new covid cases for each day in CA
ggplot(CA_only_NO2, aes(date_formatted, Arithmetic.Mean)) +
  geom_bar(stat="identity")+ 
  ggtitle("Arithmetic Mean of 1 HR NO2 (parts per billion")+
  labs(y="Arithmetic Mean of 1 HR NO2 (parts per billion in CA", x = "Date (March 2020 - Nov 2020)")


#---

#creating maps for each pollutant in California
CA_projection_pollution_NO2 = CA_NO2 %>% 
  st_transform(2870)

ggplot(CA_covid_map, 
       aes(fill = cases %>% as.numeric)) +
  geom_sf() + 
  labs(title="Number of New Covid Cases in California on Jan 19, 2021") +
  labs(fill = "New Cases")

###########3 OLD COVID MAP MAKING


#log-transformed
CA_covid_march_log <- ggplot(CA_covid_map, 
                             aes(fill = cases %>% as.numeric)) +
  geom_sf() + 
  scale_fill_gradient(low = "blue", high = "red", limits=c(0, 28550)) +
  labs(title="Number of New Covid Cases in California on March, 18, 2020") + 
  labs(fill = "New Cases")  

# March, 18, 2020 - log transformed
CA_counties_sf_projection_zone1$county = CA_counties_sf_projection_zone1$NAME
CA_covid_map <- merge(CA_counties_sf_projection_zone1,covid_data_CA_Mar1820,by="county")
ggplot(CA_covid_map, 
       aes(fill = log(cases) %>% as.numeric)) +
  geom_sf() + 
  labs(title="Number of New Covid Cases (log-transformed) in California on March, 18, 2020") + 
  labs(fill = "New Cases (log-transformed)")

# Aug, 19, 2020  
CA_counties_sf_projection_zone1$county = CA_counties_sf_projection_zone1$NAME
CA_covid_map <- merge(CA_counties_sf_projection_zone1,covid_data_CA_Aug1920,by="county")
ggplot(CA_covid_map, 
       aes(fill = cases %>% as.numeric)) +
  geom_sf() + 
  labs(title="Number of New Covid Cases in California on Aug, 19, 2020") +
  labs(fill = "New Cases")

# Aug, 19, 2020 - log transformed
CA_counties_sf_projection_zone1$county = CA_counties_sf_projection_zone1$NAME
CA_covid_map <- merge(CA_counties_sf_projection_zone1,covid_data_CA_Aug1920,by="county")
ggplot(CA_covid_map, 
       aes(fill = log(cases) %>% as.numeric)) +
  geom_sf() + 
  labs(title="Number of New Covid Cases (log-transformed) in California on Aug, 19, 2020") +
  labs(fill = "New Cases (log-transformed)")

# Jan 19, 2021  
CA_counties_sf_projection_zone1$county = CA_counties_sf_projection_zone1$NAME
CA_covid_map <- merge(CA_counties_sf_projection_zone1,covid_data_CA_Jan1921,by="county")
ggplot(CA_covid_map, 
       aes(fill = cases %>% as.numeric)) +
  geom_sf() + 
  labs(title="Number of New Covid Cases in California on Jan 19, 2021") +
  labs(fill = "New Cases")

# Jan 19, 2021 - log transformed
CA_counties_sf_projection_zone1$county = CA_counties_sf_projection_zone1$NAME
CA_covid_map <- merge(CA_counties_sf_projection_zone1,covid_data_CA_Jan1921,by="county")
ggplot(CA_covid_map, 
       aes(fill = log(cases) %>% as.numeric)) +
  geom_sf() + 
  labs(title="Number of New Covid Cases (log-transformed) in California on Jan 19, 2021") +
  labs(fill = "New Cases (log-transformed)")

