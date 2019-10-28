library(tidyverse)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(sf)
library(geosphere)

#import the IRMA tweets
Irma_Tweets <- read_csv("DATA/Irma_Tweets.csv")

IP <- Irma_Tweets %>% 
  select(longitude, latitude)

#import USA (or Coastline)
USA <- ne_countries(country = 'United States of America', scale = 50, returnclass = "sp")
#CL <- ne_coastline( scale = 50, returnclass = "sp")

#find distance to coast from tweets
dist <- dist2Line(IP, USA)

#Join the Dist to tweets
Irma_Tweets_Coast <- cbind(Irma_Tweets, as.data.frame(dist))

Irma_Tweets_Coast <- Irma_Tweets_Coast %>% 
  rename(coast_longitude = lon, coast_latitude = lat) %>%
  select(-c(ID))
  
write_csv(Irma_Tweets_Coast,  "DATA/Irma_Tweets_Coast.csv")

##########
# #Plot US coastline
# ggplot(data = USA) +  geom_sf()



