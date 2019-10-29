#Use Hurdat2 track to compute the distance of a tweet from the eye at a given time

library(tidyverse)
library(geosphere)
library(zoo)
library(lubridate)


######################################################################
#import the IRMA tweets
Irma_Tweets <- read_csv("DATA/Irma_Tweets_Coast.csv")

Irma_Tweets <- Irma_Tweets %>%
  rename(CLat = coast_latitude, Clong = coast_longitude)

######################################################################
#Get the track in shape
#load Irma tracks
IrmaTrack <- read_csv("DATA/IrmaTrack.csv")

#hours from the track into the correct format
IrmaTrack <- IrmaTrack %>%  
  mutate(hour=recode(hour, 
                     `0` = '00:00:00',
                     `600` ='06:00:00',
                     `1200` ='12:00:00',
                     `1800` ='18:00:00',
                     `545` ='05:45:00',
                     `1115` ='11:15:00',
                     `1300` ='13:00:00',
                     `1630` ='16:30:00',
                     `300` ='03:00:00',
                     `500` ='05:00:00',
                     `1930` ='19:30:00'))

#put track date into the correct format
IrmaTrack$datetime <- ymd(IrmaTrack$datetime)

#combine date and time
IrmaTrack <- IrmaTrack %>%
  unite("DateAndTime", datetime:hour, sep = " ", remove = FALSE)

#make it a real datetime
IrmaTrack$DateAndTime <- ymd_hms(IrmaTrack$DateAndTime)

#remove N and W from lat and long, respectively. Add a '-' sign before long value
IrmaTrack$lat = as.numeric(substr(IrmaTrack$lat,1,nchar(IrmaTrack$lat)-1))
IrmaTrack$log = -as.numeric(substr(IrmaTrack$log,1,nchar(IrmaTrack$log)-1))

############################################################
#Find the location of eye at the time of each tweet (using zoo)

#Latitude
HLa <- zoo( IrmaTrack$lat,IrmaTrack$DateAndTime)
#Make a tweet time data frame
TT <- unique(Irma_Tweets$created_at)
#and a vector of length TT that is an NA
TLa <- zoo(rep(NA, length(TT)),TT)
#make a zoo object
za <- merge(HLa,TLa)
#Interpolate to find time
EyeLocationatTweetLa <- fortify.zoo(na.approx(za))

#Longitude
HLo <- zoo( IrmaTrack$log,IrmaTrack$DateAndTime)
#Make a tweet time data frame
TT <- unique(Irma_Tweets$created_at)
#and a vector of length TT that is an NA
TLo <- zoo(rep(NA, length(TT)),TT)
#make a zoo object
zo <- merge(HLo,TLo)
#Interpolate to find time
EyeLocationatTweetLo <- fortify.zoo(na.approx(zo))

##############################

#Join Eye location to tweet times in Irma_Tweets
Irma_TweetsT <-  left_join(Irma_Tweets, EyeLocationatTweetLa, by = c("created_at" = "Index"))
Irma_TweetsT <-  left_join(Irma_TweetsT, EyeLocationatTweetLo, by = c("created_at" = "Index"))

#remove anre rename some columns
Irma_TweetsT <- Irma_TweetsT %>%
  select(-TLa, -TLo) %>%
  rename(EyeLat = HLa, EyeLong = HLo)


# need to compute the geodesic of the tweet to that location (using geosphere)
TweetLoc <- Irma_TweetsT %>% 
  select(longitude, latitude)

EyeLocation <- Irma_TweetsT %>% 
  select(EyeLong, EyeLat)

dist <- distGeo(TweetLoc,EyeLocation)

#Join the Dist to tweets
Irma_Tweets_Coast_Eye <- cbind(Irma_TweetsT, as.data.frame(dist))

Irma_Tweets_Coast_Eye  <- Irma_Tweets_Coast_Eye  %>% 
  rename(DistanceToEye = dist) 

#write to csv
write_csv(Irma_Tweets_Coast_Eye,  "DATA/Irma_Tweets_Coast_Eye.csv")

# Make plot of tweeted pictures with distance (x) and image score (y) 
ggplot(Irma_Tweets_Coast_Eye, aes(x=DistanceToEye, y=imageScore)) + geom_point() + 
  geom_smooth()

# make plot of tweeted picture with time (x) and image score (y) 
ggplot(Irma_Tweets_Coast_Eye, aes(x=created_at, y=imageScore)) + geom_point() + 
  geom_smooth()


