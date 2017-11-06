library(ggplot2)
library(ggmap)
library(RColorBrewer)
library(data.table)
library(dplyr)
# to read .xls
library(gdata)
# can clean zipcodes from ints to proper zips (missing leading 0's) also has lat and lng for zips
library(zipcode)


setwd('/Users/Mike/Desktop/')

#read in data dictionary
dictionary <- fread('EDA_College-Scorecard/data_dictionary-Table 1.csv',data.table = FALSE, na.strings = c('','NULL'))
#save the variable name and the more readable dev-friendly names
readable_names <-  dictionary[,c('VARIABLE NAME','developer-friendly name')]
readable_names$`developer-friendly name` <- gsub(' ','_',readable_names$`developer-friendly name`)


#list of specific column names
wanted_cols <- scan('EDA_College-Scorecard/colnamesweneed.txt',character())
colleges <- fread('datasets/CollegeScorecard_Raw_Data/MERGED2015_16_PP.csv', select = wanted_cols ,data.table = FALSE,na.strings = c('','NULL'))

#select only desired columns in readable_names
readable_names <- filter(readable_names,readable_names$`VARIABLE NAME` %in% wanted_cols)

#reorder readable_names to match the order of column names in colleges data frame
readable_names <- readable_names[match(colnames(colleges),readable_names$`VARIABLE NAME`),]
#check to make sure the order is correct
sum(colnames(colleges) == readable_names$`VARIABLE NAME`)
#rename columns of colleges with dev-friendly name
colnames(colleges) <- readable_names$`developer-friendly name`

#get names of all files of raw data
raw_filenames <- list.files('datasets/CollegeScorecard_Raw_Data/', pattern = "*.csv", recursive = FALSE)

#create empty data frame with column names to hold data for all years and add a column for year
all_data <- data.frame(matrix(ncol=length(wanted_cols) + 1,nrow=0))
colnames(all_data) <- c('year',readable_names$`developer-friendly name`)

#read in each csv and add to all_data
setwd('datasets/CollegeScorecard_Raw_Data/')
for(fn in raw_filenames){
  #read in raw data, only selecting the columns we want
  current <- fread(fn, select = wanted_cols ,data.table = FALSE,na.strings = c('','NULL'))
  
  #check that the current data.frame has col names in the same order as readable_names, which is the same order in all_data
  if(sum(colnames(current) == readable_names$`VARIABLE NAME`) == ncol(current)) print('Column names in correct order!')
  
  #add column with academic year e.g. '2015_16'
  current <- cbind.data.frame(rep_len(gsub('MERGED|_PP|.csv','',fn),nrow(current)), current, stringsAsFactors = FALSE)
  #assign the dev friendly col names
  colnames(current) <- colnames(all_data)
  #add to all_data
  all_data <- rbind.data.frame(all_data,current)
}

# Save current state for easy continuation
setwd('/Users/Mike/Desktop/Datasets')
saveRDS(all_data, file="CollegeScorecard_combinedyears.rds")
all_data <- readRDS("CollegeScorecard_combinedyears.rds")

#add missing lat and longs based on zips

#clean College data zipcodes - some have trailing 4 digits
all_data$zip <-sapply(all_data$zip, function(x) substr(x, 1, 5))

#update lat and lng
data(zipcode)
nm <- c("location.lat", "location.lon")
ll <- c("latitude", "longitude")
all_data1 <- zipcode[match(all_data$zip, zipcode$zip), ll]

points <- all_data[nm]

map<-get_map(location='united states', zoom=4, maptype = "terrain",
             source='google',color='color')

YlOrBr <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")


ggmap(map) + geom_density(data=all_data1, aes(x=all_data1$longitude, y=all_data1$latitude, fill=..level.., alpha=..level..),
                          geom="polygon", size=0.01, bins=16) +
  scale_fill_gradient(low="red", high="green") +
  scale_alpha(range = c(0,0.1), guide=FALSE)

ggmap(map) + geom_point(
  aes(x=all_data1$longitude, y=all_data1$latitude), colour = 'red', 
  data=all_data1, alpha=.5, na.rm = T) + labs(title = "Location of Colleges") + ggsave("Location_of_College.pdf")
