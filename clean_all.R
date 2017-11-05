library(data.table)
library(dplyr)
# to read .xls
library(gdata)
# can clean zipcodes from ints to proper zips (missing leading 0's) also has lat and lng for zips
library(zipcode)


setwd('/home/kevinisagirl/Desktop/workspace/datamunging/Project')

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
setwd('/home/kevinisagirl/Desktop/workspace/datamunging/Project/EDA_College-Scorecard/')
saveRDS(all_data, file="CollegeScorecard_combinedyears.rds")
all_data <- readRDS("CollegeScorecard_combinedyears.rds")

#add missing lat and longs based on zips

#clean College data zipcodes - some have trailing 4 digits
all_data$zip <-sapply(all_data$zip, function(x) substr(x, 1, 5))

#update lat and lng
data(zipcode)
nm <- c("location.lat", "location.lon")
ll <- c("latitude", "longitude")
all_data[nm] <- zipcode[match(all_data$zip, zipcode$zip), ll]

# add rurality
all_data$rurality <- NA
# Data in sheet 4, documentation in other sheets of the .xls file
# We are using the rurality codes documented in tab A1
zipcoderuralitydata = read.xls("http://www.psc.isr.umich.edu/dis/data/kb/downloads/t1101_ziprural.xls", sheet=5, stringsAsFactors = FALSE)
#zip codes in rural data are missing leading 0's
zipcoderuralitydata$zip <- clean.zipcodes(zipcoderuralitydata$zip)
tsn <- "rurality"
zcrdn <- "ru2003"
all_data[tsn] <- zipcoderuralitydata[match(all_data$zip, zipcoderuralitydata$zip), zcrdn]

# save state for continuation
setwd('/home/kevinisagirl/Desktop/workspace/datamunging/Project/EDA_College-Scorecard/')
saveRDS(all_data, file="CollegeScorecard_Rurality.rds")
all_data <- readRDS("CollegeScorecard_Rurality.rds")

# Completeness checks
# replace "PrivacySuppressed" with NA
all_data[all_data == "PrivacySuppressed"] <- NA

# count nulls based on year
library(doBy)
# how many rows per year
yearcounts <- c(unname(table(all_data$year)))
# count how many na's in each column for each year
nacount <- all_data %>%
              group_by(year) %>%
              summarise_each(funs(sum(is.na(.))))

# get a score for each column for each year
completeness.by.year <- 1-nacount[,-c(1)]/yearcounts

# remove columns that have <50% values for every year
bool.complete.by.year <- completeness.by.year > .5
bad_cols <- names(which(colSums(bool.complete.by.year)==0))
good_cols <- names(all_data)[!(names(all_data) %in% bad_cols)]
cleaned_data <- as.data.frame(all_data)[,good_cols]

# save data
setwd('/home/kevinisagirl/Desktop/workspace/datamunging/Project/EDA_College-Scorecard/')
saveRDS(cleaned_data, file="Clean_CollegeScorecard_Rurality.rds")
cleaned_data <- readRDS("Clean_CollegeScorecard_Rurality.rds")

# create new completeness table for use in data analysis
yearcounts <- c(unname(table(cleaned_data$year)))
nacount <- all_data %>%
  group_by(year) %>%
  summarise_each(funs(sum(is.na(.))))

# get a score for each column for each year
# use this to help when analyzing data
# note, we have College Scorecard Locale for 2015_16
completeness.by.year <- cbind(year = nacount$year, 1-nacount[,-c(1)]/yearcounts, stringsAsFactors=FALSE)
# save data
setwd('/home/kevinisagirl/Desktop/workspace/datamunging/Project/EDA_College-Scorecard/')
saveRDS(completeness.by.year, file="Completeness_Clean_CollegeScorecard_Rurality.rds")
completeness.by.year <- readRDS("Completeness_Clean_CollegeScorecard_Rurality.rds")


