library(data.table)
library(dplyr)
# to read .xls
library(gdata)
# can clean zipcodes from ints to proper zips (missing leading 0's) also has lat and lng for zips
library(zipcode)


setwd('/Users/Mike/Desktop/')

#read in data dictionary
dictionary <- fread('EDA_College-Scorecard1/data_dictionary-Table 1.csv',data.table = FALSE, na.strings = c('','NULL'))
#save the variable name and the more readable dev-friendly names
readable_names <-  dictionary[,c('VARIABLE NAME','developer-friendly name')]
readable_names$`developer-friendly name` <- gsub(' ','_',readable_names$`developer-friendly name`)


#list of specific column names
wanted_cols <- scan('EDA_College-Scorecard1/colnamesweneed.txt',character())
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



library(ggplot2)
library(ggmap)
library(RColorBrewer)
# count nulls based on year
library(doBy)
library(magrittr)
library(dplyr)
# how many rows per year
yearcounts <- c(unname(table(all_data$year)))
# count how many na's in each column for each year
nacount <- all_data %>%
  group_by(year) %>%
  summarise_all(funs(sum(is.na(.))))

# get a score for each column for each year
completeness.by.year <- 1-nacount[,-c(1)]/yearcounts

# remove columns that have <50% values for every year
bool.complete.by.year <- completeness.by.year > .5
bad_cols <- names(which(colSums(bool.complete.by.year)==0))
good_cols <- names(all_data)[!(names(all_data) %in% bad_cols)]
cleaned_data <- as.data.frame(all_data)[,good_cols]

# convert character columns to numer where necessary
cleaned_data <- transform(cleaned_data, share_firstgeneration = as.numeric(share_firstgeneration),
                          share_firstgeneration_parents.highschool = as.numeric(share_firstgeneration_parents.highschool),
                          share_firstgeneration_parents.middleschool = as.numeric(share_firstgeneration_parents.middleschool),
                          share_firstgeneration_parents.somecollege = as.numeric(share_firstgeneration_parents.somecollege),
                          demographics.age_entry = as.numeric(demographics.age_entry),
                          demographics.over_23_at_entry = as.numeric(demographics.over_23_at_entry),
                          demographics.first_generation = as.numeric(demographics.first_generation))



map<-get_map(location='united states', zoom=4, maptype = "terrain",
                   source='google',color='color')

YlOrBr <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")


ggmap(map) + geom_density(data=all_data1, aes(x=all_data1$longitude, y=all_data1$latitude, fill=..level.., alpha=..level..),
               geom="polygon", size=0.01, bins=16) +
  scale_fill_gradient(low="red", high="green") +
  scale_alpha(range = c(0,0.1), guide=FALSE)

ggmap(map) + geom_point(
  aes(x=all_data1$longitude, y=all_data1$latitude), colour = "red", 
  data=all_data1, alpha=.5, na.rm = T) + labs(title = "Location of Colleges") + ggsave("Location_of_College.pdf")


ggplot(all_data1, aes(x = all_data1$longitude, y = all_data1$latitude)) + 
  geom_point() + ggmap(map)
  stat_density2d(aes(fill=..density..), geom = "tile", contour = FALSE) + ggmap()
  scale_fill_gradient2(low = "white", high = "red")

ggplot(data = cleaned_data, aes(faculty_salary,program_percentage.science_technology)) + geom_point() + labs(title = "Percentage of Science and Tech Programs vs Faculty Salary")
+ ggsave("Tech Programs vs Faculty Salary.pdf")



ggplot(data = cleaned_data, aes(x=faculty_salary)) + 
  geom_line(aes(y=program_percentage.engineering_technology, color="Engineering Tech")) + 
  geom_line(aes(y=program_percentage.education, color="Education")) + 
  geom_line(aes(y= program_percentage.military, color="Military")) + 
  labs(title = "Program Percentages vs Faculty Salary") + 
  xlab("Faculty Salary") + 
  ylab("Program Percentages") + 
  ggsave("Multiple Program Percentages vs Faculty Salary.pdf")
  

jpeg('location_of_college')

g <- ggplot(clean)

saveRDS(location_of_college, file = "Plotted College Locatiob.rds")

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
setwd('/Users/Mike/Desktop/Datasets')
saveRDS(all_data, file="CollegeScorecard_Rurality.rds")
all_data <- readRDS("CollegeScorecard_Rurality.rds")

# Completeness checks
# replace "PrivacySuppressed" with NA
all_data[all_data == "PrivacySuppressed"] <- NA


g <- ggplot(cleaned_data, aes(cleaned_data$state, cleaned_data$faculty_salary))

ggplot(collegeData, aes(cleaned_data$state)) + geom_bar(fill = "blue", position = position_stack(reverse = TRUE))  + xlab("State") + labs(title="Number of colleges per State")+ ggsave("Number of Colleges per State.pdf")


a <- aggregate(cleaned_data, list(state = cleaned_data$state), mean)

group_by(cleaned_data, state) %>% summarise_all(funs(mean(., na.rm = TRUE)), cleaned_data$faculty_salary)

library(plyr)
new <- subset(collegeData, select = c(state,rurality))
new[is.na(new)] <- 0
z <- ddply(new, .(state), summarize, rural = mean(rurality))
ggplot(z, aes(z$state,z$rural)) + geom_point() + ggsave("Average Rural Score per State.pdf")

barplot(z)

rurality_state <- collegeData$

cleaned_data$

# save data
setwd('/Users/Mike/Desktop/EDA_College-Scorecard')
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
#setwd('/Users/Mike/Desktop/Datasets')
#saveRDS(completeness.by.year, file="Completeness_Clean_CollegeScorecard_Rurality.rds")
#completeness.by.year <- readRDS("Completeness_Clean_CollegeScorecard_Rurality.rds")


