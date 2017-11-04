library(data.table)
library(dplyr)


setwd('~/Documents/EDA/group_proj_1/')

#read in data dictionary
dictionary <- fread('CSDataDictionary/data_dictionary-Table 1.csv',data.table = FALSE, na.strings = c('','NULL'))
#save the variable name and the more readable dev-friendly names
readable_names <-  dictionary[,c('VARIABLE NAME','developer-friendly name')]
readable_names$`developer-friendly name` <- gsub(' ','_',readable_names$`developer-friendly name`)


#list of specific column names
wanted_cols <- scan('colnamesweneed.txt',character())
colleges <- fread('CollegeScorecard_Raw_Data/MERGED2015_16_PP.csv', select = wanted_cols ,data.table = FALSE,na.strings = c('','NULL'))

#select only desired columns in readable_names
readable_names <- filter(readable_names,readable_names$`VARIABLE NAME` %in% wanted_cols)

#reorder readable_names to match the order of column names in colleges data frame
readable_names <- readable_names[match(colnames(colleges),readable_names$`VARIABLE NAME`),]
#check to make sure the order is correct
sum(colnames(colleges) == readable_names$`VARIABLE NAME`)
#rename columns of colleges with dev-friendly name
colnames(colleges) <- readable_names$`developer-friendly name`

#get names of all files of raw data
raw_filenames <- list.files('./CollegeScorecard_Raw_Data/')

#create empty data frame with column names to hold data for all years and add a column for year
all_data <- data.frame(matrix(ncol=length(wanted_cols) + 1,nrow=0))
colnames(all_data) <- c('year',readable_names$`developer-friendly name`)

#read in each csv and add to all_data
setwd('./CollegeScorecard_Raw_Data/')
for(fn in raw_filenames){
  #read in raw data, only selecting the columns we want
  current <- fread(fn, select = wanted_cols ,data.table = FALSE,na.strings = c('','NULL'))
  
  #check that the current data_frame has col names in the same order as readable_names, which is the same order in readable_names
  if(sum(colnames(current) == readable_names$`VARIABLE NAME`) == ncol(current)) print('Column names in correct order!')
  
  #add column with academic year e.g. '2015_16'
  current <- cbind.data.frame(rep_len(gsub('MERGED|_PP|.csv','',fn),nrow(current)), current)
  #assign the dev friendly col names
  colnames(current) <- colnames(all_data)
  #add to all_data
  all_data <- rbind.data.frame(all_data,current)
}

#count number of non-nulls for each column
num_non_null <- colSums((is.na(all_data))==FALSE)

#get names of columns that have 50% or more non-null values
non_null_cols <- which((num_non_null/nrow(all_data)) > 0.5)




