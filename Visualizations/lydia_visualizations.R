library(ggplot2)
library(dplyr)

setwd('~/Documents/EDA/group_proj_1/EDA_College-Scorecard/')
collegeData <- readRDS("Clean_CollegeScorecard_Rurality.rds")
completeness <- readRDS("Completeness_Clean_CollegeScorecard_Rurality.rds")


setwd('~/Documents/EDA/group_proj_1/EDA_College-Scorecard/Visualizations/')

ggplot(collegeData[collegeData$year == "2015_16",], aes(x=factor(rurality), y=program_percentage.theology_religious_vocation)) + stat_summary(fun.y="mean", geom = "bar") + ggsave("theology.png")

ggplot(collegeData[collegeData$year == "2015_16",], aes(x=factor(rurality), y=program_percentage.construction)) + stat_summary(fun.y="mean", geom = "bar") + ggsave("construction.png")

ggplot(collegeData[collegeData$year == "2015_16",], aes(x=factor(rurality), y=program_percentage.mathematics)) + stat_summary(fun.y="mean", geom = "bar") + ggsave("math.png")

ggplot(collegeData[collegeData$year == "2015_16",], aes(x=factor(rurality), y=program_percentage.security_law_enforcement)) + stat_summary(fun.y="mean", geom = "bar") + ggsave("law_enfor.png")

ggplot(collegeData[collegeData$year == "2015_16",], aes(x=factor(rurality), y=program_percentage.computer)) + stat_summary(fun.y="mean", geom = "bar") + ggsave("computer.png")

ggplot(collegeData[collegeData$year == "2015_16",], aes(x=factor(rurality), y=program_percentage.visual_performing)) + stat_summary(fun.y="mean", geom = "bar") + ggsave("visual_perf.png")

ggplot(collegeData[collegeData$year == "2015_16",], aes(x=factor(rurality), y=program_percentage.personal_culinary)) + stat_summary(fun.y="mean", geom = "bar") + ggsave("pers_culi.png")

ggplot(collegeData[collegeData$year == "2015_16",], aes(x=factor(rurality), y=program_percentage.education)) + stat_summary(fun.y="mean", geom = "bar") + ggsave("education.png")


library(ggmap)

map<-get_map(location='united states', zoom=4, maptype = "terrain",source='google',color='color')

coll_1516 = collegeData[collegeData$year == "2015_16",]

ggmap(map) + geom_point(
  aes(x=location.lon, y=location.lat, show_guide=TRUE ,colour = rurality),
  data=coll_1516, alpha=.5, na.rm = T) + labs(title = "Colleges by Rurality Level") + ggsave("rurality_map.pdf")

ggmap(map) + geom_point(
  aes(x=location.lon, y=location.lat, show_guide=TRUE), color = 'red',
  data=coll_1516, alpha=.5, na.rm = T) + labs(title = "All Colleges") + ggsave("colleges_map.png")


