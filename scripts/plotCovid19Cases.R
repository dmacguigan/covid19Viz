# Script to make maps of Covid-19 cases in the United States
# Dan MacGuigan
# Yale University

# NEED TO HAVE ImageMacgick installed to create gifs
# https://imagemagick.org/

# load packages
library(ncdf4)
library(maps)
library(rgdal)
library(scales)
library(gridExtra)
library(maptools)
library(scales)
library(raster)
library(rasterVis)
library(rgeos)
library(ggplot2)
library(grid)
library(svglite)
library(mapplots)
library(mapproj)
library(sf)
library(stringr)
library(filesstrings)
library(plyr)
#needed for plotting
options(scipen=1000000) 


########################################################################################################################
# read and clean Covid-19 case data
########################################################################################################################

# read in data from Johns Hopkins
# https://github.com/CSSEGISandData/COVID-19
# get data with "git clone https://github.com/CSSEGISandData/COVID-19 JHU_COVID-19"
setwd("/Users/dmacguigan/Documents/covid19/JHU_COVID-19/csse_covid_19_data/csse_covid_19_daily_reports")
files <- list.files(path=".", pattern="*.csv")
caseData = lapply(files, read.csv)
cleanCaseData <- list()

# read in list of US counties and states
stateCountyRef <- read.csv("/Users/dmacguigan/Documents/covid19/data/stateList.csv", header=TRUE)
stateCountyRef$FIPS <- formatC(stateCountyRef$FIPS, width=2, format="d", flag = "0")
stateCountyRef$stateFIPS_name <- paste(stateCountyRef$FIPS, stateCountyRef$county, sep="_")
stateAbrv <- unique(data.frame(stateCountyRef$state, stateCountyRef$abrv))
colnames(stateAbrv) <- c("state", "abrv")

# extract rows with "US" in "Country.Region" column
# also extract only the country, state, and confirmed cases columns
for(i in 1:10){
  print(i)
  t <- subset(caseData[[i]], Country.Region == "US")
  t <- droplevels(t) # drop unused levels in data frame
  colnames(t)[which(names(t) == "X...Province.State")] <- "Province.State"
  t1 <- aggregate(t$Confirmed, by=list(Category=t$Province.State), FUN=sum) # sum confirmed cases by state
  t1$Category <- as.factor(t1$Category)
  t2 <- aggregate(t$Deaths, by=list(Category=t$Province.State), FUN=sum) # sum confirmed cases by state
  cleanCaseData[[i]] <- data.frame(t1$Category,t1$x,t2$x)
  colnames(cleanCaseData[[i]]) <- c("Province.State","Confirmed","Deaths")
}

# extract rows with "US" in "Country.Region" column
# also extract only the country, state, and confirmed cases columns'
# need to substitute state abbreviations in these dfs
for(i in 11:30){
  print(i)
  t <- subset(caseData[[i]], Country.Region == "US")
  t <- droplevels(t) # drop unused levels in data frame
  t <- t[grepl(", ", t$Province.State),] # get only rows that contain ", "
  t$Province.State <- unlist(lapply((strsplit(as.character(t$Province.State), split=", ")),`[[`, 2)) # split state abbreviation from Province.State column
  # replace abbreviations with state names
  for(j in 1:nrow(stateAbrv)){
    t$Province.State[t$Province.State==as.character(stateAbrv$abrv[j])] <- as.character(stateAbrv$state[j])
  }
  t <- t[which(t$Province.State %in% as.character(stateAbrv$state)),] # get only rows that have state names
  t1 <- aggregate(t$Confirmed, by=list(Category=t$Province.State), FUN=sum) # sum confirmed cases by state
  t1$Category <- as.factor(t1$Category) 
  t2 <- aggregate(t$Deaths, by=list(Category=t$Province.State), FUN=sum) # sum confirmed cases by state
  cleanCaseData[[i]] <- data.frame(t1$Category,t1$x,t2$x)
  colnames(cleanCaseData[[i]]) <- c("Province.State","Confirmed","Deaths")
}

# have to deal with this day's data seperately because Diamond Princess cruise was not properly recorded
# extract rows with "US" in "Country.Region" column
# also extract only the country, state, and confirmed cases columns'
# need to substitute state abbreviations in these dfs
i=31
print(i)
t <- subset(caseData[[i]], Country.Region == "US")
t <- droplevels(t) # drop unused levels in data frame
t <- t[grepl(", ", t$Province.State),] # get only rows that contain ", "
t <- t[-c(1,2,4),] # remove rows that contain data from Diamond Princess cruise
t$Province.State <- unlist(lapply((strsplit(as.character(t$Province.State), split=", ")),`[[`, 2)) # split state abbreviation from Province.State column
# replace abbreviations with state names
for(j in 1:nrow(stateAbrv)){
  t$Province.State[t$Province.State==as.character(stateAbrv$abrv[j])] <- as.character(stateAbrv$state[j])
}
t <- t[which(t$Province.State %in% as.character(stateAbrv$state)),] # get only rows that have state names
t1 <- aggregate(t$Confirmed, by=list(Category=t$Province.State), FUN=sum) # sum confirmed cases by state
t1$Category <- as.factor(t1$Category) 
t2 <- aggregate(t$Deaths, by=list(Category=t$Province.State), FUN=sum) # sum confirmed cases by state
cleanCaseData[[i]] <- data.frame(t1$Category,t1$x,t2$x)
colnames(cleanCaseData[[i]]) <- c("Province.State","Confirmed","Deaths")

# extract rows with "US" in "Country.Region" column
# also extract only the country, state, and confirmed cases columns'
# need to substitute state abbreviations in these dfs
for(i in 32:48){
  print(i)
  t <- subset(caseData[[i]], Country.Region == "US")
  t <- droplevels(t) # drop unused levels in data frame
  t <- t[grepl(", ", t$Province.State),] # get only rows that contain ", "
  t$Province.State <- unlist(lapply((strsplit(as.character(t$Province.State), split=", ")),`[[`, 2)) # split state abbreviation from Province.State column
  # replace abbreviations with state names
  for(j in 1:nrow(stateAbrv)){
    t$Province.State[t$Province.State==as.character(stateAbrv$abrv[j])] <- as.character(stateAbrv$state[j])
  }
  t <- t[which(t$Province.State %in% as.character(stateAbrv$state)),] # get only rows that have state names
  t1 <- aggregate(t$Confirmed, by=list(Category=t$Province.State), FUN=sum) # sum confirmed cases by state
  t1$Category <- as.factor(t1$Category) 
  t2 <- aggregate(t$Deaths, by=list(Category=t$Province.State), FUN=sum) # sum confirmed cases by state
  cleanCaseData[[i]] <- data.frame(t1$Category,t1$x,t2$x)
  colnames(cleanCaseData[[i]]) <- c("Province.State","Confirmed","Deaths")
}

# extract rows with "US" in "Country.Region" column
# also extract only the country, state, and confirmed cases columns
for(i in 49:51){
  print(i)
  t <- subset(caseData[[i]], Country.Region == "US")
  t <- droplevels(t) # drop unused levels in data frame
  t1 <- aggregate(t$Confirmed, by=list(Category=t$Province.State), FUN=sum) # sum confirmed cases by state
  t1$Category <- as.factor(t1$Category) 
  t2 <- aggregate(t$Deaths, by=list(Category=t$Province.State), FUN=sum) # sum confirmed cases by state
  cleanCaseData[[i]] <- data.frame(t1$Category,t1$x,t2$x)
  colnames(cleanCaseData[[i]]) <- c("Province.State","Confirmed","Deaths")
}

i=52
print(i)
t <- subset(caseData[[i]], Country.Region == "US")
t <- droplevels(t) # drop unused levels in data frame
colnames(t)[which(names(t) == "X...Province.State")] <- "Province.State"
t1 <- aggregate(t$Confirmed, by=list(Category=t$Province.State), FUN=sum) # sum confirmed cases by state
t1$Category <- as.factor(t1$Category) 
t2 <- aggregate(t$Deaths, by=list(Category=t$Province.State), FUN=sum) # sum confirmed cases by state
cleanCaseData[[i]] <- data.frame(t1$Category,t1$x,t2$x)
colnames(cleanCaseData[[i]]) <- c("Province.State","Confirmed","Deaths")

# extract rows with "US" in "Country.Region" column
# also extract only the country, state, and confirmed cases columns
for(i in 53:60){
  print(i)
  t <- subset(caseData[[i]], Country.Region == "US")
  t <- droplevels(t) # drop unused levels in data frame
  t1 <- aggregate(t$Confirmed, by=list(Category=t$Province.State), FUN=sum) # sum confirmed cases by state
  t1$Category <- as.factor(t1$Category) 
  t2 <- aggregate(t$Deaths, by=list(Category=t$Province.State), FUN=sum) # sum confirmed cases by state
  cleanCaseData[[i]] <- data.frame(t1$Category,t1$x,t2$x)
  colnames(cleanCaseData[[i]]) <- c("Province.State","Confirmed","Deaths")
}

# THIS IS WHERE COUNTY DATA STARTS BEING REPORTED
# extract rows with "US" in "Country_Region" column
# also change the column headers to match first 60 data structures
# also extract only the country, state, county, and confirmed cases columns
for(i in 61:length(caseData)){
  print(i)
  t <- subset(caseData[[i]], Country_Region == "US")
  t <- droplevels(t) # drop unused levels in data frame
  colnames(t)[which(names(t) == "Country_Region")] <- "Country.Region"
  colnames(t)[which(names(t) == "Province_State")] <- "Province.State"
  colnames(t)[which(names(t) == "Admin2")] <- "County"
  t1 <- aggregate(t$Confirmed, by=list(Category=t$Province.State), FUN=sum) # sum confirmed cases by state
  t1$Category <- as.factor(t1$Category) 
  t2 <- aggregate(t$Deaths, by=list(Category=t$Province.State), FUN=sum) # sum confirmed cases by state
  cleanCaseData[[i]] <- data.frame(t1$Category,t1$x,t2$x)
  colnames(cleanCaseData[[i]]) <- c("Province.State","Confirmed","Deaths")
}

# replace all NA values with 0s
cleanCaseData <- lapply(cleanCaseData, function(d) { d[is.na(d)] <- 0; d })

# write all cleaned data to new csv files
setwd("/Users/dmacguigan/Documents/covid19/data")
for(i in 1:length(cleanCaseData)){
  write.csv(x = cleanCaseData[[i]], file = files[i], quote = FALSE, row.names = FALSE, col.names = TRUE)
}


########################################################################################################################
# plot Covid-19 case data by state
########################################################################################################################

# read in state shapefiles
# data from https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
states <- readOGR("/Users/dmacguigan/Documents/covid19/shapefiles/cb_2018_us_state_5m", "cb_2018_us_state_5m")
states@data$id = rownames(states@data)
states.points = fortify(states, region="id")
states.df = join(states.points, states@data, by="id")

# read in list of US counties and states
stateCountyRef <- read.csv("/Users/dmacguigan/Documents/covid19/data/stateList.csv", header=TRUE)
stateCountyRef$FIPS <- formatC(stateCountyRef$FIPS, width=2, format="d", flag = "0")
stateCountyRef$stateFIPS_name <- paste(stateCountyRef$FIPS, stateCountyRef$county, sep="_")

# plot map
for(i in 1:length(cleanCaseData)){
  print(i)
  
  ##############################
  # cumulative cases
  ##############################
  
  # create some temporary data structures
  temp.df <- states.df
  temp.df$Confirmed <- rep(0,nrow(temp.df))
  
  # function to replace values in temp.df with matching number of Confirmed cases
  replace <- function(toReplaceVector, toMatchVector, matchValue, replaceValue){
    matchIndex <- which(toMatchVector == matchValue)
    toReplaceVector[matchIndex] <- replaceValue
    return(toReplaceVector)
  }
  
  for(j in 1:nrow(cleanCaseData[[i]])){
    temp.df$Confirmed <- replace(temp.df$Confirmed, 
            as.character(temp.df$NAME),
            as.character(cleanCaseData[[i]]$Province.State[j]),
            cleanCaseData[[i]]$Confirmed[j] )
  }
  
  # plot cumulative cases map
  map <- ggplot(temp.df) +
    aes(long, lat, group=group, fill=Confirmed) +
    geom_polygon(colour = "gray", lwd=0.4) +
    ylim(c(24, 50)) +
    xlim(c(-127, -65)) +
    theme_minimal() +
    ggtitle("Cumulative confirmed COVID-19 cases") +
    scale_fill_gradientn(name="", 
                        colours=c("white", "yellow", "#fd8d3c", "#e31a1c", "#800026", "black"),
                        #values=c(0,1000,100000),
                        trans="pseudo_log", labels=comma,
                        limits=c(0,1000000), breaks=c(0,10,100,1000,10000,100000,1000000)) +
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank()) +
    coord_map()
  
  ##############################
  # daily new cases
  ##############################
  
  # create some temporary data structures
  temp.df_prevDay <- states.df
  temp.df_prevDay$Confirmed <- rep(0,nrow(temp.df))
    
  if(i > 1){
    for(j in 1:nrow(cleanCaseData[[i-1]])){
      temp.df_prevDay$Confirmed <- replace(temp.df_prevDay$Confirmed, 
                                   as.character(temp.df$NAME),
                                   as.character(cleanCaseData[[i-1]]$Province.State[j]),
                                   cleanCaseData[[i-1]]$Confirmed[j] )
    }
    temp.df$Confirmed <- temp.df$Confirmed - temp.df_prevDay$Confirmed
    temp.df$Confirmed[temp.df$Confirmed<0] <- 0
  }
  
  # plot daily new cases map
  map2 <- ggplot(temp.df) +
    aes(long, lat, group=group, fill=Confirmed) +
    geom_polygon(colour = "gray", lwd=0.4) +
    ylim(c(24, 50)) +
    xlim(c(-127, -65)) +
    theme_minimal() +
    ggtitle("Daily new confirmed COVID-19 cases") +
    scale_fill_gradientn(name="", 
                        colours=c("white", "yellow", "#fd8d3c", "#e31a1c", "#800026", "black"),
                        #values=c(0,1000,100000),
                        trans="pseudo_log", labels=comma,
                        limits=c(0,1000000), breaks=c(0,10,100,1000,10000,100000,1000000)) +
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank()) +
    coord_map()

  ##############################
  # cumulative deaths
  ##############################
  
  # create some temporary data structures
  temp.df <- states.df
  temp.df$Deaths <- rep(0,nrow(temp.df))
  
  # function to replace values in temp.df with matching number of Confirmed deaths
  replace <- function(toReplaceVector, toMatchVector, matchValue, replaceValue){
    matchIndex <- which(toMatchVector == matchValue)
    toReplaceVector[matchIndex] <- replaceValue
    return(toReplaceVector)
  }
  
  for(j in 1:nrow(cleanCaseData[[i]])){
    temp.df$Deaths <- replace(temp.df$Deaths, 
            as.character(temp.df$NAME),
            as.character(cleanCaseData[[i]]$Province.State[j]),
            cleanCaseData[[i]]$Deaths[j] )
  }
  
  # plot cumulative deaths map
  map3 <- ggplot(temp.df) +
    aes(long, lat, group=group, fill=Deaths) +
    geom_polygon(colour = "gray", lwd=0.4) +
    ylim(c(24, 50)) +
    xlim(c(-127, -65)) +
    theme_minimal() +
    ggtitle("Cumulative confirmed COVID-19 deaths") +
    scale_fill_gradientn(name="", 
                        colours=c("white", "#9ecae1", "#4292c6", "#08519c", "#08519c", "black"),
                        #values=c(0,1000,100000),
                        trans="pseudo_log", labels=comma,
                        limits=c(0,1000000), breaks=c(0,10,100,1000,10000,100000,1000000)) +
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank()) +
    coord_map()
  
  ##############################
  # daily new deaths
  ##############################
  
  # create some temporary data structures
  temp.df_prevDay <- states.df
  temp.df_prevDay$Deaths <- rep(0,nrow(temp.df))
    
  if(i > 1){
    for(j in 1:nrow(cleanCaseData[[i-1]])){
      temp.df_prevDay$Deaths <- replace(temp.df_prevDay$Deaths, 
                                   as.character(temp.df$NAME),
                                   as.character(cleanCaseData[[i-1]]$Province.State[j]),
                                   cleanCaseData[[i-1]]$Deaths[j] )
    }
    temp.df$Deaths <- temp.df$Deaths - temp.df_prevDay$Deaths
    temp.df$Deaths[temp.df$Deaths<0] <- 0
  }
  
  # plot daily new deaths map
  map4 <- ggplot(temp.df) +
    aes(long, lat, group=group, fill=Deaths) +
    geom_polygon(colour = "gray", lwd=0.4) +
    ylim(c(24, 50)) +
    xlim(c(-127, -65)) +
    theme_minimal() +
    ggtitle("Daily new confirmed COVID-19 deaths") +
    scale_fill_gradientn(name="", 
                         colours=c("white", "#9ecae1", "#4292c6", "#08519c", "#08519c", "black"),
                        #values=c(0,1000,100000),
                        trans="pseudo_log", labels=comma,
                        limits=c(0,1000000), breaks=c(0,10,100,1000,10000,100000,1000000)) +
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank()) +
    coord_map()
  
  plots <- arrangeGrob(grobs=list(map,map2,map3,map4), nrow=2, ncol=2, top=textGrob(gsub(".csv", "", files[i]), gp=gpar(fontsize=24,fontface="bold")))

  # save maps as PNG files
  setwd("/Users/dmacguigan/Documents/covid19/plots/dayPlots")
  ggsave(plot=plots,
         filename = paste(gsub(".csv", "", files[i]), ".png", sep=""),
         width = 12, height = 6.5,
         device = "png",
         dpi=600)


}

########################################################################################################################
# create gif of Covid-19 case data maps by state
# need to have ImageMacick installed
# https://imagemagick.org/index.php
# alternatively, use external software (e.g. Adobe Illustrator)
########################################################################################################################

setwd("/Users/dmacguigan/Documents/covid19/plots/dayPlots")

# NEED TO UPDATE THE LAST FILE NAME AFTER "-delay 500" BASED ON CURRENT JHU REPO
# second delay lets the gif pause on the final frame
system("convert -delay 30 *.png -delay 400 04-19-2020.png US_covid-19_timelapse.gif")

file.move("/Users/dmacguigan/Documents/covid19/plots/dayPlots/US_covid-19_timelapse.gif", 
"/Users/dmacguigan/Documents/covid19/plots/", 
overwrite=TRUE)

########################################################################################################################
# plot Covid-19 case data by county
########################################################################################################################

# read in list of US counties and states
stateCountyRef <- read.csv("/Users/dmacguigan/Documents/covid19/data/stateList.csv", header=TRUE)
stateCountyRef$FIPS <- formatC(stateCountyRef$FIPS, width=2, format="d", flag = "0")
stateCountyRef$state_county <- paste(as.character(stateCountyRef$FIPS), as.character(stateCountyRef$county), sep="_")

# get data by county
# THIS IS WHERE COUNTY DATA STARTS BEING REPORTED
# extract rows with "US" in "Country_Region" column
# also change the column headers to match first 60 data structures
# also extract only the country, state, county, and confirmed cases columns
cleanCaseData_county <- list()
for(i in 61:length(caseData)){
  print(i)
  t <- subset(caseData[[i]], Country_Region == "US")
  t <- droplevels(t) # drop unused levels in data frame
  colnames(t)[which(names(t) == "X...FIPS")] <- "FIPS"
  t$FIPS_county <- t$FIPS
  # add NYC data for all NYC counties
  t$Confirmed[which(t$FIPS_county==36005 | t$FIPS_county==36081 | t$FIPS_county==36047 | t$FIPS_county==36085)] <- t$Confirmed[which(t$FIPS_county==36061)]
  # create reduced data frame
  cleanCaseData_county[[i-60]] <- data.frame(t$FIPS_county,t$Confirmed,t$Deaths)
  colnames(cleanCaseData_county[[i-60]]) <- c("FIPS_county", "Confirmed", "Deaths")
}

# replace all NA values with 0s
cleanCaseData_county <- lapply(cleanCaseData_county, function(d) { d[is.na(d)] <- 0; d })

# read in county shapefiles
# data from https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
counties <- readOGR("/Users/dmacguigan/Documents/covid19/shapefiles/cb_2018_us_county_5m", "cb_2018_us_county_5m")
counties@data$FIPS_county <- as.numeric(paste(as.character(counties@data$STATEFP), as.character(counties@data$COUNTYFP), sep=""))
counties@data$id = rownames(counties@data)
counties.points = fortify(counties, region="id")
counties.df = join(counties.points, counties@data, by="id")

# plot map
for(i in 1:length(cleanCaseData_county)){
  print(i)
  ##############################
  # cumulative cases
  ##############################
  
  # create some temporary data structures
  temp.df <- counties.df
  temp.df$Confirmed <- rep(0,nrow(temp.df))
  
  # function to replace values in temp.df with matching number of Confirmed cases
  replace <- function(toReplaceVector, toMatchVector, matchValue, replaceValue){
    matchIndex <- which(toMatchVector == matchValue)
    toReplaceVector[matchIndex] <- replaceValue
    return(toReplaceVector)
  }
  
  for(j in 1:nrow(cleanCaseData_county[[i]])){
    temp.df$Confirmed <- replace(temp.df$Confirmed, 
                                 temp.df$FIPS_county,
                                 cleanCaseData_county[[i]]$FIPS_county[j],
                                 cleanCaseData_county[[i]]$Confirmed[j] )
  }
  
  # plot cumulative cases map
  map <- ggplot(temp.df) +
    aes(long, lat, group=group, fill=Confirmed) +
    geom_polygon(colour = NA, lwd=0.1) +
    ylim(c(24, 50)) +
    xlim(c(-127, -65)) +
    theme_minimal() +
    ggtitle("Cumulative confirmed COVID-19 cases") +
    scale_fill_gradientn(name="", 
                         colours=c("white", "yellow", "#fd8d3c", "#e31a1c", "#800026", "black"),
                         trans="pseudo_log", labels=comma,
                         limits=c(0,100000), breaks=c(0,10,100,1000,10000,100000)) +
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank()) +
    geom_polygon(data=states, aes(x=long, y=lat, group=group), fill=NA, colour="gray", lwd=0.25)+
    coord_map()
  
  ##############################
  # daily new cases
  ##############################
  
  # create some temporary data structures
  temp.df_prevDay <- counties.df
  temp.df_prevDay$Confirmed <- rep(0,nrow(temp.df))
  
  # function to replace values in temp.df with matching number of Confirmed cases
  replace <- function(toReplaceVector, toMatchVector, matchValue, replaceValue){
    matchIndex <- which(toMatchVector == matchValue)
    toReplaceVector[matchIndex] <- replaceValue
    return(toReplaceVector)
  }

  if(i == 1){
    temp.df$Confirmed <- 0
  } else if(i > 1){
    for(j in 1:nrow(cleanCaseData_county[[i-1]])){
      temp.df_prevDay$Confirmed <- replace(temp.df_prevDay$Confirmed, 
                                           temp.df_prevDay$FIPS_county,
                                           cleanCaseData_county[[i-1]]$FIPS_county[j],
                                           cleanCaseData_county[[i-1]]$Confirmed[j])
    }
    temp.df$Confirmed <- temp.df$Confirmed - temp.df_prevDay$Confirmed
    temp.df$Confirmed[temp.df$Confirmed<0] <- 0
  }
  
  # plot daily new cases map
  map2 <- ggplot(temp.df) +
    aes(long, lat, group=group, fill=Confirmed) +
    geom_polygon(colour = NA) +
    ylim(c(24, 50)) +
    xlim(c(-127, -65)) +
    theme_minimal() +
    ggtitle("Daily new confirmed COVID-19 cases") +
    scale_fill_gradientn(name="", 
                         colours=c("white", "yellow", "#fd8d3c", "#e31a1c", "#800026", "black"),
                         trans="pseudo_log", labels=comma,
                         limits=c(0,100000), breaks=c(0,10,100,1000,10000,100000)) +
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank()) +
    geom_polygon(data=states, aes(x=long, y=lat, group=group), fill=NA, colour="gray", lwd=0.25)+
    coord_map()
  
    ##############################
  # cumulative deaths
  ##############################
  
  # create some temporary data structures
  temp.df <- counties.df
  temp.df$Deaths <- rep(0,nrow(temp.df))
  
  # function to replace values in temp.df with matching number of Confirmed deaths
  replace <- function(toReplaceVector, toMatchVector, matchValue, replaceValue){
    matchIndex <- which(toMatchVector == matchValue)
    toReplaceVector[matchIndex] <- replaceValue
    return(toReplaceVector)
  }
  
  for(j in 1:nrow(cleanCaseData_county[[i]])){
    temp.df$Deaths <- replace(temp.df$Deaths, 
                                 temp.df$FIPS_county,
                                 cleanCaseData_county[[i]]$FIPS_county[j],
                                 cleanCaseData_county[[i]]$Deaths[j] )
  }
  
  # plot cumulative deaths map
  map3 <- ggplot(temp.df) +
    aes(long, lat, group=group, fill=Deaths) +
    geom_polygon(colour = NA, lwd=0.1) +
    ylim(c(24, 50)) +
    xlim(c(-127, -65)) +
    theme_minimal() +
    ggtitle("Cumulative confirmed COVID-19 deaths") +
    scale_fill_gradientn(name="", 
                         colours=c("white", "#9ecae1", "#4292c6", "#08519c", "#08519c", "black"),
                         trans="pseudo_log", labels=comma,
                         limits=c(0,100000), breaks=c(0,10,100,1000,10000,100000)) +
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank()) +
    geom_polygon(data=states, aes(x=long, y=lat, group=group), fill=NA, colour="gray", lwd=0.25)+
    coord_map()
  
  ##############################
  # daily new deaths
  ##############################
  
  # create some temporary data structures
  temp.df_prevDay <- counties.df
  temp.df_prevDay$Deaths <- rep(0,nrow(temp.df))
  
  # function to replace values in temp.df with matching number of Confirmed deaths
  replace <- function(toReplaceVector, toMatchVector, matchValue, replaceValue){
    matchIndex <- which(toMatchVector == matchValue)
    toReplaceVector[matchIndex] <- replaceValue
    return(toReplaceVector)
  }

  if(i == 1){
    temp.df$Deaths <- 0
  } else if(i > 1){
    for(j in 1:nrow(cleanCaseData_county[[i-1]])){
      temp.df_prevDay$Deaths <- replace(temp.df_prevDay$Deaths, 
                                           temp.df_prevDay$FIPS_county,
                                           cleanCaseData_county[[i-1]]$FIPS_county[j],
                                           cleanCaseData_county[[i-1]]$Deaths[j])
    }
    temp.df$Deaths <- temp.df$Deaths - temp.df_prevDay$Deaths
    temp.df$Deaths[temp.df$Deaths<0] <- 0
  }
  
  # plot daily new deaths map
  map4 <- ggplot(temp.df) +
    aes(long, lat, group=group, fill=Deaths) +
    geom_polygon(colour = NA) +
    ylim(c(24, 50)) +
    xlim(c(-127, -65)) +
    theme_minimal() +
    ggtitle("Daily new confirmed COVID-19 deaths") +
    scale_fill_gradientn(name="", 
                         colours=c("white", "#9ecae1", "#4292c6", "#08519c", "#08519c", "black"),
                         trans="pseudo_log", labels=comma,
                         limits=c(0,100000), breaks=c(0,10,100,1000,10000,100000)) +
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank()) +
    geom_polygon(data=states, aes(x=long, y=lat, group=group), fill=NA, colour="gray", lwd=0.25)+
    coord_map()
  
  
  plots <- arrangeGrob(grobs=list(map,map2,map3,map4), nrow=2, ncol=2, top=textGrob(gsub(".csv", "", files[i+60]), gp=gpar(fontsize=24,fontface="bold")))
  
  # save maps as PNG files
  setwd("/Users/dmacguigan/Documents/covid19/plots/dayPlots_counties")
  ggsave(plot=plots,
         filename = paste(gsub(".csv", "", files[i+60]), "_counties.png", sep=""),
         width = 12, height = 6.5,
         device = "png",
         dpi=600)
}

########################################################################################################################
# create gif of Covid-19 case data maps by county
# need to have ImageMacick installed
# https://imagemagick.org/index.php
# alternatively, use external software (e.g. Adobe Illustrator)
########################################################################################################################

setwd("/Users/dmacguigan/Documents/covid19/plots/dayPlots_counties")

# NEED TO UPDATE THE LAST FILE NAME AFTER "-delay 500" BASED ON CURRENT JHU REPO
# second delay lets the gif pause on the final frame
system("convert -delay 30 *.png -delay 400 04-19-2020_counties.png US_covid-19_timelapse_counties.gif")

file.move("/Users/dmacguigan/Documents/covid19/plots/dayPlots_counties/US_covid-19_timelapse_counties.gif", 
"/Users/dmacguigan/Documents/covid19/plots/",
overwrite=TRUE)


