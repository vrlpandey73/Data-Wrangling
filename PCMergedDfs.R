# Code for everything so far
rm(list=ls()) 
library("OData")
library("httr")  
library("data.table")

# Download the Inclusive Internet index excel file as csv, and put it in working directory
# it will probably have a different name for you guys. 
internet <- read.csv("Inclusive Internet Index.csv")

# Importing WHO indicators from Atheni API 
GDP<-downloadResourceCsv("http://apps.who.int/gho/athena/api/GHO/WHS7_143.csv?filter=YEAR:2013", ",")   

Life<-downloadResourceCsv("http://apps.who.int/gho/athena/api/GHO/WHOSIS_000001.csv?filter=YEAR:2013", ",")  

Life2<-downloadResourceCsv("http://apps.who.int/gho/athena/api/GHO/WHOSIS_000015.csv?filter=YEAR:2013", ",")  

Hospital<-downloadResourceCsv("http://apps.who.int/gho/athena/api/GHO/DEVICES00.csv?filter=YEAR:2013", ",")  

Hospital_S<-downloadResourceCsv("http://apps.who.int/gho/athena/api/GHO/DEVICES05.csv?filter=YEAR:2013", ",")   


# Cleaning WHO Indicators 
GDP$GDP_PERCENTAGE = GDP$Numeric  

GDP<-GDP[,c(4,5,7,13)]  

Life$Life_Expectancy<-Life$Numeric  

Life<-Life[,c("YEAR","REGION","COUNTRY","SEX","Life_Expectancy")]  

Life2$Life_Exp_60<-Life2$Numeric  

Life2<-Life2[,c("YEAR","REGION",("COUNTRY"),"SEX","Life_Exp_60")]  

Life2$COUNTRY<-sort(Life2$COUNTRY,decreasing = F)  

Hospital_S$Specialized_Hospitals<-Hospital_S$Numeric  

Hospital_S<-Hospital_S[,c(3,4,5,11)]  

Hospital$Hospitals<-Hospital$Numeric  

Hospital<-Hospital[,c("YEAR","REGION","COUNTRY","Hospitals")]  

# Getting rid of extra life rows  

setDT(Life) 
Life = Life[ , .(MEASURE = mean(Life_Expectancy)), by = .(COUNTRY)] 


setDT(Life2) 
Life2 = Life2[ , .(MEASURE = mean(Life_Exp_60)), by = .(COUNTRY)] 


# Merging WHO indicators together
WHO<-merge(GDP,Hospital,by='COUNTRY')  

WHO<-WHO[,-c(5,6)]  

WHO<-merge(WHO,Hospital_S,by="COUNTRY")  

WHO<-WHO[,-c(6,7)]  

Life_Exp<-merge(Life,Life2,by="COUNTRY")  

Life_Exp$Life_Exp_0 <- Life_Exp$MEASURE.x 

Life_Exp$Life_Exp_60 <- Life_Exp$MEASURE.y 

# All of WHO indicators into single data frame 
Med_df<-merge(GDP,Life_Exp,by="COUNTRY")  

Med_df<-Med_df[,-c(5,6)] 

# Have to download this file from GitHub: https://gist.github.com/tadast/8827699
# put that in working directory
# It contains both Alpha-2 code and Alpha-3 code. 
# This contains both versions of country naming that the internet df and Med_df use 
# This is our name dictionary
code_dict <- read.csv("countries_codes_and_coordinates.csv")[,c(1,2,3)]

# Cleaning data for horizontal merge 
Med_df$country <- as.character(Med_df$COUNTRY)
code_dict$three <- as.character(code_dict$Alpha.3.code)
code_dict$three <- gsub(" ", "", code_dict$three)

Med_df_merged <- merge(Med_df, code_dict, by.x = "country", by.y = "three")
Med_df_merged$two <- gsub(" ", "", Med_df_merged$Alpha.2.code)
Med_df_merged$two <- as.character(Med_df_merged$two)
internet$country <- as.character(internet$ISO)

# Merging two data frames 
df <- merge(internet, Med_df_merged, by.x = "country", by.y = "two")

# Cleaning final df 
df$ISO <- NULL
df$country_code_2 <- df$country
df$country_code_3 <- df$Alpha.3.code
df$country <- NULL
df$Alpha.3.code <- NULL
df$Country.Group <- NULL
df$Alpha.2.code <- NULL
df$COUNTRY <- NULL
df$country.y <- NULL
df

library(ggplot2)
library(dplyr)
require(maps)
require(viridis)

lifemap<-df[,c("Country","Life_Exp_0")]
lifemap$region<-lifemap$Country
lifemap$Country<-NULL
#dataset for life expectancy map

world_map <- map_data("world")
life.exp.map <- left_join(lifemap, world_map, by = "region")                 
#merging the world map's coordinate with life expectancy map's values

ggplot(life.exp.map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = Life_Exp_0 ), color = "Grey")+
  scale_fill_viridis_c(option = "D")
#code for the map

#slighty different code for geoplotting Total.electricity.Access per country
emap<-df[,c(28,8)]
emap$region<-emap$Country
tolower(emap$region)
lifemap$Country<-NULL
emap$Total_electricity_access<-emap$Total.electricity.access.....of.population
emap$Total.electricity.access.....of.population<-NULL
world_map <- map_data("world")
emap <- left_join(emap, world_map, by = "region")
ggplot(emap, aes(long, lat, group = group))+
  geom_polygon(aes(fill = Total_electricity_access ), color = "Grey")+
  scale_fill_viridis_c(option = "C")
