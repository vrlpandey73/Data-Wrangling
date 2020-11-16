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


# Cleaning WHO Indicators 
GDP$GDP_PERCENTAGE = GDP$Numeric  

GDP<-GDP[,c(4,5,7,13)]  

Life$Life_Expectancy<-Life$Numeric  

Life<-Life[,c("YEAR","REGION","COUNTRY","SEX","Life_Expectancy")]  

Life2$Life_Exp_60<-Life2$Numeric  

Life2<-Life2[,c("YEAR","REGION",("COUNTRY"),"SEX","Life_Exp_60")]  

Life2$COUNTRY<-sort(Life2$COUNTRY,decreasing = F)  

# Getting rid of extra life rows  

setDT(Life) 
Life = Life[ , .(MEASURE = mean(Life_Expectancy)), by = .(COUNTRY)] 


setDT(Life2) 
Life2 = Life2[ , .(MEASURE = mean(Life_Exp_60)), by = .(COUNTRY)] 


# Merging WHO indicators together

Life_Exp<-merge(Life,Life2,by="COUNTRY")  

Life_Exp$Life_Exp_0 <- Life_Exp$MEASURE.x 

Life_Exp$Life_Exp_60 <- Life_Exp$MEASURE.y 

# All of WHO indicators into single data frame 
Med_df<-merge(GDP,Life_Exp,by="COUNTRY")  

Med_df<-Med_df[,-c(5,6)] 

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
save(df, file="NewMergedData.rda")

