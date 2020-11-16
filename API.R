rm(list=ls()) 



library(OData)  



library(httr)  



library(data.table) 



GDP<-downloadResourceCsv("http://apps.who.int/gho/athena/api/GHO/WHS7_143.csv?filter=YEAR:2013", ",")   







Life<-downloadResourceCsv("http://apps.who.int/gho/athena/api/GHO/WHOSIS_000001.csv?filter=YEAR:2013", ",")  







Life2<-downloadResourceCsv("http://apps.who.int/gho/athena/api/GHO/WHOSIS_000015.csv?filter=YEAR:2013", ",")  







Hospital<-downloadResourceCsv("http://apps.who.int/gho/athena/api/GHO/DEVICES00.csv?filter=YEAR:2013", ",")  







Hospital_S<-downloadResourceCsv("http://apps.who.int/gho/athena/api/GHO/DEVICES05.csv?filter=YEAR:2013", ",")   

head(Hospital) 

head(Hospital_S) 







GDP$GDP_PERCENTAGE = GDP$Numeric  



GDP<-GDP[,c(4,5,7,13)]  







Life$Life_Expectancy<-Life$Numeric  



Life<-Life[,c("YEAR","REGION","COUNTRY","SEX","Life_Expectancy")]  







Life2$Life_Exp_60<-Life2$Numeric  



Life2<-Life2[,c("YEAR","REGION",("COUNTRY"),"SEX","Life_Exp_60")]  



Life2$COUNTRY<-sort(Life2$COUNTRY,decreasing = F)  



head(Life) 



Hospital_S$Specialized_Hospitals<-Hospital_S$Numeric  



Hospital_S<-Hospital_S[,c(3,4,5,11)]  







Hospital$Hospitals<-Hospital$Numeric  



Hospital<-Hospital[,c("YEAR","REGION","COUNTRY","Hospitals")]  



# Getting rid of extra life rows  



setDT(Life) 

Life = Life[ , .(MEASURE = mean(Life_Expectancy)), by = .(COUNTRY)] 

Life 



setDT(Life2) 

Life2 = Life2[ , .(MEASURE = mean(Life_Exp_60)), by = .(COUNTRY)] 

Life2 



WHO<-merge(GDP,Hospital,by='COUNTRY')  

head(WHO) 

str(WHO) 

WHO<-WHO[,-c(5,6)]  



WHO<-merge(WHO,Hospital_S,by="COUNTRY")  



WHO<-WHO[,-c(6,7)]  



Life_Exp<-merge(Life,Life2,by="COUNTRY")  



Life_Exp<-Life_Exp[,-c(6,7)]  

Life_Exp$Life_Exp_0 <- Life_Exp$MEASURE.x 

Life_Exp$Life_Exp_60 <- Life_Exp$MEASURE.y 



Med_df<-merge(WHO,Life_Exp,by="COUNTRY")  

head(Med_df) 

Med_df<-Med_df[,-c(7,8)] 

