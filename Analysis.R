
library(dplyr)
library(ggplot2)
require(maps) 
require(viridis) 

load("NewMergedData.rda")

## Analysis

# This plot shows GDP% spending on health for all countries
qplot(df$Country,
      df$GDP_PERCENTAGE,
      geom = "col",
      fill = I("blue"),
      colour = I("black"),
      xlab = "Country",
      ylab = "GDP%",
      main = "GDP% Spending on Health by Country",
      aes(reorder(df$Country, df$GDP_PERCENTAGE))) +
  theme(axis.text.x = element_text(angle = 80, vjust=0.5))

# This code subsets the top and bottow GDP% spending countries
subset<- subset(df, df$GDP_PERCENTAGE > 11)
subset2<- subset(df, df$GDP_PERCENTAGE < 3)
rbind_subsets = rbind(subset, subset2)

# This plot shows the narrowed countires and their GDP% spending
qplot(rbind_subsets$Country,
      rbind_subsets$GDP_PERCENTAGE,
      geom = "col",
      fill = I("blue"),
      colour = I('black'),
      xlab = "Country",
      ylab = "GDP%",
      main = "GDP% Spending on Health by Country",
      aes(reorder(rbind_subsets$Country, rbind_subsets$GDP_PERCENTAGE))) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5))

# This plot shows the narrowed countries and their Internet Users as % of Population
qplot(rbind_subsets$Country,
      rbind_subsets$Internet.users..percent.of.population......of.population,
      geom = "col",
      fill = I("blue"),
      colour = I("black"),
      xlab = "Country",
      ylab = "Internet Users % of Population",
      main = "Internet Users % of Population by Country",
      aes(reorder(rbind_subsets$Country, rbind_subsets$Internet.users..percent.of.population......of.population))) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5))

# This plot shows the linear regression model between the GDP% spending on health
# and Internet Users as % of of Population. This also displays the correlation
# between the two variables.
y <-lm(df$GDP_PERCENTAGE~df$Internet.users..percent.of.population......of.population)
plot(df$Internet.users..percent.of.population......of.population,  df$GDP_PERCENTAGE,
     xlab = "Internet Users as % of Population",
     ylab = "GDP%",
     main = "GDP% Spending on Health compared to Internet Users as % of Popluation")
abline(y, col = "red")

cor(df$GDP_PERCENTAGE, df$Internet.users..percent.of.population......of.population)


# This code summarizes the Internet Users as % of Population by Region and
# displays it in a tibble
df_summary = group_by(df,df$REGION)
summary<-summarize(df_summary,
                   total_countries = n(),
                   min=min(Internet.users..percent.of.population......of.population),
                   avg=mean(Internet.users..percent.of.population......of.population),
                   med=median(Internet.users..percent.of.population......of.population),
                   max=max(Internet.users..percent.of.population......of.population),
                   stdev= sd(Internet.users..percent.of.population......of.population))
summary

# Does electricy access affect some regions more than others for life expectancy  
# Life expectancy at age 0  
cor(df$Total.electricity.access.....of.population, df$Life_Exp_0) 
mod <- lm(df$Life_Exp_0~df$Total.electricity.access.....of.population) 
plot(df$Total.electricity.access.....of.population, df$Life_Exp_0,  
     main = "Electricity Access compared to Life Expectancy from age 0",  
     xlab = "% of Pop with Electricity Access",
     ylab = "Life Expectancy at Age 0") 
abline(mod) 
summary(mod) 
qqnorm(mod$residuals) 
qqline(mod$residuals,  col="red") 
shapiro.test(mod$residuals) 

# Life expectancy at age 60 
cor(df$Total.electricity.access.....of.population, df$Life_Exp_60) 
mod2 <- lm(df$Life_Exp_60~df$Total.electricity.access.....of.population) 
plot(df$Total.electricity.access.....of.population, df$Life_Exp_60,  
     main = "Electricity Access compared to Life Expectancy from age 60",  
     xlab = "% of Pop with Electricity Access",
     ylab = "Life Expectancy at Age 60") 
abline(mod2) 
summary(mod2) 
qqnorm(mod2$residuals) 
qqline(mod2$residuals,  col="red") 
shapiro.test(mod2$residuals) 

# World Maps
lifemap<-df[,c("Country","Life_Exp_0")] 
lifemap$region<-lifemap$Country 
lifemap$Country<-NULL 

# dataset for life expectancy map 
world_map <- map_data("world") 
life.exp.map <- left_join(lifemap, world_map, by = "region")        

# merging the world map's coordinate with life expectancy map's values 
ggplot(life.exp.map, aes(long, lat, group = group))+ 
  geom_polygon(aes(fill = Life_Exp_0 ), color = "Grey")+ 
  scale_fill_viridis_c(option = "D") 

# code for the map 
# slighty different code for geoplotting Total.electricity.Access per country 
emap<-df[,c(26,8)]
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
