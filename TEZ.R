library(readr)
library(rlang)
library(data.table)
library(tidyverse)
library(caret)
library(dplyr)
library(leaflet)
library(ggplot2)
library(googleVis)
crime <- read_csv("C:/Users/asli/Desktop/crime.csv")

trainRowNumbers<-createDataPartition(y=crime$OFFENSE_CODE_GROUP, p=0.01,
                                     list=FALSE)

crimes<-crime[trainRowNumbers,]
dim(crimes)

library(rCharts)
library(reprex)
library(tidyverse)
blue <- "['blue']"
red <-  "['red']"
yellow <- "['yellow']"



crimes$INCIDENT_NUMBER<- as.factor(as.character(crimes$INCIDENT_NUMBER))
crimes$OFFENSE_CODE <- as.numeric(as.character(crimes$OFFENSE_CODE))
crimes$OFFENSE_CODE_GROUP<- as.factor(as.character(crimes$OFFENSE_CODE_GROUP))
crimes$OFFENSE_DESCRIPTION<- as.factor(as.character(crimes$OFFENSE_DESCRIPTION))
crimes$DISTRICT<- as.factor(as.character(crimes$DISTRICT))
crimes$DAY_OF_WEEK<- as.factor(as.character(crimes$DAY_OF_WEEK))
crimes$UCR_PART<- as.factor(as.character(crimes$UCR_PART))
crimes$STREET<- as.factor(as.character(crimes$STREET))
crimes$YEAR<- as.numeric(crimes$YEAR)
crimes$MONTH<- as.numeric(crimes$MONTH)
crimes$HOUR<- as.numeric(crimes$HOUR)
crimes$OCCURRED_ON_DATE<-as.Date(crimes$OCCURRED_ON_DATE)


#######################################################################################

#SUÇ SAYILARI(NUMBER OF CRIMES)

counts <- table(crime$OFFENSE_CODE_GROUP)
barplot(counts, main="TYPES OF CRIMES", 
        ylab="Number of CRIMES")







#PÝECHART  

df <- as.data.frame(table(crimes$OFFENSE_CODE_GROUP))
colnames(df) <- c("class", "freq")

pie <- ggplot(df, aes(x = "", y=freq, fill = factor(class))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="class", 
       x=NULL, 
       y=NULL, 
       title="Pie Chart of class", 
       caption="Source: crime")

pie + coord_polar(theta = "y", start=0)

# Source: Categorical variable.

pie <- ggplot(crime, aes(x = "", fill = factor(OFFENSE_CODE_GROUP))) + 
  geom_bar(width = 1) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="class", 
       x=NULL, 
       y=NULL, 
       title="Pie Chart of class", 
       caption="Source: crime")

pie + coord_polar(theta = "y", start=0)


#three different variable

g <- ggplot(crimes, aes(DAY_OF_WEEK))
g + geom_bar(aes(fill=OFFENSE_CODE_GROUP), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Categorywise Bar Chart")



#################hour

g <- ggplot(crimes, aes(HOUR))
g + geom_bar(aes(fill=OFFENSE_CODE_GROUP), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Categorywise Bar Chart")

g <- ggplot(crimes, aes(DISTRICT))
g + geom_bar(aes(fill=OFFENSE_CODE_GROUP), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Categorywise Bar Chart")

g <- ggplot(crime, aes(MONTH))
g + geom_bar(aes(fill=OFFENSE_CODE_GROUP), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Categorywise Bar Chart")
#########################################
library(ggmap)

library(rworldmap)

for(i in 1:nrow(crimes)){
  
  result <- geocode(crimes$Location[i], output = "latlona", source = "google")
  
  crimes$Long[i] <- as.numeric(result[1])
  
  crimes$Lat[i] <- as.numeric(result[2])
  
}







install.packages("leaflet")
library(leaflet)
library(geojson)


m <- leaflet() %>%
  
  addTiles() %>%
  

  
  addProviderTiles("Esri.WorldTopoMap")%>%
  addMarkers(data = crimes, lng = ~Long, lat=~ Lat, popup = ~paste(OFFENSE_CODE_GROUP),
             labelOptions = labelOptions(noHide = F, direction = 'auto'),
             options = markerOptions(riseOnHover = TRUE),
             clusterOptions = markerClusterOptions()
             
             
             
             
             
  )

m



#########################################

model1<-glm(YEAR~HOUR,data=crimes)
summary(model1)
plot(model1)


df2 <- aggregate(OFFENSE_CODE_GROUP~ YEAR + MONTH + HOUR + DAY_OF_WEEK , data = crimes, FUN = length)


df2$DAY_OF_WEEK<-gsub("Monday","1",df2$DAY_OF_WEEK)
df2$DAY_OF_WEEK<-gsub("Friday","5",df2$DAY_OF_WEEK)
df2$DAY_OF_WEEK<-gsub("Tuesday","2",df2$DAY_OF_WEEK)
df2$DAY_OF_WEEK<-gsub("Wednesday","3",df2$DAY_OF_WEEK)
df2$DAY_OF_WEEK<-gsub("Thursday","4",df2$DAY_OF_WEEK)
df2$DAY_OF_WEEK<-gsub("Saturday","6",df2$DAY_OF_WEEK)
df2$DAY_OF_WEEK<-gsub("Sunday","7",df2$DAY_OF_WEEK)
library(DT)
df2$DAY_OF_WEEK<-as.numeric(as.character(df2$DAY_OF_WEEK))

mydat <- data.table(year = df2$YEAR,
                    month=df2$MONTH,
                    hour=df2$HOUR,
                    day_of_week=df2$DAY_OF_WEEK,
                    offense_code_group=df2$OFFENSE_CODE_GROUP
)



which.max(df2$OFFENSE_CODE_GROUP)
df2$MONTH[which.max(df2$OFFENSE_CODE_GROUP)]
######## write.csv(crimes,'crimess.csv')

####write.csv(df2,"df22.csv")

##########¦3write.csv(df2,"df3.csv")
