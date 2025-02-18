rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyverse)
library(naniar)
library(ggstats)
library(reshape2)
library(gridExtra)
library(lubridate)
library(sf)
library(stringr)
library(mapview)
library(data.table)

LA_permit<-  read.csv("E:\\Job search\\Manitoba_ds\\LA_permit_info.csv", header = TRUE)
Old_LA_permit<-  read.csv("E:\\Job search\\Manitoba_ds\\permit_info.csv", header = TRUE)

head(LA_permit)
dim(LA_permit)

summary(LA_permit)

str(LA_permit)

table(LA_permit$Permit.Status)

table(LA_permit$Inspection.Type)
table(LA_permit$Inspection.Result)

sum(is.na(LA_permit))

#######################################################

LA_permit$Inspection.Date <- mdy(LA_permit$Inspection.Date)

head(LA_permit$Permit.Status)

barplot(round(prop.table(table(LA_permit$Permit.Status)),2)*100)

round(prop.table(table(LA_permit$Permit.Status)),2)*100

### CofO issued, permit issued, finaled, or other (in progress, withdrawn, closed, revoked, expired)

LA_permit$Permit.Status<-factor(LA_permit$Permit.Status)
LA_permit<-LA_permit %>% 
  mutate(status = ifelse(Permit.Status=='CofO Issued','CofO Issued',
                         ifelse(Permit.Status=='Issued', 'Permit Issued',
                                ifelse(Permit.Status=='Permit Finaled', 'Permit Finaled', 'Other'))))



LA_permit%>%
  count(status)%>%mutate(pct= prop.table(n) * 100)%>%
  ggplot(aes(x = status, y = pct))+
  geom_bar(stat="identity", fill="steelblue")+geom_text(aes(label=paste0(sprintf("%1.0f", pct),"%")),
                                      position=position_stack(vjust=0.5), color="black") + 
  theme_bw()+ylab("Percentages") +xlab("Status") +
  ggtitle("Percentages of various permit Status")+
  theme(legend.position = "bottom", plot.title = element_text(size = 9))+
  coord_flip()

tab<-prop.table(table(LA_permit$status))*100

##stargazer::stargazer(data.frame(tab), summary = FALSE, digits = 2)

##### Question 2

str(LA_permit)

dt <- data.table(coords = LA_permit$Latitude.Longitude)

# Extract latitude and longitude
geo<-dt[, c("latitude", "longitude") := tstrsplit(gsub("[()]", "", coords), ", ", fixed = TRUE)]

# Convert latitude and longitude to numeric
geo<-dt[, `:=`(latitude = as.numeric(latitude), longitude = as.numeric(longitude))]

# Inspect the results
head(geo)

LA_permit<- cbind(LA_permit, geo)

sf_data <- LA_permit %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

Old_LA_permit$PERMIT<- factor(Old_LA_permit$PCIS.Permit..)
LA_permit$PERMIT<- factor(LA_permit$PERMIT)

old_permit<- Old_LA_permit%>% select(c(PERMIT,Contractor.City, Contractor.State))

permit_data<-merge(LA_permit, old_permit, by="PERMIT")


permit_data%>%
  count(status, Contractor.State)%>%mutate(pct= prop.table(n) * 100)%>%
  ggplot(aes(x = status, y = pct, fill=Contractor.State))+
  geom_bar(stat="identity")+geom_text(aes(label=paste0(sprintf("%1.0f", pct),"%")),
                                                        position=position_stack(vjust=0.5), color="black") + 
  theme_bw()+ylab("Percentages") +xlab("Status") +
  ggtitle("Percentages of various permit Status by States")+
  theme(legend.position = "bottom", plot.title = element_text(size = 9))+
  coord_flip()

tab2<-prop.table(table(permit_data$Contractor.State))*100

stargazer::stargazer(data.frame(tab2), summary = FALSE, digits = 2)


######## Question 3



















