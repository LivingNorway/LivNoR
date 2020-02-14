

#setwd("M:/My Documents/Prosjekter/Arbeidsbord/Ryper/FjellrypeRadioTeletriDataLierne")

library(tidyr)
library(dplyr)
library(sp)
library(uuid)
library(reshape2)
library(lubridate)
library(rworldmap)

d <- read.csv2("RockPtarmiganExample/Data_RadioTelemetry_ROPT_Lierne2012_2014.txt", header=T, sep=";")


#######################################################################################
#### ADDING GEOGRAPHIC CONTEXT TO THE DATA; 
### Transforming coordinates - from UTM W33 to log/lat
#### Assigning country to the points; 

crs_temp33 <- CRS("+proj=utm +zone=33 +ellps=WGS84")
crs_temp_longlat <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

coords <- data.frame(d$X.koordinat, d$Y.koordinat)
temp1 <- SpatialPoints(coords, proj4string=crs_temp33)
temp1 <- spTransform(temp1, crs_temp_longlat)
colnames(temp1@coords) <- c("decimalLongitude","decimalLatitude")

## Extracting info regarding cuntry 
data(countriesLow)
test <- sp::over(temp1, countriesLow)

##################
### Adding coordinateUncertaintyInMeters
### Adding geodeticDatum
### Adding locality
### Adding geodeticDatum
### adding country, countryID (ISO 3166-1 alfa-2) & locality (if occurence is in Norway)

d <- d %>% mutate(decimalLongitude=temp1$decimalLongitude, decimalLatitude=temp1$decimalLatitude,
                  geodeticDatum=paste("WGS84"),  
                  coordinateUncertaintyInMeters=ifelse(Plottsikkerhet==1, 20, 
                  ifelse(Plottsikkerhet==2, 500, 1000)), country=test$NAME, countryCode=test$ISO_A2, 
                  locality=ifelse(country=="Norway", "Indre Namdalen", ""))


#############################################################
#### CONVERTING TIME-DATE OBJECTS TO ISO-STANDARDS; 
### Changing Date-format, using ISO 8601

d$eventDate <- as.Date(d$Dato, "%d.%m.%Y")

### Changing time format - terrible job becuase of strange format 

temp <- colsplit(d$Klokke, " ", c("date1", "time1"))
temp2 <- gsub(".", ':', temp$time1, fixed = T)
temp3 <- strptime(temp2 , "%H:%M:%S")
temp4 <- strftime(temp3, format="%H:%M")
d$eventTime <- paste(temp4, "CEST", sep=" ")


###########################################################################################
### TRANSFORMING INFORAMTION THAT DOES NOT EASILY INTO dwc fiels; 
### Constructing the dynamicProperties field
### event: two levels (capture OR radiotelemetry/recapture)
### state: three levels (alive OR dead (harvest) OR dead (non-harvest mortality))
### PUT TOGETHER IN RECORD: {event: ; state: }

### Could probably use r -> json package here; replace and explore
d <- d%>% mutate(event=ifelse(Status==2, "capture", "radiotelemetry/recapture"))
d <- d %>% mutate(state=ifelse(Status==0, "dead (harvest)", 
                               ifelse(Status==1 | Status==2, "alive", "dead (non-harvest mortality)")))
#d <- d %>% mutate(dynamicProperties=paste("{event:", event, "; state:", state, "}", sep=""))
d <- d %>% mutate(dynamicProperties=paste('{"event":', '"', event, '"', ',"state":', '"',state, '"',"}", sep=""))


#########################################################################################
### AGE (juvenile: <365 days old, adult>366 days. 
### Age has to be recalculated, because in the raw data we only store age at capture

d <- d %>% mutate(Year=year(eventDate))
Date_Birth <- d %>% filter(Status==2) %>%
       select(Tagnr, Dato, Alder, Year) %>% 
       mutate(doB=ifelse(Alder==1, "2010-01-01", paste(Year-1, "06-01", sep="-"))) %>%
       select(Tagnr, doB) 

d <- left_join(d, Date_Birth)

d <- d %>% mutate(jul_event=julian(d$eventDate), jul_doB=julian(as.Date(d$doB)))
d <- d %>% mutate(age=julian(d$eventDate)-julian(as.Date(d$doB)))
d <- d %>% mutate(lifeStage=ifelse(age>365, "adult", "juvenile"))

##########################################################################################
### Adding information about taxonomy; 
### This should be improved - to make sure consistency

d <- d %>% mutate(scientificName="Lagopus muta", kingdom="Animalia", phylum="Chordata", 
                  class="Aves", order="Galliformes", family="Tetraonidae", genus="Lagopus", 
                  taxonID="http://www.artsdatabanken.no/Taxon/3991")

###########################################################################################
#### SOME ADDITINAL INFORMATION
### ownerInstitutionCode: Adding inforamiton about the data set/owner institution
### basisOfRecord: HumanObservation

d <- d %>% mutate(ownerInstitutionCode="NINA", basisOfRecord="HumanObservation",
                  sex=ifelse(Kjønn==1, "M", "F"))

### Renaming some columns
names(d)[names(d) == "Tagnr"] <- "organismName"
names(d)[names(d) == "Ving.1"] <- "Length_left_wing"
names(d)[names(d) == "Ving.2"] <- "Length_rigth_wing"
names(d)[names(d) == "Vekt"] <- "weigth"
names(d)[names(d) == "Bryst"] <- "Length_sternum"


###########################################################################################
#### PREPARING & FINALIZING THE OCCURENCE TABLE: 

Occurrence_dat <- d[,c("organismID", "organismName","occurrenceID", "sex", "lifeStage", "eventDate", "eventTime", 
                       "basisOfRecord", "dynamicProperties", "scientificName", "kingdom",
                       "phylum", "class", "order", "family", "genus", "taxonID", "decimalLatitude",
                       "decimalLongitude", "geodeticDatum", "coordinateUncertaintyInMeters",
                       "country", "countryCode", "locality", "ownerInstitutionCode")]



############################################################################################
##### PREPARING MEASURMENT AND FACTS TABLE

MM <- d %>% filter(Status==2)

MM <- MM[,c("organismID", "organismName", "occurrenceID", 
          "Length_left_wing", "Length_rigth_wing", "weigth", "Length_sternum")]

MM <- reshape(MM, timevar="measurementType", 
                varying=c("Length_left_wing", "Length_rigth_wing", "weigth", "Length_sternum"),
                times=c("Length_left_wing", "Length_rigth_wing", "weigth", "Length_sternum"),
                v.names = "measurementValue",
                direction="long")

rownames(MM) <- NULL
MM <- MM %>% mutate(measurementUnit=ifelse(measurementType=="weigth", "g", "mm"))
MM$id <- NULL
MM <- drop_na(MM)
MM <- MM %>% mutate(measurementValue=round(measurementValue,0))
MM <- MM[,c("occurrenceID", "organismID", "organismName", "measurementValue", "measurementType", "measurementUnit")]

###########################################################################################
#### WRITING THE FILES; 
## Occurence_data 
## Measurement_and_Facts_data

write.table(Occurrence_dat, "RockPtarmiganExample/Occurrence_data.txt", row.names = FALSE, col.names=TRUE, sep="\t", quote = FALSE)
write.table(MM, "RockPtarmiganExample/Measurement_and_Facts_data.txt", row.names = FALSE, col.names=TRUE, sep="\t")


#### WOrkflow - component; 
## Need to use a validater at this point





