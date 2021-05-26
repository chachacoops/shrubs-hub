library(dplyr)
library(readr)

rm(list=ls()) 

#https://link.springer.com/article/10.1007/s00265-018-2602-7

getwd()
setwd("/Users/charlottecooper/Desktop/Masters/Dissertation/RightRFID")
#Prepping the csvs
#Checking for adequate samples
#Males S shape
#Males open
#Females S shape
#Females open

##### MALES RIGHT SIDE S
rrfid <- list.files(path="~/Desktop/Masters/Dissertation/RightRFID", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 
write.csv(rrfid, "/Users/charlottecooper/Desktop/Masters/Dissertation/MalesSFinal/combinedRRFID.csv")

#read in the csv
rrfid<- read.csv("/Users/charlottecooper/Desktop/Masters/Dissertation/MalesSFinal/combinedRRFID.csv", sep = ";", header = TRUE, quote = "")
names(rrfid) <- gsub("\\.", "", names(rrfid)) #remove full stops from column names

rrfid$Location <- "Right" #add location

View(rrfid)
names(rrfid)
#remove first weird column and remove any duplication, and save as a new data frame
rrfid <- select(rrfid, -XIdentifier)
rrfid <- rrfid[!duplicated(rrfid),]

names(rrfid)[names(rrfid) == "GPScoordinatesX1"] <- "GPScoordinates"

#check how many times each bird is recorded over the time period
rrfid2 %>% group_by(Transpondercode) %>%
  summarise(count=length(Transpondercode))

##### MALES LEFT SIDE S

setwd("/Users/charlottecooper/Desktop/Masters/Dissertation/LeftRFID")
lrfid <- list.files(path="~/Desktop/Masters/Dissertation/LeftRFID", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 
write.csv(lrfid, "/Users/charlottecooper/Desktop/Masters/Dissertation/MalesSFinal/combinedLRFID.csv")

#read in the csv
setwd("/Users/charlottecooper/Desktop/Masters/Dissertation/MalesSFinal")
lrfid<- read.csv("combinedLRFID.csv", sep = ";", header = TRUE, quote = "")
names(lrfid) <- gsub("\\.", "", names(lrfid)) #remove full stops from column names

lrfid$Location <- "Left"

#remove first weird column and remove any duplication, and save as a new data frame
lrfid <- select(lrfid, -XIdentifier)
lrfid <- lrfid[!duplicated(lrfid),]
View(lrfid)

#check how many times each bird is recorded over the time period
lrfid %>% group_by(Transpondercode) %>%
  summarise(count=length(Transpondercode))
names(lrfid)

##### Combine Left and Right final CSVs

crfid <- rbind(rrfid, lrfid)
View(crfid)
crfid$Date <- dmy(crfid$Date)
crfid$Time <- hms(crfid$Time)
class(crfid$Date)
class(crfid$Time)
?hms

##### social network
# Load packages -----------------------------------------------------------
install.packages("lubridate")
install.packages("igraph")
install.packages("asnipe")
install.packages("ggraph")

require(lubridate)
require(igraph)
require(asnipe)
require(ggraph)
class(crfid$Time)

###### Test OxU algorithm ------------------------------------------------------
subset.ox <- crfid %>% filter(date(Date) >= "01-04-2021" & date(Date) <= "08-04-2021")

#format data
subset.ox <- subset.ox %>% mutate(date.time = dmy_hms(paste(subset.ox$Date, subset.ox$Time, sep = " ")),
                                  location = rep("Left", "Right")) %>%
  arrange(date.time) %>%
  mutate(formatted.time = as.numeric(date.time - date.time[1])) %>%
  select(formatted.time, crfid, Date)

#Create network
assoc <- gmmevents(time = subset.ox$formatted.time, identity = subset.ox$rfid) #assign birds into groups
network.ox <- get_network(assoc[[1]]) #Get association matrix
inetwork.ox <- graph_from_adjacency_matrix(network.ox, weighted = TRUE, mode = "undirected") #Convert it to pass into igraph
inetwork.metrics <- nodes %>% mutate(degree = degree(inetwork.ox, v = rfid),
                                     strength = strength(inetwork.ox, v = rfid),
                                     betweenness = betweenness(inetwork.ox, v = rfid, directed = TRUE, nobigint = FALSE))
write.csv(inetwork.metrics, file = "output.files/inetwork.metrics.csv", row.names = FALSE)

# Plot graphs -------------------------------------------------------------

graph.ox <- ggraph(inetwork.ox) +
  geom_edge_link() +
  geom_node_point(size = 7, shape=21, fill="white", colour="black") + 
  geom_node_text(aes(label = name), repel = TRUE, cex = 1.5, colour = "blue") +
  ggtitle("Oxford algorithm")
ggsave(graph.ox, filename = "output.files/graph.ox.png")


##### other
class(rrfid2$Date)


names(rrrfid2)
class(rrrfid2$Time)
as.numeric(rrrfid2$Time)
View(rrrfid2)
class(rrrfid2)
rrrfid2$Time

install.packages("chron")
require(chron)
tImes = chron(times = rrrfid2$Time)
tImes
class(tImes)
tImes %/% 010
as.numeric(tImes)
plot(tImes)

test <- rrrfid2[1:100,]

View(test)
class(test$Time)
new_test <- gsub(x = test$Time,
                 pattern = ":",
                 replacement = "")

test <- mutate(test, TimeN=new_test)
test$TimeN <- as.numeric(test$TimeN)
class(test$TimeN)
test$TimeN %/% 10
unique(test$TimeN%/%10)

id_list <- unique(rrrfid2$Transpondercode)


for(Transpondercode in id_list){
  unique(test$TimeN%/%10)
}

