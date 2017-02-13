library(gdata)
library(dplyr)
library(xlsx)
library(ggplot2)
library(lubridate)
library(plyr)
library(stringr)
library(reshape2)

setwd("C:/Users/LTM/DSS/CourseProject/Data")

files <- dir()

workDat <- NULL

for(file in files){
        
        testIn <- read.xls(file,header=FALSE)
        testIn <- data.frame(testIn)
        
        names <- c("clients","stylists","wait","walkout")
        times <- testIn[3:14,1]
        testIn[,1] <- NULL
        
        srow <- 1
        erow <- 14
        scol <- 1
        ecol <- 4
        
        
        ##Need to check that the dates are correct as I go down in rows
        for(i in 1:6){ 
                scol <- 1
                ecol <- 4
                
                ##Collates acrosse the first line of rows firsss
                for(i in 1:5){ 
                        ##subsets the first day of data
                        #print(srow)
                        tempDat <- testIn[srow:erow,scol:ecol]
                        date <- tempDat[1,1]
                        colnames(tempDat) <- names <- c("clients","stylists","wait","walkout")
                        tempDat <- tempDat[3:14,1:4]
                        
                        #colnames(tempDat) <- names
                        tempDat$date <- date
                        tempDat$time <- times
                        workDat <- rbind(workDat,tempDat)
                        
                        ##Adjust the counters
                        scol <- scol +4
                        ecol <- ecol +4
                        
                }
                
                ##Adjust the counters
                srow <- srow +14
                erow <- erow +14
                
        }
        
        
}

########################
##PRE-PROCESS THE DATA##
########################

##Note the paste is needed to convert correctly from a factor
workDat$clients <- as.numeric(paste(workDat$clients))
workDat$stylists <- as.numeric(paste(workDat$stylists))
workDat$wait <- as.numeric(paste(workDat$wait))

dat <- subset(workDat, clients >= 0)
dat <- subset(workDat, stylists >= 0)
dat <- subset(workDat, wait >= 0)

dat$walkout <- NULL
dat <- dat[complete.cases(dat),]

##Convert date to PositX with lubridation
dat$date <- ymd(dat$date)

dat$month <- month(dat$date)
dat$day <- wday(dat$date,label = TRUE)

##Save It

saveRDS(dat,"ClientFlow.rds")
table(dat$stylists)

################################
##SUMMARY STATS FOR SCHEDULING##
################################

####This is the one that I want to use for the simulation
byHour <- aggregate(clients~time,data=dat,FUN=mean)
rates <- byHour
rates <- rbind(rates[12,], rates[1:11,]) #gets things in the right order - 10 am was at the end for some reaosn
hours <- c('9-10 am','10-11 am','11-12 am', '12-1 pm', '1-2 pm', '2-3 pm', '3-4 pm', '4-5 pm', '5-6 pm', '6-7 pm', '7-8 pm', '8-9 pm')
hours2<- c(9,10,11,12,1,2,3,4,5,6,7,8)
rates$hours2 <- hours2

p <- qplot(rates$hours,rates$clients, geom = "point") +geom_line(group =1)
p
barplot(rates$clients,names.arg = rates$hours2, main = "Arrivals by Hour",ylab = "# of Clients")


byMon <- aggregate(clients~month,data=dat,FUN=mean)
p <- qplot(byMon$month,byMon$clients, geom = "point") +geom_line(group =1)
p

byDay <- aggregate(clients~day,data=dat,FUN=mean)
p <- qplot(byDay$day,byDay$clients, geom = "point")
p

barplot(byDay$clients,names.arg = byDay$day, main = "Mean Hourly Arrivals by Day",ylab = "# of Clients")


fit <- lm(clients ~ time + day, data = dat)
summary(fit)

unique(dat$day)

##Add the variable for difference between patrons and stylists - kind of a gross queue size
dat$dif <- with(dat, clients-stylists)
mean(dat$dif)

###################################################################################
##READ IN DATA ON HOW LONG THINGS TAKE AND ADD PROB OF OCCURRENCE BASED ON GENDER##
###################################################################################

##Read in the service times
setwd("C:/Users/LTM/DSS/CourseProject/")
file <- "ServList.xlsx"
servTime <- read.xlsx(file,1)

##Factor to char conversion
servTime$category <- as.character(servTime$category)
servTime$Service <- as.character(servTime$Service)

##Column names to lower case 
colnames(servTime) <- tolower(colnames(servTime))

##values to lowers case
servTime[,1] <- tolower(servTime[,1])
servTime[,2] <- tolower(servTime[,2])

##Save the data out if you choose for later
#saveRDS(servTime,"servTime.rds") 
#servTime <- readRDS("servTime.rds")

###########################
##READ IN THE GENDER DATA##
###########################

##Data comes from https://github.com/hadley/data-baby-names
##Processed from SSA
babyNames <- read.csv("C:/Users/LTM/DSS/CourseProject/baby-names.csv")
babyNames <- subset(babyNames, year > 1940)
babyNames$name <- as.character(babyNames$name)
agNames<- aggregate(percent ~  name + sex, data = babyNames, FUN = mean)

##Returns only the more likely gender
opts <- unique(agNames$name)
maxNames <- NULL
for(i in opts) {
        #print(i)
        temp <- agNames[agNames$name== i,]
        #print(temp)
        max <- max(temp$percent)
        temp <- temp[temp$percent == max,]
        #print(temp)
        maxNames <- rbind(maxNames,temp)
}

####################
##POS SERVICE DATA##
####################

setwd("C:/Users/LTM/DSS/CourseProject/pos")
files <- dir()

posIn <- data.frame()

for(file in files){
        temp <- read.xls(file,header=FALSE, blank.lines.skip = TRUE)
        temp <- temp[7:nrow(temp),c(1,4,6,12,16,22)]
        
        names <- c("Date","Trans","Item","Client","Employee","Total")
        colnames(temp) <- names
        
        temp$Client <- as.character(temp$Client)
        temp$Item <- as.character(temp$Item)
        temp$Employee <- as.character(temp$Employee)
        temp$Total <- as.numeric(temp$Total)
        temp$fnClient <- sapply(strsplit(temp$Client, ' '), function(x) x[1])
        
        temp <- subset(temp, Item != "Item")
        temp <- temp[complete.cases(temp),]
        
        posIn <- rbind(posIn,temp)
}


#####################
##GENDER ASSIGNMENT##
#####################

##Merges the gender by name, this one only keeps matching records
genPos <- merge(posIn,maxNames, by.x = 'fnClient', by.y = 'name', all.x = FALSE, all.y = FALSE)
genPos$sex <- as.character((genPos$sex))

##Column names to lower case 
colnames(genPos) <- tolower(colnames(genPos))

##name and item to lower case
genPos[,1] <- tolower(genPos[,1])
genPos[,4] <- tolower(genPos[,4])

##How to look at the items by gender....
#nrow(genPos[genPos$sex == "girl",])/nrow(genPos)

#######################################################
##MODEL THE CHANCE THAT A SERVICE IS PICKED BY GENDER##
#######################################################

##Merges teh data frames to add in teh code
genPos <- merge(genPos,servTime, by.x = 'item', by.y = 'service', all.x = TRUE, all.y = FALSE)

genPos$date <- as.character(genPos$date)
genPos$date <- mdy_hms(genPos$date)
genPos$date2 <- as.Date(genPos$date, format="%m/%d/%Y")

##Get a df of the unique transactions with a key for later
unTrans <- unique(genPos[,c(4,17)])
key <- seq(1:nrow(unTrans))
unTrans <- cbind(unTrans,key)

##Adds the key back to the genPos
genPos <- merge(genPos,unTrans, by.x = c('trans','date2'), by.y = c('trans','date2'), all.x = TRUE, all.y = FALSE)
genPos$code <- as.character(genPos$code)

##number of unique transactions
total <- max(key)

##Average bill by sex
aggregate(total~sex, data = genPos, FUN = mean)

##Make a new table that has each transaction collapsed into single row with all codes pasted
codeList <- as.character()
inGen <- genPos[!is.na(genPos$code),]
keys <- unique(inGen$key)

for(i in keys){
        temp <- subset(inGen, key == i) #match the values
        temp$codes <- paste(temp$code, sep="", collapse="")
        temp <- temp[1,]
        codeList <- rbind(codeList,temp)
        
}

##Count up the number of occurrences for each combo of codes
counts <- table(codeList$codes)
counts <- data.frame(counts)
counts$prob <- counts$Freq/sum(counts$Freq)


probServ <- data.frame(with(codeList, table(codes,sex)))
probServ$sex <- as.character(probServ$sex)
probServ$codes <- as.character(probServ$codes)
probServ$numCode <- seq(from = 1,to = nrow(probServ),1)
#probServ$prob <- probServ$Freq/sum(probServ$Freq)

test

##Save it out
saveRDS(counts,"counts.RDS")


sexs <- data.frame(table(codeList$sex))
sexs$prop <- sexs$Freq/33658
sexs

countsBack<- counts

