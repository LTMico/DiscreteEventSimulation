library(simmer)
library(gdata) #For reading xls
library(xlsx) #For reading xlsx
library(DiagrammeR)
library(triangle)
library(pracma)

###########################
##FLOWCHART OF THE SYSTEM##
###########################

##Flowchart of the System
DiagrammeR("
graph LR;
        Arrive --> Check-In
        Check-In --> Balk
        Check-In --> Stylist
        Stylist --> Payment
        ")

############################################################################


#############################################
##SET A SEED IF YOU WANT TO BE REPRODUCIBLE##
#############################################

#set.seed(1978)

#################################################################
##MAKE A SCHEDULE ACCORDING TO A NON-STATIONARY POISSON PROCESS##
#################################################################

schedIt <- function() {
        
        ##Here is where you can do a given day or month etc
        ##It is now the average by hour irrespectice of day
        rates <- byHour
        
        rates <- rbind(rates[12,], rates[1:11,]) #gets things in the right order - 10 am was at the end for some reaosn
        maxRate <- max(rates$clients)
        maxRate <- maxRate/60
        expMean<- 1/maxRate
        
        ##Make a lot first - ~100 hours worth
        
        #denseGen <- rexp(12000,rate=(1/maxRate))
        denseGen <- rexp(12000,rate=(maxRate))
        
        ##Converts IAT into time of day
        clockSched <- as.numeric()
        temp <- 0
        for(i in 1:length(denseGen)) {
                clockSched <- c(clockSched, temp+denseGen[i])
                temp <- temp+denseGen[i]
        }
        
        ##Takes only the ones that happen before closing
        clockSched <- clockSched[clockSched < 720]
        
        ##'Thins' the values to match the probalities by hour
        rem <- as.numeric()
        startTime <- 0
        endTime <- 60
        for(hour in 1:nrow(rates)) {
                #print(hour)
                rate <- rates$clients[hour]
                rate <- rate/60
                #print(rate)
                for(i in 1:length(clockSched)) {
                        #print(i)
                        test <- runif(1,0:1) > rate/maxRate
                        #print(test)
                        if(clockSched[i] >= startTime & clockSched[i] < endTime) {
                                if(test){rem <- c(rem,i) } }
                        
                }
                startTime <- startTime + 60 
                endTime <- endTime + 60
        }
        
        ##Removes the values from the clock schedule
        clockSched <- clockSched[-rem]
        
        ##converts the clock time back to an updated IAT after 'thinning'
        temp <- 0
        IAT <- as.numeric()
        for(i in 1:length(clockSched)){
                IAT <- c(IAT,clockSched[i]-temp)
                temp <- clockSched[i]
        }
        
        ##Bookkeeping to make a data frame
        id <- seq(1,length(clockSched))
        sched <- cbind(id,clockSched,IAT)  
        sched <- data.frame(sched)
        
        return(sched)
        
}

##Unit Test - should be ~mean of 10
sched <- schedIt()
mean(sched$IAT)

###########################################
##MAKE A LIST OF IATS PERFECTLY SCHEDULED##
###########################################



######################################
##SERVICE SELECTION TIMEOUT FUNCTION##
######################################



servCode <- function() {
        
        ##Here is the basic stat model with counts for service choice by gender
        probs <- probServ
        
        ##ASSUMPTION##
        ##72/28 GENDER SPLIT OF PATRONS
        ##determine if male of female and set an attribute
        ##Data is based on the POS data mining in the companion code
        gender <- sample(1:2, size=1, prob=c(.72,.28), replace=TRUE)
        
        ##Subset to pick the right gender
        if(gender == 1){ probs <- subset(probs, sex == "boy") }
        if(gender == 2){ probs <- subset(probs, sex == "girl") }
        
        probs$prob  <- probs$Freq/sum(probs$Freq)
        
        ##Pick the service bundle by gender
        numCode <- sample(probs$numCode,size =1, prob = probs$prob )
        #code <- paste(as.character(gender),code, sep='') # Note that I'm tracking gender here
        
        return(numCode)
}

##Unit Test for the servCode()
for(i in 1:50) {print(servCode())}

#####################################################
##STYLING TIMEOUT FUNCTION BASED ON CHOSEN SERVICES##
#####################################################


styleTime <- function(numCode) {
        ##go through the code and if a letter is in it add a random time based on the data from Bishops
        tempT <- numeric()
        code <- probServ$code[probServ$numCode == numCode]
        #print(code)
        for(i in letters) { 
                st <- servTime[servTime$code == i,]
                if(grepl(i,code)>0){ 
                        tempT <- c(tempT,rtriangle(1, a=st$min, b=st$max,c=st$mode)) }
                        #print(i)
                        #print(tempT)
        }
        ##returns a list that needs to be summed to get the total time
        return(sum(tempT))
        
}

##Unit testing for styleTime

numCode <- 33
test <- styleTime(numCode)
test
sum(test)
styleTime(servCode())
for(i in 1:50) {print(styleTime(servCode()))}

####################
##BALKING FUNCTION##
####################

balk <- function(envs,styNum) {
        q <- get_queue_count(envs, name = 'stylist')
        p <- sigmoid(q, a = 2, b = styNum) #REPLACE B WITH NUMBER OF STYLISTS
        test <- runif(1, min = 0, max = 1)
        if ( test < p) {return(1)} ##Sends the customer to balking
        return(2) ##Sends the customer to a stylist
      
}

balk(envs,styNum)
get_queue_count(data[1], name = "stylist")
envsRep[1]

#####################################
##SET-UP THE SIMULATION ENVIRONMENT##
#####################################
reset(envs)
envs <- simmer("Bishops") 

t0 <- create_trajectory("Bishops") %>%
        set_attribute("timein", function() now(envs)) %>%
        seize("checkin", 1) %>%
        timeout(function() rtriangle(1, a=.5, b=3,c=1)) %>% 
        release("checkin", 1) %>%
        branch(function() balk(envs,styNum), merge=c(FALSE, TRUE), #Branch based on queue length
                create_trajectory("balk") %>%
                        set_attribute("balk", function() get_queue_count(envs, name = 'stylist')),
                create_trajectory("style") %>%
                        set_attribute("numCode", function() servCode()) %>%
                        seize("stylist",1) %>%
                        timeout(function() styleTime(numCode)) %>% 
                        release("stylist", 1))

##Define the number of staff
checkNum <- 1
styNum <- 5

##Schedules in case I want to use them
checkSched <- schedule(c(0, 50), c(1,1), period=60)
stySched <- schedule(c(0, 50), c(5,5), period=60)


##Add the resources and the entities
envs %>% 
        add_resource("checkin", checkSched) %>%
        add_resource("stylist", stySched) %>%
        add_generator("customer", t0, function() sched$IAT[(get_n_generated(envs, "customer")) + 1] , mon = 2) 
        #add_generator("customer", t0, function() rexp(1,rate=.2) , mon = 2) 



########################################
##REPLICATE THE SIMULATION - BASE CASE##
########################################

##How many times to replicate
reps <- 10
# 
envs %>% run(until=730)
# head(get_mon_arrivals(envs))
t <- get_mon_attributes(envs)
# #att <- get_mon_attributes(envs)
# 
# sched <- schedIt()
# head(sched)
# 
# 
# 
# # Run from 9am to 9:10pm = 730 minutes
# envsRep <- lapply(1:reps, function(i) {
#         sched <- schedIt()
#         envs %>% run(until=730)
# })
# 


##############################
###THREE DIFFERENT SCHEDULES##
#############################

##FOUR STYLISTS
stySched <- schedule(c(0, 50), c(5,5), period=180)


ptm <- proc.time()
envs <- lapply(1:1000, function(i) {
        sched <- schedIt() #makes the IATs
        
        envs <<- simmer("Bishops") 
        
        t0 <- create_trajectory("Bishops") %>%
                set_attribute("timein", function() now(envs)) %>%
                seize("checkin", 1) %>%
                timeout(function() rtriangle(1, a=.5, b=3,c=1)) %>% 
                release("checkin", 1) %>%
                branch(function() balk(envs,styNum), merge=c(FALSE, TRUE), #Branch based on queue length
                        create_trajectory("balk") %>%
                                set_attribute("balk", function() get_queue_count(envs, name = 'stylist')),
                        create_trajectory("style") %>%
                                set_attribute("numCode", function() servCode()) %>%
                                seize("stylist",1) %>%
                                timeout(function() styleTime(numCode)) %>% 
                                release("stylist", 1))
        
        
        ##Define the number of staff
        checkNum <- 1
        styNum <- 6
        
        ##Schedules in case I want to use them
        checkSched <- schedule(c(0, 50), c(1,1), period=60)
        stySched <- schedule(c(0, 50), c(6,6), period=60)
        
        ##Add the resources and the entities
        envs %>% 
                add_resource("checkin", checkSched) %>%
                add_resource("stylist", stySched) %>%
                add_generator("customer", t0, function() sched$IAT[(get_n_generated(envs, "customer")) + 1] , mon = 2) 
        
        envs %>% run(until=730)
})

endT <- proc.time() - ptm
endT #~8 min again 

