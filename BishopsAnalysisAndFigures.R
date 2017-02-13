##Code below is written assuming that reps were generate using the lapply approach

##The idea is to run one function on a set of reps, and have it return a dataframe 
##of the basic statistics of interest
##This streamlines the analytical process GREATLY
##And should be a useful addition to the package

##Generic Metrics to calculate
## - mean queue size
## - max queue size
## - mean time in queue
## - max time in queue
## - # of entities through the system

##Bishops specific
## - # of resources that balked
## - fiscal impact of balk

##Resource specfic
## - resource utilization


##convert the data from a given run into a standard name for processing
##Note that data will be the first argument to the function call later when I finish it
data <- envs
reps <- 100
##Second argument
repName <- 'Base'

##################
##START FUNCTION##
##################


simmerMetsRange <- function(data,reps) {
        ##Sets the names of the output metrics
        metricNames <- c('RepName','MeanQueueSize','MaxQueueSize','MeanTimeinQueue',
                'MaxTimeinQueue','NumThrough', 'NumBalked', 'ResourceUtilization')
        
        ##initializes the data frame for output
        df <- data.frame(matrix(ncol = 8, nrow = reps))
        df <- data.frame(df)
        colnames(df) <- metricNames
        
        ##Assigns the name to df
        df$RepName<- repName
        
        ##Extract the basic data object for subsequent analysis
        arrivals <- get_mon_arrivals(data)
        res <- get_mon_resources(data)
        att <- get_mon_attributes(data)
        
        #Calculate the total time and queue time for each entity for later
        arrivals$total <- with(arrivals, end_time-start_time)
        arrivals$queue <- with(arrivals, total-activity_time)
        
        #####################################
        ## mean #Number Balked
        balkCount <- function(att){
                a <- table(att$key)
                a<- a[names(a)== 'balk']
                return(as.integer(a))
                
        }
        
        
        temp <- numeric()
        for(rep in 1:reps){
                tempDat <- subset(att, replication == rep)
                tempDat <- subset(tempDat, key == 'balk')
                temp <- c(temp,nrow(tempDat))
                
        }
        
        df$NumBalked <- temp
        ##############################################################
                
        ### mean number through
        temp <- aggregate(name~replication, data = att[att$key == 'timein',], FUN = NROW)
        df$NumThrough <- as.numeric(temp$name) - df$NumBalked
                
        ###Max queue Size
        ##Aggregate the max queue size by resource and resource and rep, than mean by rep
                
        temp <- aggregate(queue~replication, data = res, FUN = max)
        df$MaxQueueSize <- temp$queue
        #boxplot(temp$queue)
                
        ##Mean queue Size for the stylist
        res2 <- filter(res, resource == "stylist")
        tdif <- c(0,diff(res2$time, lag=1))
        res2<-cbind(res2,tdif)
        res2 <- mutate(res2,qTimeProd = tdif*queue)
        res2 <- res2[res2$tdif >= 0,]
        qTimeProdSum <- aggregate(qTimeProd~replication,data = res2, FUN = sum)
        meanQueueSize <- mutate(qTimeProdSum, avg = qTimeProd/730)
        df$MeanQueueSize <- meanQueueSize$avg
        #CI(meanQueueSize$avg)
                
        ##Mean time in queue by rep
        temp <- aggregate(queue~replication, data = arrivals, FUN = mean)
        df$MeanTimeinQueue <- temp$queue
        #boxplot(temp$queue)
                
        ##Max time in queue
        temp <- aggregate(queue~replication, data = arrivals, FUN = max)
        df$MaxTimeinQueue <- temp$queue
        #boxplot(temp$queue)
                
        ##Resource utilization
                
        ##First let's plot it
        #plot_resource_utilization(envsRep, resources = c('checkin','stylist'))
                
                
        ##I hacked this from the plot_resource utilization
        ##returns resource utilization by rep
        ##equal to time in use vs time idle
                
        get_resource_utilization <- function(envs, resources) {
                #checkInstall(c("dplyr", "tidyr", "ggplot2", "scales"))
                # Hack to avoid spurious notes
                resource <- item <- value <- server <- queue <- system <- replication <- capacity <- runtime <- 
                        in_use <- utilization <- Q25 <- Q50 <- Q75 <- time <- NULL
                
                if (is.list(envs)) env <- envs[[1]]
                else env <- envs
                
                monitor_data <- envs %>% get_mon_resources(data="counts") %>% 
                        dplyr::filter(resource %in% resources) %>%
                        tidyr::gather(item, value, server, queue, system) %>%
                        dplyr::mutate(item = factor(item)) %>%
                        dplyr::filter(item == "server") %>%
                        dplyr::group_by(resource) %>%
                        dplyr::mutate(capacity = get_capacity(env, resource[[1]])) %>% 
                        dplyr::group_by(replication) %>%
                        dplyr::mutate(runtime = max(time)) %>%
                        dplyr::group_by(resource, replication, capacity, runtime) %>%
                        dplyr::mutate(in_use = (time-dplyr::lag(time)) * dplyr::lag(value)) %>%
                        dplyr::group_by(resource, replication, capacity, runtime) %>%
                        dplyr::summarise(in_use = sum(in_use, na.rm=T)) %>%
                        dplyr::ungroup() %>%
                        dplyr::mutate(utilization = in_use / capacity / runtime) %>%
                        dplyr::group_by(resource, capacity) #%>%
                        #                 dplyr::summarise(Q25 = stats::quantile(utilization, .25),
                        #                         Q50 = stats::quantile(utilization, .5),
                        #                         Q75 = stats::quantile(utilization, .75))
                        #         
                return(monitor_data)
        }
                
        temp<-get_resource_utilization(data, resources = c('checkin','stylist'))
                
        ##just the stylist
        temp <- subset(temp, resource == 'stylist')
        df$ResourceUtilization <- temp$utilization
                
        return(df)
        
}


######################
##METRIC CALCULATION##
######################

##Note that I ran the sim by changing schedules
##Then reassigning data to a new object for processing

##Four stylists
envsFour <- envs
temp <- simmerMetsRange(envs,reps=1000)
rangeDatFour <- temp

##FIVE STYLISTS
envsFive <- envs
temp <- simmerMetsRange(envs,reps=1000)
rangeDatFive <- temp

##SIX 
# <- schedule(c(0, 50), c(6,6), period=60)
envsSix <- envs
temp <- simmerMetsRange(envs,reps=1000)
rangeDatSix <- temp


# Some more metric code...
# #means
# repMeans <- apply(temp[,2:ncol(temp)],2, FUN = mean)
# #means and 95% CIs
# repCIs <- apply(temp[,2:ncol(temp)],2, FUN = CI)
# ##Quants
# repQuants <- apply(temp[,2:ncol(temp)],2, FUN = quantile)


#################
##ACF PLOTTING##
#################

resACF <- subset(res, replication = 1)
acf(resACF$queue, lag.max = 50, main = "ACF Queue")
pacf(resACF$queue)

################################
##PRODUCTION TABLES & BOXPLOTS##
################################

##Four stylists
#rangeDatFour
repCIs <- apply(rangeDatFour[,2:ncol(rangeDatFour)],2, FUN = CI)
test <- t(repCIs)
write.xlsx(test,"fourStylists.xlsx")

df.m <- melt(rangeDatFour[,2:ncol(rangeDatFour)])
p <- ggplot(data = df.m, aes(x=variable, y=value)) + geom_boxplot()
p + facet_wrap( ~ variable, scales="free")


##FIVE STYLISTS
#rangeDatFive
repCIs <- apply(rangeDatFive[,2:ncol(rangeDatFive)],2, FUN = CI)
test <- t(repCIs)
write.xlsx(test,"fiveStylists.xlsx")

df.m <- melt(rangeDatFive[,2:ncol(rangeDatFive)])
p <- ggplot(data = df.m, aes(x=variable, y=value)) + geom_boxplot()
p + facet_wrap( ~ variable, scales="free")


##SIX 
#rangeDatSix
repCIs <- apply(rangeDatSix[,2:ncol(rangeDatSix)],2, FUN = CI)
test <- t(repCIs)
write.xlsx(test,"sixStylists.xlsx")

df.m <- melt(rangeDatSix[,2:ncol(rangeDatSix)])
p <- ggplot(data = df.m, aes(x=variable, y=value)) + geom_boxplot()
p + facet_wrap( ~ variable, scales="free")


#########
##ANOVA##
#########
stylists <- c(rep(4,1000),rep(5,1000),rep(6,1000))

allDat <- rbind(rangeDatFour,rangeDatFive,rangeDatSix)
allDat$stylists <- as.factor(stylists)
allDat$MeanQueueSize <- as.numeric(allDat$MeanQueueSize)


##ANOVA Function so I can use apply
appAnova <- function(var) {
        fit <- lm(var ~ stylists, data = allDat)
        return(anova(fit))
        
}

##Runs the ANOVA on each metric
anDat <- allDat[,2:8]
AOVS <- apply(anDat,2, FUN = appAnova)
AOVS
     
