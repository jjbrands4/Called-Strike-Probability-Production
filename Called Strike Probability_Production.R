# R Script used to calculate the called strike probability of a given pitch.

# Set up environment
require(mgcv)
require(lme4)
require(sqldf)
require(openair)
library(corrplot)
library(plyr)
library(dplyr)
library(ggplot2)
library(RODBC)
library(tictoc)

# Connect to database and retrieve data
db<-odbcConnect("XXX")
db2<-dbConnect(odbc::odbc(),.connection_string = 
                 "XXX")

# Check if script is ready to be run for the day
sqlQuery(db,"
         DECLARE @currentTime dateTime 
         set @currentTime = getdate()
         
         WHILE((Select flag from XXX) <> 1)
         BEGIN
         WAITFOR DELAY '00:00:15'
         
         IF(datediff(mi,@currentTime,getDate()) > 40)
         BEGIN
         RAISERROR('We waited too long to run R script.',16,1)
         BREAK
         END
         END
         
         IF( (Select flag from XXX) <> 1)
         BEGIN
         RAISERROR('Called Strike Probability script must have not run properly, flag isn't set to 1!',16,1)
         RETURN
         END
         ")

flag <- sqlQuery(db,"SELECT FLAG FROM XXX")

# Run the script when it's time
if(flag[1,1]==1){
  
  # Read in data
  testCSprob<-dbSendQuery(db2, "select * from XXX")
  testCSprob<-dbFetch(testCSprob)
  
  # Data exploration - Correlation Matrix
  #selected_columns <- c("isStrike", "balls", "strikes", "outs", "releaseSpeed", 
  #                      "plateLocationHeight", "plateLocationSide", "batterHeight", 
  #                      "isTop", "pfxX", "pfxZ")
  
  #correlationsInit <- cor(testCSprob[selected_columns], use = "complete.obs")
  
  #corrplot(correlationsInit, 
  #          method = 'number',
  #          order = 'original',
  #          type = 'lower',
  #          tl.col = "black",
  #          number.cex = 0.6,
  #          col = viridis(200),
  #          tl.cex = 0.7,
  #         addCoef.col = "black")   
  
  # Clean bad counts
  testCSprob <- testCSprob %>%
    filter(count %in% c("0-0", "0-1", "0-2", "1-1", "1-2", "2-1", "2-2", "3-2"))
  
  # Clean bad handedness
  testCSprob <- testCSprob %>% 
    mutate(throws = case_when(throws == "R" ~ "R",
                              throws == "L" ~ "L",
                              pitchType == "U" ~	"R"))
  
  # Clean bad pitch types
  testCSprob$pitchType[testCSprob$pitchType == "Fastball"] <- "Four-Seam FB"
  testCSprob$pitchType[testCSprob$pitchType == "Change Up"] <- "Changeup"
  testCSprob$pitchType[testCSprob$pitchType == "Other"] <- "Undefined"
  
  # NULL fill height where needed to avg batter height
  testCSprob$batterHeight[is.na(testCSprob$batterHeight)] <- 72
  
  # Subset for complete records of interest    
  testCSprob <- subset(testCSprob, !(is.na(testCSprob$releaseSpeed) | is.na(plateLocationHeight) | is.na(plateLocationSide) | is.na(pfxX) | is.na(pfxZ) |
                                       is.na(CorrectedZCoord) | is.na(CorrectedXCoord) | is.na(pitchType) | is.na(batterHeight) | is.na(isTop) | is.na(bats) | is.na(throws) |
                                       is.na(count) | is.na(batterLevel)))
  
  # Pitch type categorical grouping for pitch types with small samples / testing different pitch type groupings
  testCSprob <- testCSprob %>% 
    mutate(pitchType = case_when(pitchType == "Eephus" ~ "Curveball",
                                 pitchType == "Knuckle Curve" ~ "Knuckle Curve",
                                 pitchType == "Screwball" ~ "Curveball",
                                 pitchType == "Fastball" ~	"Fastball",
                                 pitchType == "Four-Seam FB" ~	"Four-Seam FB",
                                 pitchType == "Two-Seam FB" ~	"Two-Seam FB",
                                 pitchType == "Slider" ~ "Slider",
                                 pitchType == "Changeup" ~	"Changeup",
                                 pitchType == "Cutter" ~ "Cutter",
                                 pitchType == "Curveball" ~ "Curveball",
                                 pitchType == "Knuckleball" ~	"Knuckleball",
                                 pitchType == "Sinker" ~ "Sinker",
                                 pitchType == "Splitter" ~ "Splitter",
                                 pitchType == "Undefined" ~ "Undefined",
                                 pitchType == "Pitch Out" ~ "Pitch Out"))
  
  # Add Pitch Zone Bins - 16x16 shown to be the best blend of percision for correlative and run time purposes
  testCSprob$CorrectedXCoord<-ifelse(testCSprob$plateLocationSide<=(0)&testCSprob$plateLocationSide>(-.2083),-1,0)
  testCSprob$CorrectedXCoord<-ifelse(testCSprob$plateLocationSide<=(-.2083)&testCSprob$plateLocationSide>(-.4166),-2,testCSprob$CorrectedXCoord)
  testCSprob$CorrectedXCoord<-ifelse(testCSprob$plateLocationSide<=(-.4166)&testCSprob$plateLocationSide>(-.6250),-3,testCSprob$CorrectedXCoord)
  testCSprob$CorrectedXCoord<-ifelse(testCSprob$plateLocationSide<=(-.6250)&testCSprob$plateLocationSide>(-.8333),-4,testCSprob$CorrectedXCoord)
  testCSprob$CorrectedXCoord<-ifelse(testCSprob$plateLocationSide<=(-.8333)&testCSprob$plateLocationSide>(-1.0075),-5,testCSprob$CorrectedXCoord)
  testCSprob$CorrectedXCoord<-ifelse(testCSprob$plateLocationSide<=(-1.0075)&testCSprob$plateLocationSide>(-1.2158),-6,testCSprob$CorrectedXCoord)
  testCSprob$CorrectedXCoord<-ifelse(testCSprob$plateLocationSide<=(-1.2158)&testCSprob$plateLocationSide>(-1.4241),-7,testCSprob$CorrectedXCoord)
  testCSprob$CorrectedXCoord<-ifelse(testCSprob$plateLocationSide<=(-1.4241),-8,testCSprob$CorrectedXCoord)
  
  testCSprob$CorrectedXCoord<-ifelse(testCSprob$plateLocationSide>(0)&testCSprob$plateLocationSide<=(.2083),1,testCSprob$CorrectedXCoord)
  testCSprob$CorrectedXCoord<-ifelse(testCSprob$plateLocationSide>(.2083)&testCSprob$plateLocationSide<=(.4166),2,testCSprob$CorrectedXCoord)
  testCSprob$CorrectedXCoord<-ifelse(testCSprob$plateLocationSide>(.4166)&testCSprob$plateLocationSide<=(.6250),3,testCSprob$CorrectedXCoord)
  testCSprob$CorrectedXCoord<-ifelse(testCSprob$plateLocationSide>(.6250)&testCSprob$plateLocationSide<=(.8333),4,testCSprob$CorrectedXCoord)
  testCSprob$CorrectedXCoord<-ifelse(testCSprob$plateLocationSide>(.8333)&testCSprob$plateLocationSide<=(1.0075),5,testCSprob$CorrectedXCoord)
  testCSprob$CorrectedXCoord<-ifelse(testCSprob$plateLocationSide>(1.0075)&testCSprob$plateLocationSide<=(1.2158),6,testCSprob$CorrectedXCoord)
  testCSprob$CorrectedXCoord<-ifelse(testCSprob$plateLocationSide>(1.2158)&testCSprob$plateLocationSide<=(1.4241),7,testCSprob$CorrectedXCoord)
  testCSprob$CorrectedXCoord<-ifelse(testCSprob$plateLocationSide>(1.4241),8,testCSprob$CorrectedXCoord)
  
  testCSprob$CorrectedZCoord<-ifelse(testCSprob$plateLocationHeight<=(2.5799)&testCSprob$plateLocationHeight>(2.3038),-1,0)
  testCSprob$CorrectedZCoord<-ifelse(testCSprob$plateLocationHeight<=(2.3038)&testCSprob$plateLocationHeight>(2.0276),-2,testCSprob$CorrectedZCoord)
  testCSprob$CorrectedZCoord<-ifelse(testCSprob$plateLocationHeight<=(2.0276)&testCSprob$plateLocationHeight>(1.7513),-3,testCSprob$CorrectedZCoord)
  testCSprob$CorrectedZCoord<-ifelse(testCSprob$plateLocationHeight<=(1.7513)&testCSprob$plateLocationHeight>(1.50),-4,testCSprob$CorrectedZCoord)
  testCSprob$CorrectedZCoord<-ifelse(testCSprob$plateLocationHeight<=(1.50)&testCSprob$plateLocationHeight>(1.2237),-5,testCSprob$CorrectedZCoord)
  testCSprob$CorrectedZCoord<-ifelse(testCSprob$plateLocationHeight<=(1.2237)&testCSprob$plateLocationHeight>(.9474),-6,testCSprob$CorrectedZCoord)
  testCSprob$CorrectedZCoord<-ifelse(testCSprob$plateLocationHeight<=(.9474)&testCSprob$plateLocationHeight>(.6711),-7,testCSprob$CorrectedZCoord)
  testCSprob$CorrectedZCoord<-ifelse(testCSprob$plateLocationHeight<=(.6711),-8,testCSprob$CorrectedZCoord)
  
  testCSprob$CorrectedZCoord<-ifelse(testCSprob$plateLocationHeight>(2.5799)&testCSprob$plateLocationHeight<=(2.797425),1,testCSprob$CorrectedZCoord)
  testCSprob$CorrectedZCoord<-ifelse(testCSprob$plateLocationHeight>(2.797425)&testCSprob$plateLocationHeight<=(3.01495),2,testCSprob$CorrectedZCoord)
  testCSprob$CorrectedZCoord<-ifelse(testCSprob$plateLocationHeight>(3.01495)&testCSprob$plateLocationHeight<=(3.232475),3,testCSprob$CorrectedZCoord)
  testCSprob$CorrectedZCoord<-ifelse(testCSprob$plateLocationHeight>(3.232475)&testCSprob$plateLocationHeight<=(3.45),4,testCSprob$CorrectedZCoord)
  testCSprob$CorrectedZCoord<-ifelse(testCSprob$plateLocationHeight>(3.45)&testCSprob$plateLocationHeight<=(3.667525),5,testCSprob$CorrectedZCoord)
  testCSprob$CorrectedZCoord<-ifelse(testCSprob$plateLocationHeight>(3.667525)&testCSprob$plateLocationHeight<=(3.88505),6,testCSprob$CorrectedZCoord)
  testCSprob$CorrectedZCoord<-ifelse(testCSprob$plateLocationHeight>(3.88505)&testCSprob$plateLocationHeight<=(4.102575),7,testCSprob$CorrectedZCoord)
  testCSprob$CorrectedZCoord<-ifelse(testCSprob$plateLocationHeight>(4.102575),8,testCSprob$CorrectedZCoord)
  
  testCSprob$zoneBucket<-factor(paste(testCSprob$CorrectedXCoord,testCSprob$CorrectedZCoord,sep=","))
  
  # Batter height bins
  testCSprob$heightFlag <- ifelse(testCSprob$batterHeight <= 70, 1, ifelse(testCSprob$batterHeight >= 75,3,2))
  
  # pitchType bins
  testCSprob$pitchTypeFlag <- factor(ifelse(testCSprob$pitchType == ('Four-Seam FB'), '4S', ifelse(testCSprob$pitchType == ('Cutter'),'CT', ifelse((testCSprob$pitchType == ('Sinker') | testCSprob$pitchType == 'Two-Seam FB'),'2S', ifelse((testCSprob$pitchType == ('Changeup') | testCSprob$pitchType == ('Splitter')),'CH', ifelse(testCSprob$pitchType == ('Slider'),'SL', ifelse((testCSprob$pitchType == ('Curveball') | testCSprob$pitchType == ('Knuckleball') | testCSprob$pitchType == ('Screwball') | testCSprob$pitchType == ('Knuckle Curve') | testCSprob$pitchType == ('Eephus')),'CB', ifelse(testCSprob$pitchType == ('Pitch Out'),'Pitch Out','Undefined'))))))))
  
  # Create GAM
  #memory.limit(size = 112000)
  #tic("Model run time")
  #strike_prob_gam_byLevel <- bam(isStrike ~  ti(plateLocationHeight,plateLocationSide,pfxX,pfxZ) + ti(plateLocationHeight,plateLocationSide)
  #                               + s(CorrectedXCoord, by = pitchTypeFlag) + s(CorrectedZCoord, by = pitchTypeFlag)
  #                               + s(plateLocationHeight) + s(plateLocationSide)
  #                               + pitchType + batterHeight + isTop + bats + throws + count + batterLevel
  #                               ,data=testCSprob, family=binomial(link=logit))
  #toc()
  #saveRDS(strike_prob_gam_byLevel,"New_CSprob.rds")
  #k.check(strike_prob_gam_byLevel)
  
  setwd("XXX")
  strike_prob_gam_byLevel <- readRDS("New_CSprob.rds")
  
  # Add predicted strike likelihood
  testCSprob$CSProb_new <- predict(strike_prob_gam_byLevel, type = "response", newdata=testCSprob)
  
  dbWriteTable(db2,"XXX",testCSprob,overwrite = TRUE,append=FALSE)
  sqlQuery(db,"UPDATE XXX SET FLAG = 2")
  
}else{
  stop("The flag in XXX needs to be set to 1!")
}
odbcClose(db)