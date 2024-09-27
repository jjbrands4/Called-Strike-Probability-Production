# Metrics for Model Comp

# Set up enviornment
require(plyr)
require(ggplot2)
require(Metrics)

# Values for the rectangular strike zone
zonebots<-c(1.482968,1.512472,1.548145)
zonetops<-c(3.413886,3.448781,3.493403)
rhbleft<-(-1.005503)
rhbright<-(0.9474737)
lhbleft<-(-0.8774387)
lhbright<-(1.011506)


# Create Model Comp table + Metric Comp table + RMSE between all CSProb and binary 0/1 called ball/strike
  ModelComp <- data.frame(rmse(testCSprob$CSProb_new,testCSprob$isStrike))
  ModelComp$RMSE <- data.frame(rmse(testCSprob$CSProb_new,testCSprob$isStrike))
  colnames(ModelComp)[1] <- "RMSE"
  
# TotalStrike%
  # % of all called strikes with CSProb >.5
  PredictedStrikes <- subset(testCSprob, CSProb_new > .5, select = c(CSProb_new,isStrike,plateLocationSide,plateLocationHeight))
  ModelComp$TotalStrikePredRightPerc <- length(which(PredictedStrikes$isStrike == 1))/NROW(PredictedStrikes$isStrike)
  
# TotalBall%
  # % of all called balls with CSProb <.5
  PredictedBalls <- subset(testCSprob, CSProb_new < .5, select = c(CSProb_new,isStrike,plateLocationSide,plateLocationHeight))
  ModelComp$TotalBallPredRightPerc <- length(which(PredictedBalls$isStrike == 0))/NROW(PredictedBalls$isStrike)
  
# NoDoubtStrike%
  # If CSProb >.9, was it predicted as a strike correctly
  PredictedNoDoubtStrikes <- subset(testCSprob, CSProb_new > .9, select = c(CSProb_new,isStrike,plateLocationSide,plateLocationHeight))
  ModelComp$NoDoubtStrikePredRightPerc <- length(which(PredictedNoDoubtStrikes$isStrike == 1))/NROW(PredictedNoDoubtStrikes$isStrike)
  
# NoDoubtBall%
  # If CSProb <.1, was it predicted as a ball correctly
  PredictedNoDoubtBalls <- subset(testCSprob, CSProb_new < .1, select = c(CSProb_new,isStrike,plateLocationSide,plateLocationHeight))
  ModelComp$NoDoubtBallPredRightPerc <- length(which(PredictedNoDoubtBalls$isStrike == 0))/NROW(PredictedNoDoubtBalls$isStrike)
  
# Edge%
  # Top Edge Pitch Subsets
  TopTopEdgeS <- subset(testCSprob, plateLocationHeight <= (zonetops[1]+.25) & plateLocationHeight > (zonetops[1]), select=c(CSProb_new,isStrike,plateLocationHeight,plateLocationSide, bats, heightFlag, mlbamgameID, inning, inningHalf, atBatNum, atBatPitchNum, date))
  BotTopEdgeS <- subset(testCSprob, plateLocationHeight >= (zonetops[1]-.25) & plateLocationHeight <= (zonetops[1]), select=c(CSProb_new,isStrike,plateLocationHeight,plateLocationSide, bats, heightFlag, mlbamgameID, inning, inningHalf, atBatNum, atBatPitchNum, date))
  TopTopEdgeA <- subset(testCSprob, plateLocationHeight <= (zonetops[2]+.25) & plateLocationHeight > (zonetops[2]), select=c(CSProb_new,isStrike,plateLocationHeight,plateLocationSide, bats, heightFlag, mlbamgameID, inning, inningHalf, atBatNum, atBatPitchNum, date))
  BotTopEdgeA <- subset(testCSprob, plateLocationHeight >= (zonetops[2]-.25) & plateLocationHeight <= (zonetops[2]), select=c(CSProb_new,isStrike,plateLocationHeight,plateLocationSide, bats, heightFlag, mlbamgameID, inning, inningHalf, atBatNum, atBatPitchNum, date))
  TopTopEdgeT <- subset(testCSprob, plateLocationHeight <= (zonetops[3]+.25) & plateLocationHeight > (zonetops[3]), select=c(CSProb_new,isStrike,plateLocationHeight,plateLocationSide, bats, heightFlag, mlbamgameID, inning, inningHalf, atBatNum, atBatPitchNum, date))
  BotTopEdgeT <- subset(testCSprob, plateLocationHeight >= (zonetops[3]-.25) & plateLocationHeight <= (zonetops[3]), select=c(CSProb_new,isStrike,plateLocationHeight,plateLocationSide, bats, heightFlag, mlbamgameID, inning, inningHalf, atBatNum, atBatPitchNum, date))
  
  # Bottom Edge Pitch Subsets
  TopBotEdgeS <- subset(testCSprob, plateLocationHeight <= (zonebots[1]+.25) & plateLocationHeight >= (zonebots[1]), select=c(CSProb_new,isStrike,plateLocationHeight,plateLocationSide, bats, heightFlag, mlbamgameID, inning, inningHalf, atBatNum, atBatPitchNum, date))
  BotBotEdgeS <- subset(testCSprob, plateLocationHeight >= (zonebots[1]-.25) & plateLocationHeight < (zonebots[1]), select=c(CSProb_new,isStrike,plateLocationHeight,plateLocationSide, bats, heightFlag, mlbamgameID, inning, inningHalf, atBatNum, atBatPitchNum, date))
  TopBotEdgeA <- subset(testCSprob, plateLocationHeight <= (zonebots[2]+.25) & plateLocationHeight >= (zonebots[2]), select=c(CSProb_new,isStrike,plateLocationHeight,plateLocationSide, bats, heightFlag, mlbamgameID, inning, inningHalf, atBatNum, atBatPitchNum, date))
  BotBotEdgeA <- subset(testCSprob, plateLocationHeight >= (zonebots[2]-.25) & plateLocationHeight < (zonebots[2]), select=c(CSProb_new,isStrike,plateLocationHeight,plateLocationSide, bats, heightFlag, mlbamgameID, inning, inningHalf, atBatNum, atBatPitchNum, date))
  TopBotEdgeT <- subset(testCSprob, plateLocationHeight <= (zonebots[3]+.25) & plateLocationHeight >= (zonebots[3]), select=c(CSProb_new,isStrike,plateLocationHeight,plateLocationSide, bats, heightFlag, mlbamgameID, inning, inningHalf, atBatNum, atBatPitchNum, date))
  BotBotEdgeT <- subset(testCSprob, plateLocationHeight >= (zonebots[3]-.25) & plateLocationHeight < (zonebots[3]), select=c(CSProb_new,isStrike,plateLocationHeight,plateLocationSide, bats, heightFlag, mlbamgameID, inning, inningHalf, atBatNum, atBatPitchNum, date))
  
  # Left Edge Pitch Subsets
  OuterLeftEdgeL <- subset(testCSprob, plateLocationSide >= (lhbleft-.25) & plateLocationSide < lhbleft, select=c(CSProb_new,isStrike,plateLocationHeight,plateLocationSide, bats, heightFlag, mlbamgameID, inning, inningHalf, atBatNum, atBatPitchNum, date))
  InnerLeftEdgeL <- subset(testCSprob, plateLocationSide <= (lhbleft+.25) & plateLocationSide >= lhbleft, select=c(CSProb_new,isStrike,plateLocationHeight,plateLocationSide, bats, heightFlag, mlbamgameID, inning, inningHalf, atBatNum, atBatPitchNum, date))
  OuterLeftEdgeR <- subset(testCSprob, plateLocationSide >= (rhbleft-.25) & plateLocationSide < rhbleft, select=c(CSProb_new,isStrike,plateLocationHeight,plateLocationSide, bats, heightFlag, mlbamgameID, inning, inningHalf, atBatNum, atBatPitchNum, date))
  InnerLeftEdgeR <- subset(testCSprob, plateLocationSide <= (rhbleft+.25) & plateLocationSide >= rhbleft, select=c(CSProb_new,isStrike,plateLocationHeight,plateLocationSide, bats, heightFlag, mlbamgameID, inning, inningHalf, atBatNum, atBatPitchNum, date))
  
  # Right Edge Pitch Subsets
  OuterRightEdgeL <- subset(testCSprob, plateLocationSide <= (lhbright+.25) & plateLocationSide > lhbright, select=c(CSProb_new,isStrike,plateLocationHeight,plateLocationSide, bats, heightFlag, mlbamgameID, inning, inningHalf, atBatNum, atBatPitchNum, date))
  InnerRightEdgeL <- subset(testCSprob, plateLocationSide >= (lhbright-.25) & plateLocationSide <= lhbright, select=c(CSProb_new,isStrike,plateLocationHeight,plateLocationSide, bats, heightFlag, mlbamgameID, inning, inningHalf, atBatNum, atBatPitchNum, date))
  OuterRightEdgeR <- subset(testCSprob, plateLocationSide <= (rhbright+.25) & plateLocationSide > rhbright, select=c(CSProb_new,isStrike,plateLocationHeight,plateLocationSide, bats, heightFlag, mlbamgameID, inning, inningHalf, atBatNum, atBatPitchNum, date))
  InnerRightEdgeR <- subset(testCSprob, plateLocationSide >= (rhbright-.25) & plateLocationSide <= rhbright, select=c(CSProb_new,isStrike,plateLocationHeight,plateLocationSide, bats, heightFlag, mlbamgameID, inning, inningHalf, atBatNum, atBatPitchNum, date))
  
# Edge%
  # If pitch location is +/- 3in (width of baseball) away from edge of zone, was it predicted as a ball or strike correctly (CSProb > or < .5)
    # LHB
    EdgePerc_L_S <- unique(subset((rbind(InnerLeftEdgeL,OuterLeftEdgeL,InnerRightEdgeL,OuterRightEdgeL,TopBotEdgeS,BotBotEdgeS,BotTopEdgeS,TopTopEdgeS)), bats == "L" & heightFlag == 1 & plateLocationHeight >= (zonebots[1]-.25) & plateLocationHeight <= (zonetops[1]+.25) & plateLocationSide >= (lhbleft-.25) & plateLocationSide <= (lhbright+.25)))
    EdgePerc_L_A <- unique(subset((rbind(InnerLeftEdgeL,OuterLeftEdgeL,InnerRightEdgeL,OuterRightEdgeL,TopBotEdgeA,BotBotEdgeA,BotTopEdgeA,TopTopEdgeA)), bats == "L" & heightFlag == 2 & plateLocationHeight >= (zonebots[2]-.25) & plateLocationHeight <= (zonetops[2]+.25) & plateLocationSide >= (lhbleft-.25) & plateLocationSide <= (lhbright+.25)))
    EdgePerc_L_T <- unique(subset((rbind(InnerLeftEdgeL,OuterLeftEdgeL,InnerRightEdgeL,OuterRightEdgeL,TopBotEdgeT,BotBotEdgeT,BotTopEdgeT,TopTopEdgeT)), bats == "L" & heightFlag == 3 & plateLocationHeight >= (zonebots[3]-.25) & plateLocationHeight <= (zonetops[3]+.25) & plateLocationSide >= (lhbleft-.25) & plateLocationSide <= (lhbright+.25)))
    
    ModelComp$EdgePerc_L_CombAvg <- mean(c((length(which(EdgePerc_L_S$isStrike == 1 & EdgePerc_L_S$CSProb >.5))/NROW(EdgePerc_L_S) + length(which(EdgePerc_L_S$isStrike == 0 & EdgePerc_L_S$CSProb <.5))/NROW(EdgePerc_L_S)), length(which(EdgePerc_L_A$isStrike == 1 & EdgePerc_L_A$CSProb >.5))/NROW(EdgePerc_L_A) + length(which(EdgePerc_L_A$isStrike == 0 & EdgePerc_L_A$CSProb <.5))/NROW(EdgePerc_L_A), length(which(EdgePerc_L_T$isStrike == 1 & EdgePerc_L_T$CSProb >.5))/NROW(EdgePerc_L_T) + length(which(EdgePerc_L_T$isStrike == 0 & EdgePerc_L_T$CSProb <.5))/NROW(EdgePerc_L_T)))
    # RHB
    EdgePerc_R_S <- unique(subset((rbind(InnerLeftEdgeR,OuterLeftEdgeR,InnerRightEdgeR,OuterRightEdgeR,TopBotEdgeS,BotBotEdgeS,BotTopEdgeS,TopTopEdgeS)), bats == "R" & heightFlag == 1 & plateLocationHeight >= (zonebots[1]-.25) & plateLocationHeight <= (zonetops[1]+.25) & plateLocationSide >= (rhbleft-.25) & plateLocationSide <= (rhbright+.25)))
    EdgePerc_R_A <- unique(subset((rbind(InnerLeftEdgeR,OuterLeftEdgeR,InnerRightEdgeR,OuterRightEdgeR,TopBotEdgeA,BotBotEdgeA,BotTopEdgeA,TopTopEdgeA)), bats == "R" & heightFlag == 2 & plateLocationHeight >= (zonebots[2]-.25) & plateLocationHeight <= (zonetops[2]+.25) & plateLocationSide >= (rhbleft-.25) & plateLocationSide <= (rhbright+.25)))
    EdgePerc_R_T <- unique(subset((rbind(InnerLeftEdgeR,OuterLeftEdgeR,InnerRightEdgeR,OuterRightEdgeR,TopBotEdgeT,BotBotEdgeT,BotTopEdgeT,TopTopEdgeT)), bats == "R" & heightFlag == 3 & plateLocationHeight >= (zonebots[3]-.25) & plateLocationHeight <= (zonetops[3]+.25) & plateLocationSide >= (rhbleft-.25) & plateLocationSide <= (rhbright+.25)))
    
    ModelComp$EdgePerc_R_CombAvg <- mean(c((length(which(EdgePerc_R_S$isStrike == 1 & EdgePerc_R_S$CSProb >.5))/NROW(EdgePerc_R_S) + length(which(EdgePerc_R_S$isStrike == 0 & EdgePerc_R_S$CSProb <.5))/NROW(EdgePerc_R_S)), length(which(EdgePerc_R_A$isStrike == 1 & EdgePerc_R_A$CSProb >.5))/NROW(EdgePerc_R_A) + length(which(EdgePerc_R_A$isStrike == 0 & EdgePerc_R_A$CSProb <.5))/NROW(EdgePerc_R_A), length(which(EdgePerc_R_T$isStrike == 1 & EdgePerc_R_T$CSProb >.5))/NROW(EdgePerc_R_T) + length(which(EdgePerc_R_T$isStrike == 0 & EdgePerc_R_T$CSProb <.5))/NROW(EdgePerc_R_T)))
    
# TopZone% - not split by bat hand, rhb Location Sides used
  # If pitch location is -3in from top of zone, was the called pitch correctly predicted (CSProb > or < .5)
    TopEdgePerc_S <- subset(unique(rbind(TopTopEdgeS,BotTopEdgeS)), heightFlag == 1 & plateLocationSide <= (rhbright+.25) & plateLocationSide >= (rhbleft-.25))
    TopEdgePerc_A <- subset(unique(rbind(TopTopEdgeA,BotTopEdgeA)), heightFlag == 2 & plateLocationSide <= (rhbright+.25) & plateLocationSide >= (rhbleft-.25))
    TopEdgePerc_T <- subset(unique(rbind(TopTopEdgeT,BotTopEdgeT)), heightFlag == 3 & plateLocationSide <= (rhbright+.25) & plateLocationSide >= (rhbleft-.25))
    
    ModelComp$TopEdgePerc_CombAvg <- mean(c(length(which(TopEdgePerc_S$isStrike == 1 & TopEdgePerc_S$CSProb >.5))/NROW(TopEdgePerc_S) + length(which(TopEdgePerc_S$isStrike == 0 & TopEdgePerc_S$CSProb <.5))/NROW(TopEdgePerc_S), length(which(TopEdgePerc_A$isStrike == 1 & TopEdgePerc_A$CSProb >.5))/NROW(TopEdgePerc_A) + length(which(TopEdgePerc_A$isStrike == 0 & TopEdgePerc_A$CSProb <.5))/NROW(TopEdgePerc_A), length(which(TopEdgePerc_T$isStrike == 1 & TopEdgePerc_T$CSProb >.5))/NROW(TopEdgePerc_T) + length(which(TopEdgePerc_T$isStrike == 0 & TopEdgePerc_T$CSProb <.5))/NROW(TopEdgePerc_T)))
    
# BotZone% - not split by bat hand, rhb Location Sides used
  # If pitch location is +3in from top of zone, was the called pitch correctly predicted (CSProb > or < .5)
    BotEdgePerc_S <- subset(unique(rbind(TopBotEdgeS,BotBotEdgeS)), heightFlag == 1 & plateLocationSide <= (rhbright+.25) & plateLocationSide >= (rhbleft-.25))
    BotEdgePerc_A <- subset(unique(rbind(TopBotEdgeA,BotBotEdgeA)), heightFlag == 2 & plateLocationSide <= (rhbright+.25) & plateLocationSide >= (rhbleft-.25))
    BotEdgePerc_T <- subset(unique(rbind(TopBotEdgeT,BotBotEdgeT)), heightFlag == 3 & plateLocationSide <= (rhbright+.25) & plateLocationSide >= (rhbleft-.25))
    
    ModelComp$BotEdgePerc_CombAvg <- mean(c(length(which(BotEdgePerc_S$isStrike == 1 & BotEdgePerc_S$CSProb >.5))/NROW(BotEdgePerc_S) + length(which(BotEdgePerc_S$isStrike == 0 & BotEdgePerc_S$CSProb <.5))/NROW(BotEdgePerc_S), length(which(BotEdgePerc_A$isStrike == 1 & BotEdgePerc_A$CSProb >.5))/NROW(BotEdgePerc_A) + length(which(BotEdgePerc_A$isStrike == 0 & BotEdgePerc_A$CSProb <.5))/NROW(BotEdgePerc_A), length(which(BotEdgePerc_T$isStrike == 1 & BotEdgePerc_T$CSProb >.5))/NROW(BotEdgePerc_T) + length(which(BotEdgePerc_T$isStrike == 0 & BotEdgePerc_T$CSProb <.5))/NROW(BotEdgePerc_T)))
    
# LeftZone%
  # If pitch location is +3in from left of zone (pitcher view), was the the called pitch correctly predicted (CSProb > or < .5)
    LeftEdgePerc_L <- subset(unique(rbind(InnerLeftEdgeL,OuterLeftEdgeL)), bats == "L" & plateLocationHeight <= (zonetops[1]+.25) & plateLocationHeight >= (zonebots[1]-.25))
    LeftEdgePerc_R <- subset(unique(rbind(InnerLeftEdgeR,OuterLeftEdgeR)), bats == "R" & plateLocationHeight <= (zonetops[1]+.25) & plateLocationHeight >= (zonebots[1]-.25))
    
    ModelComp$LeftEdgePerc_CombAvg <- mean(c(length(which(LeftEdgePerc_L$isStrike == 1 & LeftEdgePerc_L$CSProb >.5))/NROW(LeftEdgePerc_L) + length(which(LeftEdgePerc_L$isStrike == 0 & LeftEdgePerc_L$CSProb <.5))/NROW(LeftEdgePerc_L), length(which(LeftEdgePerc_R$isStrike == 1 & LeftEdgePerc_R$CSProb >.5))/NROW(LeftEdgePerc_R) + length(which(LeftEdgePerc_R$isStrike == 0 & LeftEdgePerc_R$CSProb <.5))/NROW(LeftEdgePerc_R)))
    
# RightZone%
  # If pitch location is -3in from right of zone (pitcher view), was the the called pitch correctly predicted (CSProb > or < .5)
    RightEdgePerc_L <- subset(unique(rbind(InnerRightEdgeL,OuterRightEdgeL)), bats == "L" & plateLocationHeight <= (zonetops[1]+.25) & plateLocationHeight >= (zonebots[1]-.25))
    RightEdgePerc_R <- subset(unique(rbind(InnerRightEdgeR,OuterRightEdgeR)), bats == "R" & plateLocationHeight <= (zonetops[1]+.25) & plateLocationHeight >= (zonebots[1]-.25))
    
    ModelComp$RightEdgePerc_CombAvg <- mean(c(length(which(RightEdgePerc_L$isStrike == 1 & RightEdgePerc_L$CSProb >.5))/NROW(RightEdgePerc_L) + length(which(RightEdgePerc_L$isStrike == 0 & RightEdgePerc_L$CSProb <.5))/NROW(RightEdgePerc_L), length(which(RightEdgePerc_R$isStrike == 1 & RightEdgePerc_R$CSProb >.5))/NROW(RightEdgePerc_R) + length(which(RightEdgePerc_R$isStrike == 0 & RightEdgePerc_R$CSProb <.5))/NROW(RightEdgePerc_R)))
    
# View results
    View(ModelComp)
    
# RMSE Variance for Edge%
    # LHB S
    EdgeVarData_0_1in_LSo <- subset(unique(EdgePerc_L_S), (plateLocationHeight >= (zonebots[1]-(1/12)) & plateLocationHeight <= (zonebots[1]) & plateLocationSide >= (lhbleft-(1/12)) & plateLocationSide <= (lhbright+(1/12))) | (plateLocationHeight <= (zonetops[1]+(1/12)) & plateLocationHeight > (zonetops[1]) & plateLocationSide >= (lhbleft-(1/12)) & plateLocationSide <= (lhbright+(1/12))) | (plateLocationSide >= (lhbleft-(1/12)) & plateLocationSide < (lhbleft) & plateLocationHeight >= (zonebots[1]-(1/12)) & plateLocationHeight <= (zonetops[1]+(1/12))) | (plateLocationSide <= (lhbright+(1/12)) & plateLocationSide > (lhbright)) & plateLocationHeight >= (zonebots[1]-(1/12)) & plateLocationHeight <= (zonetops[1]+(1/12)))
    EdgeVarData_0_1in_LSi <- subset(unique(EdgePerc_L_S), (plateLocationHeight > (zonebots[1]) & plateLocationHeight <= (zonebots[1] + (1/12)) & plateLocationSide > (lhbleft) & plateLocationSide < (lhbright)) | (plateLocationHeight < (zonetops[1]) & plateLocationHeight >= (zonetops[1]-(1/12)) & plateLocationSide > (lhbleft) & plateLocationSide < (lhbright)) | (plateLocationSide > (lhbleft) & plateLocationSide <= (lhbleft+(1/12)) & plateLocationHeight > (zonebots[1]) & plateLocationHeight < (zonetops[1])) | (plateLocationSide < (lhbright) & plateLocationSide >= (lhbright-(1/12))) & plateLocationHeight > (zonebots[1]) & plateLocationHeight < (zonetops[1]))
    EdgeVarData_0_1in_LS <- unique(rbind(EdgeVarData_0_1in_LSo, EdgeVarData_0_1in_LSi))
    EdgeVarData_1_2in_LSo <- subset(unique(EdgePerc_L_S), (plateLocationHeight >= (zonebots[1]-(2/12)) & plateLocationHeight < (zonebots[1]-(1/12)) & plateLocationSide >= (lhbleft-(2/12)) & plateLocationSide <= (lhbright+(2/12))) | (plateLocationHeight <= (zonetops[1]+(2/12)) & plateLocationHeight > (zonetops[1]+(1/12)) & plateLocationSide >= (lhbleft-(2/12)) & plateLocationSide <= (lhbright+(2/12))) | (plateLocationSide >= (lhbleft-(2/12)) & plateLocationSide < (lhbleft-(1/12)) & plateLocationHeight >= (zonebots[1]-(2/12)) & plateLocationHeight <= (zonetops[1]+(2/12))) | (plateLocationSide <= (lhbright+(2/12)) & plateLocationSide > (lhbright+(1/12))) & plateLocationHeight >= (zonebots[1]-(2/12)) & plateLocationHeight <= (zonetops[1]+(2/12)))
    EdgeVarData_1_2in_LSi <- subset(unique(EdgePerc_L_S), (plateLocationHeight <= (zonebots[1]+(2/12)) & plateLocationHeight > (zonebots[1]+(1/12)) & plateLocationSide >= (lhbleft+(2/12)) & plateLocationSide <= (lhbright-(2/12))) | (plateLocationHeight >= (zonetops[1]-(2/12)) & plateLocationHeight < (zonetops[1]-(1/12)) & plateLocationSide >= (lhbleft+(2/12)) & plateLocationSide <= (lhbright-(2/12))) | (plateLocationSide <= (lhbleft+(2/12)) & plateLocationSide > (lhbleft+(1/12)) & plateLocationHeight >= (zonebots[1]+(1/12)) & plateLocationHeight <= (zonetops[1]-(1/12))) | (plateLocationSide >= (lhbright-(2/12)) & plateLocationSide < (lhbright-(1/12))) & plateLocationHeight >= (zonebots[1]+(1/12)) & plateLocationHeight <= (zonetops[1]-(1/12)))
    EdgeVarData_1_2in_LS <- unique(rbind(EdgeVarData_1_2in_LSo, EdgeVarData_1_2in_LSi))
    EdgeVarData_2_3in_LSi <- subset(unique(EdgePerc_L_S), (plateLocationHeight >= (zonebots[1]-(3/12)) & plateLocationHeight < (zonebots[1]-(2/12)) & plateLocationSide >= (lhbleft-(3/12)) & plateLocationSide <= (lhbright+(3/12))) | (plateLocationHeight <= (zonetops[1]+(3/12)) & plateLocationHeight > (zonetops[1]+(2/12)) & plateLocationSide >= (lhbleft-(3/12)) & plateLocationSide <= (lhbright+(3/12))) | (plateLocationSide >= (lhbleft-(3/12)) & plateLocationSide < (lhbleft-(2/12)) & plateLocationHeight >= (zonebots[1]-(3/12)) & plateLocationHeight <= (zonetops[1]+(3/12))) | (plateLocationSide <= (lhbright+(3/12)) & plateLocationSide > (lhbright+(2/12))) & plateLocationHeight >= (zonebots[1]-(3/12)) & plateLocationHeight <= (zonetops[1]+(3/12)))
    EdgeVarData_2_3in_LSo <- subset(unique(EdgePerc_L_S), (plateLocationHeight <= (zonebots[1]+(3/12)) & plateLocationHeight > (zonebots[1]+(2/12)) & plateLocationSide >= (lhbleft+(3/12)) & plateLocationSide <= (lhbright-(3/12))) | (plateLocationHeight >= (zonetops[1]-(3/12)) & plateLocationHeight < (zonetops[1]-(2/12)) & plateLocationSide >= (lhbleft+(3/12)) & plateLocationSide <= (lhbright-(3/12))) | (plateLocationSide <= (lhbleft+(3/12)) & plateLocationSide > (lhbleft+(2/12)) & plateLocationHeight >= (zonebots[1]+(2/12)) & plateLocationHeight <= (zonetops[1]-(2/12))) | (plateLocationSide >= (lhbright-(3/12)) & plateLocationSide < (lhbright-(2/12))) & plateLocationHeight >= (zonebots[1]+(2/12)) & plateLocationHeight <= (zonetops[1]-(2/12)))
    EdgeVarData_2_3in_LS <- unique(rbind(EdgeVarData_2_3in_LSo, EdgeVarData_2_3in_LSi))
    # LHB A
    EdgeVarData_0_1in_LAo <- subset(unique(EdgePerc_L_A), (plateLocationHeight >= (zonebots[2]-(1/12)) & plateLocationHeight <= (zonebots[2]) & plateLocationSide >= (lhbleft-(1/12)) & plateLocationSide <= (lhbright+(1/12))) | (plateLocationHeight <= (zonetops[2]+(1/12)) & plateLocationHeight > (zonetops[2]) & plateLocationSide >= (lhbleft-(1/12)) & plateLocationSide <= (lhbright+(1/12))) | (plateLocationSide >= (lhbleft-(1/12)) & plateLocationSide < (lhbleft) & plateLocationHeight >= (zonebots[2]-(1/12)) & plateLocationHeight <= (zonetops[2]+(1/12))) | (plateLocationSide <= (lhbright+(1/12)) & plateLocationSide > (lhbright)) & plateLocationHeight >= (zonebots[2]-(1/12)) & plateLocationHeight <= (zonetops[2]+(1/12)))
    EdgeVarData_0_1in_LAi <- subset(unique(EdgePerc_L_A), (plateLocationHeight > (zonebots[2]) & plateLocationHeight <= (zonebots[2] + (1/12)) & plateLocationSide > (lhbleft) & plateLocationSide < (lhbright)) | (plateLocationHeight < (zonetops[2]) & plateLocationHeight >= (zonetops[2]-(1/12)) & plateLocationSide > (lhbleft) & plateLocationSide < (lhbright)) | (plateLocationSide > (lhbleft) & plateLocationSide <= (lhbleft+(1/12)) & plateLocationHeight > (zonebots[2]) & plateLocationHeight < (zonetops[2])) | (plateLocationSide < (lhbright) & plateLocationSide >= (lhbright-(1/12))) & plateLocationHeight > (zonebots[2]) & plateLocationHeight < (zonetops[2]))
    EdgeVarData_0_1in_LA <- unique(rbind(EdgeVarData_0_1in_LAo, EdgeVarData_0_1in_LAi))
    EdgeVarData_1_2in_LAo <- subset(unique(EdgePerc_L_A), (plateLocationHeight >= (zonebots[2]-(2/12)) & plateLocationHeight < (zonebots[2]-(1/12)) & plateLocationSide >= (lhbleft-(2/12)) & plateLocationSide <= (lhbright+(2/12))) | (plateLocationHeight <= (zonetops[2]+(2/12)) & plateLocationHeight > (zonetops[2]+(1/12)) & plateLocationSide >= (lhbleft-(2/12)) & plateLocationSide <= (lhbright+(2/12))) | (plateLocationSide >= (lhbleft-(2/12)) & plateLocationSide < (lhbleft-(1/12)) & plateLocationHeight >= (zonebots[2]-(2/12)) & plateLocationHeight <= (zonetops[2]+(2/12))) | (plateLocationSide <= (lhbright+(2/12)) & plateLocationSide > (lhbright+(1/12))) & plateLocationHeight >= (zonebots[2]-(2/12)) & plateLocationHeight <= (zonetops[2]+(2/12)))
    EdgeVarData_1_2in_LAi <- subset(unique(EdgePerc_L_A), (plateLocationHeight <= (zonebots[2]+(2/12)) & plateLocationHeight > (zonebots[2]+(1/12)) & plateLocationSide >= (lhbleft+(2/12)) & plateLocationSide <= (lhbright-(2/12))) | (plateLocationHeight >= (zonetops[2]-(2/12)) & plateLocationHeight < (zonetops[2]-(1/12)) & plateLocationSide >= (lhbleft+(2/12)) & plateLocationSide <= (lhbright-(2/12))) | (plateLocationSide <= (lhbleft+(2/12)) & plateLocationSide > (lhbleft+(1/12)) & plateLocationHeight >= (zonebots[2]+(1/12)) & plateLocationHeight <= (zonetops[2]-(1/12))) | (plateLocationSide >= (lhbright-(2/12)) & plateLocationSide < (lhbright-(1/12))) & plateLocationHeight >= (zonebots[2]+(1/12)) & plateLocationHeight <= (zonetops[2]-(1/12)))
    EdgeVarData_1_2in_LA <- unique(rbind(EdgeVarData_1_2in_LAo, EdgeVarData_1_2in_LAi))
    EdgeVarData_2_3in_LAi <- subset(unique(EdgePerc_L_A), (plateLocationHeight >= (zonebots[2]-(3/12)) & plateLocationHeight < (zonebots[2]-(2/12)) & plateLocationSide >= (lhbleft-(3/12)) & plateLocationSide <= (lhbright+(3/12))) | (plateLocationHeight <= (zonetops[2]+(3/12)) & plateLocationHeight > (zonetops[2]+(2/12)) & plateLocationSide >= (lhbleft-(3/12)) & plateLocationSide <= (lhbright+(3/12))) | (plateLocationSide >= (lhbleft-(3/12)) & plateLocationSide < (lhbleft-(2/12)) & plateLocationHeight >= (zonebots[2]-(3/12)) & plateLocationHeight <= (zonetops[2]+(3/12))) | (plateLocationSide <= (lhbright+(3/12)) & plateLocationSide > (lhbright+(2/12))) & plateLocationHeight >= (zonebots[2]-(3/12)) & plateLocationHeight <= (zonetops[2]+(3/12)))
    EdgeVarData_2_3in_LAo <- subset(unique(EdgePerc_L_A), (plateLocationHeight <= (zonebots[2]+(3/12)) & plateLocationHeight > (zonebots[2]+(2/12)) & plateLocationSide >= (lhbleft+(3/12)) & plateLocationSide <= (lhbright-(3/12))) | (plateLocationHeight >= (zonetops[2]-(3/12)) & plateLocationHeight < (zonetops[2]-(2/12)) & plateLocationSide >= (lhbleft+(3/12)) & plateLocationSide <= (lhbright-(3/12))) | (plateLocationSide <= (lhbleft+(3/12)) & plateLocationSide > (lhbleft+(2/12)) & plateLocationHeight >= (zonebots[2]+(2/12)) & plateLocationHeight <= (zonetops[2]-(2/12))) | (plateLocationSide >= (lhbright-(3/12)) & plateLocationSide < (lhbright-(2/12))) & plateLocationHeight >= (zonebots[2]+(2/12)) & plateLocationHeight <= (zonetops[2]-(2/12)))
    EdgeVarData_2_3in_LA <- unique(rbind(EdgeVarData_2_3in_LAo, EdgeVarData_2_3in_LAi))
    # LHB T
    EdgeVarData_0_1in_LTo <- subset(unique(EdgePerc_L_T), (plateLocationHeight >= (zonebots[3]-(1/12)) & plateLocationHeight <= (zonebots[3]) & plateLocationSide >= (lhbleft-(1/12)) & plateLocationSide <= (lhbright+(1/12))) | (plateLocationHeight <= (zonetops[3]+(1/12)) & plateLocationHeight > (zonetops[3]) & plateLocationSide >= (lhbleft-(1/12)) & plateLocationSide <= (lhbright+(1/12))) | (plateLocationSide >= (lhbleft-(1/12)) & plateLocationSide < (lhbleft) & plateLocationHeight >= (zonebots[3]-(1/12)) & plateLocationHeight <= (zonetops[3]+(1/12))) | (plateLocationSide <= (lhbright+(1/12)) & plateLocationSide > (lhbright)) & plateLocationHeight >= (zonebots[3]-(1/12)) & plateLocationHeight <= (zonetops[3]+(1/12)))
    EdgeVarData_0_1in_LTi <- subset(unique(EdgePerc_L_T), (plateLocationHeight > (zonebots[3]) & plateLocationHeight <= (zonebots[3] + (1/12)) & plateLocationSide > (lhbleft) & plateLocationSide < (lhbright)) | (plateLocationHeight < (zonetops[3]) & plateLocationHeight >= (zonetops[3]-(1/12)) & plateLocationSide > (lhbleft) & plateLocationSide < (lhbright)) | (plateLocationSide > (lhbleft) & plateLocationSide <= (lhbleft+(1/12)) & plateLocationHeight > (zonebots[3]) & plateLocationHeight < (zonetops[3])) | (plateLocationSide < (lhbright) & plateLocationSide >= (lhbright-(1/12))) & plateLocationHeight > (zonebots[3]) & plateLocationHeight < (zonetops[3]))
    EdgeVarData_0_1in_LT <- unique(rbind(EdgeVarData_0_1in_LTo, EdgeVarData_0_1in_LTi))
    EdgeVarData_1_2in_LTo <- subset(unique(EdgePerc_L_T), (plateLocationHeight >= (zonebots[3]-(2/12)) & plateLocationHeight < (zonebots[3]-(1/12)) & plateLocationSide >= (lhbleft-(2/12)) & plateLocationSide <= (lhbright+(2/12))) | (plateLocationHeight <= (zonetops[3]+(2/12)) & plateLocationHeight > (zonetops[3]+(1/12)) & plateLocationSide >= (lhbleft-(2/12)) & plateLocationSide <= (lhbright+(2/12))) | (plateLocationSide >= (lhbleft-(2/12)) & plateLocationSide < (lhbleft-(1/12)) & plateLocationHeight >= (zonebots[3]-(2/12)) & plateLocationHeight <= (zonetops[3]+(2/12))) | (plateLocationSide <= (lhbright+(2/12)) & plateLocationSide > (lhbright+(1/12))) & plateLocationHeight >= (zonebots[3]-(2/12)) & plateLocationHeight <= (zonetops[3]+(2/12)))
    EdgeVarData_1_2in_LTi <- subset(unique(EdgePerc_L_T), (plateLocationHeight <= (zonebots[3]+(2/12)) & plateLocationHeight > (zonebots[3]+(1/12)) & plateLocationSide >= (lhbleft+(2/12)) & plateLocationSide <= (lhbright-(2/12))) | (plateLocationHeight >= (zonetops[3]-(2/12)) & plateLocationHeight < (zonetops[3]-(1/12)) & plateLocationSide >= (lhbleft+(2/12)) & plateLocationSide <= (lhbright-(2/12))) | (plateLocationSide <= (lhbleft+(2/12)) & plateLocationSide > (lhbleft+(1/12)) & plateLocationHeight >= (zonebots[3]+(1/12)) & plateLocationHeight <= (zonetops[3]-(1/12))) | (plateLocationSide >= (lhbright-(2/12)) & plateLocationSide < (lhbright-(1/12))) & plateLocationHeight >= (zonebots[3]+(1/12)) & plateLocationHeight <= (zonetops[3]-(1/12)))
    EdgeVarData_1_2in_LT <- unique(rbind(EdgeVarData_1_2in_LTo, EdgeVarData_1_2in_LTi))
    EdgeVarData_2_3in_LTi <- subset(unique(EdgePerc_L_T), (plateLocationHeight >= (zonebots[3]-(3/12)) & plateLocationHeight < (zonebots[3]-(2/12)) & plateLocationSide >= (lhbleft-(3/12)) & plateLocationSide <= (lhbright+(3/12))) | (plateLocationHeight <= (zonetops[3]+(3/12)) & plateLocationHeight > (zonetops[3]+(2/12)) & plateLocationSide >= (lhbleft-(3/12)) & plateLocationSide <= (lhbright+(3/12))) | (plateLocationSide >= (lhbleft-(3/12)) & plateLocationSide < (lhbleft-(2/12)) & plateLocationHeight >= (zonebots[3]-(3/12)) & plateLocationHeight <= (zonetops[3]+(3/12))) | (plateLocationSide <= (lhbright+(3/12)) & plateLocationSide > (lhbright+(2/12))) & plateLocationHeight >= (zonebots[3]-(3/12)) & plateLocationHeight <= (zonetops[3]+(3/12)))
    EdgeVarData_2_3in_LTo <- subset(unique(EdgePerc_L_T), (plateLocationHeight <= (zonebots[3]+(3/12)) & plateLocationHeight > (zonebots[3]+(2/12)) & plateLocationSide >= (lhbleft+(3/12)) & plateLocationSide <= (lhbright-(3/12))) | (plateLocationHeight >= (zonetops[3]-(3/12)) & plateLocationHeight < (zonetops[3]-(2/12)) & plateLocationSide >= (lhbleft+(3/12)) & plateLocationSide <= (lhbright-(3/12))) | (plateLocationSide <= (lhbleft+(3/12)) & plateLocationSide > (lhbleft+(2/12)) & plateLocationHeight >= (zonebots[3]+(2/12)) & plateLocationHeight <= (zonetops[3]-(2/12))) | (plateLocationSide >= (lhbright-(3/12)) & plateLocationSide < (lhbright-(2/12))) & plateLocationHeight >= (zonebots[3]+(2/12)) & plateLocationHeight <= (zonetops[3]-(2/12)))
    EdgeVarData_2_3in_LT <- unique(rbind(EdgeVarData_2_3in_LTo, EdgeVarData_2_3in_LTi))
    
    # RHB S
    EdgeVarData_0_1in_RSo <- subset(unique(EdgePerc_R_S), (plateLocationHeight >= (zonebots[1]-(1/12)) & plateLocationHeight <= (zonebots[1]) & plateLocationSide >= (rhbleft-(1/12)) & plateLocationSide <= (rhbright+(1/12))) | (plateLocationHeight <= (zonetops[1]+(1/12)) & plateLocationHeight > (zonetops[1]) & plateLocationSide >= (rhbleft-(1/12)) & plateLocationSide <= (rhbright+(1/12))) | (plateLocationSide >= (rhbleft-(1/12)) & plateLocationSide < (rhbleft) & plateLocationHeight >= (zonebots[1]-(1/12)) & plateLocationHeight <= (zonetops[1]+(1/12))) | (plateLocationSide <= (rhbright+(1/12)) & plateLocationSide > (rhbright)) & plateLocationHeight >= (zonebots[1]-(1/12)) & plateLocationHeight <= (zonetops[1]+(1/12)))
    EdgeVarData_0_1in_RSi <- subset(unique(EdgePerc_R_S), (plateLocationHeight > (zonebots[1]) & plateLocationHeight <= (zonebots[1] + (1/12)) & plateLocationSide > (rhbleft) & plateLocationSide < (rhbright)) | (plateLocationHeight < (zonetops[1]) & plateLocationHeight >= (zonetops[1]-(1/12)) & plateLocationSide > (rhbleft) & plateLocationSide < (rhbright)) | (plateLocationSide > (rhbleft) & plateLocationSide <= (rhbleft+(1/12)) & plateLocationHeight > (zonebots[1]) & plateLocationHeight < (zonetops[1])) | (plateLocationSide < (rhbright) & plateLocationSide >= (rhbright-(1/12))) & plateLocationHeight > (zonebots[1]) & plateLocationHeight < (zonetops[1]))
    EdgeVarData_0_1in_RS <- unique(rbind(EdgeVarData_0_1in_RSo, EdgeVarData_0_1in_RSi))
    EdgeVarData_1_2in_RSo <- subset(unique(EdgePerc_R_S), (plateLocationHeight >= (zonebots[1]-(2/12)) & plateLocationHeight < (zonebots[1]-(1/12)) & plateLocationSide >= (rhbleft-(2/12)) & plateLocationSide <= (rhbright+(2/12))) | (plateLocationHeight <= (zonetops[1]+(2/12)) & plateLocationHeight > (zonetops[1]+(1/12)) & plateLocationSide >= (rhbleft-(2/12)) & plateLocationSide <= (rhbright+(2/12))) | (plateLocationSide >= (rhbleft-(2/12)) & plateLocationSide < (rhbleft-(1/12)) & plateLocationHeight >= (zonebots[1]-(2/12)) & plateLocationHeight <= (zonetops[1]+(2/12))) | (plateLocationSide <= (rhbright+(2/12)) & plateLocationSide > (rhbright+(1/12))) & plateLocationHeight >= (zonebots[1]-(2/12)) & plateLocationHeight <= (zonetops[1]+(2/12)))
    EdgeVarData_1_2in_RSi <- subset(unique(EdgePerc_R_S), (plateLocationHeight <= (zonebots[1]+(2/12)) & plateLocationHeight > (zonebots[1]+(1/12)) & plateLocationSide >= (rhbleft+(2/12)) & plateLocationSide <= (rhbright-(2/12))) | (plateLocationHeight >= (zonetops[1]-(2/12)) & plateLocationHeight < (zonetops[1]-(1/12)) & plateLocationSide >= (rhbleft+(2/12)) & plateLocationSide <= (rhbright-(2/12))) | (plateLocationSide <= (rhbleft+(2/12)) & plateLocationSide > (rhbleft+(1/12)) & plateLocationHeight >= (zonebots[1]+(1/12)) & plateLocationHeight <= (zonetops[1]-(1/12))) | (plateLocationSide >= (rhbright-(2/12)) & plateLocationSide < (rhbright-(1/12))) & plateLocationHeight >= (zonebots[1]+(1/12)) & plateLocationHeight <= (zonetops[1]-(1/12)))
    EdgeVarData_1_2in_RS <- unique(rbind(EdgeVarData_1_2in_RSo, EdgeVarData_1_2in_RSi))
    EdgeVarData_2_3in_RSi <- subset(unique(EdgePerc_R_S), (plateLocationHeight >= (zonebots[1]-(3/12)) & plateLocationHeight < (zonebots[1]-(2/12)) & plateLocationSide >= (rhbleft-(3/12)) & plateLocationSide <= (rhbright+(3/12))) | (plateLocationHeight <= (zonetops[1]+(3/12)) & plateLocationHeight > (zonetops[1]+(2/12)) & plateLocationSide >= (rhbleft-(3/12)) & plateLocationSide <= (rhbright+(3/12))) | (plateLocationSide >= (rhbleft-(3/12)) & plateLocationSide < (rhbleft-(2/12)) & plateLocationHeight >= (zonebots[1]-(3/12)) & plateLocationHeight <= (zonetops[1]+(3/12))) | (plateLocationSide <= (rhbright+(3/12)) & plateLocationSide > (rhbright+(2/12))) & plateLocationHeight >= (zonebots[1]-(3/12)) & plateLocationHeight <= (zonetops[1]+(3/12)))
    EdgeVarData_2_3in_RSo <- subset(unique(EdgePerc_R_S), (plateLocationHeight <= (zonebots[1]+(3/12)) & plateLocationHeight > (zonebots[1]+(2/12)) & plateLocationSide >= (rhbleft+(3/12)) & plateLocationSide <= (rhbright-(3/12))) | (plateLocationHeight >= (zonetops[1]-(3/12)) & plateLocationHeight < (zonetops[1]-(2/12)) & plateLocationSide >= (rhbleft+(3/12)) & plateLocationSide <= (rhbright-(3/12))) | (plateLocationSide <= (rhbleft+(3/12)) & plateLocationSide > (rhbleft+(2/12)) & plateLocationHeight >= (zonebots[1]+(2/12)) & plateLocationHeight <= (zonetops[1]-(2/12))) | (plateLocationSide >= (rhbright-(3/12)) & plateLocationSide < (rhbright-(2/12))) & plateLocationHeight >= (zonebots[1]+(2/12)) & plateLocationHeight <= (zonetops[1]-(2/12)))
    EdgeVarData_2_3in_RS <- unique(rbind(EdgeVarData_2_3in_RSo, EdgeVarData_2_3in_RSi))
    # RHB A
    EdgeVarData_0_1in_RAo <- subset(unique(EdgePerc_R_A), (plateLocationHeight >= (zonebots[2]-(1/12)) & plateLocationHeight <= (zonebots[2]) & plateLocationSide >= (rhbleft-(1/12)) & plateLocationSide <= (rhbright+(1/12))) | (plateLocationHeight <= (zonetops[2]+(1/12)) & plateLocationHeight > (zonetops[2]) & plateLocationSide >= (rhbleft-(1/12)) & plateLocationSide <= (rhbright+(1/12))) | (plateLocationSide >= (rhbleft-(1/12)) & plateLocationSide < (rhbleft) & plateLocationHeight >= (zonebots[2]-(1/12)) & plateLocationHeight <= (zonetops[2]+(1/12))) | (plateLocationSide <= (rhbright+(1/12)) & plateLocationSide > (rhbright)) & plateLocationHeight >= (zonebots[2]-(1/12)) & plateLocationHeight <= (zonetops[2]+(1/12)))
    EdgeVarData_0_1in_RAi <- subset(unique(EdgePerc_R_A), (plateLocationHeight > (zonebots[2]) & plateLocationHeight <= (zonebots[2] + (1/12)) & plateLocationSide > (rhbleft) & plateLocationSide < (rhbright)) | (plateLocationHeight < (zonetops[2]) & plateLocationHeight >= (zonetops[2]-(1/12)) & plateLocationSide > (rhbleft) & plateLocationSide < (rhbright)) | (plateLocationSide > (rhbleft) & plateLocationSide <= (rhbleft+(1/12)) & plateLocationHeight > (zonebots[2]) & plateLocationHeight < (zonetops[2])) | (plateLocationSide < (rhbright) & plateLocationSide >= (rhbright-(1/12))) & plateLocationHeight > (zonebots[2]) & plateLocationHeight < (zonetops[2]))
    EdgeVarData_0_1in_RA <- unique(rbind(EdgeVarData_0_1in_RAo, EdgeVarData_0_1in_RAi))
    EdgeVarData_1_2in_RAo <- subset(unique(EdgePerc_R_A), (plateLocationHeight >= (zonebots[2]-(2/12)) & plateLocationHeight < (zonebots[2]-(1/12)) & plateLocationSide >= (rhbleft-(2/12)) & plateLocationSide <= (rhbright+(2/12))) | (plateLocationHeight <= (zonetops[2]+(2/12)) & plateLocationHeight > (zonetops[2]+(1/12)) & plateLocationSide >= (rhbleft-(2/12)) & plateLocationSide <= (rhbright+(2/12))) | (plateLocationSide >= (rhbleft-(2/12)) & plateLocationSide < (rhbleft-(1/12)) & plateLocationHeight >= (zonebots[2]-(2/12)) & plateLocationHeight <= (zonetops[2]+(2/12))) | (plateLocationSide <= (rhbright+(2/12)) & plateLocationSide > (rhbright+(1/12))) & plateLocationHeight >= (zonebots[2]-(2/12)) & plateLocationHeight <= (zonetops[2]+(2/12)))
    EdgeVarData_1_2in_RAi <- subset(unique(EdgePerc_R_A), (plateLocationHeight <= (zonebots[2]+(2/12)) & plateLocationHeight > (zonebots[2]+(1/12)) & plateLocationSide >= (rhbleft+(2/12)) & plateLocationSide <= (rhbright-(2/12))) | (plateLocationHeight >= (zonetops[2]-(2/12)) & plateLocationHeight < (zonetops[2]-(1/12)) & plateLocationSide >= (rhbleft+(2/12)) & plateLocationSide <= (rhbright-(2/12))) | (plateLocationSide <= (rhbleft+(2/12)) & plateLocationSide > (rhbleft+(1/12)) & plateLocationHeight >= (zonebots[2]+(1/12)) & plateLocationHeight <= (zonetops[2]-(1/12))) | (plateLocationSide >= (rhbright-(2/12)) & plateLocationSide < (rhbright-(1/12))) & plateLocationHeight >= (zonebots[2]+(1/12)) & plateLocationHeight <= (zonetops[2]-(1/12)))
    EdgeVarData_1_2in_RA <- unique(rbind(EdgeVarData_1_2in_RAo, EdgeVarData_1_2in_RAi))
    EdgeVarData_2_3in_RAi <- subset(unique(EdgePerc_R_A), (plateLocationHeight >= (zonebots[2]-(3/12)) & plateLocationHeight < (zonebots[2]-(2/12)) & plateLocationSide >= (rhbleft-(3/12)) & plateLocationSide <= (rhbright+(3/12))) | (plateLocationHeight <= (zonetops[2]+(3/12)) & plateLocationHeight > (zonetops[2]+(2/12)) & plateLocationSide >= (rhbleft-(3/12)) & plateLocationSide <= (rhbright+(3/12))) | (plateLocationSide >= (rhbleft-(3/12)) & plateLocationSide < (rhbleft-(2/12)) & plateLocationHeight >= (zonebots[2]-(3/12)) & plateLocationHeight <= (zonetops[2]+(3/12))) | (plateLocationSide <= (rhbright+(3/12)) & plateLocationSide > (rhbright+(2/12))) & plateLocationHeight >= (zonebots[2]-(3/12)) & plateLocationHeight <= (zonetops[2]+(3/12)))
    EdgeVarData_2_3in_RAo <- subset(unique(EdgePerc_R_A), (plateLocationHeight <= (zonebots[2]+(3/12)) & plateLocationHeight > (zonebots[2]+(2/12)) & plateLocationSide >= (rhbleft+(3/12)) & plateLocationSide <= (rhbright-(3/12))) | (plateLocationHeight >= (zonetops[2]-(3/12)) & plateLocationHeight < (zonetops[2]-(2/12)) & plateLocationSide >= (rhbleft+(3/12)) & plateLocationSide <= (rhbright-(3/12))) | (plateLocationSide <= (rhbleft+(3/12)) & plateLocationSide > (rhbleft+(2/12)) & plateLocationHeight >= (zonebots[2]+(2/12)) & plateLocationHeight <= (zonetops[2]-(2/12))) | (plateLocationSide >= (rhbright-(3/12)) & plateLocationSide < (rhbright-(2/12))) & plateLocationHeight >= (zonebots[2]+(2/12)) & plateLocationHeight <= (zonetops[2]-(2/12)))
    EdgeVarData_2_3in_RA <- unique(rbind(EdgeVarData_2_3in_RAo, EdgeVarData_2_3in_RAi))
    # RHB T
    EdgeVarData_0_1in_RTo <- subset(unique(EdgePerc_R_T), (plateLocationHeight >= (zonebots[3]-(1/12)) & plateLocationHeight <= (zonebots[3]) & plateLocationSide >= (rhbleft-(1/12)) & plateLocationSide <= (rhbright+(1/12))) | (plateLocationHeight <= (zonetops[3]+(1/12)) & plateLocationHeight > (zonetops[3]) & plateLocationSide >= (rhbleft-(1/12)) & plateLocationSide <= (rhbright+(1/12))) | (plateLocationSide >= (rhbleft-(1/12)) & plateLocationSide < (rhbleft) & plateLocationHeight >= (zonebots[3]-(1/12)) & plateLocationHeight <= (zonetops[3]+(1/12))) | (plateLocationSide <= (rhbright+(1/12)) & plateLocationSide > (rhbright)) & plateLocationHeight >= (zonebots[3]-(1/12)) & plateLocationHeight <= (zonetops[3]+(1/12)))
    EdgeVarData_0_1in_RTi <- subset(unique(EdgePerc_R_T), (plateLocationHeight > (zonebots[3]) & plateLocationHeight <= (zonebots[3] + (1/12)) & plateLocationSide > (rhbleft) & plateLocationSide < (rhbright)) | (plateLocationHeight < (zonetops[3]) & plateLocationHeight >= (zonetops[3]-(1/12)) & plateLocationSide > (rhbleft) & plateLocationSide < (rhbright)) | (plateLocationSide > (rhbleft) & plateLocationSide <= (rhbleft+(1/12)) & plateLocationHeight > (zonebots[3]) & plateLocationHeight < (zonetops[3])) | (plateLocationSide < (rhbright) & plateLocationSide >= (rhbright-(1/12))) & plateLocationHeight > (zonebots[3]) & plateLocationHeight < (zonetops[3]))
    EdgeVarData_0_1in_RT <- unique(rbind(EdgeVarData_0_1in_RTo, EdgeVarData_0_1in_RTi))
    EdgeVarData_1_2in_RTo <- subset(unique(EdgePerc_R_T), (plateLocationHeight >= (zonebots[3]-(2/12)) & plateLocationHeight < (zonebots[3]-(1/12)) & plateLocationSide >= (rhbleft-(2/12)) & plateLocationSide <= (rhbright+(2/12))) | (plateLocationHeight <= (zonetops[3]+(2/12)) & plateLocationHeight > (zonetops[3]+(1/12)) & plateLocationSide >= (rhbleft-(2/12)) & plateLocationSide <= (rhbright+(2/12))) | (plateLocationSide >= (rhbleft-(2/12)) & plateLocationSide < (rhbleft-(1/12)) & plateLocationHeight >= (zonebots[3]-(2/12)) & plateLocationHeight <= (zonetops[3]+(2/12))) | (plateLocationSide <= (rhbright+(2/12)) & plateLocationSide > (rhbright+(1/12))) & plateLocationHeight >= (zonebots[3]-(2/12)) & plateLocationHeight <= (zonetops[3]+(2/12)))
    EdgeVarData_1_2in_RTi <- subset(unique(EdgePerc_R_T), (plateLocationHeight <= (zonebots[3]+(2/12)) & plateLocationHeight > (zonebots[3]+(1/12)) & plateLocationSide >= (rhbleft+(2/12)) & plateLocationSide <= (rhbright-(2/12))) | (plateLocationHeight >= (zonetops[3]-(2/12)) & plateLocationHeight < (zonetops[3]-(1/12)) & plateLocationSide >= (rhbleft+(2/12)) & plateLocationSide <= (rhbright-(2/12))) | (plateLocationSide <= (rhbleft+(2/12)) & plateLocationSide > (rhbleft+(1/12)) & plateLocationHeight >= (zonebots[3]+(1/12)) & plateLocationHeight <= (zonetops[3]-(1/12))) | (plateLocationSide >= (rhbright-(2/12)) & plateLocationSide < (rhbright-(1/12))) & plateLocationHeight >= (zonebots[3]+(1/12)) & plateLocationHeight <= (zonetops[3]-(1/12)))
    EdgeVarData_1_2in_RT <- unique(rbind(EdgeVarData_1_2in_RTo, EdgeVarData_1_2in_RTi))
    EdgeVarData_2_3in_RTi <- subset(unique(EdgePerc_R_T), (plateLocationHeight >= (zonebots[3]-(3/12)) & plateLocationHeight < (zonebots[3]-(2/12)) & plateLocationSide >= (rhbleft-(3/12)) & plateLocationSide <= (rhbright+(3/12))) | (plateLocationHeight <= (zonetops[3]+(3/12)) & plateLocationHeight > (zonetops[3]+(2/12)) & plateLocationSide >= (rhbleft-(3/12)) & plateLocationSide <= (rhbright+(3/12))) | (plateLocationSide >= (rhbleft-(3/12)) & plateLocationSide < (rhbleft-(2/12)) & plateLocationHeight >= (zonebots[3]-(3/12)) & plateLocationHeight <= (zonetops[3]+(3/12))) | (plateLocationSide <= (rhbright+(3/12)) & plateLocationSide > (rhbright+(2/12))) & plateLocationHeight >= (zonebots[3]-(3/12)) & plateLocationHeight <= (zonetops[3]+(3/12)))
    EdgeVarData_2_3in_RTo <- subset(unique(EdgePerc_R_T), (plateLocationHeight <= (zonebots[3]+(3/12)) & plateLocationHeight > (zonebots[3]+(2/12)) & plateLocationSide >= (rhbleft+(3/12)) & plateLocationSide <= (rhbright-(3/12))) | (plateLocationHeight >= (zonetops[3]-(3/12)) & plateLocationHeight < (zonetops[3]-(2/12)) & plateLocationSide >= (rhbleft+(3/12)) & plateLocationSide <= (rhbright-(3/12))) | (plateLocationSide <= (rhbleft+(3/12)) & plateLocationSide > (rhbleft+(2/12)) & plateLocationHeight >= (zonebots[3]+(2/12)) & plateLocationHeight <= (zonetops[3]-(2/12))) | (plateLocationSide >= (rhbright-(3/12)) & plateLocationSide < (rhbright-(2/12))) & plateLocationHeight >= (zonebots[3]+(2/12)) & plateLocationHeight <= (zonetops[3]-(2/12)))
    EdgeVarData_2_3in_RT <- unique(rbind(EdgeVarData_2_3in_RTo, EdgeVarData_2_3in_RTi))
    
    EdgeVar <- data.frame(rmse(EdgeVarData_0_1in_LS$isStrike,EdgeVarData_0_1in_LS$CSProb))
    data.frame(sqrt(sum(EdgeVarData_0_1in_LS$CSProb^2)/NROW(EdgeVarData_0_1in_LS$CSProb)))
    colnames(EdgeVar)[1] <- "LS_0_1in"
    EdgeVar$LS_1_2in <- data.frame(rmse(EdgeVarData_1_2in_LS$isStrike,EdgeVarData_1_2in_LS$CSProb))
    EdgeVar$LS_2_3in <- data.frame(rmse(EdgeVarData_2_3in_LS$isStrike,EdgeVarData_2_3in_LS$CSProb))
    EdgeVar$LA_0_1in <- data.frame(rmse(EdgeVarData_0_1in_LA$isStrike,EdgeVarData_0_1in_LA$CSProb))
    EdgeVar$LA_1_2in <- data.frame(rmse(EdgeVarData_1_2in_LA$isStrike,EdgeVarData_1_2in_LA$CSProb))
    EdgeVar$LA_2_3in <- data.frame(rmse(EdgeVarData_2_3in_LA$isStrike,EdgeVarData_2_3in_LA$CSProb))
    EdgeVar$LT_0_1in <- data.frame(rmse(EdgeVarData_0_1in_LT$isStrike,EdgeVarData_0_1in_LT$CSProb))
    EdgeVar$LT_1_2in <- data.frame(rmse(EdgeVarData_1_2in_LT$isStrike,EdgeVarData_1_2in_LT$CSProb))
    EdgeVar$LT_2_3in <- data.frame(rmse(EdgeVarData_2_3in_LT$isStrike,EdgeVarData_2_3in_LT$CSProb))
    EdgeVar$RS_0_1in <- data.frame(rmse(EdgeVarData_0_1in_RS$isStrike,EdgeVarData_0_1in_RS$CSProb))
    EdgeVar$RS_1_2in <- data.frame(rmse(EdgeVarData_1_2in_RS$isStrike,EdgeVarData_1_2in_RS$CSProb))
    EdgeVar$RS_2_3in <- data.frame(rmse(EdgeVarData_2_3in_RS$isStrike,EdgeVarData_2_3in_RS$CSProb))
    EdgeVar$RA_0_1in <- data.frame(rmse(EdgeVarData_0_1in_RA$isStrike,EdgeVarData_0_1in_RA$CSProb))
    EdgeVar$RA_1_2in <- data.frame(rmse(EdgeVarData_1_2in_RA$isStrike,EdgeVarData_1_2in_RA$CSProb))
    EdgeVar$RA_2_3in <- data.frame(rmse(EdgeVarData_2_3in_RA$isStrike,EdgeVarData_2_3in_RA$CSProb))
    EdgeVar$RT_0_1in <- data.frame(rmse(EdgeVarData_0_1in_RT$isStrike,EdgeVarData_0_1in_RT$CSProb))
    EdgeVar$RT_1_2in <- data.frame(rmse(EdgeVarData_1_2in_RT$isStrike,EdgeVarData_1_2in_RT$CSProb))
    EdgeVar$RT_2_3in <- data.frame(rmse(EdgeVarData_2_3in_RT$isStrike,EdgeVarData_2_3in_RT$CSProb))
  
    View(EdgeVar)
    
# Plot on various zones
  
  # Pick the left and right zone specs for either lhb or rhb
    # LHB
    #left<-lhbleft
    #right<-lhbright
    # RHB
    left<-rhbleft
    right<-rhbright
  
  # Top and bottom of zone for average height (2), (1) is short and (3) is tall
  top<-zonetops[2]
  bottom<-zonebots[2]
  
  kZone<-data.frame(plateLocationSide=c(left,left,right,right,left),
                    plateLocationHeight=c(bottom,top,top,bottom,bottom))
  
  # Create ggplot
  ggplot(kZone, aes(plateLocationSide, plateLocationHeight)) +
    geom_tile(data=testCSprob,
              aes(x=plateLocationSide, y=plateLocationHeight, fill=CSProb_new), 
              height=0.025, width=0.025) +  # Increased the size of the tiles
    scale_fill_gradientn(limits = c(0, 1), 
                         colours = colorRampPalette(c("blue3", "blue", "slateblue", "turquoise1", "white", "yellow", "orange", "red2"))(100), # More colors
                         name = "Called Strike Probability") +  # Change scale fill gradient title
    geom_path(lwd=1, col="black") +
    coord_fixed() +
    xlim(-3, 3) +
    ylim(0, 5) +
    labs(
      x = "Plate Location - Side",      
      y = "Plate Location - Height",     
      title = "RHB (Avg Height) Called Strike Probability"  
    ) +
    theme(plot.title = element_text(hjust = 0.5))