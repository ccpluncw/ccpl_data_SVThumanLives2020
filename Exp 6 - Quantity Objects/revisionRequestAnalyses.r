
require(chValues)
require(chutils)
library(chMorals)
library(dplyr)
library(RRW)
library(BayesFactor)
library(ggplot2)


ch.pooledSE <- function (se1, se2, n1, n2) {
  pSE <- sqrt(((n1 - 1)*se1^2 + (n2 - 1)*se2^2)/((n1 - 1) + (n2 - 1)))
  return(pSE)
}

fitCol <- "fit.RT"
resCol <- "res.RT"

params<-ch.readMoralsDBfile("moralsDBfile.txt")

mainDir <- getwd()
mainDir <- getwd()
ch.newDir (mainDir, "more analyses")
maDir <- getwd()
setwd(mainDir)

### read in data
analysisReadyData.sn <-read.table("analysisReadyData.sn.txt", header=T, sep="\t", quote="\"")

setwd(maDir)

################################################
### analyses on quant and value choice
################################################

## get quantity grouping information
data <- ch.moralsQuantsToGrps(analysisReadyData.sn, "Item1", "Item2", c(1,10,40))

#subset to get the same Item/people data
analysisReadyData.sn.same <- data[data$p1==data$p2,]
analysisReadyData.sn.same <- analysisReadyData.sn.same[complete.cases(analysisReadyData.sn.same),]

## get p(HQO) information etc.,
### same item data
data.same <- ch.moralsGetQuantityPhit(analysisReadyData.sn.same, "p1Quant", "p2Quant", "keyDef", respChoiceVal = c("Yes", "No"),"targetPresent", c(TRUE, FALSE))
data.all <- ch.moralsGetQuantityPhit(data, "p1Quant", "p2Quant", "keyDef", respChoiceVal = c("Yes", "No"),"targetPresent", c(TRUE, FALSE))
data.all$logQdiff <- log(data.all$qDiff)


#### All Data Analysis
  params$statsOutputFilePrefix <- "statsAll2OutputFile.txt"
  minN <- params$minOverlapN

  #predict p(HQO) by qDiff - re-run for posterity
  filename = file.path(paste(params$dt.set,"all pHitQ by Qdiff raw.pdf",sep=""))
  qOutList.rawAll <- ch.plotTwoLinearFits(data.all, "qDiff", "res.RT", "pHitQ", y1Label = "RTres", y2Label = "p(HQO)", xlab="Quantity Difference", minN = params$minOverlapN, ylimMax2 = 1, filename=filename)
  quant.int <- coef(qOutList.rawAll$y2Fit)["(Intercept)"]
  quant.se <- coef(summary(qOutList.rawAll$y2Fit))["(Intercept)", 2]

  #predict p(HQO) by log(qDiff) - reviewer request
  filename = file.path(paste(params$dt.set,"all pHitQ by Qdiff log.pdf",sep=""))
  qOutList.logAll <- ch.plotTwoLinearFits(data.all, "logQdiff", "res.RT", "pHitQ", y1Label = "RTres", y2Label = "p(HQO)", xlab="Log Quantity Difference", minN = minN, ylimMax2 = 1, filename=filename)

  #prepare data to predict p(HQO) by value
  data.all <- data.all[data.all$qDiff !=0,]
  df.tmp <- as.data.frame(data.all %>% group_by(OvrlpQuantConsistent, qDiff) %>% summarise(
      N = length(pHitQ),
      percentHit = mean(pHitQ),
      mRT = mean(res.RT),
      meanOv =  ch.round_any(mean(overlapRound), .02, round),
    ) )

  if (!is.null(minN)) {
    df.tmp <- df.tmp[df.tmp$N > minN,]
  }
  df.tmp <- droplevels(df.tmp)

  df.tmp$sType <- ifelse(df.tmp$OvrlpQuantConsistent==FALSE, 1, -1)
  df.tmp$sTypeOv <- df.tmp$sType*df.tmp$meanOv
  df.tmp$negStype <- -1*df.tmp$sType

  #predict value only
  modelFit.value <- lm(percentHit ~ 1 + negStype + sTypeOv, data=df.tmp)
  #predict value plus quantity
  modelFit.valueQuant <- lm(percentHit ~ 1 + negStype + sTypeOv + qDiff, data=df.tmp)

########

#### same Item data
  params$statsOutputFilePrefix <- "statsSameOutputFile.txt"
  minNsame <- 6

### Pure Monotonicity
### round qDiff because there are far fewer datapoints and we need stability.
data.same$qDiff2 <- ch.round_any(data.same$qDiff, 2, round)
df.tmp.1 <- as.data.frame(data.same %>% group_by(qDiff2) %>% summarise(
    N = length(pHitQ),
    percentHit = mean(pHitQ),
    mRT = mean(res.RT),
    ### round overlap because there are far fewer datapoints and we need stability.
    meanOv =  ch.round_any(mean(overlapRound), .02, round),
  ) )

### effect of p(HQO) predicted by value
df.tmp <- as.data.frame(data.same %>% group_by(OvrlpQuantConsistent, qDiff) %>% summarise(
    N = length(pHitQ),
    percentHit = mean(pHitQ),
    mRT = mean(res.RT),
    meanOv =  ch.round_any(mean(overlapRound), .02, round),
  ) )

df.tmp.2 <- as.data.frame(df.tmp %>% group_by(OvrlpQuantConsistent, meanOv) %>% summarise(
    N2 = sum(N),
    percentHit2 = sum(N*percentHit)/sum(N),
    mRT2 = sum(N*mRT)/sum(N),
  ) )

df.tmp2 <- df.tmp.2[df.tmp.2$N2 > minNsame & df.tmp.2$OvrlpQuantConsistent,]

cex1 = 1.5
op <- par(mfrow=c(2,1),bty="n", font=1, family='serif', mar=c(2,5,2,5), oma=c(3,0,3,0), cex=1.25, las=1)

  #plot p(HQO) by value
  ## exponential decay
  pHitFit <- ch.plot.pHit(df.tmp2$meanOv, df.tmp2$percentHit2, cex1 = cex1, printR2 = F, yLabel  = NA)
  ## linear
  RTfit <- ch.plot.lm(df.tmp2$meanOv, df.tmp2$mRT2, cex1 = cex1, printR2 = F, yLabel  = NA)
  dev.copy(pdf,"same Mono Value decay.pdf")
  dev.off()
  ## linear
  pHitFit.lm <- ch.plot.lm(df.tmp2$meanOv, df.tmp2$percentHit2, cex1 = cex1, printR2 = F, yLabel  = NA)
  RTfit <- ch.plot.lm(df.tmp2$meanOv, df.tmp2$mRT2, cex1 = cex1, printR2 = F, yLabel  = NA)
  dev.copy(pdf,"same Mono Value lm.pdf")
  dev.off()

  #plot p(HQO) by quantity
  ## linear
  y1Fit <- ch.plot.lm(df.tmp.1$qDiff2, df.tmp.1$percentHit, cex1 = cex1, printR2 = F)
  mono.int <- coef(y1Fit)["(Intercept)"]
  mono.se <- coef(summary(y1Fit))["(Intercept)", 2]
  ## linear
  y2Fit <- ch.plot.lm(df.tmp.1$qDiff2, df.tmp.1$mRT, cex1 = cex1, printR2 = F)
  dev.copy(pdf,"same Mono Quant.pdf")
  dev.off()

par(op)

## test difference between quant and mono intercepts
pSE = ch.pooledSE(quant.se, mono.se, length(predict(qOutList.rawAll$y2Fit)), length(predict(y1Fit)))
tIntDiff <- (quant.int - mono.int)/pSE
dfIntDiff <- length(predict(qOutList.rawAll$y2Fit)) + length(predict(y1Fit)) - 4
pIntDiff <- 2*pt(-abs(tIntDiff),df=dfIntDiff)

sink("pHQOanalysisOut.txt", append=F)
  cat("\n\n\n*********************** All Data Quantity Analysis ***********************n\n")
  cat("\n\n Minimum N per group: ", minN + 1, "\n\n")
  cat("\n*** p(HQO) = value  ***\n")
  print(summary(modelFit.value))
  cat("\n\n*** p(HQO) = value  + quant***\n")
  print(summary(modelFit.valueQuant))

  cat("\n\n\n*********************** Reviewer Request: Log Quantity Analysis ***********************n\n")
  cat("\n\n**** RT = qDiff (standard analysis - raw data) ****\n\n")
  print(summary(qOutList.rawAll$y1Fit))
  cat("\n\n**** p(Hit)quant = qDiff (standard analysis - raw data) ****\n\n")
  print(summary(qOutList.rawAll$y2Fit))
  cat("\n\n**** RT = log(qDiff) ****\n\n")
  print(summary(qOutList.logAll$y1Fit))
  cat("\n\n**** p(Hit)quant = log(qDiff) ****\n\n")
  print(summary(qOutList.logAll$y2Fit))

  cat("\n\n\n*********************** Same Item/Person Quantity Analysis ***********************n\n")
  cat("\n\n Minimum N per group: ", minNsame + 1, "\n\n")
  cat("\n*** p(HQO) = qDiff ***\n")
  cat("\nNumber of bins: ", length(df.tmp.1[df.tmp.1$N>minNsame,"N"]))
  cat("\nmean N per bin: ", mean(df.tmp.1[df.tmp.1$N>minNsame,"N"]))
  cat("\nSD N per bin: ", sd(df.tmp.1[df.tmp.1$N>minNsame,"N"]), "\n")
  print(summary(y1Fit))
  cat("\n t-intercept different from quantity analysis on all data (not just same): \n")
  cat("\n t", "(", dfIntDiff,") =", tIntDiff, ", p =", pIntDiff, "\n\n")
  cat("\n*** RT_HQO = qDiff ***\n")
  print(summary(y2Fit))

  cat("\n\n*** p(HQO) = value ***\n")
  cat("\nNumber of bins: ", length(df.tmp2$N2))
  cat("\nmean N per bin: ", mean(df.tmp2$N2))
  cat("\nSD N per bin: ", sd(df.tmp2$N2), "\n")
  cat("\n*** lm ***\n")
  print(summary(pHitFit.lm))
  cat("\n*** decay ***\n")
  print(summary(pHitFit$nlsObject))
  cat("R Square = ", pHitFit$r2 , "\n")
  cat("\n*** RT_HQO = value ***\n")
  print(summary(RTfit))

sink(NULL)

setwd(mainDir)
