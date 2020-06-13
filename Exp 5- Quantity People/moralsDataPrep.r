library(chMorals)
library(chutils)

fitCol <- "fit.RT"
resCol <- "res.RT"

params<-ch.readMoralsDBfile("moralsDBfile.txt")

mainDir <- getwd()
ch.newDir (mainDir, params$gpSubDir)
gpDir <- getwd()
setwd(mainDir)

ch.newDir (mainDir, params$itemSubDir)
itemDir <- getwd()
setwd(mainDir)

### read in data
data.raw <-read.table(params$moralsTaskDataFile, header=T, sep="\t", quote="\"")
data.ovrlp <-read.table(params$valueOverlapDataFile, header=T, sep="\t", quote="\"")

######_____REMOVE PRACTICE TRIALS _____######
data.raw <- data.raw[data.raw$trial_type >=1, ]

### do Prep analysis
processedData <- ch.moralsDataPrep(data.raw, "sn", "keybRT", "overlap", "direction", "trial", "keyDef", respChoiceVal = c("Yes", "No"), item1cols = c("Item1"), item2cols = c("Item2"), overlapItem1cols = c("IA1"), overlapItem2cols = c("IB1"), params )

### Filter data
analysisReadyData <- ch.moralsFilterData(processedData, "sn", "keybRT", "overlapRound", "avgRT", "correct",c(1,0), item1cols = c("Item1"), item2cols = c("Item2"), params)

### Do RT and p(Hit Analysis on Group Data - remove learning effects for the group)
analysisReadyData.gp <- ch.moralsGrpRTpHit(analysisReadyData, "trial", "keybRT", fitCol, resCol, "overlapRound", "keyDef",c("Yes", "No"), "correct",c(1,0), params)

### Do RT and p(Hit Analysis on individual subject Data - remove learning effects for each subject)
analysisReadyData.sn <- ch.moralsSnRTpHit(analysisReadyData.gp, "sn", "trial", "keybRT", fitCol, resCol, "overlap", "correct", c(1,0),  params)

#Do d'analysis as a group, but use the data whereby the learning effects were removed by subject
df.dPrime <- ch.moralsDprimeAnalysis(analysisReadyData.sn, "overlapRound", "correct", c(1,0), "targetPresent", c(TRUE,FALSE), resCol, params = params, filenameID = "gp")

#Do an item analysis on the data.  Doesn't matter whether use group or sn data - no rt analysis is done
itemAnalDat <- ch.moralsItemChoiceAnalysis(analysisReadyData.gp, "Item1", "Item2", "overlapRound", "dirOverlap","keyDef", respChoiceVal = c("Yes", "No"), params, saveFigures = T)

#### For experiments with quantity variable manipulations, do an analysis a quantity analysis
  ### first plot the directional overlap by quantity grouping
do.filename <- file.path(itemDir,paste0(params$dt.set,"Quantity by Item",".pdf"))
parOp <- par(mfrow=c(1,1), mai=c(1,1,1,1), omi=c(1,.75,.25,1), las=2, cex=1.25, lwd=2, bty='n', xpd = T)
overOut <- ch.moralsGetAndPlotQuantDirOverlap(data.ovrlp, "IA1", "IB1", "overlap", "direction", c(1,10,40), filename = do.filename, parOp = parOp, cexLegend = 1, lgndPlacement = c(0,1), cex1 = 1.25)
par(parOp)
#Then analyze the effects of quantity
quantData <- ch.moralsQuantAnalyis(analysisReadyData.sn, "Item1", "Item2", c(1,10,40),"keyDef", respChoiceVal = c("Yes", "No"),"targetPresent", c(TRUE, FALSE), "overlapRound", params, showLegend = FALSE)

write.table(analysisReadyData.gp, "analysisReadyData.gp.txt", col.names=T, row.names=F, quote=F, sep="\t")
write.table(analysisReadyData.sn, "analysisReadyData.sn.txt", col.names=T, row.names=F, quote=F, sep="\t")
