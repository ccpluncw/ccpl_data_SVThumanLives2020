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
processedData <- ch.moralsDataPrep(data.raw, "sn", "keybRT", "overlap", "direction", "trial", "keyDef", respChoiceVal = c("Yes", "No"), item1cols = c("ItemX"), item2cols = c("ItemY"), overlapItem1cols = c("IA1"), overlapItem2cols = c("IB1"), params )

### Filter data
analysisReadyData <- ch.moralsFilterData(processedData, "sn", "keybRT", "overlapRound", "avgRT", "correct",c(1,0), item1cols = c("ItemX"), item2cols = c("ItemY"), params)

### Do RT and p(Hit Analysis on Group Data - remove learning effects for the group)
analysisReadyData.gp <- ch.moralsGrpRTpHit(analysisReadyData, "trial", "keybRT", fitCol, resCol, "overlapRound", "keyDef",c("Yes", "No"), "correct",c(1,0), params)

### Do RT and p(Hit Analysis on individual subject Data - remove learning effects for each subject)
analysisReadyData.sn <- ch.moralsSnRTpHit(analysisReadyData.gp, "sn", "trial", "keybRT", fitCol, resCol, "overlap", "correct", c(1,0),  params)

#Do d'analysis as a group, but use the data whereby the learning effects were removed by subject
df.dPrime <- ch.moralsDprimeAnalysis(analysisReadyData.sn, "overlapRound", "correct", c(1,0), "targetPresent", c(TRUE,FALSE), resCol, params = params, filenameID = "gp")

#Do an item analysis on the data.  Doesn't matter whether use group or sn data - no rt analysis is done
itemAnalDat <- ch.moralsItemChoiceAnalysis(analysisReadyData.gp, "ItemX", "ItemY", "overlapRound", "dirOverlap","keyDef", respChoiceVal = c("Yes", "No"), params, saveFigures = T)

write.table(analysisReadyData.gp, "analysisReadyData.gp.txt", col.names=T, row.names=F, quote=F, sep="\t")
write.table(analysisReadyData.sn, "analysisReadyData.sn.txt", col.names=T, row.names=F, quote=F, sep="\t")
