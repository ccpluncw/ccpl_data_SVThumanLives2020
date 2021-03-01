
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

gPlotLinearFit <- function(data,xColName, yColName, filename) {

  p <- ggplot(data, aes(x=eval(parse(text=xColName)), y=eval(parse(text=yColName)),)) + geom_point() + geom_smooth(method = lm, colour = "black", linetype=11, size=.5) + theme_bw() + xlab(xColName) + ylab(yColName)

  p <- p + coord_cartesian(ylim = c(-1, 1))
  p <- p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text=element_text(size=14))
  p <- p + geom_hline(yintercept = 0,colour = "grey", linetype="dotted", size=.5 )
  ggsave(filename, plot = p, width = 7, height = 7)

  return(p)
}

fitCol <- "fit.RT"
resCol <- "res.RT"

mainDir <- getwd()
mainDir <- getwd()
ch.newDir (mainDir, "more analyses")
maDir <- getwd()
setwd(mainDir)

### read in data
valueOverlapDataFile <- c("quantityPeopleOverlaps.txt", "quantityObjectsOverlaps.txt")
for(fileN in valueOverlapDataFile) {
  if(fileN == "quantityPeopleOverlaps.txt") {
    prefix <- "humanLives"
  } else {
    prefix <- "economicGoods"
  }
  data.ovrlp <-read.table(fileN, header=T, sep="\t", quote="\"")
  data.ovrlp$IA1 <-as.character(data.ovrlp$IA1)
  data.ovrlp$IB1 <-as.character(data.ovrlp$IB1)

  setwd(maDir)
  data.ovrlp.1 <- ch.moralsQuantsToGrps(data.ovrlp, "IA1", "IB1", c(1,10,40))
  data.ovrlp.1$qDiff <- data.ovrlp.1$p2Quant - data.ovrlp.1$p1Quant
  #remove qDiff == 0 because direction is ambiguous.  There is no "greater quantity" group so there is no way to assign a direction.
  data.ovrlp.1 <- data.ovrlp.1[data.ovrlp.1$qDiff !=0,]

  ### when qDiff is negative, reverse order of IA1 and IB1, change "direction", and recalculate qDiff as positive ###
  data.ovrlp.1 <- transform(data.ovrlp.1, IA1 = ifelse(qDiff < 0, IB1, IA1), IB1 = ifelse(qDiff < 0, IA1, IB1))
  data.ovrlp.1$direction <- ifelse(data.ovrlp.1$qDiff < 1, data.ovrlp.1$direction * -1, data.ovrlp.1$direction)
  data.ovrlp.1$qDiff <- ifelse(data.ovrlp.1$qDiff < 1, data.ovrlp.1$qDiff * -1, data.ovrlp.1$qDiff)

  data.ovrlp.1$dirO <- abs(data.ovrlp.1$overlap-1)* data.ovrlp.1$direction
  data.ovrlp.1$catDif <- paste(data.ovrlp.1$p1GrpSize,data.ovrlp.1$p2GrpSize, sep=" vs ")

  dat.mo.all <- mean(data.ovrlp.1$dirO)
  dat.SDo.all <- sd(data.ovrlp.1$dirO)
  dat.n.all <- length(data.ovrlp.1$dirO)
  ####

  ### get only same item/people data ###
    data.ovrlp.same <- data.ovrlp.1[data.ovrlp.1$p1 == data.ovrlp.1$p2,]
    dat.mo <- mean(data.ovrlp.same$dirO)
    dat.SDo <- sd(data.ovrlp.same$dirO)
    dat.n <- length(data.ovrlp.same$dirO)

  #Raw Same Items
    fileName <- paste(prefix, "dirOverlap by qDiff with error only same items.pdf")
    p.same <- gPlotLinearFit(data.ovrlp.same, "qDiff", "dirO", fileName)

    qDiff.lm <- lm(data.ovrlp.same$dirO~data.ovrlp.same$qDiff)
    pDiff <- as.numeric(overlapToP (abs(1 - qDiff.lm$coeff[1])))
    zDist <- qnorm(pDiff)

    ### repeated measures AOV for
    require(nlme)
    require(multcomp)
    data.ovrlp.same$catDif <- as.factor(data.ovrlp.same$catDif)
    catDiff.lme <- lme(dirO ~ qDiff, random=(~1|p1), data=data.ovrlp.same)
    catDiff.aov <- lme(dirO ~ catDif, random=(~1|p1), data=data.ovrlp.same)
    catDiff.aov.tuk <- glht(catDiff.aov, linfct=mcp(catDif = "Tukey"), test = adjusted(type = "bonferroni"))

  #Raw All Items
    fileName <- paste(prefix, "dirOverlap by qDiff with error all data.pdf")
    p.all <- gPlotLinearFit(data.ovrlp.1, "qDiff", "dirO", fileName)

    qDiff.all.lm <- lm(data.ovrlp.1$dirO~data.ovrlp.1$qDiff)
    catDiff.all.lme <- lme(dirO ~ qDiff, random=(~1|p1), data=data.ovrlp.1)
    qDiff.all.log.lm <- lm(data.ovrlp.1$dirO~log(data.ovrlp.1$qDiff))

    pDiff.all <- as.numeric(overlapToP (abs(1 - qDiff.all.lm$coeff[1])))
    zDist.all <- qnorm(pDiff)

  #log
    l.qDiff.lm <- lm(data.ovrlp.same$dirO~log(data.ovrlp.same$qDiff))

  #Calculate t.tests and bayes factors for same data
    altDO <- seq(-1,0,0.02)
    pAltHyp <- NULL
    for(i in altDO) {
      t.out2.tmp <- t.test(data.ovrlp.same$dirO,mu=i)
      t.bf <- extractBF(ttestBF(data.ovrlp.same$dirO,mu=i, rscale="ultrawide"))$bf
      tmp.df <- data.frame(altDO = i, p = t.out2.tmp$p.value, BF = t.bf)
      pAltHyp <- ch.rbind(pAltHyp, tmp.df)
    }

  #Same Items: Test small effects model vs larger effects model using Bayes factor
    altMu <- -.2
    bf.small <- ttestBF(data.ovrlp.same$dirO, nullInterval = c(altMu,0), mu=altMu, rscale="ultrawide")
    bf.larger <- ttestBF(data.ovrlp.same$dirO, nullInterval = c(altMu,-1), mu=altMu, rscale="ultrawide")
    BFSmallEffect <- bf.small[1]/bf.larger[1]
    BFSmallEffect.lg10 <- log10(extractBF(BFSmallEffect)$bf)

  #All Items: Test small effects model vs larger effects model using Bayes factor
    altMu <- -.2
    bf.small.all <- ttestBF(data.ovrlp.1$dirO, nullInterval = c(altMu,0), mu=altMu, rscale="ultrawide")
    bf.larger.all <- ttestBF(data.ovrlp.1$dirO, nullInterval = c(altMu,-1), mu=altMu, rscale="ultrawide")
    BFSmallEffect.all <- bf.small.all[1]/bf.larger.all[1]
    BFSmallEffect.all.lg10 <- log10(extractBF(BFSmallEffect.all)$bf)

  #plot t-test and BayesFactor results
  op <-	par(mfrow=c(1,1), bg="white",  bty="n", font=2, family='serif', mar=c(5,6,4,7), las=1)

    with(pAltHyp, plot(altDO, p, pch=16))
    with(pAltHyp, lines(altDO, p))
    abline(.05, 0, col="lightgrey", lty=3)
    fileName <- paste(prefix, "Alt Hypothesis dirOverlap by p value.pdf")
    dev.copy(pdf,fileName)
    dev.off()

    with(pAltHyp, plot(log10(BF) ~ altDO, pch=16, ylim=c(0,70)))
    with(pAltHyp, lines(log10(BF) ~ altDO))
    abline(2, 0, col="grey", lty=3)
    fileName <- paste(prefix, "Alt Hypothesis dirOverlap by BF value.pdf")
    dev.copy(pdf,fileName)
    dev.off()

  par(op)

  fileName <- paste(prefix, "overlapOut.txt")
  sink(fileName)
  cat("\n ******************** Only Same Item/People ******************** \n\n")
    cat("\n ***** Regression Directional Overlab = quantity difference ***** \n")
    cat("\n ***** RAW ***** \n")
    print(summary(qDiff.lm))
    cat("\n ***** Intercept effect size ***** \n")
    cat("\n Probability of Lower Quantity Group having a higher value than Higher Quantity Group: ", pDiff)
    cat("\n Z-score separation between value distributions: ", zDist)
    cat("\n\n ***** Bayes Test small vs large effect size: Effect -0.2 < DO < 0 vs -1 < D0 < -.2 ***** \n")
    cat("\n ***** Bayes Small Effect ***** \n")
    print(bf.small)
    cat("\n\n ***** Bayes Larger Effect ***** \n")
    print(bf.larger)
    cat("\n\n ***** Bayes Small vs Larger Effect ***** \n")
    print(BFSmallEffect)
    cat("\n Log10 BF: ", BFSmallEffect.lg10)
    cat("\n\n ***** for the reviewer ***** \n")
    cat("\n ***** lme with probe as random variable and qDiff is continuous ***** \n")
    print(summary(catDiff.lme))
    cat("\n ***** lme with probe as random variable and qDiff is categorized by small-v-medium, medium-v-large, large-v-small ***** \n")
    print(anova(catDiff.aov))
    cat("\n ***** Tukey Post Hoc ***** \n")
    print(summary(catDiff.aov.tuk))

    cat("\n\n\n ***** LOG ***** \n")
    print(summary(l.qDiff.lm))

    cat("\n\n Mean Directional Overlap: ", dat.mo)
    cat("\n\n SD Directional Overlap: ", dat.SDo)
    cat("\n\n N: ", dat.n)
    cat("\n ******************** END Same Item/People ******************** \n\n\n")


    cat("\n ******************** ALL Item/People ******************** \n\n")
    cat("\n ***** Regression Directional Overlab = quantity difference ***** \n")
    cat("\n ***** RAW ***** \n")
    print(summary(qDiff.all.lm))
    cat("\n ***** Intercept effect size ***** \n")
    cat("\n Probability of Lower Quantity Group having a higher value than Higher Quantity Group: ", pDiff.all)
    cat("\n Z-score separation between value distributions: ", zDist.all)
    cat("\n\n ***** Bayes Test small vs large effect size: Effect -0.2 < DO < 0 vs -1 < D0 < -.2 ***** \n")
    cat("\n ***** Bayes Small Effect ***** \n")
    print(bf.small.all)
    cat("\n\n ***** Bayes Larger Effect ***** \n")
    print(bf.larger.all)
    cat("\n\n ***** Bayes Small vs Larger Effect ***** \n")
    print(BFSmallEffect.all)
    cat("\n Log10 BF: ", BFSmallEffect.all.lg10)
    cat("\n\n Mean Directional Overlap: ", dat.mo.all)
    cat("\n\n SD Directional Overlap: ", dat.SDo.all)
    cat("\n\n N: ", dat.n.all)
    cat("\n\n ***** for the reviewer ***** \n")
    cat("\n ***** lme with probe as random variable and qDiff is continuous ***** \n")
    print(summary(catDiff.all.lme))
    cat("\n\n\n ***** LOG ***** \n")
    print(summary(qDiff.all.log.lm))
    cat("\n ******************** END ALL Item/People ******************** \n\n\n")
  sink(NULL)

  setwd(mainDir)
}
