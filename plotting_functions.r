library(plyr)

plotStimulus <- function(stimulus, dataframe=results, add=FALSE, title=stimulus, lineColor="cyan4", alpha=0.2, showIndividuals=FALSE, ratingName="Educated", ylim=c(0,100), lwd=10, showPoints=FALSE, pointTimes=seq(1,10)) {
  ### A function for making a plot of mean ratings over time for a stimulus (use add=TRUE to overlay multiple stimuli)
  # stimulus: name of stimulus
  # dataframe: name of dataframe containing timepoint data
  # add: if TRUE, add the line to previously existing plot; if FALSE, create new plot
  # title: plot title
  # lineColor: color of line, not applicable if add=FALSE
  # alpha: transparency of 95% confidence intervals (0=invisible, 1=totally opaque)
  # showIndividuals: if TRUE, also plot each individual participant's ratings
  # ratingName: name of rating scale to display on y-axis
  # ylim: limits of y-axis
  # lwd: line width
  # showPoints: if TRUE, add points to show where tokens of interest are. Currently only implented for (ING)
  # pointTimes: not sure if this does anything???
  ### Example usage:
  # plotStimulus('robert reverse ING-IN-ING-IN', dataframe=educated)
  # plotStimulus('robert reverse IN-ING-IN-ING', dataframe=educated, add=TRUE)
  temp <- dataframe
  temp <- temp[temp$stimulus==stimulus,]
  print(length(unique(temp$participant)))
  
  if (add==FALSE) {
    plot(0,0, xlim=c(0, max(temp$time) + 4), ylim=ylim, xlab="Time (seconds)", ylab=paste("Rating (", ratingName, ")", sep=''), col="white", main=title) 
    lineColor <- 'coral3'}
  
  # colors for individual lines
  individualColors <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928')
  
  if (showIndividuals==TRUE) {
    for (participant in unique(temp$participant)) {
      thisPerson <- temp[temp$participant==participant,]
      thisPerson <- thisPerson[order(thisPerson$roundedTime),]
      lines(rating ~ roundedTime, dat=thisPerson, col=(sample(individualColors))[1], lwd=2, lty=(sample(seq(4)))[1])
    }
  }  
  
  # Calculate mean rating by time plus 95% confidence intervals for each timepoint
  # (Note: these confidence intervals are smaller than they really should be, because we aren't controlling for data being autocorrelated)
  means <- ddply(temp, ~ roundedTime, summarize, stdError=1.96*sd(rating)/sqrt(length(rating)), rating=mean(rating), veryFinal=mean(veryFinalRating))
  means$upperBound <- means$rating + means$stdError
  means$lowerBound <- means$rating - means$stdError
  lines(means$roundedTime, means$rating, col=adjustcolor(lineColor, alpha.f=0.9), lwd=lwd)
  
  ### Add 'very final rating' (last position slider was left in, even if after the audio finished playing)
  # (because of the way the web data collection was original coded, don't have time data for this--just know it's after the stimulus ended)
  veryFinalX <- c(means$roundedTime[nrow(means)], means$roundedTime[nrow(means)] + 4)
  veryFinalY <- c(means$rating[nrow(means)], max(means$veryFinal))
  lines(veryFinalX, veryFinalY, lty=2, lwd=2, col=lineColor)
  
  # Confidence intervals
  polygon(c(means$roundedTime, rev(means$roundedTime)), c(means$lowerBound, rev(means$upperBound)), col=adjustcolor(lineColor, alpha.f=alpha), border=NA)
  
  if (showPoints==TRUE) {
    for (pointTime in pointTimes) {
      points(pointTime, means[means$roundedTime == round(pointTime),]$rating, col="orange", pch=19)
    }
  }
  
}


makePosthocPlot <- function(dataframe, plottingColors=rainbow(8), ylim=c(1,10)) {
  ### Plot mean posthoc rating for each stimulus in a dataframe, as a notched boxplot
  # (currently not very pretty, could prettify later)
  
  ### Example usage:
  # makePosthocPlot(slike)
  
  myData <- dataframe
  ### Summarize by participant, to have a dataframe with just one posthoc rating per participant
  by_participant <- ddply(myData, ~ participant + stimulus, summarize, posthoc=mean(posthoc))
  by_participant2 <- with(by_participant, reorder(stimulus, posthoc, mean, na.rm=TRUE))
  ### Then calculate average for each stimulus
  by_stimulus <- ddply(by_participant, ~ stimulus, summarize, posthoc=mean(posthoc, na.rm=TRUE))
  by_stimulus$stimulus <- factor(by_stimulus$stimulus, levels=levels(by_participant2))
  by_stimulus <- by_stimulus[order(by_stimulus$stimulus),]
  #print(by_stimulus)
  boxplot(posthoc ~ by_participant2, dat=by_participant, col=plottingColors, las=2, ylim=ylim)
  # Plot means, since it can be hard to see difference between boxplots
  lines(seq(1, nrow(by_stimulus)), by_stimulus$posthoc)
  points(seq(1, nrow(by_stimulus)), by_stimulus$posthoc, pch=19)
  # Label each box with the stimulus name
  text(seq(1, length(by_stimulus$stimulus)), by_stimulus$posthoc + 1, by_stimulus$stimulus, cex=.5)
}

makeSlopesPlot <- function(dataframe, stimulusA, stimulusB, col1="darkseagreen4", col2="deeppink1", col3='blue', main=paste(stimulusA, stimulusB, sep=' - '), ylim=c(-10,10), color_order=col1, token_text=c(), legend_text=c(""), legend_colors=col1, ylab="Guise A Slope - Guise B Slope") {
  # Takes a slopes dataframe, like those generated in educated_prep.r,
  # with columns named 'slope' and 'tokenNum'.
  # Will plot the rating for stimulusA minus stimulusB for each slope interval as a bar graph
  
  ### Label/color parameters are set automatically for (ING) and 'like' (but not 'slike') stimuli. Otherwise:
  # color_order: either a single color or a vector of colors, this is the order the bars will be colored in
  # token_text: vector of labels to display for each bar
  # legend_text: labels for each color in legend
  # legend_colors: colors to show in legend
  
  myData <- dataframe
  
  # Calculate means/conf. ints. for each slope
  # Have to make these into global variables (using <<-) for some weird reason,
  # because of how ddply works
  stimulusA <<- stimulusA
  stimulusB <<- stimulusB
  slopeMeans <- ddply(myData, ~ tokenNum, summarize, meanA=mean(slope[stimulus==stimulusA]), errorA=sd(slope[stimulus==stimulusA])/(length(slope[stimulus==stimulusA])), meanB=mean(slope[stimulus==stimulusB]), errorB=sd(slope[stimulus==stimulusB])/(length(slope[stimulus==stimulusB])))
  
  slopeMeans$diff <- slopeMeans$meanA - slopeMeans$meanB
  slopeMeans$SE <- 1.96 * sqrt(slopeMeans$errorA + slopeMeans$errorB)
  slopeMeans$upper <- slopeMeans$diff + slopeMeans$SE
  slopeMeans$lower <- slopeMeans$diff - slopeMeans$SE
  
  barplot(slopeMeans$diff, col=color_order, ylab=ylab, main=main, ylim=ylim)
  abline(h=0)
  text(seq(.7, length.out=nrow(slopeMeans), by=1.2), slopeMeans$upper + .2, token_text, cex=.5)
  segments(seq(.7, length.out=nrow(slopeMeans), by=1.2), slopeMeans$lower, seq(.7, length.out=nrow(slopeMeans), by=1.2), slopeMeans$upper)
  # Confidence interval upper / lower bars
  segments(seq(.7, length.out=nrow(slopeMeans), by=1.2)-.1, slopeMeans$lower, seq(.7, length.out=nrow(slopeMeans), by=1.2)+.1, slopeMeans$lower)
  segments(seq(.7, length.out=nrow(slopeMeans), by=1.2)-.1, slopeMeans$upper, seq(.7, length.out=nrow(slopeMeans), by=1.2)+.1, slopeMeans$upper)
  legend("topright", legend_text, col=legend_colors, pch=15)
  
  return(slopeMeans)
}

### debugging pars
# myData <- ingSlopes
# groupingFactor <- 'guise'
# ylim <- c(-5,3)
# labelHeight <- 1

makeSlopesSummaryPlot <- function(myData, ylim='auto', main='', groupingFactor='guise', labelHeight = .5, lineTopHeight = .5, stimulusTextHeight=1, ylab='Mean slider movement (higher = more educated)', groupingLabelCex=1) {
  # Takes a dataframe of slopes data and plots mean difference between guises (averaging across individual tokens)
  # by default, ylim is automatically chosen but you can also set it as you would for a normal plot
  myData <- myData
  
  theseX <- c(1,2,4,5,7,8,10,11,13,14,16,17)
  confWidth <- .2
  ingColors <- c('coral3', 'cyan4')
  
  if (groupingFactor=='guise') {
    slopeMeans2 <- ddply(myData, ~ guise, summarize, meanA=mean(slope), errorA=1.96*sd(slope)/(sqrt(length(slope))))
    theseX <- c(1,2)
  }
  
  else {
    myData$groupingFactor <- myData[,groupingFactor]
    slopeMeans2 <- ddply(myData, ~ guise + groupingFactor, summarize, meanA=mean(slope), errorA=1.96*sd(slope)/(sqrt(length(slope))))
    slopeMeans2 <- slopeMeans2[order(slopeMeans2$groupingFactor, slopeMeans2$guise),]
    theseX <- theseX[1:(nrow(slopeMeans2))]
  }
  
  print(slopeMeans2)
  
  if (ylim=="auto") {
    if (groupingFactor=='guise') {
      ylim <- c(min(slopeMeans2$meanA - slopeMeans2$errorA), max(slopeMeans2$meanA + slopeMeans2$errorA) + labelHeight*2)
    }
    else {
      ylim <- c(min(slopeMeans2$meanA - slopeMeans2$errorA), max(slopeMeans2$meanA + slopeMeans2$errorA + 3))
    }
  }
  
  plot(theseX, slopeMeans2$meanA, ylim=ylim, pch=19, cex=2, col=ingColors, xaxt='n', ylab=ylab, xlab='', xlim=c(0,max(theseX) + 1), main=main)
  segments(theseX, slopeMeans2$meanA + slopeMeans2$errorA, theseX, slopeMeans2$meanA - slopeMeans2$errorA, col=ingColors)
  segWidth <- .05
  segments(theseX+segWidth, slopeMeans2$meanA + slopeMeans2$errorA, theseX-segWidth, slopeMeans2$meanA + slopeMeans2$errorA, col=ingColors)
  segments(theseX+segWidth, slopeMeans2$meanA - slopeMeans2$errorA, theseX-segWidth, slopeMeans2$meanA - slopeMeans2$errorA, col=ingColors)
  
  
  text(theseX, slopeMeans2$meanA + slopeMeans2$errorA + labelHeight, slopeMeans2$guise, col=ingColors)

  
  if (groupingFactor!='guise') {
    abline(v=3, col="gray")
    abline(v=6, col="gray")
    abline(h=max(slopeMeans2$meanA + slopeMeans2$errorA) + labelHeight + lineTopHeight, col="gray")
    factorLabels <- slopeMeans2$groupingFactor[c(TRUE, FALSE)]
    text(c(1.5,4.5,7.5), rep(max(slopeMeans2$meanA + slopeMeans2$errorA) + labelHeight + stimulusTextHeight, 3), unique(slopeMeans2$groupingFactor), font=2, cex=groupingLabelCex)
  }
}
