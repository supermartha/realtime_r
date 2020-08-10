library('reshape2')
library('plyr')

#####
#####
##### Get slopes of n seconds after each token ######
getPriorGuise <- function(stimulus, tokenNum, token_guise_dataframe) {
  if (tokenNum != 1) {
    priorNum <- tokenNum - 1
    priorGuise <- token_guise_dataframe[token_guise_dataframe$stimulus==stimulus & token_guise_dataframe$token_num==tokenNum,]$guise
    return(priorGuise)
  }
  return('first')
}


getNextTokenTime <- function(times, tokenNum, total_seconds) {
  # Check if next token number is in list of times
  # If so, return it, otherwise return the last second
  newToken <- tokenNum + 1
  if (newToken > length(times)) {return(total_seconds)}
  else{return(times[newToken])}
}

getSlope <- function(thisRow, n, tokenNum, total_seconds, calculateEachNSeconds, token_guise_dataframe) {

  thisRow <- data.frame(as.list(thisRow), stringsAsFactors = FALSE)
  
  stimulus <- thisRow$stimulus
  
  # Verify that stimulus is in token_guise_dataframe
  if ((stimulus %in% unique(token_guise_dataframe$stimulus)==FALSE)) {
    stop(paste("ERROR: stimulus '", stimulus, "' missing from token_guise_dataframe"))
  }
  
  # Get time of token according to stimulus name
  time1 <- token_guise_dataframe[token_guise_dataframe$stimulus==stimulus & token_guise_dataframe$token_num==tokenNum,]$time
  
  if (calculateEachNSeconds==TRUE) {
    # for n-second windows after each token instead of token-to-token slopes
    time2 <- time1 + n
    # If token time + n greater than total num. seconds, use last second
    if (time2 > total_seconds) {time2 <- total_seconds}
  }
  else {
    ordered_times <- sort(token_guise_dataframe[token_guise_dataframe$stimulus==stimulus,]$time)
    time2 <- getNextTokenTime(ordered_times, tokenNum, total_seconds)
  }
  
  time1_rating <- as.numeric(as.character(thisRow[,paste("rating", time1, sep=".")]))
  time2_rating <- as.numeric(as.character(thisRow[,paste("rating", time2, sep=".")]))
  slope <- time2_rating - time1_rating
  return(slope)
}

getGuise <- function(stimulus, tokenNum, token_guise_dataframe) {
  return(token_guise_dataframe[token_guise_dataframe$stimulus==stimulus & token_guise_dataframe$token_num==tokenNum,]$guise)
}

addGuiseInfo <- function(dataframe, token_guise_dataframe) {
  print('...adding guise info')
  myData <- dataframe
  
  myData$guise <-  mapply(getGuise, stimulus=myData$stimulus, tokenNum=myData$tokenNum, MoreArgs=list(token_guise_dataframe=token_guise_dataframe))
  
  myData$tokenStimulus <- paste(myData$tokenNum, myData$stimulus, sep='*')
  myData$tokenStimulus <- mapply(gsub, 'slope', '', myData$tokenStimulus)
  
  print('...adding prior guise')
  myData$priorGuise <- mapply(getPriorGuise, myData$stimulus, myData$tokenNum, MoreArgs=list(token_guise_dataframe=token_guise_dataframe))

  print('...converting strings to factors')
  myData$guise <- factor(myData$guise)
  myData$tokenStimulus <- factor(myData$tokenStimulus)
  myData$priorGuise <- factor(myData$priorGuise)
  
  return(myData)
}


calculateSlopes <- function(dataframe, token_guise_dataframe, n=5, calculateEachNSeconds=TRUE, roundingFactor=.5) {
  ### dataframe = the dataframe
  #### n = how many seconds after token to calculate slopes for, if calculateEachNSeconds==TRUE
  ### calculate each n seconds: if TRUE, calculate slopes for N seconds after each token; if FALSE, calculate token-to-token slopes
  # roundingFactor = 1 for rounded to nearest second, 2 for rounded to nearest .5 second, 4 for nearest .25 second, etc.
  
  print('...Calculating some slopes...')
  myData <- dataframe
  
  ### Verify that token_guise_dataframe contains the correct columns
  for (column in c('token_num', 'stimulus', 'time', 'guise')) {
    if ((column %in% colnames(token_guise_dataframe)) == FALSE) {
      stop(paste('ERROR: token_guise_dataframe is missing required column:', column))
    }
  }
  
  ### Round token_guise_dataframe to match results data rounding
  token_guise_dataframe$time <- round(token_guise_dataframe$time*roundingFactor)/roundingFactor
  
  ### Calculate how many tokens
  howManyTokens <- length(unique(token_guise_dataframe$token_num))
  
  ### Reshape the data
  # 1) Get rid of X.1, time, originalTime, timePlusParticipant, x b/c these are different per timepoint
  myData$X.1 <- NULL
  myData$time <- NULL
  myData$originalTime <- NULL
  myData$timePlusParticipant <- NULL
  myData$x <- NULL
  myData$X <- NULL
  myData$timestamp <- NULL
  
  original_ncol <- ncol(myData)
  
  ## Some stimuli are slightly longer than others, check to get the shortest one
  ## and use that as the max time
  maxes <- c()
  for (stimulus in unique(myData$stimulus)) {
    thisMax <- max(myData[myData$stimulus==stimulus,]$roundedTime)
    maxes <- c(maxes, thisMax)
  }
  total_seconds <- min(maxes) 
  
  # Drop late times
  myData <- myData[myData$roundedTime <= total_seconds,]
  
  # 2) Then reshape so there's a column for each second
  myData2 <- reshape(myData, v.names="rating", idvar = "participant", timevar = "roundedTime", direction = "wide")
  
  ### Now for each token, use getSlope function to get the slope
  for (tokenCount in seq(howManyTokens)) {
    myData2[,paste("slope", tokenCount)] <- apply(X=myData2, MARGIN=1, FUN=getSlope, n=n, tokenNum=tokenCount, total_seconds=total_seconds, calculateEachNSeconds=calculateEachNSeconds, token_guise_dataframe=token_guise_dataframe)
  }
  
  ### Delete extra columns, reshape, and return
  myData3 <- myData2[-c((original_ncol -1) :(original_ncol + total_seconds*roundingFactor - 1)) ]
  myData4 <- melt(myData3, id.vars=c(colnames(myData3[c(1:(original_ncol-2))])))
  colnames(myData4)[colnames(myData4)=="variable"] <- "tokenNum"
  colnames(myData4)[colnames(myData4)=="value"] <- "slope"
  myData4$tokenNum <- as.numeric(as.character(mapply(gsub, 'slope ', '', myData4$tokenNum)))
  myData4 <- addGuiseInfo(myData4, token_guise_dataframe)
  return(myData4)
}