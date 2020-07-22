addTimepoints <- function(resData, roundTo=1) {
  #############################
  # For preparation of SLIDER data, used by general_prep.r and prolific_prep.r.
  # Because original data only has timepoints for times participants actually moved the slider,
  # need to add in timepoints for moments when they aren't moving. This is VERY SLOW but IMPORTANT.
  # roundTo = round to what, in what seconds? (default = 1 second, might also want to try .5 sec or .1 sec)
  #############################
  results <- resData
  roundingFactor <- 1/roundTo
  results$roundedTime <- round(results$time*roundingFactor)/roundingFactor
  
  numParticipants <- length(unique(results$participant))
  participantCount <- 0
  # Iterate thru participants
  for (participant in unique(results$participant)) {
    # Keep track of how many participants in order to print progress
    participantCount <- participantCount + 1
    if (participantCount%%10==0) {print(paste("Processing files:", participantCount, "of", numParticipants))}
    temp <- results[results$participant == participant,]
    # Create a 'veryFinalRating' column for the participant's last slider rating
    # (can compare in analysis to their post-hoc rating -- are they correlated?)
    if (length(temp$rating) > 1) {
      temp$veryFinalRating <- temp$rating[length(temp$rating) - 1]
    }
    else (temp$veryFinalRating <- temp$rating)
    results[results$participant==participant,]$veryFinalRating <- unique(temp$veryFinalRating)
    
    # Figure out how long the stimulus is, by finding the max time for that stimulus among ALL participants
    # (This is maybe a bit hacky, if you only have a few participants you could hand-code how long each
    # stimulus is supposed to be.) This is to make sure you fill in timepoints all the way to the end of
    # the stimulus.
    howMany <- max(results[results$stimulus==unique(temp$stimulus),]$roundedTime)
    
    # Now, for each second (or quarter-second, whatever you've rounded to) til the end of the stimulus...
    for (n in seq(0 + roundTo, howMany, by=roundTo)) {
      # ... check if there's already a datapoint at that time.
      if ((n %in% unique(temp$roundedTime))==FALSE) {
        # If there's NOT already a datapoint at that time, create a new row for it
        newRow <- tail(temp, 1) # just copy the last existing row of the dataframe to all the columns
        newRow$roundedTime <- n
        newRow$time <- n
        newRow$originalTime <- 'no'
        if (n==howMany) {
          newRow$originalTime <- 'yes'
          newRow$time <- howMany} # To make lines extend for plotting
        # Now find the previous existing timestamp
        # and set rating to rating at previous timestamp
        # e.g. if there's no rating for second 9, set it to the rating at second 8
        superTemp <- temp[temp$time < n,] # all times for this participant less than current time
        if (nrow(superTemp) == 0) {newRow$rating <- temp[temp$time==min(temp$time),]$rating[1]} # If there's no previous time, set to rating at earliest existing time
        else {newRow$rating <- mean(superTemp[superTemp$time==max(superTemp$time),]$rating)} # set to previous timestamp rating
        results <- rbind(results, newRow) # add to results dataframe
      }
    }
  }
  return(results)
}

prettyString <- function(theString, maxLength=50, collapse = "\n") 
  ### This function from "Jim Price", found here: http://r.789695.n4.nabble.com/Fitting-large-titles-in-a-plot-td842272.html
{ 
  words <- unlist(strsplit(theString, " ")) 
  wordLengths <- unlist(lapply(strsplit(words, ""), length)) 
  
  if(max(wordLengths) > maxLength) 
    stop("maxChar must be increased due to string length") 
  
  count = wordLengths[1] 
  results = vector() 
  currentLine = words[1] 
  
  for(i in 2:length(words)) 
  { 
    if((count + wordLengths[i] + 1) > maxLength) 
    { 
      results = c(results, currentLine) 
      currentLine = words[i] 
      count = wordLengths[i] 
    } 
    else 
    { 
      currentLine = paste(currentLine, words[i]) 
      count = count + wordLengths[i] + 1 
    } 
  } 
  if(length(currentLine)) 
    results <- c(results, currentLine) 
  
  paste(results, collapse = collapse) 
} 