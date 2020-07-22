library(plyr)
source('functions.r')

#### This script reads in web data and creates a dataframe called 'prolific', 
#### with one row for each participant's rating at each timepoint (e.g. each second).
#### This dataframe is also saved as a CSV file for faster loading in the future,
### by default called prolific_data.csv.

##############
resultsDir <- 'results/' # Directory which contains subdirectories for results (participantIDs, slider, posthoc, etc.). Use 'sample_results/' to try with sample results
resFile <- 'prolific_data.csv' # File to save results to
roundTo <- .5 # Round data to nearest what (1 = nearest second, .5 = nearest half-second, .1 = nearest tenth second, etc.)
# NB The smaller you choose, the longer this file will take to run and the heftier your resulting dataframe will be
##############

#### for generating sample results
resultsDir <- 'sample_results/'
resFile <- 'sample_prolific_data.csv'

### Initialize dataframe for results
prolific <- data.frame(time=as.numeric(), rating=as.numeric(), stimulus=as.character(), participant=as.character(), version=as.character(), shortID=as.character())

filenames <- list.files(paste(resultsDir, "slider/", sep=''), full.names=FALSE)

### Read in each results file and add to prolific dataframe
### Assumes filenames are formatted stimulusName1_stimulusName2_stimulusName3_participantID
### (e.g. robert_all_ING_f019324jadbfc)
for (file in filenames) {
  asVector <- unlist(strsplit(file, "_"))
  stimulus <- paste(asVector[1:3], collapse=' ')
  participant <- gsub('.csv', '', asVector[4])
  temp <- read.delim(paste(resultsDir, "slider/", file, sep=''))
  if (nrow(temp) != 0) { # Make sure it's not empty
    temp$x <- NULL
    temp$stimulus <- stimulus
    temp$participant <- participant
    temp[1,]$rating <- 50 # For some reason webcode labels first rating as 0, even though slider starts at 50; fix this
    timeZero <- temp[1,]
    timeZero$time <- 0
    temp <- rbind(temp, timeZero)
    prolific <- rbind(prolific, temp)
  }
}

prolific$roundedTime <- round(prolific$time, 0) # Initialize roundedTime column
prolific$originalTime <- 'yes'
prolific$veryFinalRating <- 'unknown'

# Because original data only has timepoints for times participants actually moved the slider,
# need to add in timepoints for moments when they aren't moving. This is VERY SLOW but IMPORTANT.
# Done according to value specified in variable roundTo
# Function 'addTimepoints' from functions.r
prolific <- addTimepoints(prolific, roundTo)

prolific$timePlusParticipant <- paste(prolific$roundedTime, prolific$participant)
prolific <- prolific[order(prolific$time),] # Order by time for non-wonky plots

######################
## DEBRIEFING COMMENTS
######################
debriefingDir <- paste(resultsDir, 'debriefing/', sep='')
filenames <- list.files(debriefingDir, full.names=FALSE)
fileCount <- 0
for (file in filenames) {
  fileCount <- fileCount + 1
  temp <- read.delim(paste(debriefingDir, file, sep=''), stringsAsFactors=FALSE)
  temp$participant <- file # Filenames are participant IDs
  if (fileCount == 1) {debriefing <- temp}
  else {debriefing <- rbind(debriefing, temp)}
}


######################
## DEMOGRAPHICS
######################
demDir <- paste(resultsDir, 'demographics/', sep='')
filenames <- list.files(demDir, full.names=FALSE)
fileCount <- 0
for (file in filenames) {
  fileCount <- fileCount + 1
  temp <- read.delim(paste(demDir, file, sep=''), stringsAsFactors=FALSE)
  temp$participant <- file
  if (fileCount == 1) {demographics <- temp}
  else {demographics <- rbind(demographics, temp)}
}

######################
## POSTHOC RATINGS
######################
posthocDir <- paste(resultsDir, 'posthoc/', sep='')
filenames <- list.files(posthocDir, full.names=FALSE)
fileCount <- 0
for (file in filenames) {
  fileCount <- fileCount + 1
  temp <- read.delim(paste(posthocDir, file, sep=''), stringsAsFactors=FALSE)
  temp$participant <- file
  if (fileCount == 1) {posthoc <- temp}
  else {posthoc <- rbind(posthoc, temp)}
}

# Dataframe with one row for each participant
participant_by_stimulus <- ddply(prolific, ~ participant, summarize, stimulus=unique(stimulus))

### Add posthoc data to main results dataframe
prolific <- merge(prolific, posthoc)
# Removes any participants who are missing posthoc data

### And add demographic info
prolific <- merge(prolific, demographics, all.X=TRUE)

### And add stimulus info to all dataframes
posthoc <- merge(posthoc, participant_by_stimulus)
debriefing <- merge(debriefing, participant_by_stimulus)
demographics <- merge(demographics, participant_by_stimulus)
demographics <- merge(demographics, posthoc)

# Print number of participants per stimulus
for (stim in unique(prolific$stimulus)) {
  temp <- prolific[prolific$stimulus==stim,]
  print(stim)
  print(length(unique(temp$participant)))
}

prolific$veryFinalRating <- as.numeric(prolific$veryFinalRating)

## Remove duplicates of roundedTime to keep from weighting people with multiple ratings per second more heavily than other people
prolific <- prolific[duplicated(prolific$timePlusParticipant, fromLast=TRUE)==FALSE,]

### Save to csv file to be able to load quickly in the future
write.csv(prolific, resFile)