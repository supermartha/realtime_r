source('plotting_functions.r') # Load plotting functions
# Requires 'ddply' and 'reshape2' packages

# Load saved data, generated by prep_web_data.r
results <- read.csv('sample_prolific_data.csv')
# To use your own data, run prep_web_data.r and then load your data (by default, saved as prolific_data.csv)

# Check the stimuli names
unique(results$stimulus)

############################
####### BASIC PLOTS ########
############################
# Compare all ING and all IN stimuli
plotStimulus(stimulus="robert all ING", dataframe=results)
plotStimulus(stimulus="robert all IN", dataframe=results, add=TRUE)
legend("topright", c('all ING', 'all IN'), lwd=2, col=c('coral3', 'cyan4'))

# Just all ING stimulus, overlay individual results
plotStimulus(stimulus="robert all ING", dataframe=results, showIndividuals=TRUE)

# Remove confidence interval and add times of (ING) tokens
plotStimulus(stimulus="robert all ING", dataframe=results, alpha=0, showPoints=TRUE, pointTimes = c(5.9, 10.2, 11.0, 20.4, 31.3, 46.5, 49.1, 53.5, 54.5, 55.6, 70.0))

# Plot posthoc ratings
makePosthocPlot(dataframe=results)
# (This plot isn't super pretty...if you want to edit this function, find it in plotting_functions.r)

########################################
####### SLOPES: PREPARATION #############
#########################################
# If you are using a matched guise paradigm, how can you measure the effects of your 
# variable of interest? In our analysis, we used "slopes" of how far up/down participants
# moved the slider in the 5 seconds after each token of (ING), and then asked whether
# the average slope was higher for ING than IN guises. This method controls for overall
# effects of content (e.g. since our speaker was Southern, everyone tended to move the
# slider towards "less educated" at the beginning of stimulus, regardless of ING guise).

### Required input: a dataframe with columns named "token_num", "stimulus", "time", (for the time of each token), and "guise", where each column contains the guise of that stimulus at that timepoint. The total number of rows in this dataframe should equal the number of stimuli times the number of tokens in each stimulus (e.g. here 6 stimuli x 11 tokens = 66 rows).
### E.g. for our sample (ING) data:
token_num <- rep(seq(1,11), 6) # We have 11 tokens of (ING) per stimulus and 6 stimuli
stimulus <- c(rep("robert all ING", 11), rep("robert all IN", 11), rep("robert all ING-IN-ING-IN", 11), rep("robert all IN-ING-IN-ING", 11), rep("robert reverse ING-IN-ING-IN", 11), rep("robert reverse IN-ING-IN-ING", 11)) # Make sure stimuli names are exactly the same as in your results dataframe
# Our first four stimuli all have their (ING) tokens at the same time, the last two have them at different times because they're in different orders
ing_forward_times <- c(5.9, 10.2, 11.0, 20.4, 31.3, 46.5, 49.1, 53.5, 54.5, 55.6, 70.0)
ing_reverse_times <- c(1.1, 2.1, 3.2, 17.6, 29.6, 32.2, 37.5, 48.4, 60.6, 65.0, 65.7)
token_time <- c(rep(ing_forward_times, 4), rep(ing_reverse_times, 2))
# Now for each stimulus, specify what guise each token is
guise.robert_all_ING <- rep('ING', 11)
guise.robert_all_IN <- rep('IN', 11)
guise.robert_all_ING_IN_ING_IN <- c('ING', 'ING', 'ING', 'IN', 'IN', 'ING', 'ING', 'IN', 'IN', 'IN', 'IN')
guise.robert_all_IN_ING_IN_ING <- c('IN', 'IN', 'IN', 'ING', 'ING', 'IN', 'IN', 'ING', 'ING', 'ING', 'ING')
guise.robert_reverse_ING_IN_ING_IN <- c('ING', 'ING', 'ING', 'IN', 'IN', 'IN', 'ING', 'ING', 'IN', 'IN', 'IN')
guise.robert_reverse_IN_ING_IN_ING <- c('IN', 'IN', 'IN', 'ING', 'ING', 'ING', 'IN', 'IN', 'ING', 'ING', 'ING')
# Combine guises so they can be a single column. Make sure these match the order of the "stimulus" vector
guise <- c(guise.robert_all_ING, guise.robert_all_IN, guise.robert_all_ING_IN_ING_IN, guise.robert_all_IN_ING_IN_ING, guise.robert_reverse_ING_IN_ING_IN, guise.robert_reverse_IN_ING_IN_ING)
# Combine into dataframe
ing_guises <- data.frame(token_num=token_num, stimulus=stimulus, time=token_time, guise=guise)
# Check our work to make sure the dataframe looks right and guises are specified correctly
ing_guises

source('slope_functions.r')

ingSlopes <- calculateSlopes(dataframe=results, token_guise_dataframe=ing_guises, roundingFactor=2)
### dataframe = the dataframe
#### n = how many seconds after token to calculate slopes for, if calculateEachNSeconds==TRUE
### calculate each n seconds: if TRUE, calculate slopes for N seconds after each token; if FALSE, calculate token-to-token slopes
# roundingFactor = 1 for rounded to nearest second, 2 for rounded to nearest .5 second, 4 for nearest .25 second, etc. (This MUST match the data, or else you will get an error. NB this is different than roundTo specified in prep_web_data.r)

########################################
####### SLOPES: PLOTS #############
#########################################
# Use makeSlopesPlot() from plotting_functions.r to plot, for each token, the difference between two stimuli
# For example, here we are comparing the mean slope after each token for the all ING stimulus minus the all IN stimulus
# If the ING guise makes the talker sound more educated, we should expect this difference to be positive for most tokens
# (Except that this sample data was randomly generated, so actually we don't expect to find an effect at all)
makeSlopesPlot(dataframe=ingSlopes, stimulusA="robert all ING", stimulusB="robert all IN", ylim=c(-30,30), legend_text='ING - IN (expect positive difference)', token_text=c('token 1', 'token 2', 'token 3', 'token 4', 'token 5', 'token 6', 'token 7', 'token 8', 'token 9', 'token 10', 'token 11'))
# E.g. in the five seconds after the first (ING) token (first green bar), people hearing the ING guise on average move the slider about 7 points towards "more educated" than people hearing the IN guise

# Compare alternating stimuli
# Since these stimuli are alternating, and we are subtracting the first stimulus minus the second for each token, for some tokens we are taking ING - IN (expect a positive difference) and for others we are taking IN- ING (expect a negative differnece)
alt_colors <- c('darkseagreen4', 'darkseagreen4' ,'darkseagreen4', 'deeppink1', 'deeppink1', 'darkseagreen4', 'darkseagreen4', 'deeppink1', 'deeppink1', 'deeppink1', 'deeppink1')
makeSlopesPlot(dataframe=ingSlopes, stimulusA="robert all ING-IN-ING-IN", stimulusB="robert all IN-ING-IN-ING", ylim=c(-30,30), color_order=alt_colors, legend_text=c('ING - IN (expect positive difference)', 'IN - ING (expect negative difference'), legend_colors =c('darkseagreen4', 'deeppink1'))
# So if this were real data, we wouldn't be getting the expected effect of (ING) for most of the tokens!

# Comparing alternating stimuli, for the stimuli with tokens in the reverse order
alt_colors_reverse <- c('darkseagreen4', 'darkseagreen4' ,'darkseagreen4', 'deeppink1', 'deeppink1', 'deeppink1', 'darkseagreen4', 'darkseagreen4', 'darkseagreen4', 'deeppink1', 'deeppink1')
makeSlopesPlot(dataframe=ingSlopes, stimulusA="robert reverse ING-IN-ING-IN", stimulusB="robert reverse IN-ING-IN-ING", ylim=c(-30,30), color_order=alt_colors_reverse, legend_text=c('ING - IN (expect positive difference)', 'IN - ING (expect negative difference'), legend_colors =c('darkseagreen4', 'deeppink1'))
# Again, if this were real data, not clearly getting the expected effect of (ING)!


### The above plots showed the differences for each token, but we can also average across tokens to see if on average we get the expected effect
source('plotting_functions.r')
makeSlopesSummaryPlot(ingSlopes, main="My Plot")
# (NB this function assumes you only have two guises, plot might look wonky otherwise?)
# If these were real data, this would be the opposite of the expected direction -- we would expect ING to be higher than IN.

# Can also group stimuli by various features, e.g. whether the stimulus had only one variant (all ING or all IN) or both variants. (To test whether the effect of (ING) is bigger if you hear the speaker use both variants, e.g. maybe you are more likely to notice it then.)
# First, add this grouping factor as a column to the dataframe
ingSlopes$stimulusType <- 'alternating'
ingSlopes[ingSlopes$stimulus %in% c('robert all ING', 'robert all IN'),]$stimulusType <- 'single variant'
# Then plot using this column as a grouping factor
source('plotting_functions.r')
makeSlopesSummaryPlot(ingSlopes, groupingFactor="stimulusType")

# Or can group responses by a demographic factor, e.g. education level
# (in the sample data there are only three types of education)
makeSlopesSummaryPlot(ingSlopes, groupingFactor="degree", main="Education Level of Participant")
# (To make these labels fancier, you could create a new column with fancy names and then group by that, e.g.:)
ingSlopes$fancyDegree <- 'Masters Degree'
ingSlopes[ingSlopes$degree=='some_college',]$fancyDegree <- 'Some College'
ingSlopes[ingSlopes$degree=='some_hs',]$fancyDegree <- 'Some High School'
makeSlopesSummaryPlot(ingSlopes, groupingFactor="fancyDegree", main="Education Level of Participant")
makeSlopesSummaryPlot(ingSlopes, groupingFactor="fancyDegree", main="Education Level of Participant", groupingLabelCex=.8) # (label text is too big on my screen, so make it smaller)

########################################
####### SLOPES: MODELS #################
#########################################
# You can use your slopes dataframe to see if there's a significant difference between guises in how far up/down people move the slider in the 5 seconds after each token
# (Here it probably shouldn't be significant b/c the data were randomly generated,
# but if we really ran this experiment we would expect to find ING has a significantly
# higher slope than IN)
library(lmerTest)

# For models, it's important to include a random intercept for audio segment b/c to account
# for overall reactions to the content of each segment, other than the variable you are 
# interested in (e.g. in actual study, everyone moved the slider down during the first
# segment b/c the speaker had a Southern accent -- we want to control for these content 
# effects). The way you specify this audio segment variable will depend on the exact design
# of your experiment.
# Four of our stimuli were the same except for (ING) guise:
ingSlopes$order <- 'forward'
# The other two had the content in the reverse order:
ingSlopes[ingSlopes$stimulus %in% c('robert reverse ING-IN-ING-IN', 'robert reverse IN-ING-IN-ING'),]$order <- 'reverse'
# Paste order with tokenNum to have a unique variable for each audio segment:
ingSlopes$audioSegment <- paste(ingSlopes$order, ingSlopes$tokenNum)

# Basic model including a random intercept for participant and audio segment
guiseModel <- lmer(slope ~ guise + (1|participant) + (1|audioSegment), dat=ingSlopes)
summary(guiseModel)
# No effect, which is reassuring because we made this data up

# Test demographic factors, e.g. do more educated participants react more strongly to (ING)
# because they are judgier?
guiseEducationModel <- lmer(slope ~ guise*degree + (1|participant) + (1|audioSegment), dat=ingSlopes)
summary(guiseEducationModel)


#####################################################################
####### MODELS OF POSTHOC (AFTER-THE-FACT) RATINGS #################
###################################################################
### Summarize by participant, to have a dataframe with just one posthoc rating per participant
by_participant <- ddply(results, ~ participant + stimulus, summarize, posthoc=unique(posthoc), minRating=min(rating), maxRating=max(rating), finalRating=tail(rating, 1), meanRating=mean(rating), medianRating=median(rating), veryFinalRating=unique(veryFinalRating))
head(by_participant)

# Investigate whether there were significant differences in posthoc ratings 
posthocModel <- lm(posthoc ~ stimulus, dat=by_participant)
summary(posthocModel)

### Analyze how well in-the-moment ratings (e.g. mean in-the-moment rating) predict posthoc ratings.s
### In our paper, we only looked at measurements starting 5 seconds after a participant first started moving the slider, to remove effects of the slider starting at 50 points.
excludeFirstMovements <- function(dataframe, secsToExclude=5) {
  ## Function that takes a dataframe. For each participant:
  ## figure out when they first started moving the slider
  ## add secsToExclude to that
  ## drop rows of dataframe where the current time is 
  ## less than when they first started moving + secsToExclude
  participantCount <- 0
  for (participant in unique(dataframe$participant)) {
    temp <- dataframe[dataframe$participant==participant,]
    temp <- temp[order(temp$roundedTime),]
    if (length(unique(temp$rating)) == 1) {
      # People who never moved the slider at all
      temp$minTime <- NA
      temp$toInclude <- FALSE}
    else {
      minTimeIndex <- min(which(temp$rating != 50))
      minTime <- temp[minTimeIndex,]$roundedTime
      temp$minTime <- minTime + secsToExclude
      temp$toInclude <- temp$roundedTime > temp$minTime
    }
    if (participantCount==0) {
      # Initialize new dataframe
      resData <- temp
    }
    else {resData <- rbind(resData, temp)}
    participantCount <- participantCount + 1
  }
  resData <- resData[resData$toInclude==TRUE,]
  return(resData)
}

newResults <- excludeFirstMovements(results)

by_participant_noFirst5 <- ddply(newResults, ~ participant + stimulus, summarize, posthoc=unique(posthoc), minRating=min(rating), maxRating=max(rating), finalRating=tail(rating, 1), meanRating=mean(rating), medianRating=median(rating), veryFinalRating=unique(veryFinalRating))

# Build models with each of these factors
model1 <- lm(posthoc ~ meanRating, dat=by_participant_noFirst5)
model2 <- lm(posthoc ~ minRating, dat=by_participant_noFirst5)
model3 <- lm(posthoc ~ maxRating, dat=by_participant_noFirst5)
model4 <- lm(posthoc ~ medianRating, dat=by_participant_noFirst5)
model5 <- lm(posthoc ~ finalRating, dat=by_participant_noFirst5)
model6 <- lm(posthoc ~ veryFinalRating, dat=by_participant_noFirst5)
# Since these data were randomly generated, there's no relationship
# but in all of our experiments with actual data we found highly 
# significant relationships, esp. between meanRating and posthoc!
summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)
summary(model6)
