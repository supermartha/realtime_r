# Python script for generating sample data
# so you can see/test how the R scripts work

# This sample data won't be very realistic --
# responses are completely randomly assigned.
# It's mostly just to test that the code works!

import random
import csv
import os

resDir = "sample_results/"
numParticipantsPerStimulus = 30

# slider
# participantIDs
# posthoc
# demographics
# debriefing

# Check if results directories exist, if not create
if os.path.exists(resDir) == False:
	os.mkdir(resDir)
for subDir in ["participantIDs", "slider", "posthoc", "demographics", "debriefing"]:
	if os.path.exists(resDir + subDir) == False:
		os.mkdir(resDir + subDir)

for n in range(numParticipantsPerStimulus):
	for stimulus in ["robert_all_ING", "robert_all_IN", "robert_all_ING-IN-ING-IN", "robert_all_IN-ING-IN-ING", "robert_reverse_ING-IN-ING-IN", "robert_reverse_IN-ING-IN-ING"]:
		participantID = hex(random.randint(500000000000,1000000000000))[2:]
		
		# participantID file
		with open(resDir + "participantIDs/" + participantID, 'w') as participantFile:
			writer = csv.writer(participantFile, delimiter='\t')
			writer.writerow(['prolificID', 'x']) 
			participantFile.write(hex(random.randint(500000000000,1000000000000))[2:] + '\t') # generate a different random ID to pretend it's the prolific ID
			# (Formatting is funny because actual webcode output is funny, this is to match it)

		# slider file
		with open(resDir + "slider/" + stimulus + '_' + participantID + '.csv', 'w') as sliderFile:
			writer = csv.writer(sliderFile, delimiter='\t')
			writer.writerow(['time', 'rating', 'timestamp', 'x'])
			# Pick a random number of timepoints to respond to
			numTimepoints = random.randint(1,25)
			# Then randomly choose what those timepoints are, and sort in order
			timepoints = random.sample(range(1,int(70.897*1000)), numTimepoints) # multiply by 1000 and then divide to get three decimal places
			timepoints = [float(x)/1000 for x in timepoints]
			timepoints.sort()
			ratings = random.sample(range(100), numTimepoints)
			for count in range(len(timepoints)):
				writer.writerow([timepoints[count], ratings[count], timepoints[count] * 1000 + 1590158167635, ''])

		# posthoc file
		with open(resDir + "posthoc/" + participantID, 'w') as posthocFile:
			writer = csv.writer(posthocFile, delimiter='\t')
			writer.writerow(['posthoc', 'x']) 
			posthocFile.write(str(random.randint(1,10)) + '\t')

		# demographics file
		with open(resDir + "demographics/" + participantID, 'w') as demographicsFile:
			writer = csv.writer(demographicsFile, delimiter='\t')
			writer.writerow(['nativeLanguages', 'hearingDisorder', 'gender', 'gender_text', 'race_white', 'race_black', 'race_latinx', 'race_asian', 'race_american_indian', 'race_pacific_islander', 'race_other', 'race_text', 'birth-year', 'occupation', 'degree', 'experiment_purpose', 'comments', 'x']) 
			demographicsFile.write('English' + '\t' + random.choice(['yes', 'no']) + '\t' + random.choice(['male', 'female', 'other']) + '\t' + '' + '\t' + random.choice(['x', '']) + '\t' + random.choice(['x', '']) + '\t' + random.choice(['x', '']) + '\t' + random.choice(['x', '']) + '\t' + random.choice(['x', '']) + '\t' + random.choice(['x', '']) + '\t' + random.choice(['x', '']) + '\t' + '' + '\t' + str(random.randint(1920,2005)) + '\t' + random.choice(['doctor', 'lawyer', 'priest', 'janitor']) + '\t' + random.choice(['some_hs', 'some_college', 'masters']) + '\t' + random.choice(['accents', 'language']) + '\t' + random.choice(['no comments', 'cool study!']) + '\t')
		
		# debriefing file
		debriefingChoices = ['', 'Great debriefing!', 'I didnt notice ING at all', '', '']
		with open(resDir + "debriefing/" + participantID, 'w') as debriefingFile:
			writer = csv.writer(debriefingFile, delimiter='\t')
			writer.writerow(['comments', 'x']) 
			debriefingFile.write(random.choice(debriefingChoices) + '\t')
