Code Book for Getting and Cleaning Data Coursera Project
The Data set that this code book describes is located in tidyData.txt.

The README.md file of this repository contains the more information on this data set.

The tidyData.txt is a space supperated file containing data. Thr first line contains the Variables and the other lines contains the Values.

Variables
Each row contains 79 averaged signal measurements for each subject and activity. There are total 81 variables as shown below:

subjectID

The Id of the subject who performend the activity. Its ranges is 1-30.

activity

The activity that the subject performend.

The Vales of the activity are:

- `WALKING` : The subject was walking
- `WALKING-UPSTAIRS` : The subject was walking upstairs
- `WALKING-DOWNSTAIRS` : The subject was walking downstairs
- `SITTING` : The subject was sitting
- `STANDING` : The subject was standing
- `LAYING` : The subject was lying
Average of Measurements
Average time-domain body acceleration in X, Y and Z directions:

timeDomainBodyAccelerometerMeanX
timeDomainBodyAccelerometerMeanY
timeDomainBodyAccelerometerMeanZ
Standard Deviation time-domain body acceleration in X, Y and Z directions:

timeDomainBodyAccelerometerStandardDeviationX
timeDomainBodyAccelerometerStandardDeviationY
timeDomainBodyAccelerometerStandardDeviationZ
Average time-domain gravity acceleration in X, Y and Z directions:

timeDomainGravityAccelerometerMeanX
timeDomainGravityAccelerometerMeanY
timeDomainGravityAccelerometerMeanZ
Standard Deviation of time-domain gravity acceleration in X, Y and Z directions:

timeDomainGravityAccelerometerStandardDeviationX
timeDomainGravityAccelerometerStandardDeviationY
timeDomainGravityAccelerometerStandardDeviationZ
Average time-domain body acceleration jerk in X, Y and Z directions:

timeDomainBodyAccelerometerJerkMeanX
timeDomainBodyAccelerometerJerkMeanY
timeDomainBodyAccelerometerJerkMeanZ
Standard Deviation of time-domain body acceleration jerk in X, Y and Z directions:

timeDomainBodyAccelerometerJerkStandardDeviationX
timeDomainBodyAccelerometerJerkStandardDeviationY
timeDomainBodyAccelerometerJerkStandardDeviationZ
Average time-domain body angular velocity in X, Y and Z directions:

timeDomainBodyGyroscopeMeanX
timeDomainBodyGyroscopeMeanY
timeDomainBodyGyroscopeMeanZ
Standard Deviation of time-domain body angular velocity in X, Y and Z directions:

timeDomainBodyGyroscopeStandardDeviationX
timeDomainBodyGyroscopeStandardDeviationY
timeDomainBodyGyroscopeStandardDeviationZ
Average time-domain body angular velocity jerk in X, Y and Z directions:

timeDomainBodyGyroscopeJerkMeanX
timeDomainBodyGyroscopeJerkMeanY
timeDomainBodyGyroscopeJerkMeanZ
Standard Deviation of time-domain body angular velocity jerk in X, Y and Z directions:

timeDomainBodyGyroscopeJerkStandardDeviationX
timeDomainBodyGyroscopeJerkStandardDeviationY
timeDomainBodyGyroscopeJerkStandardDeviationZ
Average time-domain body acceleration magnitude(derived using Euclidean norm) in X, Y and Z directions:

timeDomainBodyAccelerometerMagnitudeMean
Standard Deviation of time-domain body acceleration magnitude(derived using Euclidean norm) in X, Y and Z directions:

timeDomainBodyAccelerometerMagnitudeStandardDeviation
Average time-domain body gravity acceleration magnitude in X, Y and Z directions:

timeDomainGravityAccelerometerMagnitudeMean
StandardDeviation of time-domain body gravity acceleration magnitude in X, Y and Z directions:

timeDomainGravityAccelerometerMagnitudeStandardDeviation
Average time-domain body acceleration jerk magnitude in X, Y and Z directions:

timeDomainBodyAccelerometerJerkMagnitudeMean
Standard Deviation of time-domain body acceleration jerk magnitude in X, Y and Z directions:

timeDomainBodyAccelerometerJerkMagnitudeStandardDeviation
Average time-domain body angular velocity magnitude in X, Y and Z directions:

timeDomainBodyGyroscopeMagnitudeMean
Standard Deviation of time-domain body angular velocity magnitude in X, Y and Z directions:

timeDomainBodyGyroscopeMagnitudeStandardDeviation
Average time-domain body angular velocity jerk magnitude in X, Y and Z directions:

timeDomainBodyGyroscopeJerkMagnitudeMean
Standard Deviation of time-domain body angular velocity jerk magnitude in X, Y and Z directions:

timeDomainBodyGyroscopeJerkMagnitudeStandardDeviation
Average frequency-domain body acceleration in X, Y and Z directions:

frequencyDomainBodyAccelerometerMeanX
frequencyDomainBodyAccelerometerMeanY
frequencyDomainBodyAccelerometerMeanZ
Standard Deviation frequency-domain body acceleration in X, Y and Z directions:

frequencyDomainBodyAccelerometerStandardDeviationX
frequencyDomainBodyAccelerometerStandardDeviationY
frequencyDomainBodyAccelerometerStandardDeviationZ
Weighted average of the frequency components of the frequency-domain body acceleration in X, Y and Z directions:

frequencyDomainBodyAccelerometerMeanFrequencyX
frequencyDomainBodyAccelerometerMeanFrequencyY
frequencyDomainBodyAccelerometerMeanFrequencyZ
Average frequency-domain body acceleration jerk in X, Y and Z directions:

frequencyDomainBodyAccelerometerJerkMeanX
frequencyDomainBodyAccelerometerJerkMeanY
frequencyDomainBodyAccelerometerJerkMeanZ
Standard Deviation of frequency-domain body acceleration jerk in X, Y and Z directions:

frequencyDomainBodyAccelerometerJerkStandardDeviationX
frequencyDomainBodyAccelerometerJerkStandardDeviationY
frequencyDomainBodyAccelerometerJerkStandardDeviationZ
Weighted average of frequency components of the frequency-domain body acceleration jerk in X, Y and Z directions:

frequencyDomainBodyAccelerometerJerkMeanFrequencyX
frequencyDomainBodyAccelerometerJerkMeanFrequencyY
frequencyDomainBodyAccelerometerJerkMeanFrequencyZ
Average frequency-domain body angular velocity in X, Y and Z directions:

frequencyDomainBodyGyroscopeMeanX
frequencyDomainBodyGyroscopeMeanY
frequencyDomainBodyGyroscopeMeanZ
Standard Deviation of frequency-domain body angular velocity in X, Y and Z directions:

frequencyDomainBodyGyroscopeStandardDeviationX
frequencyDomainBodyGyroscopeStandardDeviationY
frequencyDomainBodyGyroscopeStandardDeviationZ
Weighted average of the frequency components of the frequency-domain body angular velocity in X, Y and Z directions:

frequencyDomainBodyGyroscopeMeanFrequencyX
frequencyDomainBodyGyroscopeMeanFrequencyY
frequencyDomainBodyGyroscopeMeanFrequencyZ
Average frequency-domain body acceleration magnitude of body acceleration:

frequencyDomainBodyAccelerometerMagnitudeMean
Standard Deviation of frequency-domain body acceleration magnitude of body acceleration:

frequencyDomainBodyAccelerometerMagnitudeStandardDeviation
Weighted average of the frequency components of the frequency-domain body acceleration magnitude:

frequencyDomainBodyAccelerometerMagnitudeMeanFrequency
Average frequency-domain body acceleration jerk magnitude in X, Y and Z directions:

frequencyDomainBodyAccelerometerJerkMagnitudeMean
Standard Deviation of frequency-domain body acceleration jerk magnitude in X, Y and Z directions:

frequencyDomainBodyAccelerometerJerkMagnitudeStandardDeviation
Weighted average of the frequency components of frequency-domain body acceleration jerk magnitude in X, Y and Z directions:

frequencyDomainBodyAccelerometerJerkMagnitudeMeanFrequency
Average frequency-domain body angular velocity magnitude in X, Y and Z directions:

frequencyDomainBodyGyroscopeMagnitudeMean
Standard Deviation frequency-domain body angular velocity magnitude in X, Y and Z directions:

frequencyDomainBodyGyroscopeMagnitudeStandardDeviation
Weighted average of the frequency components of frequency-domain body angular velocity magnitude in X, Y and Z directions:

frequencyDomainBodyGyroscopeMagnitudeMeanFrequency
Average frequency-domain body angular velocity jerk magnitude in X, Y and Z directions:

frequencyDomainBodyGyroscopeJerkMagnitudeMean
Standard Deviation frequency-domain body angular velocity jerk magnitude in X, Y and Z directions:

frequencyDomainBodyGyroscopeJerkMagnitudeStandardDeviation
Weighted average of the frequency components of frequency-domain body angular velocity jerk magnitude in X, Y and Z directions:

frequencyDomainBodyGyroscopeJerkMagnitudeMeanFrequency
Transformations
The source file containing the data is:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

The transformations made to it are as listed below:

The training and the test sets were merged to create one data set.

The measurements of the mean and standard deviation for each measurement were extracted.

The activities in the data set were named with descriptive activity names.

The data set was labeled with descriptive variable names.

From the data set in step 4, a second, independent tidy data set with the average of each variable for each activity and each subject was created.
