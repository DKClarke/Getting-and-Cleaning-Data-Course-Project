# Getting-and-Cleaning-Data-Course-Project
Repo for the Getting and Cleaning Data course in the Data Science specialisation in Coursera

# run_analysis.R
This script outputs a data frame that contains the mean values for each subject, activity, feature, feature spatial dimension, and summary type (mean or standard deviation) from the UCI HAR Dataset

The script contains the function run_analysis, which when assigned to a variable (i.e. output <- run_analysis()) will store the output data frame in output. It requires the tidyr package, so as long as this is installed in your version of R, it will load it if not already loaded.

The script first loads in the activity labels and features files in the base UCI HAR Dataset folder, then loops through each measure (mean, then standard deviation) and within each measure, loops through each folder (test and then train). Within each measure/folder combination, it loads the subject IDs, activity IDs, and the columns from the data which correspond to the desired measure. It replaces the numeric activity IDs with decriptions from the activity labels file. It also uses the gather() function to convert the multiple columns loaded into a single variable called "feature", with the values for each feature in a variable called "value". Additionally, it creates a "spatial dimension" variable that codes whether the features have a spatial dimension (i.e. -X, -Y, or -Z, or not), as well as adding variables "summary type" which indicate which measure (i.e. mean or standard deviation) the current data frame represents, and "data type" which indicates which folder the data is from (i.e. train or test).

Once looped through the two folders, the data frames for each folder are merged. Similarly, once looped through both measures, the data frames for each measure are merged. This final merged data frame is then split using the split() function along subject, activity, feature, feature spatial dimension, and summary time before the mean for each unique combination of these is calculated using sapply(). Incomplete cases are then removed from the output of this, and after rearranging the columns for readability, this is what is returned by the function.

# Code book

The final tidy dataset contains the variables:

1. subject.ID: unique subject identifier, numeric 

values: 30 distinct numeric values

2. activity.label: description of the activity being undertaken by the subject, factor

values: walking, walking upstairs, walking downstairs, sitting, standing, laying

3. feature: the feature being measured, description taken from the features.txt file, factor

values: 17 distinct levels

4. feature.spatial.dimension: the spatial dimension of the feature being measured, factor

values: X, Y, Z, no dimension

5. data.summary.type: whether the value represents the mean or standard deviation of the feature, factor

values: mean, standard deviation

6. mean.of.feature.summary: the mean value for the data indicated by the other variables, numeric
