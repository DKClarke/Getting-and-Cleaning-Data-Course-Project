# Script works with the UCI HAR Dataset. Takes the mean and standard deviation
# values of the features in the dataset from both the test and training datasets
# and merges them together. The merged data is then averaged across subject
# number, activity, feature, the spatial dimension of the feature, and finally 
# whether the data represents the mean or standard deviation, to output the 
# final data frame.

# Script assumes that the UCI HAR Dataset folder is in your working directory
run_analysis <- function() {

# Requires the tidyr package, which is loaded if necessary
require("tidyr");

# Create a variable to store a string into the UCI HAR Dataset folder
dataFolder = paste(getwd(), "/UCI HAR Dataset/", sep = "");

# Here we read the activity_labels.txt file so we can attach meaningful 
# descriptions to the different activity labels in the data
activityLabels = read.table(paste(dataFolder, "activity_labels.txt", sep = ""), 
                            stringsAsFactors = FALSE);

# Edit the strings in activityLabels to make them more readable - replaces
# underscores with a space and set to lowercase
activityLabels = gsub("_", " ",tolower(as.character(activityLabels[,2])));
                                       
# Here we read in the features.txt file so that we can extract which rows of 
# data correspond to mean and standard deviation measurements
features = read.table(paste(dataFolder, "features.txt", sep = ""), 
                      stringsAsFactors = FALSE);

# Here we create a vector that stores the 2 strings we're going to search for in
# our features data frame to get out the features that contain mean and standard 
# deviation data

# Here we create a list that stores the two summary measures we're looking for,
# mean and standard deviation, as well as the strings we need to search for 
# to find each one in the features data
measuresToGet = list("measures" = c("mean", "standard deviation"), 
                     "strings to find" = c("-mean\\()", "-std\\()"));

# We create a list to store the row numbers associated with the two measures, 
# as well as the names of the features themselves

# This list stores the rows in the features dataset that correspond to a given
# measureToGet$measures value, as well as the labels for those rows taken from
# the features data
measureFeatures = list("rows" = c(), "labels" = c());

# We create a vector of the folders in the UCI HAR Dataset that we want to merge
# data together from - test and train

# Here we create a vector with the names of the two folders we're going to get 
# our data from, the test and train folders
foldersToCheck = c("test", "train");

# This list stores information about the three datasets from each folder that 
# we're going to access, where for each table we assign a vector that contains
# the string we're going to search for to find that table, then a boolean
# describing whether we want to use that table as a factor, and finally, if
# the boolean is true, a list that contains the arguments we want to pass to
# the factor() function to correctly factorise the data - these are created
# as lists so we can use them in the do.call() function, and also are empty
# at the moment as we need to load the data first to assign the correct values
# to these
tablesInfo = list("subject IDs" = c("subject_", TRUE, list("levels" = c())), 
                  "activity label" = c("y_", TRUE, list("labels" = c())),
                  "data" = c("X_", FALSE));

# This list will store the data for each of the measuresToGet values at the end
# of each loop
storageList = list();

# Loop through the tow measuresToGet values (mean and std)
for(j in seq_along(measuresToGet)) {
    
    # In the measuresFeatures$columns section store the columns in features that 
    # match the string from measuresToGet$strings to find called in the current
    # loop
    measureFeatures$columns = grep(measuresToGet$`strings to find`[j], 
                                   features[,2]);
    
    # In the measuresFeatures$labels section store the actual names in features 
    # associated with the columns we just retrieved, but remove the name of the
    # measure we're looking for from them
    measureFeatures$labels = features[measureFeatures$columns,2];
    measureFeatures$labels = gsub(measuresToGet$`strings to find`[j], "", 
                                  measureFeatures$labels);
    
    # Here we create a list to store the data from the folders we're looping
    # through, the test and train folders
    foldersData = list();
  
    # Loop through the values in foldersToCheck
    for(i in seq_along(foldersToCheck)) {
        
        # Retrieve a list of the files within the folder specified by
        # foldersToCheck[i]
        fileList = list.files(paste(dataFolder, foldersToCheck[i], sep = ""), 
                              pattern = ".txt", full.names = TRUE);
        
        # Create a list to store the various data frames we're going to load in 
        # using tablesToOpen, i.e., subject data, activity label data, and the
        # data itself
        DFList = list();
        
        # Loop through the 3 tables we need to load in
        for (k in seq_along(tablesInfo)) {
            
            # First load it into a tempData variable, where each iteration of k
            # loads in the file whose name matches the name of the kth table
            # specified in tablesInfo i.e. for k=1, we load the table which 
            # matches "subject_"
            tempData = read.table(fileList[grep(tablesInfo[[k]][1], 
                                                   fileList)]);
            # If the table we're loading in is the data table, we only load
            # in the columns that are associated with the measure we're looping
            # through
            if(names(tablesInfo)[k] == "data") {
                DFList[[k]] = tempData[,measureFeatures$columns];
            
            # Otherwise, we just store the data in DFList    
            } else {
                DFList[[k]] = tempData;
            }
        }
        
        # Here we set the names of the data frames in DFList to the names of the
        # tables from tablesInfo
        names(DFList) = names(tablesInfo);
        
        # Now we fill the arguments we're going to use to factorise the subjects
        # and activity label tables, where for subjects we're passing in the 
        # levels of the factor as the unique values of the subject IDs, and for
        # activity labels we're passing in the labels of the factor as the names
        # specified in activityLabels - except we make sure we only pass in the
        # labels that are present in our data by getting out the labels that
        # match the unique activity label numbers in DFList (we sort this so 
        # the labels retrieved are in the right order for the levels present)
        tablesInfo$`subject IDs`[[3]] = unique(DFList$`subject IDs`[,1]);
        tablesInfo$`activity label`[[3]] = 
            activityLabels[sort(unique(DFList$`activity label`[,1]))];

        # Here we loop through the 3 tables again
        for (k in seq_along(tablesInfo)) {
            
            # If the table is going to be turned into a factor, we replace the
            # table with it once it has been turned into a factor, passing the 
            # arguments for that factor function from tablesInfo
            if(tablesInfo[[k]][2] == TRUE) { 
                DFList[[k]] = do.call(factor, c(list(x = DFList[[k]][,1]), 
                tablesInfo[[k]][3]));
            }
        }
        
        # Here we create a new section in DFList called combined that binds
        # together the subject IDs, activity labels, and data in a single 
        # data frame
        DFList$combined = cbind(DFList$`subject IDs`, DFList$`activity label`, 
                            DFList$data);
        
        # We set the names of the data in this combined data frame to be the 
        # names of our first 2 tables - subjects and activity labels - and then
        # label all the columns of data with the labels from measuresFeatures
        names(DFList$combined) = c(names(DFList[1:2]), measureFeatures$labels);
        
        # We then store a new data frame in DFList called gathered, which 
        # gathers all the columns in combined that come after activity label
        # (i.e. all the data columns) into a single column called feature, and
        # we store the values for each of those columns in value
        DFList$gathered = gather(DFList$combined, "feature", "value", 
                                 (grep("activity label", 
                                      names(DFList$combined))+1):
                                     length(names(DFList$combined)));
        
        # Here we create a list to store identifiers of whether the features in
        # our gathered data have spatial dimensions i.e. are in the X, Y, or Z 
        # direction. If they are in the X direction (indicated by the presence
        # of "-X" in the feature name), we create an integer vector of 1's where
        # this is true, likewise we create integer vectors where 2, and 3s are
        # present where "-Y" and "-Z" are present. Where 0's are present, the
        # feature has no spatial vector
        spatialDimensionInds = list("XInd" = 
                            as.integer(grepl("-X", DFList$gathered$feature)),
                                    "YInd" = 
                            as.integer(grepl("-Y", DFList$gathered$feature))*2,
                                    "ZInd" = 
                            as.integer(grepl("-Z", DFList$gathered$feature))*3);

        # Here we create a numeric vector that will store the combined numbers
        # from all the spatialDimensionInds sections
        dimensionMatrix = numeric(length(spatialDimensionInds$XInd));
        
        # We loop through the spatial dimension indicators, and for each one, if
        # at a given index it has a non zero value, we assign that value to the
        # same index in dimensionMatrix
        for(k in seq_along(spatialDimensionInds)) {
            dimensionMatrix[spatialDimensionInds[[k]]!=0] = 
                spatialDimensionInds[[k]][spatialDimensionInds[[k]]!=0];
        }
    
        # dimensionMatrix now contains 0,1,2, and 3, to represent no spatial
        # dimension, and X, Y, or Z dimension, We turn this into a factor
        # with those 4 levels
        dimensionMatrix = factor(dimensionMatrix, labels = c("No Dimension", 
                                                             "X", "Y", "Z"))
        
        # We then add dimension as a column to our gathered data frame
        DFList$gathered$dimension = dimensionMatrix;
        
        # We then replace the instances of "-X", "-Y", and "-Z" in our feature
        # column with nothing as that data is now represented in the dimension
        # column
        DFList$gathered$feature = gsub("-X|-Y|-Z", "", DFList$gathered$feature);
        
        # We then turn our feature column into a factor
        DFList$gathered$feature = factor(DFList$gathered$feature);
        
        # Here we add a summary type column to the data that denotes which 
        # measure from measuresToGet the data represents, as well as adding a
        # data type column that denote which folder the data came from
        DFList$gathered$'summary type' = rep(measuresToGet$measures[j], 
                                             nrow(DFList$gathered));
        DFList$gathered$'data type' = rep(foldersToCheck[i], 
                                          nrow(DFList$gathered));        

        # At the end of the folder loop, we store the gathered data in 
        # foldersData
        foldersData[[i]] = DFList$gathered;

    }
    
    # Once we've looped through both folders for a given measure, we store the
    # a merged form of the data from both folders in storageList
    storageList[[j]] = merge(foldersData[[1]], foldersData[[2]], all = TRUE);
    
}

# Once we've looped through both measures, we store a merged form of that data
# in a data frame called finalMerged, within which we then factorise the summary
# and data type columns
finalMerged = merge(storageList[[1]], storageList[[2]], all = TRUE);
finalMerged$`summary type` = factor(finalMerged$`summary type`);
finalMerged$`data type` = factor(finalMerged$`data type`);

# Now we split the values of the data between subject, activity, feature, 
# dimension, and summary type so that we can calculate the average value for 
# each subject, at each activity level, for each feature, for each spatial 
# dimension, for each summary type, where we will be averaging across both the
# test and training data
splitData = split(finalMerged$value, 
                  list(finalMerged$`subject IDs`, finalMerged$`activity label`, 
                       finalMerged$feature, finalMerged$dimension, 
                       finalMerged$`summary type`));

# We then apply the mean() function across our split data, generating a list 
# that contains the averages, before we convert it to a data frame
averageData = sapply(splitData, mean, na.rm = TRUE);
averageDF = (as.data.frame(averageData));

# The averageDF data frame now contains a single column with the average values,
# where the name of each row contains all the information we averaged across. We
# store these row names in labels, before then adding these labels as a second
# column to averageDF and then removing its row names
labels = rownames(averageDF);
averageDF$labels = labels;
rownames(averageDF) = c();

# We then separate the column containing the details of what was averaged across
# by separating the second column in averageDF by the "." separator
separatedDF = separate(averageDF, labels, c('subject ID', 'activity label', 
                                            'feature', 
                                            'feature spatial dimension', 
                                            'data summary type'), sep = "\\.")

# To remove NaNs in the dataset, we create a separatedClean data frame with only
# complete cases from separatedDF and then set the first column name 
# appropriately (otherwise it remains averageData)
separatedClean = separatedDF[complete.cases(separatedDF),];
names(separatedClean)[1] = 'mean of feature summary';

# Lastly, we move the column of the mean values to the end of the dataset to
# make it more readable
separatedClean = separatedClean[c(2:6,1)];

}