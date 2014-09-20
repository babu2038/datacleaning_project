    data1 <- read.table("train/X_train.txt")

    data2 <-read.table("test/X_test.txt")

    X_com <- rbind(data1,data2)

    data1 <- read.table("train/subject_train.txt")

    data2 <-read.table("test/subject_test.txt")

    S_com <- rbind(data1,data2)

    data1 <- read.table("train/y_train.txt")

    data2 <-read.table("test/y_test.txt")

    Y_com <- rbind(data1,data2)



    # extracts only the measurement of mean and standard deviation

    feature <- read.table("features.txt")

        indices_of_good_features <- grep("-mean\\(\\)|-std\\(\\)", feature[, 2])
	X_com <- X_com[, indices_of_good_features]
	names(X_com) <- feature[indices_of_good_features, 2]
	names(X_com) <- gsub("\\(|\\)", "", names(X_com))
	names(X_com) <- tolower(names(X_com))

	#  descriptive activity names to name the activities in the data set
	
	activities <- read.table("activity_labels.txt")
	activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
	Y_com[,1] = activities[Y_com[,1], 2]

	names(Y_com) <- "activity"

	#Appropriately labels the data set with descriptive activity names.

	names(S_com) <- "subject"
	cleaned <- cbind(S_com, Y_com,X_com)
	write.table(cleaned, "merged_clean_data.txt")
	# Creates a 2nd, independent tidy data set with the average  

	uniqueSubjects = unique(S_com)[,1]
	numSubjects = length(unique(S_com)[,1])
	numActivities = length(activities[,1])
	numCols = dim(cleaned)[2]

	result = cleaned[1:(numSubjects*numActivities), ]
	row = 1

	for (s in 1:numSubjects) {
	for (a in 1:numActivities) {
	result[row, 1] = uniqueSubjects[s]
	result[row, 2] = activities[a, 2]
	tmp <- cleaned[cleaned$subject==s &
	cleaned$activity==activities[a, 2], ]
	result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
	row = row+1
	}
	}
	 write.table(result, "data_set_with_the_averages.txt")

