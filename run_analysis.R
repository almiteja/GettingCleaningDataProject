#Getting and Cleaning Data, Course Project Draft 2
tidy_data <- function(x="UCI HAR Dataset") {
        library(plyr)
        test_data<-read.table("~/UCI HAR Dataset/test/X_test.txt") #read data into dfs
        train_data<-read.table("~/UCI HAR Dataset/train/X_train.txt")
        test_subjects<-read.table("~/UCI HAR Dataset/test/subject_test.txt")
        test_activity<-read.table("~/UCI HAR Dataset/test/y_test.txt")
        train_subjects<-read.table("~/UCI HAR Dataset/train/subject_train.txt")
        train_activity<-read.table("~/UCI HAR Dataset/train/y_train.txt")
        data_labels<-read.table("~/UCI HAR Dataset/features.txt")
        data_labels2 <- make.names(data_labels$V2) #clean up data labels
        colnames(test_data) <- data_labels2 #add new names to the data tables
        colnames(train_data) <- data_labels2
        colnames(test_subjects) <- "Subject" #Name the subject column
        colnames(train_subjects) <- "Subject" 
        colnames(test_activity) <- "Activity" #Name the Activity column
        colnames(train_activity) <- "Activity"
        test_means_data <- test_data[,grep("mean[.]",colnames(test_data))] #select means measurements
        test_std_data <- test_data[,grep("std[.]",colnames(test_data))] #select std measurements
        train_means_data <- train_data[,grep("mean[.]",colnames(train_data))]
        train_std_data <- train_data[,grep("std[.]",colnames(train_data))]
        test_table <- cbind(test_subjects,test_activity,test_means_data,test_std_data) #combine subject, activity, measurements
        train_table <- cbind(train_subjects,train_activity,train_means_data,train_std_data)
        full_table <- rbind(train_table, test_table)
        full_table2 <- merge(Activities, full_table, by.x="V1", by.y="Activity") #add activity names to table
        full_table3 <- cbind(full_table2$Subject,full_table2$V2, full_table2[4:69])
        full_table3 <- rename(full_table3, c("full_table2$Subject"="Subject", "full_table2$V2" = "Activity"))
        final_means <- aggregate(full_table3[3:68], by=list(full_table3$Subject,full_table3$Activity), mean)
        final_means2 <- rename(final_means, c("Group.2" = "Activity", "Group.1" = "Subject"))
        write.table(full_table3, file="~/UCI HAR Dataset/full_table_means_stds.txt")                       
        write.table(final_means2, file="~/UCI HAR Dataset/means_tidy_data.txt", row.name=FALSE)
}