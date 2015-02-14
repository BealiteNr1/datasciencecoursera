      cName <- read.csv("features.txt", sep="", header=FALSE)
      cName <- as.vector(cName[,2])
      
      file2 <- read.csv("test/X_test.txt", sep = "", header=FALSE, nrows=10)
      classes <- sapply(file2, class)
      
      #"disable" cols other than "*-mean()-*" or "*-std()-*" 
      for (i in 1:length(classes)) {
      	found <- length(grep("-mean()-",cName[i], fixed=TRUE)) + length(grep("-std()",cName[i], fixed=TRUE))
         	if (found == 0) {
         		classes[i] <- "NULL"
         		cName[i] <- ""
         	}
         }
      
      cName <- cName[cName!=""]
      
      
      #read train file
      file1 <- read.csv("train/X_train.txt", sep = "", header=FALSE, colClasses = classes)
      subject <- read.csv("train/subject_train.txt", sep = "", header=FALSE)
      file1 <- cbind(subject, file1)
      y <- read.csv("train/y_train.txt", sep = "", header=FALSE)
      file1 <- cbind(y, file1)
      
      #read test file
      file2 <- read.csv("test/X_test.txt", sep = "", header=FALSE, colClasses = classes)
      subject <- read.csv("test/subject_test.txt", sep = "", header=FALSE)
      file2 <- cbind(subject, file2)
      y <- read.csv("test/y_test.txt", sep = "", header=FALSE)
      file2 <- cbind(y, file2)
      
      #merge to one data frame
      filesunion <- rbind(file1, file2)
      
      #add the two column descriptors at the beginning and set colnames
      colnames(filesunion) <- c("act_nr","subject",cName)
      
      #read the activity labels which then can be linked to column [act_nr]
      act_labels <- read.csv("activity_labels.txt", sep="", header=FALSE)
      colnames(act_labels) <- c("act_nr","activity")
      
      #compute the mean of all variables grouped by activity and subject
      filetidy <- aggregate(x = filesunion[,3:ncol(filesunion)], by = list(factor(filesunion$act_nr), factor(filesunion$subject)), FUN = "mean")
      
      #reset the names of the group columns
      colnames(filetidy)[1:2] <- c("act_nr","subject")
      
      #add a column with activity labels
      filetidy <- merge(x=act_labels, y=filetidy, by="act_nr")

      #export to textfile
      write.table(x=filetidy, file="Getting_and_Cleaning_C1_tidy.txt", row.name=FALSE) 
