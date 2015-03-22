install.packages("data.table")
install.packages("reshape2")
library(data.table)
library(reshape2)

#read activity labels from activity_labels.txt
#V1: label no.; V2: label description
act.labels<-read.table("activity_labels.txt")



########################################################################
# PART I - MERGE THE TRAINING AND THE TEST SETS TO CREAT ONE DATA SET ##
########################################################################
#subject related training and testing datasets
sub.train<-read.table("./train/subject_train.txt")
sub.test<-read.table("./test/subject_test.txt")
sub<-rbind(sub.train,sub.test)
dim(sub)

#x related training and test datasets
x.train<-read.table("./train/X_train.txt")
x.test<-read.table("./test/X_test.txt")
x<-rbind(x.train,x.test)
dim(x)

y.train<-read.table("./train/y_train.txt")
y.test<-read.table("./test/y_test.txt")
y<-rbind(y.train,y.test)

#assign descriptive lables to the corresponding columns
features<-read.table("features.txt")
names(x)<-features[,2]
colnames(y)<-"Activity"
colnames(sub)<-"Subject"

#combine all three datasets together into allInOne dataset
allInOne<-cbind(x,y,sub) 
dim(allInOne)

####################################################################
## PART II - EXTRACTS ONLY THE MEASUREMENTS ON THE MEAN AND       ##
## STANDARD DEVIATION FOR EACH MEASUREMENT                        ##
####################################################################
features<-read.table("features.txt")
f.ind<- grepl("mean()|std()", features[,2])
x.extract<-x[,f.ind]
names(x.extract)<-features[f.ind,2]


allInOne.extract<-cbind(x.extract,y,sub)


#######################################################################
## PART III - USES DESCRIPTIVE ACTIVITY NAMES TO NAME THE ACTIVITIES ##
## IN THE DATA SET                                                   ##
#######################################################################
allInOne.extract$Activity <- as.character(allInOne.extract$Activity)
for (i in 1:6){
  allInOne.extract$Activity[allInOne.extract$Activity == i] <- as.character(act.labels[i,2])
}


######################################################################
## PART IV - APPROPRIATELY LABELS THE DATASET WITH DESCRIPTIVE      ##
## VARIABLE NAMES                                                   ## 
######################################################################
changeName<-function(oldName,newName,ds){
  gsub(oldName, newName, names(ds))
  }

names(allInOne.extract)<-changeName("Acc","Accelerometer",allInOne.extract)
names(allInOne.extract)<-changeName("Gyro","Gyroscope",allInOne.extract)
names(allInOne.extract)<-changeName("BodyBody","Body",allInOne.extract)
names(allInOne.extract)<-changeName("Mag","Magnitude",allInOne.extract)
names(allInOne.extract)<-changeName("^t","Time",allInOne.extract)
names(allInOne.extract)<-changeName("^f", "Frequency",allInOne.extract)
names(allInOne.extract)<-changeName("tBody", "TimeBody",allInOne.extract)
names(allInOne.extract)<-changeName("-mean()", "Mean",allInOne.extract)
names(allInOne.extract)<-changeName("-std()", "STD",allInOne.extract)
names(allInOne.extract)<-changeName("-freq()", "Frequency",allInOne.extract)
names(allInOne.extract)<-changeName("angle", "Angle",allInOne.extract)
names(allInOne.extract)<-changeName("gravity", "Gravity",allInOne.extract)

names(allInOne.extract)


######################################################################
## PART V - FROM THE DATASET IN STEP4, CREATES A SECOND, INDEPENDENT##
## DATASET WITH THE AVERGAGE OF EACH VARIABLE FOR EACH ACTIVITY AND ## 
## EACH SUBJECT                                                     ## 
######################################################################
allInOne<-data.table(allInOne.extract)
attach(allInOne)
clean.data<-aggregate(.~as.factor(Subject)+Activity,allInOne,mean)
clean.data <- clean.data[order(clean.data$Subject,clean.data$Activity),]
write.table(clean.data, file = "clean.txt", row.names = FALSE)
