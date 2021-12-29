
XN <- read.csv("XN Survey Data.csv")

head(XN)
View(XN)
colnames(XN)
#Step 1, isolating header to change format

question_header <- XN[1,]
XN1 <- XN[-c(1:3),]

question_header[4]
#Step 2, formatting duration column, disconnecting the headers, reconnecting the headers

View(XN1)
XN1$Duration..in.seconds. <- as.numeric(as.character(XN1$Duration..in.seconds.))
XN1$Duration..in.seconds. <- XN1$Duration..in.seconds./60
XN1$Duration..in.seconds. <- as.character(XN1$Duration..in.seconds.)
question_header$Duration..in.seconds. <- as.character(question_header$Duration..in.seconds.)
XN3 <- rbind(question_header, XN1)
#Step 3, deleting unwanted columns

View(XN3)
XN3$EndDate <- NULL
XN3$IPAddress <- NULL
XN3$Status <- NULL
XN3$Progress <- NULL
XN3$Finished <- NULL
XN3$RecordedDate <- NULL  
XN3$RecipientLastName <- NULL
XN3$RecipientFirstName <- NULL
XN3$ExternalReference <- NULL
XN3$UserLanguage <- NULL
XN3$Q4 <- NULL
XN5$Q11_5_TEXT...Topics <- NULL

#Step 4 changing column names and format
names(XN3)[names(XN3) == "Duration..in.seconds."] <- "Duration in minutes"
names(XN3)[names(XN3) == "Duration (in seconds)"] <- "Duration in minutes"
XN3[XN3=="Duration (in seconds)"]<-"Duration (minutes)"
View(XN3)
XN4 <- XN3

question_header <- XN3[1,]
XN4 <- XN3[-c(1),]
XN4$`Duration in minutes` <- as.numeric(XN4$`Duration in minutes`)
XN4$`Duration in minutes`<-round(XN4$`Duration in minutes`, 2)

XN5 <- rbind(question_header, XN4)

#Step 5 creating separate table with text/free response answers
XN_Open_Ended <- XN5[, c(28,29,31,33,39)]
XN6 <- XN5[, -c(28,29,31,33,39)]

dup <- duplicated(XN6)
XNDupes <- XN6[dup == TRUE, ]
#shows no dupes

XN_Final <- XN6[-c(1),]

#Step 6 exporting as CSV
write.csv(XN_Final, file = "Capstone XN.csv", row.names = FALSE)
write.csv(XN_Open_Ended, file = "Capstone XN Open Ended.csv", row.names = FALSE)

table(XN_Final == "Disagree")

#Step 7 for loop changing the layout
XN_Final_test <- XN_Final
XN_Final_test <- XN_Final[,12:27]


q<- data.frame() 
for (j in 1:16) {
for (i in 1:93) {
  {
   q[(j-1)*93+i,1] = names(XN_Final_test)[j]
   q[(j-1)*93+i,2] = XN_Final_test[i,j]
   q[(j-1)*93+i,3] = XN_Final$Q5[i]
    }
  }
}

write.csv(q, file = "Likert Visual CSV.csv", row.names = FALSE)

#Step 8 transforming Q10 columns
XN_Final_test <- XN_Final

XN_Final_test$Q10_1 <- as.character(XN_Final_test$Q10_1)
XN_Final_test$Q10_1[XN_Final_test$Q10_1 == "10- Extremely"] <- "10"
XN_Final_test$Q10_1[XN_Final_test$Q10_1 == "1- Not at all"] <- "1"
XN_Final_test$Q10_1 <- as.numeric(XN_Final_test$Q10_1)

XN_Final_test$Q10_2 <- as.character(XN_Final_test$Q10_2)
XN_Final_test$Q10_2[XN_Final_test$Q10_2 == "10- Extremely"] <- "10"
XN_Final_test$Q10_2[XN_Final_test$Q10_2 == "1- Not at all"] <- "1"
XN_Final_test$Q10_2 <- as.numeric(XN_Final_test$Q10_2)

write.csv(XN_Final_test, file = "NEW XN.csv", row.names = FALSE)

#Step 10, chi squared test if individual vs group has independence in selecting NPS/question
#splitting each question into individual vs group

XN_Indiv <- XN_Final[which(XN_Final$Q5 == "An individual student" ),]
XN_Group <- XN_Final[which(XN_Final$Q5 == "A small group (2-5 students)"| XN_Final$Q5=="A large team (5 or more students)"),]
XN_Indiv <- XN_Indiv [, -c(1:10, 28:34)]
XN_Group <- XN_Group [, -c(1:10, 28:34)]
XN_Indiv_test <- XN_Indiv_test[,-1]
XN_Group <- XN_Group[,-1]
sum(is.na(XN_Final$Q5))
table(XN_Final$Q5)

#setting up Chi square tables for Q6
install.packages('reshape2')
library(reshape2)
library(plyr)

#transposing and counting results into table for Individual table
melted <- melt(XN_Indiv_test)
test <- dcast(melted, value ~ Var2, fun.aggregate = length)
XChiQ61 <- t(test)
XChiQ61 <-XChiQ61[,-1]
XChiQ61 <- as.data.frame(XChiQ61)
Indiv_Chi <- XChiQ61

#transposing and counting results into table for Individual table
XN_Group_test <- t(XN_Group)
melted2 <- melt(XN_Group_test)
test2 <- dcast(melted2, value ~ Var1, fun.aggregate = length)
XChi_Group <- t(test2)
XChi_Group <-XChi_Group[,-1]
XChi_Group <- as.data.frame(XChi_Group)
Group_Chi <- XChi_Group
Group_Chi$V6 <- rep(0,nrow(Group_Chi))
Group_Chi[1,"V6"] <- "Strongly Disagree"
  
#Indiv_Chi and Group_Chi are final tables to run chi squared test
Chi_test <- rbind(Indiv_Chi, Group_Chi)
#Chi squared test for Q1
Q1 <- Chi_test[c('Q6_1','Q6_17'),]

for (i in 1:ncol(Q1))
{
Q1[,i] <- as.integer(Q1[,i])
}

chisq.test(Q1)
#pvalue is less than .05 being .03 therefore the question is independent

#Q2
Q2 <- Chi_test[c('Q6_2','Q6_21'),]

for (i in 1:ncol(Q2))
{
  Q2[,i] <- as.integer(Q2[,i])
}

chisq.test(Q2)

#Q3
Q3 <- Chi_test[c('Q6_3','Q6_31'),]

for (i in 1:ncol(Q3))
{
  Q3[,i] <- as.integer(Q3[,i])
}

chisq.test(Q3)

#Q6
Q6 <- Chi_test[c('Q6_6','Q6_61'),]

for (i in 1:ncol(Q6))
{
  Q6[,i] <- as.integer(Q6[,i])
}

chisq.test(Q6)

#Q8
Q8 <- Chi_test[c('Q6_8','Q6_81'),]

for (i in 1:ncol(Q8))
{
  Q8[,i] <- as.integer(Q8[,i])
}

chisq.test(Q8)

#Q9
Q9 <- Chi_test[c('Q6_9','Q6_91'),]

for (i in 1:ncol(Q9))
{
  Q9[,i] <- as.integer(Q9[,i])
}

chisq.test(Q9)

#Q10
Q10 <- Chi_test[c('Q6_10','Q6_101'),]

for (i in 1:ncol(Q10))
{
  Q10[,i] <- as.integer(Q10[,i])
}

chisq.test(Q10)

#Q11
Q11 <- Chi_test[c('Q6_11','Q6_111'),]

for (i in 1:ncol(Q11))
{
  Q11[,i] <- as.integer(Q11[,i])
}

chisq.test(Q11)

#Q12
Q12 <- Chi_test[c('Q6_12','Q6_121'),]

for (i in 1:ncol(Q12))
{
  Q12[,i] <- as.integer(Q12[,i])
}

chisq.test(Q12)

#Q13
Q13 <- Chi_test[c('Q6_13','Q6_131'),]

for (i in 1:ncol(Q13))
{
  Q13[,i] <- as.integer(Q13[,i])
}

chisq.test(Q13)

#Q14
Q14 <- Chi_test[c('Q6_14','Q6_141'),]

for (i in 1:ncol(Q14))
{
  Q14[,i] <- as.integer(Q14[,i])
}

chisq.test(Q14)

#Q15
Q15 <- Chi_test[c('Q6_15','Q6_151'),]

for (i in 1:ncol(Q15))
{
  Q15[,i] <- as.integer(Q15[,i])
}

chisq.test(Q15)