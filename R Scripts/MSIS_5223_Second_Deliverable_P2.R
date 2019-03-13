#Set working directory
setwd("/Users/ishan/Desktop/OSU Academic/Spring 2018/MSIS 5223 - Programming for Data Science/Project/Bike MS/P2/Our Project")

#Install required Libraries and import them 
install.packages("readxl")
library("readxl")

#Load datasets
participants <- read_excel("participantsV2.xlsx")
biketeam <- read_excel("biketeamsV2.xlsx")

#Check the column names
colnames(participants)
colnames(biketeam)

#Merging the datasets into a final dataset
total <- merge(participants,biketeam ,by="TeamID")

#Checking the column names for the merged dataset
colnames(total)

#Importing library for the dummy variables (data transformation)
library(dummies)

################################
#Creating the dummy variables for the first variable
bike_TeamDivision=dummy(total[,c('TeamDivision.x')], sep='_')
colnames(bike_TeamDivision)

#Renaming the column names
colnames(bike_TeamDivision)=c('TeamDivision_1','TeamDivision_2','TeamDivision_3','TeamDivision_4','TeamDivision_5','TeamDivison_6', 'TeamDivison_7')

#Converting the created dummy variables into a dataframe and merging it with the final dataset
bike_TeamDivision=as.data.frame(bike_TeamDivision)
total=data.frame(total,bike_TeamDivision)

################################
#Creating the dummy variables for the second variable
bike_ispriorparticipant=dummy(total[,c('IsPriorParticipant')], sep ='_')
colnames(bike_ispriorparticipant)

#Renaming the column names
colnames(bike_ispriorparticipant)=c('bike_ispriorparticipant_1','bike_ispriorparticipant_2')

#Converting the created dummy variables into a dataframe and merging it with the final dataset
bike_ispriorparticipant=as.data.frame(bike_ispriorparticipant)
total=data.frame(total,bike_ispriorparticipant)

################################
#Creating the dummy variables for the third variable
bike_participantconnectiontoms=dummy(total[,c('ParticipantConnectiontoMS')], sep='_')
colnames(bike_participantconnectiontoms)

#Renaming the column names
colnames(bike_participantconnectiontoms)=c('bike_participantconnectiontoms_1','bike_participantconnectiontoms_2','bike_participantconnectiontoms_3','bike_participantconnectiontoms_4','bike_participantconnectiontoms_5','bike_participantconnectiontoms_6','bike_participantconnectiontoms_7','bike_participantconnectiontoms_8','bike_participantconnectiontoms_9','bike_participantconnectiontoms_10','bike_participantconnectiontoms_11','bike_participantconnectiontoms_12', 'bike_participantconnectiontoms_13', 'bike_participantconnectiontoms_14')

#Converting the created dummy variables into a dataframe and merging it with the final dataset
bike_participantconnectiontoms=as.data.frame(bike_participantconnectiontoms)
total=data.frame(total,bike_participantconnectiontoms)

################################
#Creating the dummy variables for the fourth variable
bike_gender=dummy(total[,c('ParticipantGender')], sep='_')
colnames(bike_gender)

#Renaming the column names
colnames(bike_gender)=c('bike_gender_1','bike_gender_2','bike_gender_3')

#Converting the created dummy variables into a dataframe and merging it with the final dataset
bike_gender=as.data.frame(bike_gender)
total=data.frame(total,bike_gender)

#Data reduction using PCA
reduction_data.pca2 = total[c("TotalofAllConfirmedGifts"  ,"TotalFromParticipant"  ,"TotalNotFromParticipant"  ,"NumberFromParticipant"   ,"NumberNotFromParticipant"  ,"TotalFeesPaid","TeamTotalConfirmed",  "TotalOnlineGifts",  "TotalOfflineConfirmedGifts",  "TotalOfflineUnconfirmedGifts", "TeamDivision_1","TeamDivision_2","TeamDivision_3","TeamDivision_4","TeamDivision_5","TeamDivison_6","TeamDivison_7","bike_ispriorparticipant_1","bike_ispriorparticipant_2","bike_gender_1","bike_gender_2","bike_gender_3" )]

pcamodel_reduc2 = princomp(reduction_data.pca2,cor=TRUE)

#Checking Eigen Values of the model created
pcamodel_reduc2$sdev^2

#Plotting the graph for the pca model created
plot(pcamodel_reduc2,main="Screenplot")

#As the above plot is not showing the total number of columns, we shall use:
screeplot(pcamodel_reduc2, npcs = 22, type = "lines")

####We would perform the FA(Factor Analysis) to confirm results of PCA
#Another method for FA
temp <- total[,c(14,15,16,17,18,36,37,38,41,42,43,44,45,46,47,48,49,64,65,66)]

#Checking the column names
colnames(temp)

#Installing and importing required libraries
install.packages("psych")
install.packages("GPArotation")
library(psych)
library(GPArotation)
fa(r=cor(temp), nfactors=12, rotate="varimax", SMC=FALSE, fm="minres")

#As we can see from the FA analysis, only 12 out of 20  variables are requrired, viz.
#Factor1: TotalofAllConfirmedGifts
#Factor2: TotalOnlineGifts  
#Factor3: bike_ispriorparticipant_1   
#Factor4: bike_gender_2 
#Factor5: TeamDivision_1  
#Factor6: TeamDivision_3
#Factor7: TeamDivision_2
#Factor8: TeamDivision_6
#Factor9: TotalFromParticipant 
#Factor10: TeamDivision_5 
#Factor11: bike_gender_3 
#Factor12: TeamDivison_7 

#We shall remove the rest columns from the dataset
#i.e. TotalNotFromParticipant, NumberFromParticipant, NumberNotFromParticipant, TotalOfflineConfirmedGifts, TotalOfflineUnconfirmedGifts, TeamDivision_4, bike_ispriorparticipant_2, bike_gender_1

#Now, checking the column names of the final dataset
colnames(total)

#Reducing the columns of the final dataset on the basis of the factor analysis results
reduced_total <- total[,c(-16,-17,-18,-37,-38, -44, -49, -64)]

#Checking the column names of the reduced dataset
colnames(reduced_total)

sapply(reduced_total, class)

#############################################################
#Checking the column names of the reduced dataset
colnames(reduced_total)

#Creating the reduced dataset with some specific columns
data3 <- reduced_total[,c(17,30,32,36,37,38,39,40,43,44,45,46,47,48,49,50,51,52,53,54,55,57,58)]

#Checking the column names of the dataset
colnames(data3)

sapply(data3, class)

##Creating training and testing data
#### Set the percentages of your subsets by giving 70% to the training and 30% to the testing
train.size = 0.7
test.size = 0.3

#### Calculate the sample sizes for both the training and testing datasets
train2 = floor(train.size * nrow(data3))
test2 = floor(test.size * nrow(data3))

#### Determine the indices each subset will have
#### 1) randomly select the indices for the training set
#### 2) determine the remaining indices not in the training set
#### 3) from the list of indices in Step 2, randomly select
#### indices for the validation set
#### 4) determine the testing-subset indices by selecting those
#### not in the validation-subset
indices.train = sort(sample(seq_len(nrow(data3)), size=train2))
indices.valid_test = setdiff(seq_len(nrow(data3)), indices.train)
indices.test = sort(sample(indices.valid_test, size=test2))

#### Use the indices to select the data from the dataframe
data3.train = data3[indices.train,]
data3.test = data3[indices.test,]

#Checking the number of rows of both the training and testing datasets
nrow(data3.train)
nrow(data3.test)

#Checking the column names of the train dataset
colnames(data3.train)

#QUESTION 1: What occupations were responsible for most of our fundraising? and common factors ?
#Performing Linear Regression for the model
#We didnt take gender 3 in the linear regression model as it was ommited during the FA

#First model - Regression
fit1 = lm(TeamTotalConfirmed ~ ParticipantOccupation +  bike_participantconnectiontoms_2 + bike_participantconnectiontoms_3 + bike_participantconnectiontoms_4 + bike_participantconnectiontoms_5 + bike_participantconnectiontoms_6 + bike_participantconnectiontoms_7 + bike_participantconnectiontoms_8 + bike_participantconnectiontoms_9 + bike_participantconnectiontoms_10 + bike_participantconnectiontoms_11 + bike_participantconnectiontoms_12+ bike_participantconnectiontoms_13+ bike_gender_2 + bike_gender_3, data = data3.train)

#Getting to know the summary of the first model
summary(fit1)

#Creating the second regression model
fit2 = lm(TeamTotalConfirmed ~ ParticipantOccupation + bike_gender_2 + bike_gender_3, data = data3.train)

#Getting to know the summary of the second model
summary(fit2)

#Creating the third regression model
fit3 = lm(sqrt(sqrt(TeamTotalConfirmed)) ~ ParticipantOccupation + log(NumberofParticipants), data = data3.train)

#Getting to know the summary of the third model
summary(fit3)

#Diagnostic plots provide checks for heteroscedasticity, normality, and influential observerations

#Checking the same for the first regression model
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit1)

#Checking the same for the second regression model
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit2)

#Checking the same for the third regression model
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit3)

#Now, Comparing the three regression models created
anova(fit1, fit2, fit3) 

#QUESTION 2: What industries have had the strongest involvement in Bike MS in the last five years?
#We ommited one gender and team division as they are dummy variables

#Creating the fourth regresion model
fit4 = lm(NumberofParticipants ~ ParticipantOccupation + bike_gender_2 + TeamDivision_1 + TeamDivision_2 + TeamDivision_3 + TeamDivision_5,  data = data3.train)

#Checking the model by verifying its summary
summary(fit4)

#Creating the fifth regresion model
fit5 = lm(NumberofParticipants ~ ParticipantOccupation + TeamDivision_1 + TeamDivision_2 + TeamDivision_3 + TeamDivision_5,  data = data3.train)

#Checking the model by verifying its summary
summary(fit5)

#Creating the sixth regresion model
fit6 = lm(NumberofParticipants ~ ParticipantOccupation + TeamDivision_1 + TeamDivision_2 + TeamDivision_3 + TeamDivision_5 + TeamTotalConfirmed,  data = data3.train)

#Checking the model by verifying its summary
summary(fit6)

#Diagnostic plots provide checks for heteroscedasticity, normality, and influential observerations

#Plotting the same for the fourth regression model 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit4)

#Plotting the same for the fifth regression model 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit5)

#Plotting the same for the sixth regression model
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit6)

#Importing the required libraries for the DW Statistic
library(car)

#Checking the Durbin Watson Statistic of the third model
durbinWatsonTest(fit3)

#Checking the VIF for checking the collinearity
vif(fit3)

#Checking the Durbin Watson Statistic of the sixth model
durbinWatsonTest(fit6)

#Checking the VIF for checking the collinearity
vif(fit6)

############Decision tree###############
#Installing and importing the required libraries for creting the decision and classification trees
install.packages("party")
library(party)
library(tree)
library(rpart)

# Create the decision tree.
tree1 <- rpart(NumberofParticipants ~ TeamDivision_1 + TeamDivision_2 + TeamDivision_3 + TeamDivision_5 + TeamTotalConfirmed, data = data3.train,method='class')

# Plot the tree.
plot(tree1)
text(tree1)


################Second tree - Classification tree for the categorical variable
tree <- ctree(ParticipantOccupation ~ NumberofParticipants + TeamDivision_1 + TeamDivision_2 + TeamDivision_3 + TeamDivision_5 + TeamTotalConfirmed, data = data3.train)
plot(tree)
text(tree)
