#Set directory
setwd("C:\\Users\\Sreeja Malka\\Desktop\\R\\p2")
library("readxl")

#load datasets
participants <- read_excel("participantsV2.xlsx")
biketeam <- read_excel("biketeamsV2.xlsx")

#check number of rows
nrow(participants)
nrow(biketeam)

#check colnames
colnames(participants)
colnames(biketeam)

#merging the datasets
total <- merge(participants,biketeam ,by="TeamID")

write.csv(total, "total.csv")

#checking the column names for the merged dataset
colnames(total)
nrow(total)

#Creating dummy variables (data transformation)
library(dummies)

bike_TeamDivision=dummy(total[,c('TeamDivision.x')], sep='_')
colnames(bike_TeamDivision)
colnames(bike_TeamDivision)=c('TeamDivision_1','TeamDivision_2','TeamDivision_3','TeamDivision_4','TeamDivision_5','TeamDivison_6', 'TeamDivison_7')
bike_TeamDivision=as.data.frame(bike_TeamDivision)
total=data.frame(total,bike_TeamDivision)

bike_ispriorparticipant=dummy(total[,c('IsPriorParticipant')], sep ='_')
colnames(bike_ispriorparticipant)
colnames(bike_ispriorparticipant)=c('bike_ispriorparticipant_1','bike_ispriorparticipant_2')
bike_ispriorparticipant=as.data.frame(bike_ispriorparticipant)
total=data.frame(total,bike_ispriorparticipant)

bike_participantconnectiontoms=dummy(total[,c('ParticipantConnectiontoMS')], sep='_')
colnames(bike_participantconnectiontoms)
colnames(bike_participantconnectiontoms)=c('bike_participantconnectiontoms_1','bike_participantconnectiontoms_2','bike_participantconnectiontoms_3','bike_participantconnectiontoms_4','bike_participantconnectiontoms_5','bike_participantconnectiontoms_6','bike_participantconnectiontoms_7','bike_participantconnectiontoms_8','bike_participantconnectiontoms_9',
                                           'bike_participantconnectiontoms_10','bike_participantconnectiontoms_11','bike_participantconnectiontoms_12', 'bike_participantconnectiontoms_13', 'bike_participantconnectiontoms_14')
bike_participantconnectiontoms=as.data.frame(bike_participantconnectiontoms)
total=data.frame(total,bike_participantconnectiontoms)

bike_gender=dummy(total[,c('ParticipantGender')], sep='_')
colnames(bike_gender)
colnames(bike_gender)=c('bike_gender_1','bike_gender_2','bike_gender_3')
bike_gender=as.data.frame(bike_gender)
total=data.frame(total,bike_gender)

colnames(total)
  
#Data reduction using PCA

reduction_data.pca2 = total[c("TotalofAllConfirmedGifts"  ,"TotalFromParticipant"  ,"TotalNotFromParticipant"  ,"NumberFromParticipant"   ,"NumberNotFromParticipant"  ,"TotalFeesPaid","TeamTotalConfirmed",  "TotalOnlineGifts",  "TotalOfflineConfirmedGifts",  "TotalOfflineUnconfirmedGifts", 
"TeamDivision_1","TeamDivision_2","TeamDivision_3","TeamDivision_4","TeamDivision_5","TeamDivison_6","TeamDivison_7",
"bike_ispriorparticipant_1","bike_ispriorparticipant_2", "bike_gender_1","bike_gender_2","bike_gender_3" )]


pcamodel_reduc2 = princomp(reduction_data.pca2,cor=TRUE)

#checking Eigen Values
pcamodel_reduc2$sdev^2

#plotting the graph
plot(pcamodel_reduc2,main="Screeplot")

#As the above plot is not showing the total number of columns, we shall use:
screeplot(pcamodel_reduc2, npcs = 22, type = "lines")

####confirm results of PCA
##another method for FA
temp = total[,c(14,15,16,17,18,34,35,36,37,38,41,42,43,44,45,46,47,48,49,64,65,66)]
colnames(temp)

library(psych)
library(GPArotation)
fa(r=cor(temp), nfactors=11, rotate="varimax", SMC=FALSE, fm="minres")

#As we can see from the FA analysis, only 11 out of 22  variables are significant, viz.

#Factor1: TeamTotalConfirmed 
#Factor2: TotalofAllConfirmedGifts
#Factor3: bike_gender_1   
#Factor4: TeamDivision_1
#Factor5: TeamDivision_3  
#Factor6: TeamDivision_2
#Factor7: TeamDivision_6
#Factor8: TeamDivision_5
#Factor9: TeamDivison_7 
#Factor10: TotalFromParticipant 
#Factor11: bike_gender_3

colnames(temp)
 
#We shall remove the rest columns from the dataset
colnames(total)

reduced_total <- total[,c(-16,-17,-18,-34,-36,-37,-38,-44,-48,-49,-65)]

colnames(reduced_total)

sapply(reduced_total, class)

############################################################

############################################################
colnames(reduced_total)
data3 <- total[,c(14,15,18,20,30,32,33,35,39,40,41,42,43,45,46,47,50,51,52,53,54,55,57,58,64,66)]

colnames(data3)
sapply(data3, class)
data3

##Creating training and testing data
#### Set the percentages of your subsets
train.size = 0.7
test.size = 0.3

#### Calculate the sample sizes
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

indices.train
indices.valid_test
indices.test

#### Use the indices to select the data from the dataframe
data3.train = data3[indices.train,]
data3.test = data3[indices.test,]

nrow(data3.train)
nrow(data3.test)

colnames(data3.train)

describe(data3$TeamTotalConfirmed)
describe(data3.train$TeamTotalConfirmed)
describe(data3.test$TeamTotalConfirmed)

describe(data3$NumberofParticipants)
describe(data3.train$NumberofParticipants)
describe(data3.test$NumberofParticipants)

#What occupations were responsible for most of our fundraising? and common factors
#linear regression
#we didnt take gender 2 in the linear regression model as it was ommited during the FA

fit4 = lm(TeamTotalConfirmed ~ ParticipantOccupation +  bike_participantconnectiontoms_2 + bike_participantconnectiontoms_3 + 
 bike_participantconnectiontoms_4 + bike_participantconnectiontoms_5 + 
 bike_participantconnectiontoms_6 + bike_participantconnectiontoms_8 + bike_participantconnectiontoms_9 +
 bike_gender_1 + bike_gender_3, data = data3.train)

summary(fit4)

fit5 = lm(TeamTotalConfirmed ~ ParticipantOccupation + bike_gender_1 + bike_gender_3, data = data3.train)

summary(fit5)

fit6 = lm(sqrt(sqrt(TeamTotalConfirmed)) ~ ParticipantOccupation + log(NumberofParticipants), data = data3.train)

summary(fit6)

#Testing the best model using the test data:
fitTest = lm(TeamTotalConfirmed ~ TotalofAllConfirmedGifts + ParticipantOccupation, data = data3.test)
summary(fitTest)


#What industries have had the strongest involvement in Bike MS in the last five years?
#We ommited one gender and team division as they are dummy variables

fit1 = lm(NumberofParticipants ~ ParticipantOccupation + bike_gender_3 + 
TeamDivision_1 + TeamDivision_2 + TeamDivision_3 + TeamDivision_5,  data = data3.train)
summary(fit1)

fit2 = lm(NumberofParticipants ~ ParticipantOccupation + TeamDivision_1 + 
TeamDivision_2 + TeamDivision_3 + TeamDivision_5,  data = data3.train)
summary(fit2)

fit3 = lm(log(NumberofParticipants) ~ ParticipantOccupation + TeamDivision_1 +
TeamDivision_2 + TeamDivision_3 + TeamDivision_5 + sqrt(TeamTotalConfirmed),  data = data3.train)
summary(fit3)

#Testing the best model using the test data:
fitTest = lm(NumberofParticipants ~ ParticipantOccupation + TeamDivision_1 + TeamDivision_2 + TeamDivision_3 + TeamDivision_5 + TeamTotalConfirmed,  data = data3.test)
summary(fitTest)

#Diagnostic plots provide checks for heteroscedasticity, normality, and influential observerations
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit1)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit2)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit3)

#comparing models
anova(fit1, fit2, fit3) 

#Diagnostic plots provide checks for heteroscedasticity, normality, and influential observerations
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit4)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit5)

library(car)
durbinWatsonTest(fit3)
vif(fit3)

durbinWatsonTest(fit6)
vif(fit6)

#Decision tree
install.packages("tree")
install.packages("party")
library(party)
library(tree)
library(rpart)

#decision tree1
tree1 <- rpart(ParticipantOccupation ~ NumberofParticipants + TeamDivision_1 + TeamDivision_2 + TeamDivision_3 + TeamDivision_5 + TeamTotalConfirmed, data = data3.train, method='class')

plot(tree1)
text(tree1)

#decision tree2
tree2 <- rpart(NumberofParticipants ~ TeamDivision_1 + TeamDivision_2 + TeamDivision_3 + TeamDivision_5 + TeamTotalConfirmed, data = data3.train, method='class')

plot(tree2)
text(tree2)
