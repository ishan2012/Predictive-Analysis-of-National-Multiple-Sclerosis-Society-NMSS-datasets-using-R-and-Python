#Setting the working directory
workingdirectory= "C:\\Users\\imalpot\\Desktop\\"
setwd(workingdirectory)

#read the target file
datafile_biketeams=read.csv(file.choose(),header = TRUE)

#Omit the null values
refined_datafile <- na.omit(datafile_biketeams)

#Check the column names of the datafile
str(refined_datafile)

#Delete the unnecessary coloumns not required for analysis
refined_datafile2 = subset(refined_datafile, select = -c(CaptainEmailDomain,TeamCaptainAcceptEmail) )

#Write the datafile into a new csv file
write.table(refined_datafile2,"New_Bike_Teams.csv",sep=",", row.names =FALSE)