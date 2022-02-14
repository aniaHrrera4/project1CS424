#split file into multiple files 

myCTAdata = read.csv("~/Downloads/project1/CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals.csv")
#myCTAdata is the large dataframe containing data for cta 42.5 mB
#Get the list of unique station names
for (name in levels(myCTAdata$stationname.s.Name)){
  #Subset the data by station
  tmp=subset(myCTAdata,stationname==name)
  #Create a new filename for each MP - the folder 'ctaData' should already exist
  fn=paste('~/Downloads/project1/ctaData/',gsub(' ','',name), '.csv',sep='')
  #Save the CSV file containing separate expenses data for each station
  write.csv(tmp,fn,row.names=FALSE)
}

