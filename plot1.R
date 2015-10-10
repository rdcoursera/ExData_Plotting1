## These functions recreate the reference plot1 for project one of Exploratory Data Analysis
## The main function to run is plot1

getfiledata <- function(filename) {
  read.csv(filename,sep=";",stringsAsFactors = FALSE,na.strings = c("?",""))
}

## Get data between to dates (including the begin and end dates)
getdatesubset <- function(hd,subsetstartdate,subsetenddate) {
  hd$Time <- strptime(paste(hd$Date,hd$Time), format = "%Y-%m-%d %H:%M:%S")
  hd <- subset(hd,hd$Date >= subsetstartdate)
  hd <- subset(hd,hd$Date <= subsetenddate)
}

## make the output png file
makehistpng <- function(hd,outfilename) {
  png(filename = outfilename, width = 480, height=480)
  par(mar=c(5.1,4.1,4.1,2.1))
  hist(hd$Global_active_power,col = "red",ylim = c(0,1200),main = "Global Active Power",xlab = "Global Active Power (kilowatts)")
  # dev.copy(png,file=outfilename,width = 480,height = 480)
  dev.off()
  
}

## main function to make the reference plot
plot1 <- function() {
  startdate <- as.Date("2007-02-01")
  enddate <- as.Date("2007-02-02")
  
  householddata <- getfiledata("../household_power_consumption.txt")
  householddata$Date <- as.Date(householddata$Date,"%d/%m/%Y")
  subhd <- getdatesubset(householddata,startdate,enddate)
  makehistpng(subhd,"plot1.png")
}
