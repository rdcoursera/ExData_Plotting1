## These functions recreate the reference plot3 for project one of Exploratory Data Analysis
## The main function to run is plot3

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
makeplotpng <- function(hd,outfilename) {
  png(filename = outfilename, width = 480, height=480)
  plot(hd$Time,hd$Sub_metering_1,type = "l",main = "", ylab = "Energy sub metering",xlab = "")
  lines(hd$Time,hd$Sub_metering_2,col = "red")
  lines(hd$Time,hd$Sub_metering_3, col = "blue")
  rect(2, 3, 3, 2, col = 4)
  legend("topright", lty = 1, col = c("black","red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"))
   # dev.copy(png,file=outfilename,width = 480,height = 480)
  dev.off()
}


## main function to make the reference plot
plot3 <- function() {
  
  startdate <- as.Date("2007-02-01")
  enddate <- as.Date("2007-02-02")
  
  householddata <- getfiledata("../household_power_consumption.txt")
  householddata$Date <- as.Date(householddata$Date,"%d/%m/%Y")
  subhd <- getdatesubset(householddata,startdate,enddate)
  makeplotpng(subhd,"plot3.png")
}