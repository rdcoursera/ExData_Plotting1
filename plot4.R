## These functions recreate the reference plot4 for project one of Exploratory Data Analysis
## The main function to run is plot4

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
  par(mfrow = c(2, 2))
  plot(hd$Time,hd$Global_active_power,type = "l",main = "", ylab = "Global Active Power",xlab = "")
  plot(hd$Time,hd$Voltage,type = "l",main = "",ylab = "Voltage",xlab = "datetime")
  plot(hd$Time,hd$Sub_metering_1,type = "l",main = "", ylab = "Energy sub metering",xlab = "")
  lines(hd$Time,hd$Sub_metering_2,col = "red")
  lines(hd$Time,hd$Sub_metering_3, col = "blue")
  legend("topright", lty = 1, col = c("black","red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"),bty = "n")
  plot(hd$Time,hd$Global_reactive_power,type = "l",main = "", ylab = "Global_reactive_power",xlab = "datetime")
  #  dev.copy(png,file=outfilename,width = 480,height = 480)
  dev.off()
}


## main function to make the reference plot
plot4 <- function() {
  
  startdate <- as.Date("2007-02-01")
  enddate <- as.Date("2007-02-02")
  
  householddata <- getfiledata("../household_power_consumption.txt")
  householddata$Date <- as.Date(householddata$Date,"%d/%m/%Y")
  subhd <- getdatesubset(householddata,startdate,enddate)
  makeplotpng(subhd,"plot4.png")
}