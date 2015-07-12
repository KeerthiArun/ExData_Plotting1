plot4 <- function(filepath){
  
  ##Read required data to datafile and call 3 plot functions for submeterings 1,2 and 3.
  
  # Last row to be skipped from text file before reading
  skipto <- grep("31/1/2007;23:59:00", readLines(filepath), value=FALSE)
  
  # Number of rows to be read from text file
  readto <- grep( "^2/2/2007;23:59:00", readLines(filepath), value=FALSE)
  rowNum <- readto-skipto
  
  # Find Colnames from the text file
  colheadings <- strsplit(x = readLines(filepath,n = 1),split = ";")[[1]]
  
  # Read data corresponding dates "1/2/2007" and "2/2/2007"
  datafile <- read.table(filepath,header=FALSE,sep=";", col.names = colheadings,skip = skipto, nrow = rowNum)
  
  #Open png and draw 4 different plots on same page ; 2 at top and 2 at bottom of the page
  png("plot4.png")
  par(mfrow=c(2,2))
  
  #Plot will show values of Global Active Power on 1/2/2007 and 2/2/2007(Thursday and Friday respectively)
  plot(datafile$Global_active_power,type="l",xaxt = "n",xlab = "", ylab = "Global Active Power")
  axis(1,at = c(1,1441,2881),labels = c("Thu","Fri","Sat"))
  
  #Plot will show values of Voltage on 1/2/2007 and 2/2/2007(Thursday and Friday respectively)
  plot(datafile$Voltage,type = "l", xlab = "datetime",ylab = "Voltage",xaxt = "n")
  axis(1,at = c(1,1441,2881),labels = c("Thu","Fri","Sat"))
  
  #Plot will show values of sub_meterings 1,2 and 3 on 1/2/2007 and 2/2/2007(Thursday and Friday respectively)
  plot.new()# Initialize plot
  box(which = "plot")# Draw a box for plot
  par(new=TRUE)#To keep the existing plot and to draw over it
  #Call plot functions for sub_metering_1, sub_metering_2 and sub_metering_3
  plot(datafile$Sub_metering_1,type = "l", xlab = "",ylab = "Energy sub metering", ylim = range(c(0,40)), axes = FALSE)
  par(new = TRUE)
  plot(datafile$Sub_metering_2,type = "l", ylim = range(c(0,40)), axes = FALSE,xlab = "",ylab = "",col="red")
  par(new = TRUE)
  plot(datafile$Sub_metering_3,type = "l", ylim = range(c(0,40)), axes = FALSE,xlab = "",ylab = "",col="blue")
  # Draw X and Y axes
  axis(1,at = c(1,1441,2881),labels = c("Thu","Fri","Sat"))
  axis(2, at = c(0,10,20,30),labels = c(0,10,20,30))
  legend(col = c("black","red","blue"),legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lwd = 1,x = 
           "topright",cex = .8,bty = "n")
  
  #Plot will show values of global reactive power on 1/2/2007 and 2/2/2007(Thursday and Friday respectively)
  plot(datafile$Global_reactive_power,type = "l", xlab = "datetime",ylab = "Global_reactive_power",xaxt = "n")
  axis(1,at = c(1,1441,2881),labels = c("Thu","Fri","Sat"))
  dev.off()#close png
  message("plot4.png available in your working directory")
}