plot3 <- function(filepath){
  
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
  
  #Open png device and draw 3 plots on top of each other
  png("plot3.png")
  plot.new()# Initialize plot
  box(which = "plot")# Draw a box for plot
  par(new=TRUE)#To keep the existing plot and to draw over it
  
  #Call plot function for sub_metering_1
  plot(datafile$Sub_metering_1,type = "l", xlab = "",ylab = "Energy sub metering", ylim = range(c(0,40)), axes = FALSE)
  
  #Call plot function for sub_metering_2
  par(new = TRUE)
  plot(datafile$Sub_metering_2,type = "l", ylim = range(c(0,40)), axes = FALSE,xlab = "",ylab = "",col="red")
  
  #Call plot function for sub_metering_3
  par(new = TRUE)
  plot(datafile$Sub_metering_3,type = "l", ylim = range(c(0,40)), axes = FALSE,xlab = "",ylab = "",col="blue")
  
  # values on X-axis from 1 to 1440 corresponds to thursday and from 1441 to 2880 to Friday
  # Mark intervals 10,20,30 on Y-axis
  axis(1,at = c(1,1441,2881),labels = c("Thu","Fri","Sat"))
  axis(2, at = c(0,10,20,30),labels = c(0,10,20,30))
  
  legend(col = c("black","red","blue"),legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lwd = 1,x = 
           "topright",cex = .8)
  dev.off()#close device
  message("plot3.png available in your working directory")
}