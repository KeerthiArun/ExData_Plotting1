plot2 <- function(filepath){
  
  ##Read required data to datafile and call plot function
  # Last row to be skipped from text file before reading
  skipto <- grep("31/1/2007;23:59:00", readLines(filepath), value=FALSE)
  # Number of rows to be read from text file
  readto <- grep( "^2/2/2007;23:59:00", readLines(filepath), value=FALSE)
  rowNum <- readto-skipto
  # Find Colnames from the text file
  colheadings <- strsplit(x = readLines(filepath,n = 1),split = ";")[[1]]
  # Read data corresponding dates "1/2/2007" and "2/2/2007"
  datafile <- read.table(filepath,header=FALSE,sep=";", col.names = colheadings,skip = skipto, nrow = rowNum)

  ##Open png device and call plot function
  png("plot2.png")
   #remove x axis index and its label
  plot(datafile$Global_active_power,type="l",xaxt = "n",xlab = "", ylab = "Global Active Power(kilowatts)")
  # Add new labels at values 1, 1441 and 2881 indicating values in the 
   # plot from 1 to 1440 were taken on thursday and from 1441 to 2880 on Friday
  axis(1,at = c(1,1441,2881),labels = c("Thu","Fri","Sat"))
  dev.off()#close device
  message("plot2.png available in your working directory")
}