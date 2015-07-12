plot1 <- function(filepath){
  
  ##Read required data to datafile and call histogram function
  
  # Last row to be skipped from text file before reading
  skipto <- grep("31/1/2007;23:59:00", readLines(filepath), value=FALSE)
  
  # Number of rows to be read from text file
  readto <- grep( "^2/2/2007;23:59:00", readLines(filepath), value=FALSE)
  rowNum <- readto-skipto 
  
  # Find Colnames from the text file
  colheadings <- strsplit(x = readLines(filepath,n = 1),split = ";")[[1]]
  
  # Read data corresponding dates "1/2/2007" and "2/2/2007"
  datafile <- read.table(filepath,header=FALSE,sep=";", col.names = colheadings,skip = skipto, nrow = rowNum)
  
  
  #Open png device and call histogram function to reconstruct plot1
  png("plot1.png")
  hist(datafile$Global_active_power,main = "Global Active Power",xlab = "Global Active Power (Kilowatts)",col = "red")
  dev.off()#close device
  message("plot1.png available in your working directory")
}