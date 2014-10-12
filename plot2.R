
plot2 <- function() {

	#load data
	energyData <- readData()
	
	#Launch PNG graphics device
	png(file = "plot2.png", width = 480, height = 480, bg = "transparent", type = "cairo-png")

	#generate plot
	with(energyData, plot(timeStamp, Global_active_power, 
		xlab = "", ylab = "Global Active Power (kilowatts)", type = "l"))
		
	#close graphics device
	dev.off()
	
}

readData <- function() {
	
	## data parameters
	fileName <- "household_power_consumption.txt"
	startDate = as.Date("2007-02-01")
	endDate = as.Date("2007-02-02")
	
	## read in small number of rows
	sampleRead <- read.table(fileName, sep = ";", header = TRUE, nrows = 5, stringsAsFactors = FALSE)
	
	## get data charateristics
	classes <- sapply(sampleRead, class)
	varNames <- colnames(sampleRead)
	leadingDays = unclass(startDate) - unclass(as.Date(sampleRead[1,1], format = "%d/%m/%Y") )
	nSkip = (leadingDays - 1) * 24 *60 + 1
	
	## read in 3 day span covering target date (first date not a full day)
	energyData <- read.table(fileName, sep = ";", header = FALSE, nrows = 3*24*60, skip = nSkip, stringsAsFactors = FALSE)
	colnames(energyData) <- varNames
	
	## convert first column to date and create time stamp
	timeStamp <- strptime(paste(energyData[,1], energyData[,2]), format = "%d/%m/%Y %H:%M:%S")
	energyData[,1] <- as.Date(energyData[,1], format = "%d/%m/%Y")
	energyData <- cbind(timeStamp, energyData)
	
	## filter data to only days of interest
	energyData <- energyData[energyData[,2] >= startDate & energyData[,2] <= endDate,]
	
	## check for two days of data
	if( dim(energyData)[1] == 2*24*60 ) {
		print("data range loaded successfully")
	} else {
		print("checksum failed")
		print(dim(energyData)[1])
	}
	
	## return loaded data
	energyData

}