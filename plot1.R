##################### save original working directory ################################
Ori_WD <- getwd()
######################################################################################

############################### test package #########################################
list.of.packages <- c("dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
######################################################################################

######################### Download file and unzip ####################################
filename <- "exdata_data_household_power_consumption.zip"

if (!file.exists(filename)){
        fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        download.file(fileURL, filename, method="libcurl")
}  
if (!file.exists("household_power_consumption.txt")) { 
        unzip(filename) 
}
######################################################################################

######################## Check if PC has enough Ram ##################################
#memory required = no. of column * no. of rows * 8 bytes/numeric
memory_required <- ((9 * 2075259 * 8 )/1048600) + as.numeric(memory.size()) - as.numeric(memory.limit())

if(memory_required > 0){
        stop("Not enough Ram to perform this task")
}

######################################################################################

############################ getting data clean ######################################

DT <- read.table("household_power_consumption.txt", header=TRUE, sep=";", na.strings = "?", colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))

## Format date to Type Date
DT$Date <- as.Date(DT$Date, "%d/%m/%Y")

## Filter data set from Feb. 1, 2007 to Feb. 2, 2007
DT <- subset(DT,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))

## Remove incomplete observation
DT <- DT[complete.cases(DT),]

## Combine Date and Time column
dateTime <- paste(DT$Date, DT$Time)

## Name the vector
dateTime <- setNames(dateTime, "DateTime")

## Remove Date and Time column
DT <- DT[ ,!(names(DT) %in% c("Date","Time"))]

## Add DateTime column
DT <- cbind(dateTime, DT)

## Format dateTime Column
DT$dateTime <- as.POSIXct(dateTime)
######################################################################################

################################## Ploting ###########################################
## Create Plot 1
hist(DT$Global_active_power, main="Global Active Power", xlab = "Global Active Power (kilowatts)", col="red")
## Save file and close device
dev.copy(png,"plot1.png", width=480, height=480)
dev.off()
######################################################################################