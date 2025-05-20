plot4 <- function() {
    
    loadLibraries()
    
    startDate = as.Date("2007-02-01")
    endDate = as.Date("2007-02-02")
    
    data <- getData(startDate, endDate)
    
    plotAll(data)
}


loadLibraries <- function() {
    library(dplyr)
}

getData <- function(startDate, endDate) {
    ## Data file should be in the working directory
    table <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", na.strings = "?") 
    
    table %>% mutate(Date = as.Date(Date, "%d/%m/%Y")) %>%                              ## Change the Date column from char to date
        filter(between(Date, startDate, endDate)) %>%                                   ## Subset the data for the selected dates
        mutate(Date_Time = as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M:%S")) ## Create a datetime 
}

plotAll <- function(data) {
    ## Open graphics device
    png("plot4.png", width = 480, height = 480, units = "px")  
    
    ## Set the layout
    par(mfrow = c(2,2))
    
    ## Draw the plots
    plotGlobalActiveLine(data)
    plotVoltageLine(data)
    plotSubMetteringLine(data)
    plotGlobalReactiveLine(data)
    
    dev.off()
    
}

plotGlobalActiveLine <- function(data) {
    dateRange <- with(data, seq(head(Date, n = 1), tail(Date, n = 1) + 1, "days")) ## This is to label the time axis 
    
    with(data, plot(Date_Time, Global_active_power, xlab = "", ylab = "Global Active Power (kilowatts)", type = "n", xaxt ="n")) ## Draw the plot
    with(data, lines(Date_Time, Global_active_power))
    with(data, axis(1, at = c(head(Date_Time, n = 1), Date_Time[nrow(data)/2], tail(Date_Time, n = 1)), labels = weekdays(dateRange, abbreviate = TRUE)))
}

plotGlobalReactiveLine <- function(data) {
    dateRange <- with(data, seq(head(Date, n = 1), tail(Date, n = 1) + 1, "days")) ## This is to label the time axis 
    
    with(data, plot(Date_Time, Global_reactive_power, xlab = "", ylab = "Global Rective Power (kilowatts)", type = "n", xaxt ="n")) ## Draw the plot
    with(data, lines(Date_Time, Global_reactive_power))
    with(data, axis(1, at = c(head(Date_Time, n = 1), Date_Time[nrow(data)/2], tail(Date_Time, n = 1)), labels = weekdays(dateRange, abbreviate = TRUE)))
}

plotVoltageLine <- function(data) {
    dateRange <- with(data, seq(head(Date, n = 1), tail(Date, n = 1) + 1, "days")) ## This is to label the time axis 
    
    with(data, plot(Date_Time, Voltage, xlab = "", ylab = "Voltage (volts)", type = "n", xaxt ="n")) ## Draw the plot
    with(data, lines(Date_Time, Voltage))
    with(data, axis(1, at = c(head(Date_Time, n = 1), Date_Time[nrow(data)/2], tail(Date_Time, n = 1)), labels = weekdays(dateRange, abbreviate = TRUE)))
}

plotSubMetteringLine <- function(data) {
    dateRange <- with(data, seq(head(Date, n = 1), tail(Date, n = 1) + 1, "days")) ## This is to label the time axis 
    
    ## Draw the plot
    with(data, plot(Date_Time, Sub_metering_1, xlab = "", ylab = "Energy sub metering", type = "n", xaxt ="n")) 
    with(data, lines(Date_Time, Sub_metering_1))
    with(data, lines(Date_Time, Sub_metering_2, col = "red"))
    with(data, lines(Date_Time, Sub_metering_3, col = "blue"))
    
    ## Add annotations
    with(data, axis(1, at = c(head(Date_Time, n = 1), Date_Time[nrow(data)/2], tail(Date_Time, n = 1)), labels = weekdays(dateRange, abbreviate = TRUE)))
    legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col = c("black", "red", "blue"), lty = 1)
}