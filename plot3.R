plot3 <- function() {
    
    loadLibraries()
    
    startDate = as.Date("2007-02-01")
    endDate = as.Date("2007-02-02")
    
    data <- getData(startDate, endDate)
    
    plotSubMetteringLine(data)
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

plotSubMetteringLine <- function(data) {
    
    dateRange <- with(data, seq(head(Date, n = 1), tail(Date, n = 1) + 1, "days")) ## This is to label the time axis 
    
    png("plot3.png", width = 480, height = 480, units = "px")  ## Open graphics device
    
    ## Draw the plot
    with(data, plot(Date_Time, Sub_metering_1, xlab = "", ylab = "Energy sub metering", type = "n", xaxt ="n")) 
    with(data, lines(Date_Time, Sub_metering_1))
    with(data, lines(Date_Time, Sub_metering_2, col = "red"))
    with(data, lines(Date_Time, Sub_metering_3, col = "blue"))
    
    ## Add annotations
    with(data, axis(1, at = c(head(Date_Time, n = 1), Date_Time[nrow(data)/2], tail(Date_Time, n = 1)), labels = weekdays(dateRange, abbreviate = TRUE)))
    legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col = c("black", "red", "blue"), lty = 1)
    
    dev.off()
    
}