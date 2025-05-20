plot2 <- function() {
    
    loadLibraries()
    
    startDate = as.Date("2007-02-01")
    endDate = as.Date("2007-02-02")
    
    data <- getData(startDate, endDate)
    
    plotGlobalActiveLine(data)
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

plotGlobalActiveLine <- function(data) {
    
    dateRange <- with(data, seq(head(Date, n = 1), tail(Date, n = 1) + 1, "days")) ## This is to label the time axis 
    
    png("plot2.png", width = 480, height = 480, units = "px")   ## Open graphics device
    
    with(data, plot(Date_Time, Global_active_power, xlab = "", ylab = "Global Active Power (kilowatts)", type = "n", xaxt ="n")) ## Draw the plot
    with(data, lines(Date_Time, Global_active_power))
    with(data, axis(1, at = c(head(Date_Time, n = 1), Date_Time[nrow(data)/2], tail(Date_Time, n = 1)), labels = weekdays(dateRange, abbreviate = TRUE)))
    
    dev.off()
    
}
