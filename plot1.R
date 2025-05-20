plot1 <- function() {

    loadLibraries()
    
    startDate = as.Date("2007-02-01")
    endDate = as.Date("2007-02-02")
    
    data <- getData(startDate, endDate)

    plotGlobalActiveHist(data)

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

plotGlobalActiveHist <- function(data) {
    png("plot1.png", width = 480, height = 480, units = "px")   ## Open graphics device
    hist(data$Global_active_power, main = "Global Active Power", xlab = "Global Active Power (kilowatts)", col = "red") ## Draw the plot
    dev.off()
}