# The function intends to answer the following question:
# "Have total emissions from PM2.5 decreased in the United States 
# from 1999 to 2008?"

create.plot1 <- function() {
        # Check if input file is in the working directory
        if (!file.exists("summarySCC_PM25.rds")) {
                stop("Input file was missing")
                geterrmessage()
        }
        
        # Read the data file
        data <- readRDS("summarySCC_PM25.rds")
        # Calculate the yearly totals
        yearly <- tapply(data$Emissions, data$year, sum)
        # Apply scale of millions
        yearly <- yearly / 10^6
        
        # Plot the data on a barplot, through the PNG device
        png("plot1.png")
        bp <- barplot(yearly, 
                      names.arg = names(yearly), 
                      xlab = "Year", 
                      ylab = "PM2.5 emission (Million Tons)", 
                      col = "azure3", 
                      ylim = c(0, 8),
                      main = "Total PM2.5 emission in the USA")
        # Add the values to each bar
        text(bp, 
             round(yearly, 2) - 0.5, 
             labels = round(yearly, 2), 
             cex = 0.9)
        dev.off()
}