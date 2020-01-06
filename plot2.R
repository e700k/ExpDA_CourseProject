# The function intends to answer the following question:
# "Have total emissions from PM2.5 decreased in the Baltimore City,
# Maryland from 1999 to 2008?"

create.plot2 <- function() {
        # Check if input file is in the working directory
        if (!file.exists("summarySCC_PM25.rds")) {
                stop("Input file was missing")
                geterrmessage()
        }
        
        # Read the data file
        data <- readRDS("summarySCC_PM25.rds")
        # Filter to only Baltimore City, Maryland
        data <- data[data$fips == "24510", ]
        # Calculate the yearly totals
        yearly <- tapply(data$Emissions, data$year, sum)
        # Apply scale of thousands
        yearly <- yearly / 10^3
        
        # Plot the data on a barplot, through the PNG device
        png("plot2.png")
        bp <- barplot(yearly, 
                      names.arg = names(yearly), 
                      xlab = "Year", 
                      ylab = "PM2.5 emission (Thousand Tons)", 
                      col = "azure3", 
                      ylim = c(0, 4),
                      main = "PM2.5 emission in the Baltimore City, MD")
        # Add the values to each bar
        text(bp, 
             round(yearly, 2) - 0.2, 
             labels = round(yearly, 2), 
             cex = 0.9)
        dev.off()
}