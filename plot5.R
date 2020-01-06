# The function intends to answer the following question:
# "Across the United States, how have emissions from coal
# combustion-related sources changed from 1999–2008?"

library(dplyr)

create.plot5 <- function() {
        # Check if input file is in the working directory
        if (!file.exists("summarySCC_PM25.rds") | 
            !file.exists("Source_Classification_Code.rds")) {
                stop("Input file was missing")
                geterrmessage()
        }
        
        # Read the data file
        data <- readRDS("summarySCC_PM25.rds")
        # Read the Code file
        code <- readRDS("Source_Classification_Code.rds")
        # Subtract motor vehicle-related sources
        vehsources <- as.vector(code[grepl("vehicle", tolower(code$EI.Sector)), ]$SCC)
        # Filter to only motor vehicle-related sources in Baltimore City
        data <- filter(data, data$SCC %in% vehsources & data$fips == "24510")
        
        # Group by type and year and summarize to get the numbers to plot
        yearly <- tapply(data$Emissions, data$year, sum)
        # Scale to millions
        yearly <- yearly / 10^3
        
        # Plot the data on a barplot, through the PNG device
        png("plot5.png")
        bp <- barplot(yearly, 
                      names.arg = names(yearly), 
                      xlab = "Year", 
                      ylab = "PM2.5 emission (Thousand Tons)", 
                      col = "azure3", 
                      ylim = c(0, 0.4),
                      main = "Motor vehicle-related PM2.5 emission in Baltimore City, MD")
        # Add the values to each bar
        text(bp, 
             round(yearly, 2) - 0.02, 
             labels = round(yearly, 4), 
             cex = 0.9)
        dev.off()
}