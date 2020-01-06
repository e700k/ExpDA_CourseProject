# The function intends to answer the following question:
# "Across the United States, how have emissions from coal
# combustion-related sources changed from 1999–2008?"

library(dplyr)

create.plot4 <- function() {
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
        # Subtract coal-combustion related sources
        coalsources <- as.vector(code[grepl("coal", tolower(code$EI.Sector)), ]$SCC)
        # Filter to only coal combustion-related sources
        data <- filter(data, data$SCC %in% coalsources)
        
        # Group by type and year and summarize to get the numbers to plot
        yearly <- tapply(data$Emissions, data$year, sum)
        # Scale to millions
        yearly <- yearly / 10^6
        
        # Plot the data on a barplot, through the PNG device
        png("plot4.png")
        bp <- barplot(yearly, 
                      names.arg = names(yearly), 
                      xlab = "Year", 
                      ylab = "PM2.5 emission (Million Tons)", 
                      col = "azure3", 
                      ylim = c(0, 0.6),
                      main = "Coal combustion-related PM2.5 emission in the USA")
        # Add the values to each bar
        text(bp, 
             round(yearly, 2) - 0.02, 
             labels = round(yearly, 4), 
             cex = 0.9)
        dev.off()
}