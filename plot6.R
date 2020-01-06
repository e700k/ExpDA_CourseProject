# The function intends to answer the following question:
# "Compare emissions from motor vehicle sources in Baltimore City 
# with emissions from motor vehicle sources in Los Angeles County, 
# California. Which city has seen greater changes over time in motor 
# vehicle emissions?"

library(dplyr)
library(ggplot2)

create.plot6 <- function() {
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
        # Filter to only motor vehicle-related sources in Baltimore City and Los Angeles counties
        data <- data %>% 
                filter(fips %in% c("24510", "06037") & SCC %in% vehsources) %>% 
                group_by(fips, year) %>% 
                summarise(total = sum(Emissions))
        # Replace FIPS with county name                  
        data$fips <- gsub("06037", "Los Angeles, CA (06037)", data$fips)
        data$fips <- gsub("24510", "Baltimore City, MD (24510)", data$fips)
        
        # Plot the data on ggplot and save it as PNG
        plot <- ggplot(data,
                       aes(as.factor(year), total)) + 
                geom_bar(
                        position="dodge",
                        stat="identity",
                        colour="black",
                        fill = "azure3") +
                facet_grid(fips ~ ., 
                           scales = "free") + 
                labs(title = "Motor vehicle-related PM2.5 emission in Baltimore City, MD and Los Angeles, CA") +
                xlab("Year") +
                ylab("PM2.5 emission (Tons)") +
                geom_text(label = round(data$total,2), nudge_y = rep(c(-200, -15),each=4))
        ggsave("plot6.png", plot, width = 8, height = 8, dpi = 150, units = "in")
}