# The function intends to answer the following question:
# "Of the four types of sources indicated by the type 
# (point, nonpoint, onroad, nonroad) variable, which of 
# these four sources have seen decreases in emissions from 
# 1999–2008 for Baltimore City? Which have seen increases 
# in emissions from 1999–2008?"

library(dplyr)
library(ggplot2)

create.plot3 <- function() {
        # Check if input file is in the working directory
        if (!file.exists("summarySCC_PM25.rds")) {
                stop("Input file was missing")
                geterrmessage()
        }
        
        # Read the data file
        data <- readRDS("summarySCC_PM25.rds")
        # Filter to only Baltimore City, Maryland
        # group by type and year and summarize to get the numbers to plot
        data <- data %>% 
                filter(fips == "24510") %>% 
                group_by(type, year) %>% 
                summarise(total = sum(Emissions))
        
        # Plot the data on ggplot and save it as PNG
        plot <- ggplot(data,
                       aes(type,total,fill=as.factor(year))) + 
                geom_bar(
                        position="dodge",
                        stat="identity",
                        colour="black") + 
                labs(
                        fill = "Year",
                        title = "PM2.5 emission in the Baltimore City, MD per type") +
                xlab("Type") +
                ylab("PM2.5 emission (Tons)") +
                scale_fill_brewer()
        ggsave("plot3.png", plot, width = 8, height = 8, dpi = 150, units = "in")
}