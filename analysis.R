library(ggplot2)

# Read in data
data <- read.csv("repdata_data_StormData.csv.bz2")

# Extract variables of interest
variables <- c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")
data <- data[, variables]

# Grouping similar events
data$group <- "OTHER"
data$group[grep("WET", data$EVTYPE, ignore.case=TRUE)] <- "WET"
data$group[grep("WINTER", data$EVTYPE, ignore.case=TRUE)] <- "WINTER"
data$group[grep("VOLCANIC", data$EVTYPE, ignore.case=TRUE)] <- "VOLCANIC"
data$group[grep("FOG", data$EVTYPE, ignore.case=TRUE)] <- "FOG"
data$group[grep("DRY", data$EVTYPE, ignore.case=TRUE)] <- "DRY"
data$group[grep("HAIL", data$EVTYPE, ignore.case=TRUE)] <- "HAIL"
data$group[grep("HEAT", data$EVTYPE, ignore.case=TRUE)] <- "HEAT"
data$group[grep("FLOOD", data$EVTYPE, ignore.case=TRUE)] <- "FLOOD"
data$group[grep("STORM", data$EVTYPE, ignore.case=TRUE)] <- "STORM"
data$group[grep("RAIN", data$EVTYPE, ignore.case=TRUE)] <- "RAIN"
data$group[grep("SNOW", data$EVTYPE, ignore.case=TRUE)] <- "SNOW"
data$group[grep("TORNADO", data$EVTYPE, ignore.case=TRUE)] <- "TORNADO"
data$group[grep("WIND", data$EVTYPE, ignore.case=TRUE)] <- "WIND"
sort(table(data$group), decreasing=TRUE)

# Transforming the exponents 
data$PROPDMGEXP[!grepl("K|M|B", data$PROPDMGEXP, ignore.case=TRUE)] <- 0
data$PROPDMGEXP[grep("K", data$PROPDMGEXP, ignore.case=TRUE)] <- 3
data$PROPDMGEXP[grep("M", data$PROPDMGEXP, ignore.case=TRUE)] <- 6
data$PROPDMGEXP[grep("B", data$PROPDMGEXP, ignore.case=TRUE)] <- 9
data$CROPDMGEXP[!grepl("K|M|B", data$CROPDMGEXP, ignore.case=TRUE)] <- 0
data$CROPDMGEXP[grep("K", data$CROPDMGEXP, ignore.case=TRUE)] <- 3
data$CROPDMGEXP[grep("M", data$CROPDMGEXP, ignore.case=TRUE)] <- 6
data$CROPDMGEXP[grep("B", data$CROPDMGEXP, ignore.case=TRUE)] <- 9

# Calculating property and crop damage
data$prop <- with(data, as.numeric(PROPDMG) * 10^as.numeric(PROPDMGEXP)) / 10^9
data$crop <- with(data, as.numeric(CROPDMG) * 10^as.numeric(CROPDMGEXP)) / 10^9

# Aggregating health and economic data
health_impact <- aggregate(x=list(impact = data$FATALITIES + data$INJURIES), by=list(event = data$group), FUN=sum, na.rm=TRUE)
economic_impact <- aggregate(x=list(impact = data$prop + data$crop), by=list(event = data$group), FUN=sum, na.rm=TRUE)

# Creating plots
health_impact.plot <- ggplot(health_impact, aes(x=impact, y=event)) +
  geom_bar(stat="identity") + 
  labs(title="Health Impact of Weather Events in the US", y="Weather Event", x="Fatalities + Injuries")
economic_impact.plot <- ggplot(economic_impact, aes(x=impact, y=event)) +
  geom_bar(stat="identity") + 
  labs(title="Economic Impact of Weather Events in the US", y="Weather Event", x="Property + Crop Damage")