install.packages("tidyverse")
install.packages("magrittr")
install.packages("dplyr")
install.packages("arules")
install.packages("arulesViz")

library(tidyverse)
library(magrittr)
library(dplyr)
library(arules)
library(arulesViz)

list_of_files <- list.files(path = "./training data/",
                            pattern = "\\.csv$",
                            full.names = TRUE)
DATASET <- list_of_files %>% 
  map_dfr(read.csv, header=TRUE, fill=TRUE)


##DATA CLEANING##

DATASET$DAY_OF_WEEK = as.character(DATASET$DAY_OF_WEEK)
DATASET$DAY_OF_WEEK = gsub("^1", "Monday", DATASET$DAY_OF_WEEK)
DATASET$DAY_OF_WEEK = gsub("^2", "Tuesday", DATASET$DAY_OF_WEEK)
DATASET$DAY_OF_WEEK = gsub("^3", "Wednesday", DATASET$DAY_OF_WEEK)
DATASET$DAY_OF_WEEK = gsub("^4", "Thursday", DATASET$DAY_OF_WEEK)
DATASET$DAY_OF_WEEK = gsub("^5", "Friday", DATASET$DAY_OF_WEEK)
DATASET$DAY_OF_WEEK = gsub("^6", "Saturday", DATASET$DAY_OF_WEEK)
DATASET$DAY_OF_WEEK = gsub("^7", "Sunday", DATASET$DAY_OF_WEEK)

DATASET$DEP_DELAY = ordered(cut(DATASET$DEP_DELAY, breaks = c(15, 30, 60, 120, Inf)), labels = c("Short", "Medium", "Long", "Severe"))

DATASET$ARR_DELAY = ordered(cut(DATASET$ARR_DELAY, breaks = c(15, 30, 60, 120, Inf)), labels = c("Short", "Medium", "Long", "Severe"))

DATASET$WEATHER_DELAY = ordered(cut(DATASET$WEATHER_DELAY, breaks = c(15, 30, 60, 120, Inf)), labels = c("Short", "Medium", "Long", "Severe"))

DATASET$CARRIER_DELAY = ordered(cut(DATASET$CARRIER_DELAY, breaks = c(15, 30, 60, 120, Inf)), labels = c("Short", "Medium", "Long", "Severe"))

DATASET$NAS_DELAY = ordered(cut(DATASET$NAS_DELAY, breaks = c(15, 30, 60, 120, Inf)), labels = c("Short", "Medium", "Long", "Severe"))

DATASET$SECURITY_DELAY = ordered(cut(DATASET$SECURITY_DELAY, breaks = c(15, 30, 60, 120, Inf)), labels = c("Short", "Medium", "Long", "Severe"))

DATASET$LATE_AIRCRAFT_DELAY = ordered(cut(DATASET$LATE_AIRCRAFT_DELAY, breaks = c(15, 30, 60, 120, Inf)), labels = c("Short", "Medium", "Long", "Severe"))

DATASET$CRS_DEP_TIME = ordered(cut(DATASET$CRS_DEP_TIME, breaks = c(0, 500, 900, 1100, 1300, 1700, 2000, 2300, 2400)), 
                               labels = c("Overnight", "Early Morning", "Mid-Morning", "Late Morning","Afternoon", "Early Evening", "Late Evening", "Overnight"))

DATASET$CRS_ARR_TIME = ordered(cut(DATASET$CRS_ARR_TIME, breaks = c(0, 500, 900, 1100, 1300, 1700, 2000, 2300, 2400)), 
                               labels = c("Overnight", "Early Morning", "Mid-Morning", "Late Morning","Afternoon", "Early Evening", "Late Evening", "Overnight"))

DATASET$DEP_TIME = ordered(cut(DATASET$DEP_TIME, breaks = c(0, 500, 900, 1100, 1300, 1700, 2000, 2300, 2400)), 
                           labels = c("Overnight", "Early Morning", "Mid-Morning", "Late Morning","Afternoon", "Early Evening", "Late Evening", "Overnight"))

DATASET$ARR_TIME = ordered(cut(DATASET$ARR_TIME, breaks = c(0, 500, 900, 1100, 1300, 1700, 2000, 2300, 2400)), 
                           labels = c("Overnight", "Early Morning", "Mid-Morning", "Late Morning","Afternoon", "Early Evening", "Late Evening", "Overnight"))

DATASET$DISTANCE = ordered(cut(DATASET$DISTANCE, breaks = c(0, 250, 500, 750, 1000, 1250, 1500, 2000, Inf)), 
                           labels = c("< 250 miles", "250-499 miles", "500-749 miles", "750-999 miles", "1000-1249 miles", "1250-1499 miles", "1500-2000 miles", "> 2000 miles"))

##RULES GENERATION##

##%pin% vs. %oin% for subsets
# subset0 <- subset(rules, subset = rhs %pin% "ARR_DELAY=" ) //%pin% restricts side to include attribute given but other attributes can appear with it (in lhs)
# subset1 <- subset(rules, subset = lhs %oin% "CRS_ARR_TIME=Early Morning") // %oin% restricts side to only include the given label

#rules with ARR_DELAY on RHS
D0 <- DATASET[, !(names(DATASET) %in% c("DEST_AIRPORT_ID", "ORIGIN_AIRPORT_ID", "DAY_OF_MONTH", "NAS_DELAY", "DEP_DELAY", "WEATHER_DELAY", "CARRIER_DELAY", "SECURITY_DELAY", "LATE_AIRCRAFT_DELAY", "MONTH", "YEAR", "ORIGIN_CITY_NAME", "ORIGIN_STATE_NM", "DEST_CITY_NAME", "DEST_STATE_NM", "DEP_TIME", "ARR_TIME", "DEP_DEL15", "ARR_DEL15", "CANCELLED", "CANCELLATION_CODE", "DIVERTED", "X"))]
D0$DAY_OF_WEEK = factor(D0$DAY_OF_WEEK)

rules = apriori(D0, parameter = list(supp = 0.001, conf = 0.04, minlen = 2, maxlen = 3), appearance = list(rhs = c('ARR_DELAY=Short', 'ARR_DELAY=Medium', 'ARR_DELAY=Long', 'ARR_DELAY=Severe')))

#DAY OF WEEK on LHS subsets:
subset0 <- subset(rules, subset = lhs %pin% "DAY_OF_WEEK=Sunday" & lift > 1.2)
inspect(sort(subset0, by = "lift"))

subset0 <- subset(rules, subset = lhs %pin% "DAY_OF_WEEK=Monday" & lift > 1.2)
inspect(sort(subset0, by = "lift"))

subset0 <- subset(rules, subset = lhs %pin% "DAY_OF_WEEK=Tuesday" & lift > 1.2)
inspect(sort(subset0, by = "lift"))

subset0 <- subset(rules, subset = lhs %pin% "DAY_OF_WEEK=Wednesday" & lift > 1.2)
inspect(sort(subset0, by = "lift"))

subset0 <- subset(rules, subset = lhs %pin% "DAY_OF_WEEK=Thursday" & lift > 1.2)
inspect(sort(subset0, by = "lift"))

subset0 <- subset(rules, subset = lhs %pin% "DAY_OF_WEEK=Friday" & lift > 1.2)
inspect(sort(subset0, by = "lift"))

subset0 <- subset(rules, subset = lhs %pin% "DAY_OF_WEEK=Saturday" & lift > 1.2)
inspect(sort(subset0, by = "lift"))


#DISTANCE on LHS subsets:  
subset0 <- subset(rules, subset = lhs %pin% "DISTANCE=< 250 miles" & lift > 1.2)
inspect(sort(subset0, by = "lift"))

subset0 <- subset(rules, subset = lhs %pin% "DISTANCE=250-499 miles" & lift > 1.2)
inspect(sort(subset0, by = "lift"))

subset0 <- subset(rules, subset = lhs %pin% "DISTANCE=500-749 miles" & lift > 1.2)
inspect(sort(subset0, by = "lift"))

subset0 <- subset(rules, subset = lhs %pin% "DISTANCE=750-999 miles" & lift > 1.2) #we start to see ARR_DELAY=Short from 750 miles +
inspect(sort(subset0, by = "lift"))

subset0 <- subset(rules, subset = lhs %pin% "DISTANCE=1000-1249 miles" & lift > 1.2)
inspect(sort(subset0, by = "lift"))

subset0 <- subset(rules, subset = lhs %pin% "DISTANCE=1500-2000 miles" & lift > 1.2)
inspect(sort(subset0, by = "lift"))

subset0 <- subset(rules, subset = lhs %pin% "DISTANCE=> 2000 miles" & lift > 1.2)
inspect(sort(subset0, by = "lift"))



#rules with NAS_DELAY on LHS
D1 <- DATASET[, !(names(DATASET) %in% c("DEST_AIRPORT_ID", "ORIGIN_AIRPORT_ID", "DAY_OF_MONTH", "DEP_DELAY", "ARR_DELAY", "WEATHER_DELAY", "CARRIER_DELAY", "SECURITY_DELAY", "LATE_AIRCRAFT_DELAY", "MONTH", "YEAR", "ORIGIN_CITY_NAME", "ORIGIN_STATE_NM", "DEST_CITY_NAME", "DEST_STATE_NM", "DEP_TIME", "ARR_TIME", "DEP_DEL15", "ARR_DEL15", "CANCELLED", "CANCELLATION_CODE", "DIVERTED", "X"))]
D1$DAY_OF_WEEK = factor(D1$DAY_OF_WEEK)

rules1 = apriori(D1, parameter = list(supp = 0.001, conf = 0.04, minlen = 2, maxlen = 3), appearance = list(lhs = c('NAS_DELAY=Short', 'NAS_DELAY=Medium', 'NAS_DELAY=Long', 'NAS_DELAY=Severe')))

subset1 <- subset(rules1, subset = rhs %pin% "DAY_OF_WEEK=" & lift > 1)
inspect(subset1)

subset1 <- subset(rules1, subset = rhs %pin% "CRS_ARR_TIME=" & lift > 1)
inspect(subset1)

subset1 <- subset(rules1, subset = rhs %pin% "DISTANCE=" & lift > 1)
inspect(subset1)



#rules with LATE_AIRCRAFT_DELAY on LHS
D2 <- DATASET[, !(names(DATASET) %in% c("DEST_AIRPORT_ID", "ORIGIN_AIRPORT_ID", "DAY_OF_MONTH", "DEP_DELAY", "ARR_DELAY", "NAS_DELAY", "CARRIER_DELAY", "WEATHER_DELAY", "SECURITY_DELAY", "MONTH", "YEAR", "ORIGIN_CITY_NAME", "ORIGIN_STATE_NM", "DEST_CITY_NAME", "DEST_STATE_NM", "DEP_TIME", "ARR_TIME", "DEP_DEL15", "ARR_DEL15", "CANCELLED", "CANCELLATION_CODE", "DIVERTED", "X"))]
D2$DAY_OF_WEEK = factor(D2$DAY_OF_WEEK)

rules2 = apriori(D2, parameter = list(supp = 0.001, conf = 0.04, minlen = 2, maxlen = 3), appearance = list(lhs = c('LATE_AIRCRAFT_DELAY=Short', 'LATE_AIRCRAFT_DELAY=Medium', 'LATE_AIRCRAFT_DELAY=Long', 'LATE_AIRCRAFT_DELAY=Severe')))

subset2 <- subset(rules2, subset = rhs %pin% "DAY_OF_WEEK=" & lift > 1)
inspect(subset2)

subset2 <- subset(rules2, subset = rhs %pin% "CRS_ARR_TIME=" & lift > 1)
inspect(subset2)

subset2 <- subset(rules2, subset = rhs %pin% "DISTANCE=" & lift > 1)
inspect(subset2)



#rules with CARRIER_DELAY on RHS
D3 <- DATASET[, !(names(DATASET) %in% c("DEST_AIRPORT_ID", "ORIGIN_AIRPORT_ID", "DAY_OF_MONTH", "DEP_DELAY", "ARR_DELAY", "NAS_DELAY", "WEATHER_DELAY", "SECURITY_DELAY", "LATE_AIRCRAFT_DELAY", "MONTH", "YEAR", "ORIGIN_CITY_NAME", "ORIGIN_STATE_NM", "DEST_CITY_NAME", "DEST_STATE_NM", "DEP_TIME", "ARR_TIME", "DEP_DEL15", "ARR_DEL15", "CANCELLED", "CANCELLATION_CODE", "DIVERTED", "X"))]
D3$DAY_OF_WEEK = factor(D3$DAY_OF_WEEK)

rules3 = apriori(D3, parameter = list(supp = 0.001, conf = 0.04, minlen = 2, maxlen = 3), appearance = list(rhs = c('CARRIER_DELAY=Short', 'CARRIER_DELAY=Medium', 'CARRIER_DELAY=Long', 'CARRIER_DELAY=Severe')))
inspect(sort(rules3, by = "lift"))



#WEATHER_DELAY ANALYSIS - NO RULES
D4 <- DATASET[, !(names(DATASET) %in% c("DEST_AIRPORT_ID", "ORIGIN_AIRPORT_ID", "DAY_OF_MONTH", "DEP_DELAY", "ARR_DELAY", "NAS_DELAY", "CARRIER_DELAY", "SECURITY_DELAY", "LATE_AIRCRAFT_DELAY", "MONTH", "YEAR", "ORIGIN_CITY_NAME", "ORIGIN_STATE_NM", "DEST_CITY_NAME", "DEST_STATE_NM", "DEP_TIME", "ARR_TIME", "DEP_DEL15", "ARR_DEL15", "CANCELLED", "CANCELLATION_CODE", "DIVERTED", "X"))]
D4$DAY_OF_WEEK = factor(D4$DAY_OF_WEEK)

rules4 = apriori(D4, parameter = list(supp = 0.001, conf = 0.04, minlen = 2, maxlen = 3), appearance = list(rhs = c('WEATHER_DELAY=Short', 'WEATHER_DELAY=Medium', 'WEATHER_DELAY=Long', 'WEATHER_DELAY=Severe')))
summary(rules4)
inspect(sort(rules4, by = "lift"))



#SECURITY_DELAY ANALYSIS - NO RULES
D5 <- DATASET[, !(names(DATASET) %in% c("DEST_AIRPORT_ID", "ORIGIN_AIRPORT_ID", "DAY_OF_MONTH", "DEP_DELAY", "ARR_DELAY", "NAS_DELAY", "CARRIER_DELAY", "WEATHER_DELAY", "LATE_AIRCRAFT_DELAY", "MONTH", "YEAR", "ORIGIN_CITY_NAME", "ORIGIN_STATE_NM", "DEST_CITY_NAME", "DEST_STATE_NM", "DEP_TIME", "ARR_TIME", "DEP_DEL15", "ARR_DEL15", "CANCELLED", "CANCELLATION_CODE", "DIVERTED", "X"))]
D5$DAY_OF_WEEK = factor(D5$DAY_OF_WEEK)

rules5 = apriori(D5, parameter = list(supp = 0.001, conf = 0.04, minlen = 2, maxlen = 3), appearance = list(rhs = c('SECURITY_DELAY=Short', 'SECURITY_DELAY=Medium', 'SECURITY_DELAY=Long', 'SECURITY_DELAY=Severe')))
summary(rules5)
inspect(sort(rules5, by = "lift"))



#rules with CANCELLED on LHS
D6 <- DATASET[, !(names(DATASET) %in% c("DEST_AIRPORT_ID", "ORIGIN_AIRPORT_ID", "DAY_OF_MONTH", "DEP_DELAY", "ARR_DELAY", "NAS_DELAY", "CARRIER_DELAY", "WEATHER_DELAY", "LATE_AIRCRAFT_DELAY", "MONTH", "YEAR", "ORIGIN_CITY_NAME", "ORIGIN_STATE_NM", "DEST_CITY_NAME", "DEST_STATE_NM", "DEP_TIME", "ARR_TIME", "DEP_DEL15", "ARR_DEL15", "CANCELLATION_CODE", "DIVERTED", "X"))]
D6$DAY_OF_WEEK = factor(D6$DAY_OF_WEEK)
D6$CANCELLED = factor(D6$CANCELLED)

rules6 = apriori(D6, parameter = list(supp = 0.001, conf = 0.04, minlen = 2, maxlen = 4), appearance = list(lhs = c('CANCELLED=1')))

library(arulesViz)
plot(subset(rules6, subset = lift > 1), method="graph")

subset6 <- subset(rules6, subset = rhs %pin% "DAY_OF_WEEK=" & lift > 1)
inspect(subset6)

subset6 <- subset(rules6, subset = rhs %pin% "CRS_DEP_TIME=" & lift > 1)
inspect(subset6)

subset6 <- subset(rules6, subset = rhs %pin% "DISTANCE=" & lift > 1)
inspect(subset6)
