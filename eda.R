## ----read-data-----------------------------------------------------------
hvac <- read.csv("HVAC.csv", header = T, stringsAsFactors = F)
building <- read.csv("building.csv", header = T, stringsAsFactors = F)
head(hvac)
head(building)
dim(hvac)
dim(building)

## ----summary-stats-------------------------------------------------------
summary(building)
summary(hvac)


## ----factor-conv---------------------------------------------------------
build_fctrs <- c("BuildingID", "Country")
building[build_fctrs] <- lapply(building[build_fctrs], as.factor)
hvac$BuildingID <- as.factor(hvac$BuildingID)
summary(hvac)
summary(building)



## ----date-conv-----------------------------------------------------------
head(hvac)
hvac$Date_Time <- as.POSIXct(paste(hvac$Date, 
                                   hvac$Time, sep = " "), 
                             format = "%m/%d/%y %H:%M:%S")
hvac$Date <- as.Date(hvac$Date_Time)
hvac$Time <- strftime(hvac$Date_Time, format = "%H:%M:%S")
head(hvac)

## ----merge-df------------------------------------------------------------

build_temps <- merge(building, hvac, by = "BuildingID")
head(build_temps)


## ----diff-temp-----------------------------------------------------------
build_temps$DiffTemp <- with(build_temps, ActualTemp - TargetTemp)


## ----plot-temp-----------------------------------------------------------
library(ggplot2)
ggplot(build_temps, aes(x = BuildingID, y = DiffTemp, fill = BuildingID)) + geom_boxplot() + guides(fill = F) + theme_bw()


## ----plot-country--------------------------------------------------------
ggplot(build_temps, aes(x = Country, y = DiffTemp, fill = Country)) + geom_boxplot() + guides(fill = F) + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

## ----group-exc, message = F, warning = F---------------------------------
library(dplyr)
build_temps <- build_temps %>% mutate(TempExcursion = ifelse(DiffTemp > 0, "Excursion", "Within Limits"), TempFlag = ifelse(DiffTemp > 0, 1, 0))
excursions <- build_temps %>% group_by(TempExcursion, BuildingID) %>% summarize(Count = n())
ggplot(excursions, aes(x = BuildingID, y = Count, fill = TempExcursion)) + geom_bar(stat = 'identity', position = 'stack') + theme_bw()

## ----split-ds------------------------------------------------------------
range(build_temps$Date)
build_temps$SplitValue <- ifelse(build_temps$Date > "2013-06-15", "Test", "Train")
split_df <- split(build_temps, build_temps$SplitValue)
names(split_df)
lapply(split_df, dim)
lapply(split_df, function(x) range(x$Date))

## ----train-model---------------------------------------------------------
temp_formula <- TempFlag ~ BuildingID + BuildingMgr + BuildingAge + HVACproduct + Country + Time + System + SystemAge
train_model <- glm(temp_formula, family = 'binomial', data = split_df[["Train"]])

## ----score-model---------------------------------------------------------
predictions <- predict(train_model, newdata = split_df[["Test"]], 
                       type = 'response')
summary(predictions)

## ----est-auc-------------------------------------------------------------
install.packages('pROC')
library(pROC)
roc_curve <- roc(split_df[["Test"]]$TempFlag, predictions)
auc_value <- auc(roc_curve)
auc_value
plot(roc_curve)

