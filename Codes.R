# Loading the required packages
library(readr)
library(epicasting)
library(Metrics)
library(forecast)
library(ggplot2)

# Importing the dataset
path <- "https://raw.githubusercontent.com/mad-stat/XEWNet/main/Dataset/Ahmedabad_data_weekly.csv"
data <- read_csv(path)
head(data)
cases = data$Cases

# Preparing train and test data
h <- 13 # length of forecast horizon
train <- subset(ts(cases), end = length(cases)-h)
tail(train, 3)
test <- tail(cases, h)

# Fitting the basic EWNet model
model <- ewnet(ts = train, NForecast = h)
model

# Fitting modified EWNet model

rain <- ts(data$Rainfall)
train_rain <- subset(rain, end = length(rain) - h)
test_rain <- head(train_rain, h)
model_2 <- ewnet(ts = train, NForecast = h, 
                 Waveletlevels = floor(log(length(ts))),
                 MaxARParam =  9,
                 PI = TRUE,
                 xreg_train = train_rain,
                 xreg_test = test_rain,
                 ret_fit = TRUE)


# Calculate RMSE between expected and predicted values
rmse(test, model_2$Forecast)


# Visualizing the training data
theme_set(
  theme_classic() +
    theme(legend.position = "right")
)

autoplot(train) + labs(
  title = "Ahmedabad Dengue Training ", 
  x = "Time (weeks)", y = "Cases")

# Visualizing expected and predicted values

final_output <- data.frame("Time" = 412:424)
final_output["EWNet_Forecast"] <- model_2$Forecast
final_output["Actual_Values"] <- test

ggplot(final_output, aes(x = Time)) +    # Create default ggplot2 line plot
  geom_line(aes(y = EWNet_Forecast), size = 1.5, color = 'blue')+ 
  geom_point(aes(y = Actual_Values), size = 3, color = 'red') + 
  labs(
    title = "Ahmedabad Dengue Forecast Horizon", 
    x = "Time (weeks)", y = "Cases")
