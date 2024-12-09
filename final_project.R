knitr::opts_chunk$set(echo = TRUE)
public_data <- read.csv('./data/NA_HQ_public_data.csv')
svi_data <- read.csv('./data/SVI2018_US.csv')
us_cities <- read.csv('./data/uscities.csv')
library(pander)

# QUESTION 1
pander(quantile(svi_data$EP_UNEMP, p = c(0, 0.25, 0.5, 0.75, 1)) )
sum(svi_data$EP_UNEMP == -999)
# We seem to get some -999, which doesn't make sense in the context of estimated unemployment rates. In this column, we have 546 values / 72837 observations encoded as -999. 

pander(quantile(svi_data$EP_PCI, p = c(0, 0.25, 0.5, 0.75, 1)))
sum(svi_data$EP_PCI == -999)
# The EP_PCI column also has 481 values encoded as -999. 

# Change the -999 to NaNs
svi_data$EP_UNEMP[svi_data$EP_UNEMP == -999] <- NA
svi_data$EP_PCI[svi_data$EP_PCI == -999] <- NA

# Filter out the NaNs and store them in another dataset called "data".
data <- svi_data[complete.cases(svi_data$EP_PCI, svi_data$EP_UNEMP), ]

plot(data$EP_UNEMP, data$EP_PCI, 
     xlab = 'Estimated Unemployment Rate', 
     ylab = 'Estimated Per Capita Income', 
     main = 'Trend in Estimated Per Capita Income Based on Estimated Unemp. Rate')

model <- lm(EP_PCI ~ EP_UNEMP, data = data)
res <- residuals(model)
plot(data$EP_UNEMP, res, 
     xlab = 'Estimated Unemployment Rate', 
     ylab = 'Estimated Per Capita Income', 
     main = 'Trend in Estimated Per Capita Income Based on Estimated Unemp. Rate')
abline(0,0, col = 'red')

qqnorm(res)
qqline(res, col = 'red')

# double log scale
data$log_unemp <- log(data$EP_UNEMP + 1e-6)
data$log_pci <- log(data$EP_PCI + 1e-6)

log_model <- lm(log_pci ~ log_unemp, data = data)

# change predictions back to the original scale
predictions <- predict(log_model, newdata = data.frame(log_unemp = data$log_unemp))
predictions <- exp(predictions)

# Plot the scatterplot
plot(data$log_unemp, data$EP_PCI,
     main = 'Estimated Per Capita Income as a Function of Log(Estimated Populated Rate)',
     xlab = 'Log(Estimated Population Rate)',
     ylab = 'Estimated Per Capita Income')

lines(data$log_unemp, predictions, col='red')

# Model evaluation
# evaluating log model
res <- residuals(log_model)
mse <- mean(res ** 2) # 0.201

# evaluating original model for interpretation purposes
mse_original <- mean((data$EP_PCI - predictions)^2)

# QUESTION 2
par(mfrow=c(1, 2))
hist(data$EP_UNEMP, main = "EP_UNEMP", xlab = "Unemployment (%)", col = "lightblue")

hist(data$EP_PCI, main = "EP_PCI", xlab = "Per Capita Income", col = "lightgreen")
# remove Nans from data
data[data == -999] <- NA
data <- data[complete.cases(data), ]

par(mfrow=c(1, 2))

# plot histograms of EP_UNINSUR and EP_MOBILE
hist(data$EP_UNINSUR, main = "EP_UNINSUR", xlab = "Uninsured (%)", col = "pink")
hist(data$EP_MOBILE, main = "EP_MOBILE", xlab = "Mobile Homes (%)", col = "yellow")

# KS test
ks_test <- ks.test(data$EP_UNEMP, data$EP_PCI, exact=TRUE)
pander(ks_test)

ks_test <- ks.test(data$EP_UNEMP, data$EP_UNINSUR, exact=TRUE)
pander(ks_test)

ks_test <- ks.test(data$EP_UNEMP, data$EP_MOBILE, exact=TRUE)
pander(ks_test)

ks_test <- ks.test(data$EP_PCI, data$EP_UNINSUR, exact=TRUE)
pander(ks_test)

ks_test <- ks.test(data$EP_PCI, data$EP_MOBILE, exact=TRUE)
pander(ks_test)

# QUESTION 3
library(dplyr)
filtered <- public_data %>%
  filter(
    hq_country == "United States of America", # filter for only locations in the US
    address_state != "", 
    address_city != "",
    address_state != 'DO NOT SEND LETTER', # doesn't have state or city
    address_state != 'England' # London, England, mistakenly recorded as a city, state in the US
  )
# Some states are spelled out by name; change to state abbreviation
filtered <- filtered %>%
  mutate(address_state = recode(
    address_state, 
    "Massachusetts" = "MA", 
    "Pennsylvania" = "PA",
    'California' = 'CA',
    'Wisconsin' = 'WI',
    'Virginia' = 'VA',
    'Texas' = 'TX',
    'Ca' = 'CA',
    'Utah' = 'UT',
    'Arizona' = 'AZ',
    'Chicago' = 'IL',
    'Illinois' = 'IL',
    'District of Columbia' = 'DC',
    'Washington' = 'WA',
    'Rhode Island' = 'RI',
    'Colorado' = 'CO',
    'COLORADO' = 'CO',
    'Michigan' = 'MI',
    'New York' = 'NY',
    'Ohio' = 'OH',
    'Delaware' = 'DE',
    'Georgia' = 'GA',
    'Minnesota' = 'MN',
    'Missouri' = 'MO',
    'Maryland' = 'MD'
  ))

filtered_df <- as.data.frame(filtered)

# Merge the data filtered from the SVI DataFrame (shows the the socieoeconomic vulnerability index for various cities in the US) with the public city data. 
merged_df <- merge(
  x = filtered_df[, c('theme', 'address_state')],
  y = data[, c('ST_ABBR', 'EP_PCI')],
  by.x = 'address_state',
  by.y = 'ST_ABBR',
  all = FALSE
)

median_pci <- median(merged_df$EP_PCI, na.rm = TRUE)

# Create a column that says whether the city's EP_PCI is higher or lower compared to the median value
merged_df <- merged_df %>%
  mutate(pci_status = ifelse(EP_PCI > median_pci, 'Higher', 'Lower'))

# Our data currently contains climate change and water action data. We will filter for climate action data since it is what we care about more. 
climate_data <- merged_df %>%
  filter(theme == 'Climate Change')

# Create a table that summarizes the number of actions taken per each location that are related to climate change. 
summary_df <- climate_data %>%
  group_by(address_state, pci_status) %>%
  summarize(
    count = n(),
    .groups = 'drop'
  )

# Is the distribution of counts of climate action normally distributed for values higher than the median?
higher_counts <- summary_df$count[summary_df$pci_status == "Higher"]
shapiro.test(higher_counts)

lower_counts <- summary_df$count[summary_df$pci_status == "Lower"]
shapiro.test(lower_counts)

wilcox.test(count ~ pci_status, data = summary_df, alternative = 'less')

# QUESTION 4
correlation <- cor(data$EP_PCI, data$EP_UNEMP, use = "complete.obs", method = "pearson")

cor_test <- cor.test(data$EP_PCI, data$EP_UNEMP, method = "pearson")
pander(cor_test)

# ADVANCED ANALYSIS
boxplot(data$EP_PCI, horizontal = TRUE, xlab = "Per Capita Income (EP_PCI)")
library(xgboost)
library(caret)

orig_features <- data[, c('EP_NOHSDP', 'EP_DISABL', 'EP_MINRTY')]
orig_target <- data$EP_PCI

Q1 <- quantile(orig_target, 0.25)
Q3 <- quantile(orig_target, 0.75)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

data_filtered <- data[orig_target >= lower_bound & orig_target <= upper_bound, ]

# Update our features & target! 
features <- data_filtered[, c('EP_NOHSDP', 'EP_DISABL', 'EP_MINRTY')]
target <- data_filtered$EP_PCI
# Split into train + test
set.seed(123)
train_index <- createDataPartition(target, p = 0.8, list = FALSE)
train_features <- features[train_index, ]
train_target <- target[train_index]
test_features <- features[-train_index, ]
test_target <- target[-train_index]

# convert to matrices
train_matrix <- as.matrix(train_features)
test_matrix <- as.matrix(test_features)

# DMatrix objects
dtrain <- xgb.DMatrix(data = train_matrix, label = train_target)
dtest <- xgb.DMatrix(data = test_matrix, label = test_target)

# hyperparameters
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.1,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.8
)
# Train the model
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 500,
  watchlist = list(train = dtrain, test = dtest),
  early_stopping_rounds = 10,
  verbose = 1
)

# PREDICTIONS
predictions <- predict(xgb_model, test_matrix)

# MODEL EVALUATIONS
# RMSE
rmse <- sqrt(mean((test_target - predictions) ^ 2))
cat("RMSE:", rmse, "\n")

# MAE
mae <- mean(abs(test_target - predictions))
cat("MAE:", mae, "\n")

# FEATURE IMPORTANCE
importance <- xgb.importance(feature_names = colnames(train_matrix), model = xgb_model)
xgb.plot.importance(importance)
cross_val <- xgb.cv(
  params = params,
  data = dtrain,
  nrounds = 500,
  nfold = 5,
  metrics = 'rmse',
  early_stopping_rounds = 10,
  verbose = 1,
  prediction = TRUE
)
best_nrounds <- cross_val$best_iteration
cat("Best number of rounds:", best_nrounds, "\n") # 56
# Train the model
new_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = best_nrounds,
  watchlist = list(train = dtrain, test = dtest),
  early_stopping_rounds = 10,
  verbose = 1
)

# PREDICTIONS
new_predictions <- predict(xgb_model, test_matrix)

# MODEL EVALUATIONS
# RMSE
new_rmse <- sqrt(mean((test_target - predictions) ^ 2))
cat("RMSE:", rmse, "\n")

# MAE
new_mae <- mean(abs(test_target - predictions))
cat("MAE:", mae, "\n")
res <- test_target - predictions
plot(res, main = "Residuals of the Model", xlab = "Per Capita Income", ylab = "Residuals")
abline(h = 0, col = "red")