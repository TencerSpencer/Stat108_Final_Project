# NOTE: TO RUN THIS, HAVE THE .csv AND THIS FILE IN THE SAME DIRECTORY THAT IS YOUR CURRENT R WORKING DIRECTORY

# ANOTHER NOTE. A GOAL OF THIS PROJECT IS TO SEE IF WE CAN RELATE POLLUTION TO MORTAILITY.

# CODE FOR PART ONE

# Function to check the correlation within a matrix. This code will be used again later.


# import and filter data
library('leaps')
dat <- read.table("mortality.csv", header = TRUE, sep = ",")


dat[,5] <- log(dat[,5])
dat[,6] <- log(dat[,6])
dat[,3] <- (dat[,3])^(1/3)
dat[,4] <- (dat[,4])^(1/3)


# remove cities
cities <- dat[,8]

filtered_model <- dat[,1:7]

# create a copy for later use
dat_copy <- filtered_model

# remove pollution terms

filtered_model$NOX = NULL
filtered_model$SO2 = NULL


# Produces matrix plot of the data
# plot(dat_no_city)

# END OF PORTION TO INCLUDE



# produce and check correlation matrix 

correlation_mat <- cor(filtered_model) # INCLUDE THIS PART IN APPENDIX

#Check_Correlation_Matrix(0, correlation_mat) # NOT THIS PART

# Produce a simple, linear fit object of our current data
first_fit <- lm(filtered_model$MORTALITY ~ ., data = filtered_model)
anova_values <- anova(first_fit)

# TYPE: anova_values

param_estimates <- first_fit$coefficients
# TYPE: param_estimates in the console to get our parameter estimates. We have 7 betas, 0-6, 0 being our intercept.
# FOR ESTIMATIONS OF STANDARD ERROR PER COEFFICIENT, TYPE summary(linear_fit) IN THE CONSOLE


# Note: Y-axis variable vs. x-axis variable
# Observed vs predicted
# Y values vs fitted Y values


plot(filtered_model$MORTALITY, first_fit$fitted.values, xlab = 'Fitted Y-Values', ylab = 'Y-Values', col = 'red',
     pch = 19, main = 'Observed Y-values vs Predicted Y-values',
     xlim = c(min(first_fit$fitted.values), max(first_fit$fitted.values)), 
     ylim = c(min(filtered_model$MORTALITY), max(filtered_model$MORTALITY)))




# WORK ON THIS INTERPRETATION

# Interpretation: There are multiple depatures from the straight line, but its form is mostly linear, except for
# extremes. In the middle, points do not adhere to the line either
# There seems to be too much variation in our current model. Most notable is the suspicion that checking
# the histogram made apparent. Here, we see that there is less data near extremes and more situated in the middle.
# This further proves that we may have outliers or may necessitate the transforming or dropping of a variable

# GO BACK TO THIS AND THE HIST B4, I THINK THIS IS AN ISSUE WITH ERROR INSTEAD.


# prepare data
x_values <- filtered_model[,1:4]
y_values <- filtered_model[,5]


# PERFORMING STEPWISE PROCEDURES RIGHT HERE
#backward_stepwise <- step(lm(filtered_model$MORTALITY ~ ., data = filtered_model), ~ 1, direction = 'both')

# PERFORM SUBSET PROCEDURES HERE


fit <- leaps(x=x_values, y = y_values, method = 'Cp')
ind = order(fit$Cp, decreasing = FALSE)
fit$Cp[ind]
best_fits <- fit$which[ind,]

fit2 <- leaps(x=x_values, y = y_values, method = 'adjr2')
ind2 = order(fit2$adjr2, decreasing = TRUE)
fit$Cp[ind2]
best_fits2 <- fit$which[ind2,]



# Pull from original data and remove NOx

filtered_model_with_NOX <- dat[,1:7]
filtered_model_with_NOX$SO2 = NULL

fit_with_NOX <- lm(filtered_model_with_NOX$MORTALITY ~ ., data = filtered_model_with_NOX)


# Perform regression measures on nitrogen model

# prepare data
x_values <- filtered_model_with_NOX[,1:5]
y_values <- filtered_model_with_NOX[,6]

# perform a stepwise
# backward_stepwise_NOX <- step(lm(filtered_model_with_NOX$MORTALITY ~ ., data = filtered_model_with_NOX), 
                             # ~ 1, direction = 'both')

fit <- leaps(x=x_values, y = y_values, method = 'Cp')
ind = order(fit$Cp, decreasing = FALSE)
fit$Cp[ind]
best_fits <- fit$which[ind,]

fit2 <- leaps(x=x_values, y = y_values, method = 'adjr2')
ind2 = order(fit2$adjr2, decreasing = TRUE)
fit$Cp[ind2]
best_fits2 <- fit$which[ind2,]

#dat[,5] <- log(dat[,5])
#dat[,6] <- log(dat[,6])
#dat[,3] <- (dat[,3])^(1/3)
#dat[,4] <- (dat[,4])^(1/3)

# Sulfur and Nitrogen added operations

Sulfur_and_Nitrogen_Added <- dat_copy

Sulfur_and_Nitrogen_Added_fit <- lm(Sulfur_and_Nitrogen_Added$MORTALITY ~., data = Sulfur_and_Nitrogen_Added)

# Perform regression measures on nitrogen and sulfur model

# prepare data
x_values <- Sulfur_and_Nitrogen_Added[,1:6]
y_values <- Sulfur_and_Nitrogen_Added[,7]

# perform a stepwise

#backward_stepwise_NOX <- step(lm(Sulfur_and_Nitrogen_Added$MORTALITY ~ ., data = Sulfur_and_Nitrogen_Added), 
                             # ~ 1, direction = 'both')

fit <- leaps(x=x_values, y = y_values, method = 'Cp')
ind = order(fit$Cp, decreasing = FALSE)
fit$Cp[ind]
best_fits <- fit$which[ind,]

fit2 <- leaps(x=x_values, y = y_values, method = 'adjr2')
ind2 = order(fit2$adjr2, decreasing = TRUE)
fit$Cp[ind2]
best_fits2 <- fit$which[ind2,]


# make new model and remove nitrogen and poor

final_model <- dat[,1:7]
final_model$NOX = NULL
final_model$POOR = NULL

correlation_mat <- cor(final_model)

final_model_line <- lm(final_model$MORTALITY ~ ., data = final_model)

residuals <- final_model_line$residuals

PRECIP <- final_model[,1]
EDU <- final_model[,2]
NONWHITE <- final_model[,3]
SO2 <- final_model[,4]



# FOR PRECIP:

# Residuals on the y-axis, independent value on the x-axis
plot(PRECIP, residuals, xlab = 'PRECIP', ylab = 'Residuals', col = 'red',
     pch = 19, main = 'Residuals vs PRECIP', xlim = c(min(PRECIP), max(PRECIP)),
     ylim = c(min(residuals), max(residuals)))

# draw line through origin
abline(0, 0, lty = 2)


# FOR EDU:

# Residuals on the y-axis, independent value on the x-axis
plot(EDU, residuals, xlab = 'EDU', ylab = 'Residuals', col = 'red',
     pch = 19, main = 'Residuals vs EDU', xlim = c(min(EDU), max(EDU)),
     ylim = c(min(residuals), max(residuals)))

# draw line through origin
abline(0, 0, lty = 2)

# FOR NONWHITE:

# Residuals on the y-axis, independent value on the x-axis
plot(NONWHITE, residuals, xlab = 'NONWHITE', ylab = 'Residuals', col = 'red',
     pch = 19, main = 'Residuals vs NONWHITE', xlim = c(min(NONWHITE), max(NONWHITE)),
     ylim = c(min(residuals), max(residuals)))

# draw line through origin
abline(0, 0, lty = 2)

# FOR SO2:

# Residuals on the y-axis, independent value on the x-axis
plot(SO2, residuals, xlab = 'SO2', ylab = 'Residuals', col = 'red',
     pch = 19, main = 'Residuals vs SO2', xlim = c(min(SO2), max(SO2)),
     ylim = c(min(residuals), max(residuals)))

# draw line through origin
abline(0, 0, lty = 2)



# build a normality plot of residuals
qqnorm(residuals, col = 'firebrick', pch = 19, main = 'Normal probability plot of residuals')
qqline(residuals)


plot(final_model$MORTALITY, final_model_line$fitted.values, xlab = 'Fitted Y-Values', ylab = 'Y-Values', col = 'red',
     pch = 19, main = 'Observed Y-values vs Predicted Y-values',
     xlim = c(min(final_model_line$fitted.values), max(final_model_line$fitted.values)), 
     ylim = c(min(final_model$MORTALITY), max(final_model$MORTALITY)))
