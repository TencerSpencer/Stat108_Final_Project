# NOTE: TO RUN THIS, HAVE THE .csv AND THIS FILE IN THE SAME DIRECTORY THAT IS YOUR CURRENT R WORKING DIRECTORY


# ANOTHER NOTE. A GOAL OF THIS PROJECT IS TO SEE IF WE CAN RELATE POLLUTION TO MORTAILITY.

# CODE FOR PART ONE

# Function to check the correlation within a matrix. This code will be used again later.

Check_Correlation_Matrix <- function(collinearity_restriction= 0, correlation_mat) {
  
  dimension_names <- dimnames(correlation_mat)
  
  # iterate rows and columns in matrix
  used_already <- 0 # placeholder
  for (i in 2:length(dimension_names[[1]])) {
    for (j in 2:length(dimension_names[[1]])) {
      
      # diagonals do not count
      if (i != j) {
        if (abs(correlation_mat[i,j]) >= collinearity_restriction) {
          
          # if we already pointed this value out, skip it
          if (sum(used_already == correlation_mat[i,j]) >= 1) {
            next
          }
          
          # add corr to list used already
          used_already <- c(used_already, correlation_mat[i,j])
          
          cat("Variables: ", dimension_names[[1]][i], " and " , dimension_names[[1]][j], 
              "have a correlation of : ", correlation_mat[i,j], "\n")
        }
      }
    }
  }
}


# ADD PORTION BELOW TO THE APPENDIX

# import and filter data
dat <- read.table("mortality.csv", header = TRUE, sep = ",")
cities <- dat[,8]
dat_no_city <- dat[,1:7]

# Produces matrix plot of the data
# plot(dat_no_city)

# END OF PORTION TO INCLUDE



# produce and check correlation matrix 

correlation_mat <- cor(dat_no_city) # INCLUDE THIS PART IN APPENDIX

#Check_Correlation_Matrix(0, correlation_mat) # NOT THIS PART

# Produce a simple, linear fit object of our current data
first_fit <- lm(dat_no_city$MORTALITY ~ ., data = dat_no_city)
anova_values <- anova(first_fit)

# TYPE: anova_values

param_estimates <- first_fit$coefficients
# TYPE: param_estimates in the console to get our parameter estimates. We have 7 betas, 0-6, 0 being our intercept.
# FOR ESTIMATIONS OF STANDARD ERROR PER COEFFICIENT, TYPE summary(linear_fit) IN THE CONSOLE


# PART 2
# Note: Y-axis variable vs. x-axis variable
# Observed vs predicted
# Y values vs fitted Y values

# INCLUDE CODE HERE
plot(dat_no_city$MORTALITY, first_fit$fitted.values, xlab = 'Fitted Y-Values', ylab = 'Y-Values', col = 'red',
     pch = 19, main = 'Observed Y-values vs Predicted Y-values',
     xlim = c(min(first_fit$fitted.values), max(first_fit$fitted.values)), 
     ylim = c(min(dat_no_city$MORTALITY), max(dat_no_city$MORTALITY)))

# INTERPRETATION HERE: If our model was perfect, the line created here would create a perfect one to one line.
# This is not the case. This graph brings to light the downfall of our basic model, where we are predicting
# both greater than and less than actual Y values.


# plotting the residuals against the independent variables
# get the residuals
residuals <- first_fit$residuals

# To get the indep data, just call dat_no_city[,n], where n is the column of the data,
PRECIP <- dat_no_city[,1]
EDU <- dat_no_city[,2]
NONWHITE <- dat_no_city[,3]
POOR <- dat_no_city[,4]
NOX <- dat_no_city[,5]
SO2 <- dat_no_city[,6]


# FOR PRECIP:

# Residuals on the y-axis, independent value on the x-axis
plot(PRECIP, residuals, xlab = 'Precipitation', ylab = 'Residuals', col = 'red',
     pch = 19, main = 'Residuals vs Precipitation', xlim = c(min(PRECIP), max(PRECIP)),
     ylim = c(min(residuals), max(residuals)))

# draw line through origin
abline(0, 0, lty = 2)

# INTERPRETATION: A good Residuals vs independent plot will present variation along the line y = 0 and will
# lack any extremes away from the origin. In our case, a couple outliers may be present in precipitation

# FOR EDU:

# Residuals on the y-axis, independent value on the x-axis
plot(EDU, residuals, xlab = 'EDU', ylab = 'Residuals', col = 'red',
     pch = 19, main = 'Residuals vs EDU', xlim = c(min(EDU), max(EDU)),
     ylim = c(min(residuals), max(residuals)))

# draw line through origin
abline(0, 0, lty = 2)

# INTERPRETATION: The residuals vs EDU show a better spread, where only a few outliers may be present
# unlike PRECIP's plot.


# FOR NONWHITE:

# Residuals on the y-axis, independent value on the x-axis
plot(NONWHITE, residuals, xlab = 'NONWHITE', ylab = 'Residuals', col = 'red',
     pch = 19, main = 'Residuals vs NONWHITE', xlim = c(min(NONWHITE), max(NONWHITE)),
     ylim = c(min(residuals), max(residuals)))

# draw line through origin
abline(0, 0, lty = 2)

# INTERPRETATION: Here, I feel as if it is important to keep track of our units. In this case, we're dealing
# with the percentage of the population that is nonwhite in 1960. Since our statistical data varies per city,
# it is worth noting that some cities may have significantly greater non-white populations than other cities.
# This is a strong indication of a problem with our current model that will need to be accounted for.
# A greater amount of outliers may exist in this data that may need to be tuned.


# FOR POOR:

# Residuals on the y-axis, independent value on the x-axis
plot(POOR, residuals, xlab = 'POOR', ylab = 'Residuals', col = 'red',
     pch = 19, main = 'Residuals vs POOR', xlim = c(min(POOR), max(POOR)),
     ylim = c(min(residuals), max(residuals)))

# draw line through origin
abline(0, 0, lty = 2)

# INTERPRETATION: POOR, as defined by our data pertains to the percentage of households with annual
# income under $3000 dollars in 1960. Outliers and the spread presented here seem to follow a similar pattern to
# NONWHITE. This further agrees with our suspicion that NONWHITE and POOR present a problem of collinearity,
# with a correlation of .704. As such, we may either need to transform or remove one of the variables.


# FOR NOX:

# Residuals on the y-axis, independent value on the x-axis
plot(NOX, residuals, xlab = 'NOX', ylab = 'Residuals', col = 'red',
     pch = 19, main = 'Residuals vs NOX', xlim = c(min(NOX), max(NOX)),
     ylim = c(min(residuals), max(residuals)))

# draw line through origin
abline(0, 0, lty = 2)

# INTERPRETATION: First impressions of this plot is that it contains severe outliers. A transformation may be necessary
# to fix this data set.

# FOR SO2:

# Residuals on the y-axis, independent value on the x-axis
plot(SO2, residuals, xlab = 'SO2', ylab = 'Residuals', col = 'red',
     pch = 19, main = 'Residuals vs SO2', xlim = c(min(SO2), max(SO2)),
     ylim = c(min(residuals), max(residuals)))

# draw line through origin
abline(0, 0, lty = 2)

# INTERPRETATION: This plot has severe outliers as well. The only difference is that the initial spread of data
# that is not outliers is significantly more spread.

# NEXT: Histogram of the residuals
hist(residuals, col = 'firebrick')

# INTERPRETATION: The histogram of the residuals does not look roughly normal. There seems to be a pile up
# of data along the origin, with a smaller than what could be considered for normal, spread.

# Normal Probability Plot Of Residuals

qqnorm(residuals, col = 'firebrick', pch = 19, main = 'Normal probability plot of residuals')
qqline(residuals)

# WORK ON THIS INTERPRETATION

# Interpretation: There are multiple depatures from the straight line, but its form is mostly linear, except for
# extremes. In the middle, points do not adhere to the line either
# There seems to be too much variation in our current model. Most notable is the suspicion that checking
# the histogram made apparent. Here, we see that there is less data near extremes and more situated in the middle.
# This further proves that we may have outliers or may necessitate the transforming or dropping of a variable

# GO BACK TO THIS AND THE HIST B4, I THINK THIS IS AN ISSUE WITH ERROR INSTEAD.


# PART 4:








