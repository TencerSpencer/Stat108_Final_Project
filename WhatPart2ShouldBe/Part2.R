

dat <- read.table("mortality.csv", header = TRUE, sep = ",")
cities <- dat[,8]
dat_no_city <- dat[,1:7]

# transform Nox and SO2 by log and nonwhite and poor by cuberoot

transformed_dat <- dat_no_city
transformed_dat[,5] <- log(transformed_dat[,5])
transformed_dat[,6] <- log(transformed_dat[,6])
transformed_dat[,3] <- (transformed_dat[,3])^(1/3)
transformed_dat[,4] <- (transformed_dat[,3])^(1/3)

  transformed_line <- lm(transformed_dat$MORTALITY ~ ., dat = transformed_dat)
# correlation_mat <- cor(transformed_dat)


plot(transformed_dat$MORTALITY, transformed_line$fitted.values, xlab = 'Fitted Y-Values', ylab = 'Y-Values', col = 'red',
     pch = 19, main = 'Observed Y-values vs Predicted Y-values',
     xlim = c(min(transformed_line$fitted.values), max(transformed_line$fitted.values)), 
     ylim = c(min(transformed_dat$MORTALITY), max(transformed_dat$MORTALITY)))

# INTERPRETATION HERE: If our model was perfect, a line created here would build a perfect one to one line.
# This is not the case. This graph brings to light the downfall of our basic model, where we are predicting
# both greater than and less than actual Y values.





# NOTE: AFTER WE WRITE THESE INTERPRETATIONS DOWN. WE MUST FIGURE IF ANY OF THESE DISPLAY NONLINEARITY.
# WE MUST POINT THEM OUT IN THEIR RESPECTIVE AREA



# plotting the residuals against the independent variables
# get the residuals
residuals <- transformed_line$residuals

# To get the indep data, just call transformed_dat[,n], where n is the column of the data,
PRECIP <- transformed_dat[,1]
EDU <- transformed_dat[,2]
NONWHITE <- transformed_dat[,3]
POOR <- transformed_dat[,4]
NOX <- transformed_dat[,5]
SO2 <- transformed_dat[,6]


# FOR PRECIP:

# Residuals on the y-axis, independent value on the x-axis
plot(PRECIP, residuals, xlab = 'Precipitation', ylab = 'Residuals', col = 'red',
     pch = 19, main = 'Residuals vs Precipitation', xlim = c(min(PRECIP), max(PRECIP)),
     ylim = c(min(residuals), max(residuals)))

# draw line through origin
abline(0, 0, lty = 2)


# INTERPRETATION: A good Residuals vs independent plot will present variation along the line y = 0 and will
# lack any extremes away from the origin. In our case, a couple outliers may be present in precipitation.

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

# INTERPRETATION: After transforming this NONWHITE data using the cuberoot, the displayed plot
# shows solid spread. Only a few outliers seem present, and there seems to be equal amounts of data
# above and below the y = 0 line.


# FOR POOR:

# Residuals on the y-axis, independent value on the x-axis
plot(POOR, residuals, xlab = 'POOR', ylab = 'Residuals', col = 'red',
     pch = 19, main = 'Residuals vs POOR', xlim = c(min(POOR), max(POOR)),
     ylim = c(min(residuals), max(residuals)))

# draw line through origin
abline(0, 0, lty = 2)

# INTERPRETATION: POOR, After transforming the data, the displayed plot shows solid spread as well.
# There may be a few outliers. Our biggest concern is how POOR and NONWHITE have extremely similar
# residual plots. This agrees with our current suspicion, where poor or nonwhite might be able to be removed.


# FOR NOX:

# Residuals on the y-axis, independent value on the x-axis
plot(NOX, residuals, xlab = 'NOX', ylab = 'Residuals', col = 'red',
     pch = 19, main = 'Residuals vs NOX', xlim = c(min(NOX), max(NOX)),
     ylim = c(min(residuals), max(residuals)))

# draw line through origin
abline(0, 0, lty = 2)

# INTERPRETATION: After transforming this data, numerous outliers still exist. It is worth noting that its spread
# above and below the line looks equal.

# FOR SO2:

# Residuals on the y-axis, independent value on the x-axis
plot(SO2, residuals, xlab = 'SO2', ylab = 'Residuals', col = 'red',
     pch = 19, main = 'Residuals vs SO2', xlim = c(min(SO2), max(SO2)),
     ylim = c(min(residuals), max(residuals)))

# draw line through origin
abline(0, 0, lty = 2)

# INTERPRETATION: After transforming this data, there seems to be equal spread above and below the line.
# These values, however, are not exactly in the center. The left side contains numerous outliers, specifically two
# possibly extreme cases, that may be causing issues with our model. It is worth noting that NOX produces a
# better residual plot than SO2 does.


# Historgram of residuals

hist(residuals, col = 'firebrick')
# INTERPRETATION: The residuals look normally distributed. This is good. ADD MORE TO THIS.

# Normal Probability Plot Of Residuals

norm_line <-qqnorm(residuals, col = 'firebrick', pch = 19, main = 'Normal probability plot of residuals')
qqline(residuals)
norm_line_corr <- cor(norm_line$x, norm_line$y)

# Interpretation: This plot follows a mostly linear line. This is an indication of normality with the residuals.
# The greatest issue we see present here is the amount of outliers present. Adjustments must be made to the data
# to fix these residuals. Our line's correlation is .9864713. This is a good thing. We want high correlation.















# Maybe we transform the data slowly and compare R^2? R^2/R^2adj is a good indicator
# If the # of vars does not change it I recall correctly. This will greatly help our analysis.


# PART 5 WILL BE TO CONDUCT TESTS/STEP REGRESSION TO CHECK DATA

# ESSENTIALY, COPY/TRIAL MOST OF THE CODE FROM PART 2. TEST MATRIX CORRELATION FIRST...
# not bad so far

