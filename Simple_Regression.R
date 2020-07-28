# NOTE: TO RUN THIS, HAVE THE .csv AND THIS FILE IN THE SAME DIRECTORY THAT IS YOUR CURRENT R WORKING DIRECTORY

dat <- read.table("mortality.csv", header = TRUE, sep = ",")
cities <- dat[,8]
dat_no_city <- dat[,1:7]

Build_Matrix_Plot <- function() {
  plot(dat)  
}

# BEGINNING GRAPHICAL PORTION. ALL #2.

# usage: call to check correlation matrix of data, set a restriction with parameter
Check_Correlation_Matrix <- function(collinearity_restriction=.5) {
  
  correlation_mat <- cor(dat_no_city)
  dimension_names <- dimnames(correlation_mat)
  
  # iterate rows and columns in matrix
  used_already <- 0 # placeholder
  for (i in 1:length(dimension_names[[1]])) {
    for (j in 1:length(dimension_names[[1]])) {
      
      # diagonals do not count
      if (i != j) {
        if (abs(correlation_mat[i,j]) >= .5) {
          
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




# Check_Correlation_Matrix()
# Build_Matrix_Plot()

# Note, part 1 assumes linearity, so we make a simple model
linear_fit <- lm(dat_no_city$MORTALITY ~ ., data = dat_no_city)
anova_values <- anova(linear_fit)
# TYPE: anova_values in the console to get the print of this

param_estimates <- linear_fit$coefficients
# TYPE: param_estimates in the console to get our parameter estimates. We have 7 betas, 0-6, 0 being our intercept.
# FOR ESTIMATIONS OF STANDARD ERROR PER COEFFICIENT, TYPE summary(linear_fit) IN THE CONSOLE



# NEXT UP: DIAGNOSTICS AND THEIR INTERPRETATIONS


