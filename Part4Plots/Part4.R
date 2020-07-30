dat <- read.table("mortality.csv", header = TRUE, sep = ",")
cities <- dat[,8]
dat_no_city <- dat[,1:7]

# transform Nox and SO2

transformed_dat <- dat_no_city
transformed_dat[,5] <- log(transformed_dat[,5])
transformed_dat[,6] <- log(transformed_dat[,6])
transformed_dat[,3] <- (transformed_dat[,3])^(1/3)
transformed_dat[,4] <- (transformed_dat[,3])^(1/3)

transformed_line <- lm(transformed_dat$MORTALITY ~ ., dat = transformed_dat)


# PART 5 WILL BE TO CONDUCT TESTS/STEP REGRESSION TO CHECK DATA

# ESSENTIALY, COPY/TRIAL MOST OF THE CODE FROM PART 2. TEST MATRIX CORRELATION FIRST...
# not bad so far