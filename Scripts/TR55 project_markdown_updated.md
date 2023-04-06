## Quant_Methods/TR55Prj

#' ---
#' title: TR55 Project
#' author: Briar Ownby-Connolly
#' date: ' `r paste("created on", Sys.Date())`'
#' output: html_document
#' ---
#' 



library(readr)
Book6 <- read_csv("~/Quant_Methods/Book6.csv")
Book6

# define x variable
x <- (Book6$`time of concentration`)
x

# define y variable
y <- (Book6$`unit peak discharge`)
y


# create dataframe with entire range from x and y
df <- data.frame(x,y)
df
plot(df, log = "xy")


# create vectors by Ia/P sections for just x 
df1x <- df$x[1:43]
df1x


df2x <- df$x[44:86]
df2x


df3x <- df$x[87:129]
df3x


df4x <- df$x[130:172]
df4x


df5x <- df$x[173:213]
df5x


df6x <- df$x[214:258]
df6x



# create vectors by Ia/P sections for just y
df1y <- df$y[1:43]
df1y

df2y <- df$y[44:86]
df2y

df3y <- df$y[87:129]
df3y

df4y <- df$y[130:172]
df4y

df5y <- df$y[173:213]
df5y

df6y <- df$y[214:258]
df6y

plot(df1x, df1y, log = "xy", xlab = "Time of concentration", ylab = "Unit Peak Discharge")
plot(df2x,df2y, log = "xy", xlab = "Time of concentration", ylab = "Unit Peak Discharge")
plot(df3x, df3y, log = "xy", xlab = "Time of concentration", ylab = "Unit Peak Discharge")
plot(df4x, df4y, log = "xy", xlab = "Time of concentration", ylab = "Unit Peak Discharge")
plot(df5x, df5y, log = "xy", xlab = "Time of concentration", ylab = "Unit Peak Discharge")
plot(df6x, df6y, log = "xy", xlab = "Time of concentration", ylab = "Unit Peak Discharge")


# compute interpolation points
x_interp <- seq(max(min(df1x), min(df2x)), min(max(df1x), max(df2x)), length.out = 100)

# transform data to logarithmic scale
log_df1x <- log(df1x)
log_df1y <- log(df1y)
log_df2x <- log(df2x)
log_df2y <- log(df2y)
log_x_interp <- log(x_interp)

# interpolation on log data
log_y_interp1 <- approx(log_df1x, log_df1y, log_x_interp)$y
log_y_interp2 <- approx(log_df2x, log_df2y, log_x_interp)$y

# exponentiate interpolated values to obtain final values on original scale
y_interp1 <- exp(log_y_interp1)
y_interp2 <- exp(log_y_interp2)

# plot original data and interpolated values
plot(df1x, df1y, type = "l", col = 'blue', xlab="Time of Concentration (hrs)", ylab = "Unit Peak Discharge (csm/in)", ylim = range(c(df1y, df2y)))
lines(df2x, df2y, type = "l", col = 'red')
lines(x_interp, y_interp1, type = "l", col = 'green')
lines(x_interp, y_interp2, type = "l", col = 'purple')



# interpolation across all six curves
#combine x and y values to make up the six dataframes

xy1 <- data.frame(x = df1x, y = df1y)
xy1

xy2 <- data.frame(x = df2x, y = df2y)
xy2

xy3 <- data.frame( x = df3x, y = df3y)
xy3

xy4 <- data.frame(x = df4x, y = df4y)
xy4

xy5 <- data.frame(x = df5x, y = df5y)
xy5

xy6 <- data.frame(x = df6x, df6y)
xy6

#create interpolation sequence
x_interp2 <- seq(max(min(df1x), min(df6x)), min(max(df1x), max(df6x)), length.out = 100)

#interpolate y values for each dataframe
y_interp1.1 <- approx(xy1$x, xy1$y, x_interp2)$y
y_interp2.2 <- approx(xy2$x, xy2$y, x_interp2)$y
y_interp3.3 <- approx(xy3$x, xy3$y, x_interp2)$y
y_interp4.4 <- approx(xy4$x, xy4$y, x_interp2)$y
y_interp5.5 <- approx(xy5$x, xy5$y, x_interp2)$y
y_interp6.6 <- approx(xy6$x, xy6$y, x_interp2)$y

#plot interpolated curves
plot(x_interp2, y_interp1.1, type = "l",col = 'red', xlab = "Time of Concentration (hrs)", ylab = "Unit Peak Discharge (csm/in)")
lines(x_interp2, y_interp2.2, type = "l", col = 'orange')
lines(x_interp2, y_interp3.3, type = "l", col = 'yellow')
lines(x_interp2, y_interp4.4, type = "l", col = 'green')
lines(x_interp2, y_interp5.5, type = "l", col = 'blue')
lines(x_interp2, y_interp6.6, type = "l", col = 'purple')


# add noise to look at output reactions 

# set seed for reproducibility
set.seed(123)

# generate random noise for each x vector
error1 <- rnorm(length(df1x), mean = 0, sd = 0.1)
error2 <- rnorm(length(df2x), mean = 0, sd = 0.1)
error3 <- rnorm(length(df3x), mean = 0, sd = 0.1)
error4 <- rnorm(length(df4x), mean = 0, sd = 0.1)
error5 <- rnorm(length(df5x), mean = 0, sd = 0.1)
error6 <- rnorm(length(df6x), mean = 0, sd = 0.1)

# add errors to the original vectors
df1x_error1 <- df1x + error1
df2x_error2 <- df2x + error2
df3x_error3 <- df3x + error3
df4x_error4 <- df4x + error4
df5x_error5 <- df5x + error5
df6x_error6 <- df6x + error6

# plot the relationship bettwen df1x_errors and respective df1ys
plot(df1x_error1, df1y, main = "Relationship between x (with error) and Y", xlab = "x (with error)", ylab = "Y")

plot(df2x_error2, df2y, main = "Relationship between x (with error) and Y", xlab = "x (with error)", ylab = "Y")

plot(df3x_error3, df3y, main = "Relationship between x (with error) and Y", xlab = "x (with error)", ylab = "Y")

plot(df4x_error4, df4y, main = "Relationship between x (with error) and Y", xlab = "x (with error)", ylab = "Y")

plot(df5x_error5, df5y, main = "Relationship between x (with error) and Y", xlab = "x (with error)", ylab = "Y")

plot(df6x_error6, df6y, main = "Relationship between x (with error) and Y", xlab = "x (with error)", ylab = "Y")

# log transform 
plot(df1x_error, df1y, log = "xy", main = "Relationship between x (with error) and Y", xlab = "x (with error)", ylab = "Y")

plot(df2x_error2, df2y,log = "xy", main = "Relationship between x (with error) and Y", xlab = "x (with error)", ylab = "Y")

plot(df3x_error3, df3y,log = "xy", main = "Relationship between x (with error) and Y", xlab = "x (with error)", ylab = "Y")

plot(df4x_error4, df4y,log = "xy", main = "Relationship between x (with error) and Y", xlab = "x (with error)", ylab = "Y")

plot(df5x_error5, df5y, log = "xy", main = "Relationship between x (with error) and Y", xlab = "x (with error)", ylab = "Y")

plot(df6x_error6, df6y, log = "xy", main = "Relationship between x (with error) and Y", xlab = "x (with error)", ylab = "Y")


# Subset dfx vectors to have the same length
df1x <- df1x[1:43]
df2x <- df2x[1:43]
df3x <- df3x[1:43]
df4x <- df4x[1:43]
df5x <- df5x[1:43]
df6x <- df6x[1:43]


# Combine x vectors with errors into a single data frame
x_with_errors <- cbind(df1x + error1, df2x + error2, df3x + error3, df4x + error4, df5x + error5, df6x + error6)

# Plot x vectors with y
matplot(x_with_errors, df$y[1:42], type = "l", xlab = "X (with error)", ylab = "Y", main = "Relationships between X (with error) and Y")
legend("topright", legend = paste0("X", 1:6), col = 1:6, lty = 1)

# log transform
matplot(x_with_errors, df$y[1:42], type = "l", log = "xy", xlab = "X (with error)", ylab = "Y", main = "Relationships between X (with error) and Y")
legend("topright", legend = paste0("X", 1:6), col = 1:6, lty = 1)




