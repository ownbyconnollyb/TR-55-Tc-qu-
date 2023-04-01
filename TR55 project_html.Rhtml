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


# generate error propagation

library(propagate)
install.packages("propagate")

log_fun1 <- with(data.frame(x = xy1$x, y = xy1$y), expression(log(y) ~ log(x)))
y_err1 <- propagate::propagate(data = xy1, expr = log_fun1, type = "log", na.rm = TRUE)$sd
y_err1 <- exp(y_err1)

log_fun2 <- with(data.frame(x = xy2$x, y = xy2$y), log(y) ~ log(x))
y_err2 <- propagate::propagate(data = data.frame(x = xy2$x, y = xy2$y), expr = log_fun2, type = "log", na.rm = TRUE)$sd
y_err2 <- exp(y_err2)

log_fun3 <- with(data.frame(x = xy3$x, y = xy3$y), log(y) ~ log(x))
y_err3 <- propagate::propagate(data = data.frame(x = xy3$x, y = xy3$y), expr = log_fun3, type = "log", na.rm = TRUE)$sd
y_err3 <- exp(y_err3)

log_fun4 <- with(data.frame(x = xy4$x, y = xy4$y), log(y) ~ log(x))
y_err4 <- propagate::propagate(data = data.frame(x = xy4$x, y = xy4$y), expr = log_fun4, type = "log", na.rm = TRUE)$sd
y_err4 <- exp(y_err4)

log_fun5 <- with(data.frame(x = xy5$x, y = xy5$y), log(y) ~ log(x))
y_err5 <- propagate::propagate(data = data.frame(x = xy5$x, y = xy5$y), expr = log_fun5, type = "log", na.rm = TRUE)$sd
y_err5 <- exp(y_err5)

log_fun6 <- with(data.frame(x = xy6$x, y = xy6$y), log(y) ~ log(x))
y_err6 <- propagate::propagate(data = data.frame(x = xy6$x, y = xy6$y), expr = log_fun6, type = "log", na.rm = TRUE)$sd
y_err6 <- exp(y_err6)

# combine interpolated y values and error into data frames
df_interp1 <- data.frame(x_interp2, y_interp1.1, y_err1)
df_interp2 <- data.frame(x_interp2, y_interp2.2, y_err2)
df_interp3 <- data.frame(x_interp2, y_interp3.3, y_err3)
df_interp4 <- data.frame(x_interp2, y_interp4.4, y_err4)
df_interp5 <- data.frame(x_interp2, y_interp5.5, y_err5)
df_interp6 <- data.frame(x_interp2, y_interp6.6, y_err6)

# Plot  data and error propagation
plot(x_interp2, y_interp1.1, type = "l",col = 'red', xlab = "Time of Concentration (hrs)", ylab = "Unit Peak Discharge (csm/in)")
lines(x, exp(df_interp1 + error), lty = 2, col = "red")
lines(x, exp(df_interp1 - error), lty = 2, col = "red")
lines(x, exp(df_interp2 + error), lty = 2, col = "orange")
lines(x, exp(df_interp2 - error), lty = 2, col = "orange")
lines(x, exp(df_interp3 + error), lty = 2, col = "yellow")
lines(x, exp(df_interp3 - error), lty = 2, col = "yellow")
lines(x, exp(df_interp4 + error), lty = 2, col = "green")
lines(x, exp(df_interp4 - error), lty = 2, col = "green")
lines(x, exp(df_interp5 + error), lty = 2, col = "blue")
lines(x, exp(df_interp5 - error), lty = 2, col = "blue")
lines(x, exp(df_interp6 + error), lty = 2, col = "purple")
lines(x, exp(df_interp6 - error), lty = 2, col = "purple")





