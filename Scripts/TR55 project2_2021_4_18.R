
# title: "TR55" 
# author: "Briar Ownby-Connolly"
# date: "2023-04-14"
# output: html_document


# ```
library(readr)
Book6 <- read_csv("Book6.csv")
Book6
# define x variable
x <- Book6$"time of concentration"

# define y variable
y <- Book6$"unit peak discharge"

# create dataframe with entire range from x and y
df <- data.frame(x,y)

#plot the data
plot(df, log = "xy", xlab= "Time of concentration (Tc), (Hours)", ylab = "Unit Peake discharge (qu), (csm/in)")

# create vectors by Ia/P sections for just x 
df1x <- df$x[1:43]
df2x <- df$x[44:86]
df3x <- df$x[87:129]
df4x <- df$x[130:172]
df5x <- df$x[173:215]
df6x <- df$x[216:258]

# create vectors by Ia/P sections for just y
df1y <- df$y[1:43]
df2y <- df$y[44:86]
df3y <- df$y[87:129]
df4y <- df$y[130:172]
df5y <- df$y[173:215]
df6y <- df$y[216:258]

#plot original data
plot(df1x, df1y, type = "l",log = "xy", col = 'blue', xlab="Time of Concentration (hrs)", ylab = "Unit Peak Discharge (csm/in)", ylim = range(c(df1y, df2y, df3y, df4y, df5y, df6y)))
lines(df2x, df2y, type = "l", col = 'red', lwd = 2)
lines(df3x, df3y, type = "l", col = 'green', lwd = 2)
lines(df4x, df4y, type = "l", col = 'purple', lwd = 2)
lines(df5x, df5y, type = "l", col = 'orange', lwd = 2)
lines(df6x, df6y, type = "l", col = 'black', lwd = 2)
legend("topright", legend = c("Curve 1", "Curve 2", "Curve 3", "Curve 4", "Curve 5", "Curve 6"), col = c('blue', 'red', 'green', 'purple', 'orange', 'black'), lty = 1)


# interpolation across all six curves
#combine x and y values to make up the six dataframes
xy1 <- data.frame(x = df1x, y = df1y)
xy2 <- data.frame(x = df2x, y = df2y)
xy3 <- data.frame( x = df3x, y = df3y)
xy4 <- data.frame(x = df4x, y = df4y)
xy5 <- data.frame(x = df5x, y = df5y)
xy6 <- data.frame(x = df6x, y = df6y)


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
plot(df1x, df1y, type = "n", col = 'blue', xlab="Time of Concentration (hrs)", log = "xy", ylab = "Unit Peak Discharge (csm/in)", ylim = range(c(df1y, df2y, df3y, df4y, df5y, df6y)))
lines(x_interp2, y_interp1.1, type = "l", col = 'green', lwd = 2)
lines(x_interp2, y_interp2.2, type = "l", col = 'purple', lwd = 2)
lines(x_interp2, y_interp3.3, type = "l", col = 'orange', lwd = 2)
lines(x_interp2, y_interp4.4, type = "l", col = 'blue', lwd = 2)
lines(x_interp2, y_interp5.5, type = "l", col = 'red', lwd = 2)
lines(x_interp2, y_interp6.6, type = "l", col = 'black', lwd = 2)

#add legend
legend("topright", legend = c("Interpolated curve 1", "Interpolated curve 2", "Interpolated curve 3", "Interpolated curve 4", "Interpolated curve 5", "Interpolated curve 6"), col = c('green', 'purple', 'orange', 'blue', 'red', 'black'), lty = 1, lwd = 2, bty = "n", x.intersp = 0.5, y.intersp = 0.5, title = "Legend")


# plot original data and interpolated values
plot(df1x, df1y, type = "l", log = "xy", col = 'blue', xlab="Time of Concentration (hrs)", ylab = "Unit Peak Discharge (csm/in)", ylim = range(c(df1y, df2y, df3y, df4y, df5y, df6y)))
lines(df2x, df2y, type = "l", col = 'red', lwd = 2)
lines(df3x, df3y, type = "l", col = 'green', lwd = 2)
lines(df4x, df4y, type = "l", col = 'purple', lwd = 2)
lines(df5x, df5y, type = "l", col = 'orange', lwd = 2)
lines(df6x, df6y, type = "l", col = 'black', lwd = 2)
lines(x_interp2, y_interp1.1, type = "l", col = 'cyan', lwd = 2,lty = "dashed")
lines(x_interp2, y_interp2.2, type = "l", col = 'magenta', lwd = 2,lty = "dashed")
lines(x_interp2, y_interp3.3, type = "l", col = 'yellow', lwd = 2,lty = "dashed")
lines(x_interp2, y_interp4.4, type = "l", col = 'gray', lwd = 2,lty = "dashed")
lines(x_interp2, y_interp5.5, type = "l", col = 'brown', lwd = 2,lty = "dashed")
lines(x_interp2, y_interp6.6, type = "l", col = 'darkgreen', lwd = 2,lty = "dashed")
legend("topright", legend = c("Curve 1", "Curve 2", "Curve 3", "Curve 4", "Curve 5", "Curve 6", "Interpolated curve 1", "Interpolated curve 2", "Interpolated curve 3", "Interpolated curve 4", "Interpolated curve 5", "Interpolated curve 6"), col = c('blue', 'red', 'green', 'purple', 'orange', 'black', 'green', 'purple', 'orange', 'blue', 'red', 'black'), lty = 1)



# generating error in time of concentration and plot against qu

# Define the range of error percentages to test
error_range <- seq(0, 50, by = 1)
adjusted_peak_discharge <- rep(NA, length(error_range))
for (i in 1:length(error_range)) {
  error <- error_range[i]
  adjustment <- 1 + error/100
  adjusted_toc <- ifelse(Book6$"time of concentration" < 5, Book6$"time of concentration" * adjustment,
                         ifelse(Book6$"time of concentration" < 10, Book6$"time of concentration" * adjustment^2,
                                Book6$"time of concentration" * adjustment^3))
  adjusted_peak_discharge[i] <- ifelse(length(adjusted_toc[adjusted_toc < 5]) == length(adjusted_peak_discharge),
                                       Book6$"unit peak discharge",
                                       ifelse(length(adjusted_toc[adjusted_toc < 10]) == length(adjusted_peak_discharge),
                                              Book6$"unit peak discharge"/adjustment,
                                              Book6$"unit peak discharge"/adjustment^2))
}

# Plot the adjusted peak discharge values as a function of error percentage in time of concentration
plot(log(error_range), log(adjusted_peak_discharge), type = "p", xlab = "Error Percentage in Time of Concentration", ylab = "Adjusted Unit Peak Discharge (csm/in)")


#linear regression model 2
# Perform a simple linear regression
model2 <- lm(adjusted_peak_discharge ~ error_range)

# View the model summary
summary(model2)

install.packages("stargazer")
library(stargazer)


stargazer(model2, type = "text")





#modified so it iterates through all six curves
# Define the range of error percentages to test
error_range <- seq(0, 50, by = 1)

# Define colors for each curve
colors <- c("blue", "red", "green", "purple", "orange", "black")

# Initialize plot
plot(0, 0, xlim = c(0, log(max(error_range))), ylim = range(c(log(xy1$y)), log(xy2$y), log(xy3$y), log(xy4$y), log(xy5$y), log(xy6$y)), 
     xlab = "Error Percentage in Time of Concentration", ylab = "Adjusted Unit Peak Discharge (csm/in)")

# Create an empty vector to store the slope estimates
slopes <- numeric(length = 6)

# Iterate through the six curves and plot the adjusted peak discharge values as a function of error percentage in time of concentration
for (i in 1:6) {
  xy <- get(paste0("xy", i))
  adjusted_peak_discharge <- rep(NA, length(error_range))
  
  for (j in 1:length(error_range)) {
    error <- error_range[j]
    adjustment <- 1 + error/100
    adjusted_toc <- ifelse(xy$x < 5, xy$x * adjustment,
                           ifelse(xy$x < 10, xy$x * adjustment^2,
                                  xy$x * adjustment^3))
    adjusted_peak_discharge[j] <- ifelse(length(adjusted_toc[adjusted_toc < 5]) == length(xy$y),
                                         xy$y,
                                         ifelse(length(adjusted_toc[adjusted_toc < 10]) == length(xy$y),
                                                xy$y/adjustment,
                                                xy$y/adjustment^2))
  }
  
  # Plot the adjusted peak discharge values for each curve
  lines(log(error_range), log(adjusted_peak_discharge), type = "p", pch = 19, col = colors[i])
  
  # Fit a linear regression model to the data and extract the slope
  fit <- lm(adjusted_peak_discharge ~ error_range)
  slopes[i] <- coef(fit)[2]
}

# Add legend
legend("bottom", legend=c("Curve 1", "Curve 2", "Curve 3", "Curve 4", "Curve 5", "Curve 6"),
       col=colors,bty = "n", lty=1, cex=0.9, pch=19, horiz= TRUE, inset=c(0, -0.1))

# Print the slope estimates
slopes


# Put slope estimates and curves in a table
library(knitr)
kable(data.frame(slopes), row.names = FALSE)

library(xtable)
xtable(data.frame(slopes), caption = "Slope Estimates")

install.packages("gt")
library(gt)
data.frame(slopes) %>%
  gt() %>%
  tab_header(title = "Slope Estimates")

summary(fit)
stargazer(fit, type = "text")
