#Example 2.1
# Install and load the nycflights13 package if not already installed
install.packages("nycflights13")
library(nycflights13)
head(flights)
dim(flights)
set.seed(42)

# Select a convenience sample of 100 flights (you can change the number as needed)
conv_sample <- flights[1:100,]

# Display the convenience sample
print("Convenience Sample:")
print(conv_sample)


#Histogram of Departure Delays
library(ggplot2)
ggplot(conv_sample, aes(x = dep_delay)) + geom_histogram(fill='blue', binwidth=5, na.rm=TRUE) +
  xlab("Minutes a Flight Departured Late") + ylab("Frequency") +
  ggtitle("Histogram of Departured Delays for Flights Leaving 3-NYC Airports")

conv_sample2 <- flights[300:400,]
ggplot(conv_sample2, aes(x = dep_delay)) + geom_histogram(fill='Red', binwidth=5, na.rm=TRUE) +
  xlab("Minutes a Flight Departured Late") + ylab("Frequency") +
  ggtitle("Histogram of Departured Delays for Flights Leaving 3-NYC Airports")



# Create overlaid frequency polygons
library(dplyr)
data("flights")
# Filter data for two airlines (you can choose different airlines)
airline1 <- "DL"  # Delta Air Lines
airline2 <- "AA"  # American Airlines
subset_data <- filter(flights, flights$carrier %in% c(airline1, airline2))
head(subset_data)
# Create overlaid frequency polygons using ggplot2
ggplot(subset_data, aes(x = dep_delay, fill = carrier, color = carrier)) +
  geom_freqpoly(binwidth = 10, alpha = 0.7) +
  labs(title = "Overlaid Frequency Polygons of Departure Delays",
       x = "Departure Delay (minutes)", y = "Frequency") +
  scale_fill_manual(values = c("DL" = "blue", "AA" = "red")) +
  scale_color_manual(values = c("DL" = "blue", "AA" = "red"))


#Example 2.3
# Install and load necessary packages if not already installed
#library(nycflights13)
#library(dplyr)
# Load the nycflights13 dataset if necessary
#data("flights")

# Set seed for reproducibility
set.seed(123)

# Generate a simple random sample of 200 flights
random_sample <- sample_n(flights, size = 200)

# Display the first few rows of the random sample
head(random_sample)


#Practice 2.1
# Generate a histogram for departure delays and arrival delays
hist(random_sample$dep_delay, 
     main = "Histogram of Departure Delays",
     xlab = "Departure Delay (minutes)",
     ylab = "Frequency",
     col = "lightblue",
     border = "black")

hist(random_sample$arr_delay, 
     main = "Histogram of Arrival Delays",
     xlab = "Arrival Delay (minutes)",
     ylab = "Frequency",
     col = "lightgreen",
     border = "black")


# Create an overlaid frequency polygon using ggplot2
library(reshape2)
delay_data <- data.frame(DepartureDelay = random_sample$dep_delay,
                         ArrivalDelay = random_sample$arr_delay)
ggplot(melt(delay_data), aes(x = value, color = variable, fill = variable)) +
  geom_freqpoly(binwidth = 10, alpha = 0.7) +
  labs(title = "Overlaid Frequency Polygons of Departure and Arrival Delays",
       x = "Delay (minutes)", y = "Frequency") +
  scale_color_manual(values = c("DepartureDelay" = "blue", "ArrivalDelay" = "red")) +
  scale_fill_manual(values = c("DepartureDelay" = "lightblue", "ArrivalDelay" = "lightcoral")) +
  theme_minimal()


#Example 2.4
# Install and load necessary packages if not already installed
if (!requireNamespace("nycflights13", quietly = TRUE)) {
  install.packages("nycflights13")
}

library(nycflights13)

# Load the nycflights13 dataset if necessary
#data("flights")

# Set seed for reproducibility
set.seed(42)

# Create a simple random sample of 100 flights
sample_size <- 100
random_sample <- sample_n(flights, size = sample_size)

# Function to calculate mode
calculate_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Calculate mean, median, and mode for departure delays in the sample
mean_departure_delay <- mean(random_sample$dep_delay, na.rm = TRUE)
median_departure_delay <- median(random_sample$dep_delay, na.rm = TRUE)
mode_departure_delay <- calculate_mode(random_sample$dep_delay)

# Display the results
print("For the random sample of 100 flights:\n")
cat("Mean Departure Delay:", mean_departure_delay, "\n")
cat("Median Departure Delay:", median_departure_delay, "\n")
cat("Mode Departure Delay:", mode_departure_delay, "\n")

#Example 4.2 Cont.
var_departure_delay <- var(random_sample$dep_delay,na.rm = TRUE)
sd_departure_delay <- sd(random_sample$dep_delay,na.rm = TRUE)

#Example 4.2 Cont.
quantile(random_sample$dep_delay,0.3, na.rm = TRUE)
quantile(random_sample$dep_delay,0.25, na.rm = TRUE)
quantile(random_sample$dep_delay,0.5, na.rm = TRUE)
quantile(random_sample$dep_delay,0.75, na.rm = TRUE)

library(mosaic)
favstats(random_sample$dep_delay, na.rm = TRUE)


#Practice
names(random_sample)
#a
ggplot(random_sample, aes(x = random_sample$air_time)) +
  geom_histogram(binwidth = 10, color ="lightblue", fill = "lightblue")
ggplot(random_sample, aes(x = random_sample$air_time)) +
  geom_density(binwidth = 10, color ="lightblue", fill = "lightblue")
#b
favstats(random_sample$air_time)
#c
group = filter(random_sample, carrier == c("UA", "AA","EV","B6","DL"))
group
boxplot(group$air_time ~ group$carrier, horizontal = TRUE, col = "lightblue")

#d
favstats(air_time~carrier, data = group)
