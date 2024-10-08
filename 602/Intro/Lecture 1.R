library(ggplot2)
# Load the Iris data set
data(iris)
scatter_plot <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species, size = Petal.Length)) +
  geom_point() +  # Adding points to the plot
  labs(title = "Scatter Plot of Sepal Length vs. Sepal Width",
       x = "Sepal Length",
       y = "Sepal Width",
       color = "Species",
       size = "Petal Length")
print(scatter_plot)


#Example 1.2
# Load mtcars dataset
data(mtcars)
# Create a scatter plot of horsepower vs. miles per gallon
scatter_plot_mtcars <- ggplot(mtcars, aes(x = hp, y = mpg, color = factor(cyl), size = wt)) +
  geom_point(alpha = 0.7) +  # Adding points with some transparency
  labs(title = "Scatter Plot of Horsepower vs. MPG",
       x = "Horsepower",
       y = "Miles per Gallon",
       color = "Number of Cylinders",
       size = "Weight")
# Display the plot
print(scatter_plot_mtcars)

#Practice
# Create a scatter plot of mpg vs. hp
scatter_plot_mtcars <- ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point() +
  labs(title = "Scatter Plot of Horsepower vs. MPG",
       x = "Horsepower",
       y = "Miles per Gallon")

# Display the plot
print(scatter_plot_mtcars)

# Create a customized scatter plot
custom_scatter_plot <- ggplot(mtcars, aes(x = hp, y = mpg, color = factor(cyl))) +
  geom_point() +
  labs(title = "Customized Scatter Plot of Horsepower vs. MPG",
       x = "Horsepower",
       y = "Miles per Gallon",
       color = "Number of Cylinders")

# Display the customized plot
print(custom_scatter_plot)

# Creating pair plots using new data frame
mtcars_new = data.frame(mtcars$mpg, mtcars$disp,  mtcars$hp, mtcars$drat, mtcars$wt)
pairs(mtcars_new)


#Example 1.3: 
# Load mtcars dataset
data(mtcars)
#part I
# Create a histogram of miles per gallon
hist_mpg <- ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(binwidth = 3, fill = "steelblue", color = "black", position = "identity", alpha = 0.7) +  # Adjust binwidth and aesthetics
  labs(title = "Histogram of Miles per Gallon",
       x = "Miles per Gallon",
       y = "Frequency")
print(hist_mpg)

#part II
# Create a histogram of miles per gallon for different numbers of cylinders
hist_mpg_cyl <- ggplot(mtcars, aes(x = mpg, fill = factor(cyl))) +
  geom_histogram(binwidth = 3, position = "identity", alpha = 0.7) +
  labs(title = "Histogram of Miles per Gallon by Number of Cylinders",
       x = "Miles per Gallon",
       y = "Frequency",
       fill = "Number of Cylinders")
print(hist_mpg_cyl)


#Practice
data(iris)
# Create a basic histogram of sepal lengths with 12 bins
hist <- ggplot(iris, aes(x = Sepal.Length)) +
  geom_histogram(binwidth = 12, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Basic Histogram of Sepal Lengths",
       x = "Sepal Length",
       y = "Frequency")
print(hist)

hist2 <- ggplot(iris, aes(x = Sepal.Length)) +
  geom_histogram(binwidth = 0.25, fill = "light blue", color = "black", alpha = 0.5) +
  labs(title = "Customized Histogram of Sepal Lengths",
       x = "Sepal Length",
       y = "Frequency")
print(hist2)

# Create faceted histograms by species
hist3 <- ggplot(iris, aes(x = Sepal.Length, fill = Species)) +
  geom_histogram(binwidth = 0.2, color = "black", alpha = 0.7) +
  facet_wrap(~Species, scales = "free_y") +
  labs(title = "Faceted Histograms of Sepal Lengths by Species",
       x = "Sepal Length",
       y = "Frequency",
       fill = "Species")
print(hist3)

density_plot <- ggplot(iris, aes(x = Sepal.Length, fill = Species)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.2, color = "black", alpha = 0.7) +
  facet_wrap(~Species, scales = "free_y") +
  geom_density(alpha = 0.5, linetype = "dashed", color = "red") +
  labs(title = "Faceted Histograms with Density Overlay",
       x = "Sepal Length",
       y = "Density",
       fill = "Species")
print(density_plot)

#Example 1.4
data(mtcars)
# Create a box plot of miles per gallon for different numbers of cylinders
box <- ggplot(mtcars, aes(x = factor(cyl), y = mpg, fill = factor(cyl))) +  geom_boxplot() +  # Adding box plots
  labs(title = "Box Plot of MPG for Different Numbers of Cylinders",
       x = "Number of Cylinders",
       y = "Miles per Gallon",
       fill = "Number of Cylinders")
print(box)


#Example 1.5:
library(dplyr)
data(mtcars)
mtcars$car_model <- rownames(mtcars)
rownames(mtcars) <- NULL
head(mtcars)
mtcars <- mtcars %>%  mutate(Model_Transmission = paste(car_model, gear, am, sep = "_"))
head(mtcars)
# Complex Box Plot
complex_box_plot <- ggplot(mtcars, aes(x = Model_Transmission, y = mpg, fill = cyl, color = cyl)) + geom_boxplot() + 
  coord_flip() +
  labs(title = "Complex Box Plot of MPG by Model and Transmission Type",
       x = "Model and Transmission Type",
       y = "Miles per Gallon")
print(complex_box_plot) 

#Example 1.6
# Create a box plot of miles per gallon (mpg) by the number of cylinders (cyl) and gears (gear)
complex_box_plot <- ggplot(mtcars, aes(x = factor(cyl), y = mpg, fill = factor(gear))) +
  geom_boxplot(outlier.shape = NA, coef = 1.5, notch = FALSE) +  # Hide outliers, add notches
  facet_grid(gear ~ cyl, scales = "free", space = "free") +  # Facet by gears and cylinders # Add jitter for individual points
  labs(title = "Complex Box Plot of MPG by Cylinders and Gears",
       x = "Number of Cylinders",
       y = "Miles per Gallon",
       fill = "Number of Gears") +
  theme_bw() +  # Use a clean theme
  scale_fill_brewer(palette = "Set3") +  # Use a color palette for fill
  theme(legend.position = "top", axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust theme settings
print(complex_box_plot)


#Example 1.7
# Load necessary packages
library(ggplot2)
# Load the "diamonds" dataset
data(diamonds)
# Create a violin plot of diamond prices for each cut quality
diamonds_violin_plot <- ggplot(diamonds, aes(x = cut, y = price, fill = cut)) +
  geom_violin(trim = TRUE, draw_quantiles = c(0.25, 0.5, 0.75), alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", color = "black", outlier.shape = NA) +
  labs(title = "Violin Plot of Diamond Prices by Cut Quality",
       x = "Cut Quality",
       y = "Price",
       fill = "Cut Quality") +
  theme_minimal()
print(diamonds_violin_plot)

