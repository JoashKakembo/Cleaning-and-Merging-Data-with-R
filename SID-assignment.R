online_sales <- data.frame(
  customer_ID = c("C001", "C002", "C003", "C004", "C005"),
  products_purchased = c("Iphone 11", "Dell laptop", 
                         "Mocasins-brown", "Airpods", "LG Tv"),
  timestamp = c("2024-04-01 08:00:00", "2024-04-02 10:30:00",
                       "2024-04-03 14:45:00", "2024-04-04 12:20:00",
                       "2024-04-05 16:00:00"),
  order_value = c(10, 20, 50, 30, 15)
)


in_store_sales <- data.frame(
  customer_ID = c("C006", "C007", "C008", "C009"),
  products_purchased = c("Samsung S7", "Sunglasses-black", "Hisence-Fridge",
               "PS4"),
  timestamp = c("2024-04-01 09:00:00","2024-04-02 11:45:00",
                         "2024-04-03 13:30:00", "2024-04-04 10:15:00"),
  store_location = c("Giant Arcade-Kikubo", "Giant Arcade-Kikubo",
                     "King-Fahad", "King-Fahad"),
  order_value = c(18, 60, 80, 25 )
)


customer_demographics <- data.frame(
  customer_ID = c("C001", "C002", "C003", "C004", "C005", "C006",
                  "C007", "C008", "C009"),
  age = c(25, 30, 40, 35, 45, 28, 50, 33, 60),
  gender = c("Male", "Male", "Female", "Male", "Female", "Male",
             "Male", "Female", "Male"),
  income_level = c("UGX 0 - UGX 500,000", "UGX 500,001 - UGX 1,000,000", 
                   "UGX 500,001 - UGX 1,000,000", "UGX > 4,000,000",
                   "UGX 1,000,001 - UGX 2,000,000", "UGX 0 - UGX 500,000",
                   "UGX > 4,000,000", "UGX 1,000,001 - UGX 2,000,000",
                   "UGX > 4,000,000")
)


product_inventory <- data.frame(
  product_ID = c("P001", "P002", "P003", "P004", "P005", "P006", "P007",
                 "P008", "P009", 
                 'P010'),
  product_name = c("Iphone 11", "Dell laptop", "Mocasins-brown", "Samsung S7",
                   "Airpods", "LG Tv", "Sunglasses-black", "Hisense-Fridge",
                   "Mackbook",  "PS4"),
  stock_level = c("High", "Moderate", "Low", "High", "High",
                  "Low", "Low", "Low", "Low", "Low"),
  SKU = c("SKU001", "SKU002", "SKU003", "SKU004", "SKU005", "SKU006", "SKU007",
          "SKU008", "SKU009", "SKU010"),
  category = c("Electronics", "Electronics", "Shoes", "Electronics",
               "Electronics", "Electronics", "Accessories",
               "Appliances", "Electronics", "Electronics")
)

marketing_campaigns <- data.frame(
  campaign_type = c("Email", "Social Media", "TV Ads", "In-Store Promotion"),
  duration_days = c(7, 14, 30, 4),
  target_audience = c("Existing customers", "New customers",
                      "General audience", "Local community"),
  outcomes = c("Increased sales", "Brand awareness", "Higher foot traffic",
               "Community engagement")
)


# Check for duplicates
in_store_sales <- in_store_sales[!duplicated(in_store_sales), ]
in_store_sales[!duplicated(in_store_sales), ]
summary(in_store_sales)

online_sales <- online_sales[!duplicated(online_sales), ]
online_sales[!duplicated(online_sales), ]
summary(online_sales)


customer_demographics <- customer_demographics[!duplicated(customer_demographics), ]
customer_demographics[!duplicated(customer_demographics), ]
summary(customer_demographics)


# ensuring consisitency
library(dplyr)

online_sales <- rename(online_sales, customer_ID = customer_Id)

in_store_sales <- rename(in_store_sales, products_purchased = product)




# Merge online sales data with customer demographic data
merged_online_data1 <- merge(online_sales, customer_demographics, by = "customer_ID")

# Merge in-store sales data with customer demographic data
merged_in_store_data1 <- merge(in_store_sales, customer_demographics, by = "customer_ID")

# Merge the two merged datasets with the third dataset (online or in-store)
merged_data_dirty <- merge(merged_online_data1, merged_in_store_data1, by = "customer_ID", all = TRUE)

# Print the merged dataset
print(merged_data_dirty)





# Merge online sales data with customer demographic data
merged_online_data <- merge(online_sales, customer_demographics, by = "customer_ID")

# Merge in-store sales data with customer demographic data
merged_in_store_data <- merge(in_store_sales, customer_demographics, by = "customer_ID")

# Merge the two merged datasets with the third dataset (online or in-store)
merged_data <- merge(merged_online_data, merged_in_store_data, by = "customer_ID", all = TRUE)

# Print the merged dataset
print(merged_data)


library(dplyr)

merged_data <- merged_data %>%
  mutate(products_purchased = coalesce(products_purchased.x, products_purchased.y)) %>%
  select(-products_purchased.x, -products_purchased.y)


library(dplyr)

merged_data <- merged_data %>%
  mutate(
    timestamp = coalesce(timestamp.x, timestamp.y),
    order_value = coalesce(order_value.x, order_value.y),
    age = coalesce(age.x, age.y),
    gender = coalesce(gender.x, gender.y),
    income_level = coalesce(income_level.x, income_level.y)
  ) %>%
  select(-timestamp.x, -timestamp.y, -order_value.x, -order_value.y,
         -age.x, -age.y, -gender.x, -gender.y, -income_level.x, -income_level.y)


#Summary Statistics
summary(merged_data[, c("order_value", "age")])



library(ggplot2)

# Pie chart for gender distribution
ggplot(merged_data, aes(x = "", fill = gender)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Gender Distribution") +
  theme_void()  



# Histogram for age
ggplot(merged_data, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Age", x = "Age", y = "Frequency")


# Stacked bar plot for gender distribution by income level
ggplot(merged_data, aes(x = income_level, fill = gender)) +
  geom_bar(position = "stack") +
  labs(title = "Gender Distribution by Income Level", x = "Income Level", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability



# Scatter plot for age vs. order value
ggplot(merged_data, aes(x = age, y = order_value)) +
  geom_point(color = "darkred") +
  labs(title = "Age vs. Order Value", x = "Age", y = "Order Value")


# Faceted histogram for age distribution by gender
ggplot(merged_data, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "lightgreen", color = "black") +
  labs(title = "Age Distribution by Gender", x = "Age", y = "Frequency") +
  facet_wrap(~ gender)


# Density plot for age distribution by gender
ggplot(merged_data, aes(x = age, fill = gender)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Age by Gender", x = "Age", y = "Density") +
  theme(legend.position = "top")







x <- merged_data$age
y <- merged_data$order_value

# Create line graph
line_plot <- plot_ly(x = ~x, y = ~y, type = "scatter", mode = "lines")

# Customize the line graph
line_plot <- line_plot %>%
  layout(title = "Line Graph",
         xaxis = list(title = "Age"),
         yaxis = list(title = "Order Value"))


line_plot



library(ggplot2)
install.packages("gridExtra")
library(gridExtra)  # For arranging multiple plots

# Assuming you have variables for gender (e.g., "male", "female"), age group (e.g., "20-30", "31-40", etc.), and income level (numeric)
ggplot(merged_data, aes(x = gender, y = age, fill = income_level)) +
  geom_bar(stat = "identity") +  # "identity" to preserve data values as bar heights
  labs(title = "Average Income Level by Gender and Age Group", x = "Gender", y = "Age Group", z = "Income Level") +
  facet_wrap(~ age, nrow = 2)  # Wrap by age group (adjust nrow for more groups)

# You can use gridExtra to arrange the faceted plots in a grid layout (optional)



# Get the library
install.packages("plotrix")
library(plotrix)

# Use your dataset to create data for the graph
age_counts <- table(merged_data$age)  # Count occurrences of each age
x <- age_counts
lbl <- names(age_counts)  # Use the unique age values as labels

# Plot the 3D pie chart
#pie3D(x, labels = lbl, explode = 0.1, main = "Pie Chart of Age Distribution")

# Increase the size of the plot area
par(mar = c(5, 5, 5, 5))  # Adjust the margin sizes as needed

# Plot the 3D pie chart without the legend
pie3D(x, labels = lbl, explode = 0.1, main = "Pie Chart of Age Distribution")





# Get the library
library(plotrix)

# Use your dataset to create data for the graph
gender_counts <- table(merged_data$gender)  # Count occurrences of each gender
x <- gender_counts
lbl <- names(gender_counts)  # Use the unique gender values as labels

# Plot the 3D pie chart
pie3D(x, labels = lbl, explode = 0.1, main = "Pie Chart of Gender Distribution")









# Summary statistics
summary(merged_data)



# Count missing values
colSums(is.na(merged_data))



# Calculate summary statistics for relevant variables
summary(merged_data$age)
mean_age <- mean(merged_data$age)
median_age <- median(merged_data$age)
# Check if mean age falls within expected range
if (mean_age >= 20 && mean_age <= 50) {
  print("Mean age is within expected range.")
} else {
  print("Mean age is outside expected range.")
}
# Check if median age falls within expected range
if (median_age >= 20 && median_age <= 50) {
  print("Median age is within expected range.")
} else {
  print("Median age is outside expected range.")
}


