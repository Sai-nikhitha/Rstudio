#TASK 1

data<-read.csv("C:/Users/Dell/Downloads/Dataset .csv")

cat("Number of rows in the dataset:",nrow(data),"\n")
cat("number of columns in dataset:",ncol(data),"\n")

missing_counts <- sapply(data, function(x) sum(is.na(x)))
cat("Missing values per column:\n")
print(missing_counts)

data_cleaned <- data  

for (col in names(data_cleaned)) {
  if (is.numeric(data_cleaned[[col]])) {
    mean_val <- mean(data_cleaned[[col]], na.rm = TRUE)
    print(data_cleaned[[col]][is.na(data_cleaned[[col]])] <- mean_val)
  } else {
    mode_val <- names(sort(table(data_cleaned[[col]]), decreasing = TRUE))[1]
    print(data_cleaned[[col]][is.na(data_cleaned[[col]])] <- mode_val)
  }
}
post_missing <- sapply(data_cleaned, function(x) sum(is.na(x)))
cat("\nMissing values after handling:\n")
print(post_missing)

str(data)
if (!is.numeric(data$Aggregate.rating)) {
  data$Aggregate.rating <- as.numeric(as.character(data$Aggregate.rating))
  cat("Converted Aggregate.rating to numeric.\n")
}

na_after_conv <- sum(is.na(data$Aggregate.rating))
cat("NAs in 'Aggregate.rating' after conversion:", na_after_conv, "\n")

if (na_after_conv > 0) {
  mean_val <- mean(data$Aggregate.rating, na.rm = TRUE)
  data$Aggregate.rating[is.na(data$Aggregate.rating)] <- mean_val
  cat("Imputed", na_after_conv, "missing values with mean =", round(mean_val, 2), "\n")
}

hist(data$Aggregate.rating,
     breaks = 20,
     main = "Distribution of Aggregate Rating",
     xlab = "Aggregate Rating",
     col = "yellow",
     border = "red")

unique_vals <- length(unique(data$Aggregate.rating))
if (unique_vals <= 10) {
  rating_factor <- as.factor(round(data$Aggregate.rating))
  cat("\nClass distribution (rounded ratings):\n")
  print(table(rating_factor))
  
  imbalance_ratio <- max(table(rating_factor)) / min(table(rating_factor))
  cat("Imbalance ratio:", round(imbalance_ratio, 2), "\n")
} else {
  cat("'Aggregate.rating' is continuous, class imbalance not applicable.\n")
}



#TASK 2

summary(data)
str(data)

# Let's say the numeric columns are: "Votes", "Average.Cost.for.two", "Aggregate.rating"
mean(data$Restaurant.ID, na.rm = TRUE)
median(data$Restaurant.ID, na.rm = TRUE)
sd(data$Restaurant.ID, na.rm = TRUE)

mean(data$Country.Code, na.rm = TRUE)
median(data$Country.Code, na.rm = TRUE)
sd(data$Country.Code, na.rm = TRUE)

mean(data$Longitude, na.rm = TRUE)
median(data$Longitude, na.rm = TRUE)
sd(data$Longitude , na.rm = TRUE)

mean(data$Latitude, na.rm = TRUE)
median(data$Latitude, na.rm = TRUE)
sd(data$Latitude , na.rm = TRUE)

mean(data$Average.Cost.for.two, na.rm = TRUE)
median(data$Average.Cost.for.two, na.rm = TRUE)
sd(data$Average.Cost.for.two , na.rm = TRUE)

mean(data$Price.range, na.rm = TRUE)
median(data$Price.range, na.rm = TRUE)
sd(data$Price.range , na.rm = TRUE)

mean(data$Aggregate.rating, na.rm = TRUE)
median(data$Aggregate.rating, na.rm = TRUE)
sd(data$Aggregate.rating , na.rm = TRUE)

mean(data$Votes, na.rm = TRUE)
median(data$Votes, na.rm = TRUE)
sd(data$Votes , na.rm = TRUE)


table(data$Country.Code)
table(data$City)
table(data$Cuisines)
install.packages("ggplot2")
library(ggplot2)
ggplot(data, aes(x = factor(Country.Code))) +
  geom_bar(fill = "violet") +
  labs(title = "Distribution of Country Code", x = "Country Code", y = "Count")
top_cities <- sort(table(data$City), decreasing = TRUE)
top_cities_df <- data.frame(City = names(top_cities), Count = as.vector(top_cities))
top_cities_df <- head(top_cities_df, 10)

ggplot(top_cities_df, aes(x = reorder(City, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Top cuisines and cities with the
 highest number of Restaurants.
")


#TASK 3

install.packages(c("ggplot2", "dplyr", "ggmap"))
library(ggplot2)
library(dplyr)
library(ggmap)
str(data)

ggplot(data, aes(x = Longitude, y = Latitude, color = Aggregate.rating)) +
  geom_point(alpha = 0.6) +
  scale_color_gradient(low = "yellow", high = "orange") +
  labs(title = "Ratings by Location", x = "Longitude", y = "Latitude", color = "Rating") +
  theme_minimal()

data %>%
  group_by(City) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(City, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 10 Cities by Restaurant Count", x = "City", y = "Number of Restaurants") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

data %>%
  group_by(Country.Code) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = factor(Country.Code), y = Count)) +
  geom_bar(stat = "identity", fill = "coral") +
  labs(title = "Restaurants by Country", x = "Country Code", y = "Count")

ggplot(data, aes(x = Longitude, y = Latitude, color = Aggregate.rating)) +
  geom_point(alpha = 0.6) +
  scale_color_gradient(low = "yellow", high = "darkgreen") +
  labs(title = "Ratings by Location", x = "Longitude", y = "Latitude", color = "Rating")+
  theme_minimal()

