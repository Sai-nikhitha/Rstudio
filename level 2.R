#TASK 1
install.packages(c("readr","dplyr"))
library(readr)
library(dplyr)

data<-read.csv("C:/Users/Dell/Downloads/Dataset .csv")
str(data)

offer_stats <- data %>%
  summarise(
    total_restaurants       = n(),
    table_booking_yes       = sum(tolower(Has.Table.booking) == "yes", na.rm = TRUE),
    online_delivery_yes     = sum(tolower(Has.Online.delivery) == "yes", na.rm = TRUE)
  ) %>%
  mutate(
    pct_table_booking   = round(table_booking_yes / total_restaurants * 100, 2),
    pct_online_delivery = round(online_delivery_yes / total_restaurants * 100, 2)
  )

print(offer_stats)

df_clean <- data %>%
  filter(
    !is.na(Has.Table.booking),
    !is.na(Aggregate.rating)
  ) %>%
  mutate(
    table_booking = tolower(Has.Table.booking) == "yes",
    rating = as.numeric(Aggregate.rating)
  )

rating_summary <- df_clean %>%
  group_by(table_booking) %>%
  summarise(
    n = n(),
    avg_rating = round(mean(rating, na.rm = TRUE), 2),
    sd_rating = round(sd(rating, na.rm = TRUE), 2)
  )

print(rating_summary)

ttest_res <- t.test(rating ~ table_booking, data = df_clean)
print(ttest_res)

delivery_stats <- data %>%
  filter(!is.na(Price.range), !is.na( Has.Online.delivery)) %>%
  mutate(
    price_range = factor(Price.range, levels = sort(unique(Price.range)), ordered = TRUE),
    online_delivery = tolower(Has.Online.delivery) == "yes"
  ) %>%
  group_by(price_range) %>%
  summarise(
    total_restaurants = n(),
    delivery_yes = sum(online_delivery, na.rm = TRUE),
    pct_delivery = round(delivery_yes / total_restaurants * 100, 2)
  ) %>%
  ungroup()

print(delivery_stats)



#TASK 2

price_stats <- data %>%
  filter(!is.na(Price.range)) %>%
  group_by(Price.range) %>%
  summarise(
    count = n()
  ) %>%
  ungroup() %>%
  mutate(
    pct = round(count / sum(count) * 100, 2)
  ) %>%
  arrange(desc(count))

print(price_stats)

most_common <- price_stats %>% slice_max(count, n = 1)
cat("Most common price range is:", most_common$Price.range, 
    "with", most_common$count, "restaurants (", most_common$pct, "% ).\n")

rating_by_price <- data %>%
  filter(!is.na(Price.range), !is.na(Aggregate.rating)) %>%
  mutate(
    price_range = factor(Price.range, levels = sort(unique(Price.range)), ordered = TRUE),
    rating = as.numeric(Aggregate.rating)
  ) %>%
  group_by(price_range) %>%
  summarise(
    avg_rating = mean(rating, na.rm = TRUE),
    sd_rating = sd(rating, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    is_max = avg_rating == max(avg_rating)
  )

print(rating_by_price)

library(ggplot2)
ggplot(rating_by_price, aes(x = price_range, y = avg_rating)) +
  geom_col(aes(fill = is_max), show.legend = FALSE) +
  scale_fill_manual(
    values = c("TRUE" = "#E69F00", "FALSE" = "#56B4E9")
  ) +
  labs(
    title = "Average Restaurant Rating by Price Range",
    x = "Price Range",
    y = "Average Rating"
  ) +
  theme_minimal()



#TASK 3
library(readr)
library(dplyr)
library(stringr)

df_features <- data %>%
  mutate(
    name_length = str_length(Restaurant.Name),
    address_length = str_length(Address),
    name_length_cat = case_when(
      name_length <= 15 ~ "short",
      name_length <= 30 ~ "medium",
      TRUE              ~ "long"
    ),
    address_length_cat = ntile(address_length, 3)
  )

summary_stats <- df_features %>%
  summarise(
    mean_name_len = round(mean(name_length, na.rm = TRUE), 2),
    sd_name_len   = round(sd(name_length, na.rm = TRUE), 2),
    mean_addr_len = round(mean(address_length, na.rm = TRUE), 2),
    sd_addr_len   = round(sd(address_length, na.rm = TRUE), 2)
  )

print(summary_stats)

name_cat_counts <- df_features %>%
  count(name_length_cat) %>%
  mutate(pct = round(n / sum(n) * 100, 2))

print(name_cat_counts)

print(head(select(df_features, Restaurant.Name, name_length, name_length_cat,
                  Address, address_length, address_length_cat)))


df_encoded <- data %>%
  mutate(
    has_table_booking = ifelse(tolower(Has.Table.booking) == "yes", 1, 0),
    has_online_delivery = ifelse(tolower(Has.Online.delivery) == "yes", 1, 0)
  )

df_encoded %>% 
  summarise(
    pct_table_booking = round(mean(has_table_booking, na.rm = TRUE) * 100, 2),
    pct_online_delivery = round(mean(has_online_delivery, na.rm = TRUE) * 100, 2)
  ) %>%
  print()


