rm(list=ls())
library("dplyr")
library("ggplot2")

groceries <- read.csv("grocery_store_data_cleaned.csv", stringsAsFactors = FALSE)


highest_selling_product <- groceries %>% 
  mutate(MONTHS = months(as.Date(substr(DATE, 1, 10), "%Y-%m-%d"))) %>%
  group_by(CATEGORY, MONTHS) %>% filter(TOTAL_PRICESELL == max(TOTAL_PRICESELL)) %>% 
  group_by(MONTHS) %>% filter(TOTAL_PRICESELL == max(TOTAL_PRICESELL)) %>% select(NAME, CATEGORY, MONTHS)


lowest_selling <- groceries %>% 
  mutate(MONTHS = months(as.Date(substr(DATE, 1, 10), "%Y-%m-%d"))) %>% 
  group_by(CATEGORY, MONTHS) %>% filter(TOTAL_PRICESELL == min(TOTAL_PRICESELL)) %>%
  group_by(MONTHS) %>% filter(TOTAL_PRICESELL == min(TOTAL_PRICESELL)) %>% select(NAME, CATEGORY, MONTHS)


highest_profit_month <- groceries %>% 
  mutate(months = months(as.Date(substr(DATE, 1, 10), "%Y-%m-%d"))) %>%
  filter(PROFIT == max(PROFIT)) %>% pull(months)

lowest_profit_month <- groceries %>% 
  mutate(months = months(as.Date(substr(DATE, 1, 10), "%Y-%m-%d"))) %>%
  filter(PROFIT == min(PROFIT)) %>% pull(months)

bar_graph <- groceries %>%
  group_by(CATEGORY) %>% 
  mutate(average_profit = mean(PROFIT)) %>% 
  arrange(-average_profit) %>% distinct(CATEGORY, average_profit)

bar_graph_positive <- bar_graph[1:3,]
bar_graph <- arrange(bar_graph, average_profit)
bar_graph_negative <- bar_graph[1:3,]

ggplot(bar_graph_positive) +
  geom_col(mapping = aes(x = CATEGORY, y = average_profit), fill = " dark orange") +
  labs(
    title = "Top 3 Most Profitable Categories",
    x = "Category",
    y = "Average Profit"
  )

ggplot(bar_graph_negative) +
  geom_col(mapping = aes(x = CATEGORY, y = average_profit), fill = "dark red") +
  labs(
    title = "Top 3 Least Profitable Categories",
    x = "Category",
    y = "Average Profit"
  )

categories <- groceries %>%
  group_by(CATEGORY) %>%
  mutate(avg_unitPriceSell = mean(UNIT_PRICESELL)) %>%
  mutate(avg_units = mean(UNITS)) %>%
  mutate(avg_totalPriceSell = mean(TOTAL_PRICESELL)) %>%
  arrange(avg_unitPriceSell) %>%
  select(CATEGORY, avg_unitPriceSell)

categories <- unique(categories)
num_categories <- nrow(categories)


top_ten <- categories[1:10,]
bottom_ten <- categories[34:43,]
  
 
transactions <- read.csv(file = "transactions.csv")

new_transactions <- transactions %>%
  group_by(month, year) %>%
  mutate(count = n()) %>%
  select(year, month, count)

new_transactions <- unique(new_transactions)  
  
  
  
#compare avg unit price sells values to top three profits and least three profits


profit <- read.csv(file = "trends.csv")


#line graph: time vs. profit
plot(profit[, "TOTAL_PRICESELL"], type = "l", col = "pink", xlab = "Month", ylab = "Dollars",
     main = "Monthly Trends: 03/2016 to 10/2019")

lines(profit[, "TOTAL_PRICEBUY"], type = "l", col = "orange")
lines(profit[, "PROFIT"], type = "l", col = "dark red")

legend("topright", legend = c("Total Price Buy", 'Total Price Sell', 'Total Profit'),
       fill, col = c("orange", "pink", "dark red"), lwd = 2)

#findings:


#2016
transactions_2016 <- new_transactions %>%
  filter(year == 2016) %>%
  select(year, month, count)

#2016 bar graph
ggplot(transactions_2016) +
  geom_col(mapping = aes(x = month, y = count), fill = "pink") +
  labs(
    title = "2016 Yearly Transactions",
    x = "Months",
    y = "Number of Transactions"
  )

#2017
transactions_2017 <- new_transactions %>%
  filter(year == 2017) %>%
  select(year, month, count)

#2017 bar graph
ggplot(transactions_2017) +
  geom_col(mapping = aes(x = month, y = count), fill = "orange") +
  labs(
    title = "2017 Yearly Transactions",
    x = "Months",
    y = "Number of Transactions"
  )

#2018
transactions_2018 <- new_transactions %>%
  filter(year == 2018) %>%
  select(year, month, count)

#2018 bar graph
ggplot(transactions_2018) +
  geom_col(mapping = aes(x = month, y = count), fill = "red") +
  labs(
    title = "2018 Yearly Transactions",
    x = "Months",
    y = "Number of Transactions"
  )

#2019
transactions_2019 <- new_transactions %>%
  filter(year == 2019) %>%
  select(year, month, count)

#2019 bar graph
ggplot(transactions_2019) +
  geom_col(mapping = aes(x = month, y = count), fill = "dark red") +
  labs(
    title = "2019 Yearly Transactions",
    x = "Months",
    y = "Number of Transactions"
  )


#What trends do you notice for the store with respect to transactions?


#yearly transactions
tot_transactions_2016 = sum(transactions_2016[, "count"])
tot_transactions_2017 = sum(transactions_2017[, "count"])
tot_transactions_2018 = sum(transactions_2018[, "count"])
tot_transactions_2019 = sum(transactions_2019[, "count"])

ggplot(transactions_2017) +
  geom_col(mapping = aes(x = month, y = count), fill = "dark red")

yearly_transactions <- data.frame("years" = c(2016, 2017, 2018, 2019),
                                  "total transactions" = c(tot_transactions_2016, tot_transactions_2017, 
                                                           tot_transactions_2018, tot_transactions_2019))

#bar graph: total yearly transactions
#note: January-March for 2016 is not counted and October-December for 2019 is not counted, which means this is not their
#      total yearly transactions. 
ggplot(yearly_transactions) +
  geom_col(mapping = aes(x = years, y = total.transactions), fill = "maroon") +
  labs(
    title = "Total Yearly Transactions",
    x = "Years",
    y = "Number of Transactions"
  )
