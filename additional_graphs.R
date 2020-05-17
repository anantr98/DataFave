library("dplyr")
library("ggplot2")
library("leaflet")
library("ggpubr")

#What trends do you notice for the store with respect to time?
profit <- read.csv(file = "trends.csv")


#line graph: time vs. profit
plot(profit[, "TOTAL_PRICESELL"], type = "l", col = "pink", xlab = "Month", ylab = "Dollars",
     main = "Monthly Trends: 03/2016 to 10/2019")
     
     lines(profit[, "TOTAL_PRICEBUY"], type = "l", col = "orange")
     lines(profit[, "PROFIT"], type = "l", col = "dark red")
     
     legend("topright", legend = c("Total Price Buy", 'Total Price Sell', 'Total Profit'),
            fill, col = c("orange", "pink", "dark red"), lwd = 2)
     
#findings:

     
#What trends do you notice for the store with respect to transactions?
transactions <- read.csv(file = "transactions.csv")

new_transactions <- transactions %>%
  group_by(month, year) %>%
  mutate(count = n()) %>%
  select(year, month, count)

new_transactions <- unique(new_transactions)

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
