##################################################################
# Name: Analysis on Transactional Data from an eCommerce Platform
# Author: Azeem Mumtaz
##################################################################

# INITIALIZATION

basePath <- "~/project/personal/product-rec-using-apriori-algorithm"
setwd(basePath)
transactionFile <- paste(basePath,"/customer_transactions.csv", sep = "")  

originalOrderDF <- read.csv("anonymized-order-data.csv")

str(originalOrderDF)
head(originalOrderDF, 10)

## Run these install commands only when needed.
install.packages('ggplot2');
install.packages('DataExplorer');
install.packages('plyr');
install.packages('arules');
install.packages('arulesViz');

library(DataExplorer)
library(ggplot2)
library(plyr)
library(arules)
library(arulesViz)

# Structure will give the structure of the data frame
str(originalOrderDF)

# Summary or factored variables in the order data framework
# NA's means missing values / non applicable values
summary(originalOrderDF)

# DATA PRE-PROCESSING
orderDF <- originalOrderDF

## Let's remove line items with negative qty_cases
orderDF <- orderDF[orderDF$qty_case > 0, ]

## Let's remove line items with negative qty_each
orderDF <- orderDF[orderDF$qty_each >= 0, ]

## Let's remove line items with negative price
orderDF <- orderDF[orderDF$price > 0, ]

summary(orderDF)
str(orderDF)

plot_missing(orderDF)

## Functions to reduce multiple observations from orderDF to one to form an order transaction / cart
## Note: Paste function in R is used to collapse multiple product descriptions 
## into one variable separated by a comma.
reduceProductToOne <- function(orderDF)
{
  paste(orderDF$product_desc, collapse = ",")
}

reducePriceToOne <- function(orderDF)
{
  sum(orderDF$price)
}

reduceQtyToOne <- function(orderDF)
{
  sum(orderDF$qty_case) + sum(orderDF$qty_each)
}

reduceDateToOne <- function(orderDF)
{
  as.Date(as.POSIXct(head(orderDF$order_date,1)/1000, origin = "1970-01-01", tz = "GMT"))
}

reduceYearToOne <- function(orderDF)
{
  format(as.Date(as.POSIXct(head(orderDF$order_date,1)/1000, origin = "1970-01-01", tz = "GMT")), 
         format="%Y")
}

## Let's apply reduce functions and create DF with order_id and combined 
##product_description using a comma
orderProductDF <- ddply(orderDF, ~ order_id, reduceProductToOne)
colnames(orderProductDF) <- c("order_id", "product_descs")

## Let's apply reduce functions and create DF with order_id and sum of total
orderPriceDF <- ddply(orderDF, ~ order_id, reducePriceToOne)
colnames(orderPriceDF) <- c("order_id", "total")

## Let's apply reduce functions and create DF with order_id and sum of quantity
orderQtyDF <- ddply(orderDF, ~ order_id, reduceQtyToOne)
colnames(orderQtyDF) <- c("order_id", "qty")

## Let's apply reduce functions and create DF with order_id and select one ordered date from the head of multiple observations
orderDateDF <- ddply(orderDF, ~ order_id, reduceDateToOne)
colnames(orderDateDF) <- c("order_id", "date")

## Let's apply reduce functions and create DF with order_id and year of the ordered date
orderYearDF <- ddply(orderDF, ~ order_id, reduceYearToOne)
colnames(orderYearDF) <- c("order_id", "year")

## Merge all above data frames to form the final orderDF
finalOrderDF <- 
            merge(
              merge(
                merge(
                  merge(
                    orderProductDF, 
                    orderQtyDF, 
                    by = c("order_id")), 
                  orderPriceDF, 
                  by = c("order_id")),
                orderDateDF,
                by = c("order_id")),
              orderYearDF,
              by = c("order_id"))

## Remove any missing values (if any)
plot_missing(finalOrderDF)
finalOrderDF <- finalOrderDF[complete.cases(finalOrderDF), ]
plot_missing(finalOrderDF)

str(finalOrderDF)
head(finalOrderDF, 9)

## Let's remove outliers in qty
boxplot(finalOrderDF$qty, ylab = "qty", main = "Boxplot for the qty")

outliers <- boxplot(finalOrderDF$qty, ylab = "qty", main = "Boxplot for the qty")$out
qt <- quantile(finalOrderDF$qty, probs = c(0.25, 0.75))
span <- 1.5 * IQR(finalOrderDF$qty)
finalOrderDF$qty <- ifelse((finalOrderDF$qty < (qt[1] - span) | finalOrderDF$qty > (qt[2] + span)), 
                           NA, 
                           finalOrderDF$qty)
finalOrderDF <- finalOrderDF[complete.cases(finalOrderDF), ]

boxplot(finalOrderDF$qty, ylab = "qty", main = "Boxplot for the qty")

str(finalOrderDF)

## Total sales by year
ggplot(data = finalOrderDF) +
  geom_col(mapping = aes(x = year, y = total))

## Let's remove unnecessary variables for the association rules analysis.
productDF <- finalOrderDF
productDF$order_id = NULL
productDF$qty = NULL
productDF$total = NULL
productDF$date = NULL
productDF$year = NULL

str(productDF)

head(productDF, 10)

## Let's write the productDF to a CSV file for further analysis
write.csv(productDF,
          transactionFile, 
          quote = FALSE, 
          row.names = FALSE)

# ASSOCIATION RULE MINING 

## Let's load productDF data from CSV in to a transaction form from arules. 
## Note: The format is basket, because I saved the product_desc in one column as a comma seperated value.
finalOrderTX <- read.transactions(
  transactionFile, 
  format = 'basket', 
  sep = ',')

View(finalOrderTX)

summary(finalOrderTX)
class(finalOrderTX)

finalOrderTX@itemInfo[10:20,]

## Let's generate the association rules. 
## Note: maxlen is default 10
## Note: support and confidence values are calculated after trying our many values. 
associationRules <- apriori(finalOrderTX, 
                            parameter = list(
                              minlen = 2,
                              maxtime = 0,
                              support = 0.017, 
                              confidence = 0.9, 
                              target = "rules"))

summary(associationRules) 

## Inspect the first 10 rules. 
inspect(associationRules[1:10])

## Top 10 rules by the support value
inspect(head(sort(associationRules, by = "support"), 10))

## The following graph displays the scatterplot of 63 rules with minimum support criterion 0.017 with 0.9 confidence
plot(associationRules)

## The following plot shows the confidence and support over the number of products in rules. It shows that the more products in the rules has lesser support value. 
plot(associationRules, method = "two-key plot")

## The following graph displays the scatterplot with other quality metrics of 63 rules with minimum support criterion 0.017 with 0.9 confidence
plot(associationRules@quality)

## Top 10 rules by the lift value
inspect(head(sort(associationRules, by = "lift"), 10))

## Rules whose confidence is above 0.965
confidentRules <- associationRules[quality(associationRules)$confidence > 0.965] 

## Let's plot all high confidence rules in the matrix plot which shows 
## the left-hand side (LHS) and the right-hand side (RHS) of the rules.
## The colour indicates the lift while each square represents the confidence between the LHS and LHR. 
## Note: The command prints both LHS and LHR values in the console. 
plot(confidentRules, 
     method = "matrix", 
     measure = c("lift", "confidence"), 
     control = list(reorder = "measure"))

associationRulesByLift <- sort(associationRules, by = "lift")

## Let's plot the high lift rules in a graph plot
rulesWithHighestLift <- head(associationRulesByLift, 5) 

inspect(rulesWithHighestLift)
plot(rulesWithHighestLift, method = "graph")

## Let's generate the frequent itemset
## An itemset refers to a collection of items that contains some relationship (Dietrich, et al., 2015)
## Let's use the same support and confidence value which used in the association rule mining
## Note: When the maxlen parameter is not set, the algorithm continues each iteration until it runs out of support or until k reaches the default maxlen=10 (Dietrich, et al., 2015)
itemsets <- apriori(finalOrderTX, 
                    parameter = list(
                      minlen = 2,
                      maxtime = 0,
                      support = 0.017, 
                      confidence = 0.9, 
                      target="frequent itemsets"))

## This will show the summary of the itemset. 
## Note: This shows that the support distributes from 0.01702 to 0.01861for 
## k-itemsets where k is 4 and minimum value is 2. 
summary(itemsets)

## highest frequent itemsets based on their support value
inspect(head(sort(itemsets, by = "support"), 10))


## Testing the Model
##
## When a customer creates a cart and add items, based on the items in the current cart, 
## Shop can show list of products derived from the association rules as recommendations before 
## the customer checkout. 
##
## Suppose, the customer has following items in the cart.
##
## •	CHEESE
## •	CHICKEN
## 
## Then the following will give rules for above two items, which customers might 
## be interested to buy

test <- apriori(finalOrderTX, 
                parameter = list(
                  minlen = 2,
                  maxtime = 0,
                  support = 0.017, 
                  confidence = 0.9), 
                appearance = list(
                  lhs = c("CHEESE", "CHICKEN"),
                  default = "rhs"))

associationRulesForTestByLift <- sort(test, by = "lift")

rulesWithHighestLiftForTest <- head(
  associationRulesForTestByLift, 
  5) 
inspect(rulesWithHighestLiftForTest)
plot(rulesWithHighestLiftForTest, method = "graph")

# Additional Analysis (not required)
itemFrequencyPlot(finalOrderTX, 
                  topN = 10, 
                  type = "absolute",
                  main="Frequently Bought Items (absolute)")

itemFrequencyPlot(finalOrderTX, 
                  topN = 10,
                  type = "relative",
                  main="Frequently Bought Items (relative)")
