# ---- Upload Dataset ----
bread_data <- read.csv("bread_basket.csv")

# ---- Basic Information ----
trans_id <- unique(bread_data$Transaction_Number)
Item <- unique(bread_data$Item)

n.transactions <- length(trans_id)
n.items <- length(Item)

sprintf("There are %d transactions.", n.transactions)
sprintf("There are %d items for selling.", n.items)

# ---- Convert Dataset into Matrix ----
complete_baskets <- matrix(0, n.transactions, n.items)
dimnames(complete_baskets) <- list(trans_id, Item)

for (i in seq_len(nrow(bread_data))) {
  current_id <- bread_data$Transaction_Number[i]
  current_item <- bread_data$Item[i]
  
  row_id <- match(current_id, trans_id)
  col_id <- match(current_item, Item)
  
  complete_baskets[row_id, col_id] <- complete_baskets[row_id, col_id] + 1
}

# ---- Association Rule ----
library(arules)

rules <- apriori(complete_baskets, 
                 parameter = list(supp = 0.04, conf = 0.01, target = "rules"))
rules_filtered <- subset(rules, lift > 1)

#inspect(rules)
#inspect(rules_filtered)

# ---- Association Rule Visualize ----
library(arulesViz)

plot(rules, 
     method = "scatterplot", 
     measure = c("support", "confidence"), 
     shading = "lift")

plot(rules_filtered, method = "graph", 
     control = list(
       type = "items",
       nodeCol = c("pink","orange"),
       edgeCol = "red"
       ))
# ---- Additional Section ----
# Create a DF that contains total counts for each items and the their frequency
item_totals <- colSums(complete_baskets)

item_counts_df <- data.frame(Item = names(item_totals), 
                             Count = as.numeric(item_totals), 
                             stringsAsFactors = FALSE)
item_counts_df$Percent_Transactions <- 
  round(item_counts_df$Count / n.transactions * 100, 2)

# Sort the dataframe from largest to smallest count
library(dplyr)

item_counts_df <- item_counts_df %>%
  arrange(desc(Count)) 

top_10_items <- head(item_counts_df, 10)
last_10_items <- tail(item_counts_df, 10)

sprintf("The top 10 purchased items are: %s", paste(top_10_items$Item, collapse = ", "))
sprintf("The last 10 purchased items are: %s", paste(last_10_items$Item, collapse = ", "))

# Visualization
library(ggplot2)

ggplot(rbind(top_10_items,last_10_items), aes(x = reorder(Item, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(Percent_Transactions)), 
            vjust = 0.5, 
            size = 3.5) +
  labs(title = "Top 10 & Last 10 Purchased Items",
       x = "Item",
       y = "Count of Purchases") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()

  
