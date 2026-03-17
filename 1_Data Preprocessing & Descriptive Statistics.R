library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(corrplot)
library(scales)
library(data.table)


setwd("E:/NTU BA/T1/6003 analytics strategy/group project")
df <- fread("marketing_campaign.csv")


# data preprocessing
# delete the quotation mark in the first column and last column
df[[1]] <- gsub("^\"|\"$", "", df[[1]])
df[[ncol(df)]] <- gsub("^\"|\"$", "", df[[ncol(df)]])
names(df) <- gsub("\"", "", names(df))
# delete the blank in the "2n Cycle“
df$Education <- gsub(" ", "", df$Education)
# delete the missing values
sum(is.na(df))
df <- na.omit(df)
fwrite(df, file = "E:/NTU BA/T1/6003 analytics strategy/group project/dataset/marketing_campaign_clean.csv")

# set the type of variables
df$Education <- factor(df$Education)
df$Marital_Status <- factor(df$Marital_Status)
df$AcceptedCmp1 <- factor(df$AcceptedCmp1)
df$AcceptedCmp2 <- factor(df$AcceptedCmp2)
df$AcceptedCmp3 <- factor(df$AcceptedCmp3)
df$AcceptedCmp4 <- factor(df$AcceptedCmp4)
df$AcceptedCmp5 <- factor(df$AcceptedCmp5)
df$Complain <- factor(df$Complain)
df$Response <- factor(df$Response)
df$Children <- df$Kidhome + df$Teenhome # calculate the number of children in one family
df$Age <- 2021 - df$Year_Birth
df <- subset(df, Marital_Status != "Absurd")

summary(df)

# 1 Basic information
# 1.1 age distribution
ggplot(df, aes(y = Age)) +
  geom_boxplot(fill = "#6DBE45") +
  labs(title = "Box Plot of Age", y = "Age")+
  theme_minimal()
# dicover that outliers of age exist in the dataset
# calculate IQR
Q1 <- quantile(df$Age, 0.25, na.rm = TRUE)
Q3 <- quantile(df$Age, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower <- Q1 - 1.5 * IQR
upper <- Q3 + 1.5 * IQR
# drop the outlier
df1 <- df[df$Age >= lower & df$Age <= upper, ]
# amended age distribution
ggplot(df1, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "#6DBE45", color = "white") +
  labs(title = "Distribution of Age",
       x = "Age",
       y = "Count") +
  theme_minimal()

# 1.2 education distribution - bar chart
ggplot(df1, aes(x = Education)) +
  geom_bar(fill = "#6DBE45", color = "white") +
  geom_text(stat = "count", 
            aes(label = ..count..),
            vjust = 0.5) + 
  labs(title = "Distribution of Education",
       x = "Education",
       y = "Count") +
  theme_minimal()

# 1.3 marital distribution - pie chart
ggplot(df1, aes(x = Marital_Status)) +
  geom_bar(fill = "#6DBE45", color = "white") +
  geom_text(stat = "count", 
            aes(label = ..count..),
            vjust = 0.5) + 
  labs(title = "Distribution of Marital Status",
       x = "Marital Status",
       y = "Count") +
  theme_minimal()


# 1.4 income distribution
# box plot of income
ggplot(df1, aes(y = Income)) +
  geom_boxplot(fill = "#6DBE45") +
  labs(title = "Box Plot of Income", y = "Income")+
  theme_minimal()

summary(df1$Income)

ggplot(df1, aes(x = Income)) +
  geom_histogram(fill = "#6DBE45", color = "white") +
  scale_x_log10(labels = comma) +
  labs(title = "Distribution of Income",
       x = "Income",
       y = "Count") +
  theme_minimal()


# 1.5 Number of Users by Children Count - bar chart
df1$Children1 <- factor(df1$Children)
ggplot(df1, aes(x = Children1)) +
  geom_bar(fill = "#6DBE45") +
  geom_text(stat = "count", 
            aes(label = ..count..),
            vjust = 0.5) + 
  labs(title = "Number of Users by Children Count",
       x = "Number of Children",
       y = "User Count") +
  theme_minimal()


# 1.6 Distribution of Recency
ggplot(df1, aes(x = Recency)) +
  geom_histogram(binwidth = 10, fill = "#6DBE45", color = "white") +
  labs(title = "Distribution of Recency",
       x = "Recency",
       y = "Count") +
  theme_minimal()

# 2 Consumption Behavior
# 2.1 consumption amount categorized by marital status
# convert the table into one column for "Product Type" and one column for "Consumption Amount".
df_long <- df %>%
  pivot_longer(cols = starts_with("Mnt"), 
               names_to = "Product",
               values_to = "Amount")
# plot the bar chart of consumption amount categorized by marital status
ggplot(df_long, aes(x = Product, y = Amount, fill = Marital_Status)) +
  geom_col(position = position_dodge()) +
  labs(title = "Purchases by Product Category and Marital Status",
       x = "Product Category",
       y = "Total Purchase Amount") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set4")

# 2.2 consumption amount categorized by education/marital status
# plot the bar chart of consumption amount categorized by education
ggplot(df_long, aes(x = Product, y = Amount, fill = Education)) +
  geom_col(position = position_dodge()) +
  labs(title = "Purchases by Product Category and Education",
       x = "Product Category",
       y = "Total Purchase Amount") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set4")

# plot the bar chart of income categorized by marital status
ggplot(df_long, aes(x = Product, y = Amount, fill = Education)) +
  geom_col(position = position_dodge()) +
  labs(title = "Purchases by Product Category and Education",
       x = "Product Category",
       y = "Total Purchase Amount") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set4")

# 2.3 response by marital status/education
# response by marital status
df1$Response <- factor(df1$Response, levels = c(0,1), labels = c("No Response", "Response"))
ggplot(df1, aes(x = Marital_Status, fill = Response)) +
  geom_bar(position = "stack") +
  scale_fill_manual(values = c("No Response" = "#006A4D", "Response" = "#6DBE45")) +
  labs(title = "Response by Marital Status",
       x = "Marital Status",
       y = "Count") +
  theme_minimal()

# response by education
ggplot(df1, aes(x = Education, fill = Response)) +
  geom_bar(position = "stack") +
  scale_fill_manual(values = c("No Response" = "#006A4D", "Response" = "#6DBE45")) +
  labs(title = "Response by Education",
       x = "Education",
       y = "Count") +
  theme_minimal()


# 2.4 number of purchases categorized by marital status/education
# convert the table into one column for "Purchase Channel" and one column for "Number of Purchases".
df_long2 <- df %>%
  pivot_longer(cols = starts_with("Num"), 
               names_to = "Purchase_channel",
               values_to = "Amount")

# plot the bar chart of number of purchases categorized by marital status
ggplot(df_long2, aes(x = Purchase_channel, y = Amount, fill = Marital_Status)) +
  geom_col(position = position_dodge()) +
  labs(title = "Number of Purchases by Channel and Marital Status",
       x = "Purchase Channel",
       y = "Total Number of Purchases") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set4")

# plot the bar chart of number of purchases categorized by education
ggplot(df_long2, aes(x = Purchase_channel, y = Amount, fill = Education)) +
  geom_col(position = position_dodge()) +
  labs(title = "Number of Purchases by Channel and Education",
       x = "Purchase Channel",
       y = "Total Number of Purchases") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set4")


# 3 Promotion Acceptance
# 3.1 percentage of accepting each campaign
p1 <- ggplot(df1, aes(x = "", fill = AcceptedCmp1)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Proportion of Participating in Campaign 1") +
  theme_void() +
  scale_fill_brewer(palette = "Set3")

p2 <- ggplot(df1, aes(x = "", fill = AcceptedCmp2)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Proportion of Participating in Campaign 2") +
  theme_void() +
  scale_fill_brewer(palette = "Set3")

p3 <- ggplot(df1, aes(x = "", fill = AcceptedCmp3)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Proportion of Participating in Campaign 3") +
  theme_void() +
  scale_fill_brewer(palette = "Set3")

p4 <- ggplot(df1, aes(x = "", fill = AcceptedCmp4)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Proportion of Participating in Campaign 4") +
  theme_void() +
  scale_fill_brewer(palette = "Set3")

p5 <- ggplot(df1, aes(x = "", fill = AcceptedCmp5)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Proportion of Participating in Campaign 5") +
  theme_void() +
  scale_fill_brewer(palette = "Set3")

(p1 | p2 | p3) / (p4 | p5)


# 4 Complain Behavior
df1$Complain <- factor(df1$Complain, levels = c(0,1), labels = c("No Complain", "Complain"))
ggplot(df1, aes(x = "", fill = Complain)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Proportion of Complain") +
  theme_void() +
  scale_fill_brewer(palette = "Set4")