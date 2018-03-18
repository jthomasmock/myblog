+++
# Date this page was created.
date = "2018-01-16"

# Project title.
title = "Customer Churn - EDA"

# Project summary to display on homepage.
summary = "Exploratory data analysis of a customer churn dataset"

# Optional image to display on homepage (relative to `static/img/` folder).
image_preview = "water_drop.jpg"

# Tags: can be used for filtering projects.
# Example: `tags = ["machine-learning", "exploratory-data-analysis"]`
tags = ["exploratory-data-analysis"]

# Optional external URL for project (replaces project detail page).
external_link = ""

# Does the project detail page use math formatting?
math = false

# Optional featured image (relative to `static/img/` folder).
[header]
image = "water_drop.jpg"
caption = ""

+++

### Executive Summary

Tenure is the biggest driver of churn, where ~42% of customer churn occurs during the first 6 months and 71.1% occurs within the first 12 months. Interestingly, 99.5% of the customer churn in the first 6 months and 98.9% in the first 12 months are customers on a month-to-month contract. Month-to-month customers make up 55% of the customer base, so while eliminating this type of contract is ill-advised, moving customers from a month-to-month to a one or two year contract should lead to decreased customer churn.

```{r, message=FALSE, warning=FALSE, echo = F}
library(tidyverse)
library(caret)
library(knitr)
library(kableExtra)
library(markdown)
library(Amelia)
library(stringr)
library(snakecase)
library(purrr)
library(randomForest)
```

### Data Preparation

Read the data into R, convert all column headers to snake_case, generate segments based on spending (low, medium, high) and custumer tenure (6 month increments).

```{r, echo = F}
churn_read <- read.csv("churn_data.csv")

# rename columns becuase I don't like CamelCase
# long live snake_case

colnames(churn_read) <- to_any_case(colnames(churn_read), case = "snake")
#churn_df <- churn_df %>% mutate(monthly_spend = monthly_charges, tenure_length = tenure)

# generate a duplicate column of monthly_spend
churn_read <- churn_read %>% mutate(monthly_spend = monthly_charges, tenure_segment = tenure, total_charges = monthly_charges*tenure)

churn_df <- churn_read

churn_df2 <- churn_df 


glimpse(churn_df)

tenure_seg <- function(tenure){
  if (tenure >= 0 && tenure <= 12) {
    return('0-12 Month')
  }else if (tenure > 12 && tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 && tenure <= 36){
    return('24-36 Month')
  }else if (tenure > 36 && tenure <= 48){
    return('36-48 Month')
  }else if (tenure > 48 && tenure <= 60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}

# apply tenure_seg function on each row of dataframe
# importantly, I tried doing this with 

# churn_df$tenure_segment <- sapply(churn_df$tenure_segment, tenure_seg)
# churn_df$tenure_segment <- sapply(churn_df$tenure,tenure_seg)

churn_df$tenure_segment <- churn_df$tenure_segment %>% map_chr(tenure_seg) 
churn_df$tenure_segment <- as.factor(churn_df$tenure_segment)
churn_df$tenure_segment <- factor(churn_df$tenure_segment, levels(churn_df$tenure_segment)[c(2, 7, 3:6, 1)])

churn_df <- churn_read

churn_df <- churn_df %>% 
  mutate(
    # use purrr to apply my segment function and create a new column
    tenure_segment = map_chr(tenure, tenure_seg),
    # convert the text into a factor
    tenure_segment = factor(tenure_segment),
    # and correctly order the levels of this factor
    tenure_segment = fct_relevel(tenure_segment, 
                       '0-12 Month', 
                       '12-24 Month', 
                       '24-36 Month', 
                       '36-48 Month', 
                       '48-60 Month',
                       '> 60 Month'))


levels(churn_df$tenure_segment)
churn_df2 <- churn_df 

churn_df$tenure_segment <- sapply(churn_df$tenure,tenure_seg)
churn_df2[[23]] <- churn_df2[[23]] %>% map_chr(tenure_seg) 

month_seg <- function(monthly_charges){
    if (monthly_charges >= 0 && monthly_charges <= 40){
        return('Low Spend')
    }else if(monthly_charges > 40 && monthly_charges <= 80){
        return('Medium Spend')
    }else if (monthly_charges >= 80 && monthly_charges > 80){
        return('High Spend')
    }
}

# apply tenure_seg function on each row of dataframe
# churn_df$monthly_charges <- sapply(churn_df$monthly_charges, month_seg)
# When I first tried to do this, I 
churn_df$monthly_spend <- sapply(churn_df$monthly_charges,month_seg)
churn_df$monthly_spend <- as.factor(churn_df$monthly_spend)
churn_df$monthly_spend <- factor(churn_df$monthly_spend, levels(churn_df$monthly_spend)[c(2, 3, 1)])

churn_df <- churn_read

churn_df <- churn_df %>% 
  mutate(monthly_segment = map_chr(monthly_charges, month_seg), 
         monthly_segment = factor(monthly_segment),
         monthly_segment = fct_relevel(monthly_segment, 
                       'Low Spend', 
                       'Medium Spend', 
                       'High Spend'))
levels(churn_df$monthly_segment)
```

#### Convert to binary coding
  Multiple lines, online security, online backup, device proteciton, tech support, streaming tv, streaming movies are converted to a yes/no binary code (exclude "no internet/phone service" as a code).

```{r, echo = F}
### Convert multiple columns to text
cols_character <- c("multiple_lines", "online_security", "online_backup", "device_protection", "tech_support", "streaming_tv", "streaming_movies")
cols_internet <- c("online_security", "online_backup", "device_protection", "tech_support", "streaming_tv", "streaming_movies")
churn_df[cols_character] <- sapply(churn_df[cols_character], as.character)

# Replace No phone service and no internet service with "No"
churn_df$multiple_lines <- str_replace(churn_df$multiple_lines, "No phone service", "No")
churn_df[cols_internet] <- data.frame(lapply(churn_df[cols_internet], function(x) {
  str_replace(x, "No internet service", "No")}))
churn_df[cols_character] <- data.frame(lapply(churn_df[cols_character], as.factor))
```

```{r, echo = F}
# Generate churn % summary data segmented by monthly segments
churn_segment_summary <- churn_df %>% 
  group_by(churn, tenure_segment) %>% 
  summarise(n = n()) %>% spread(churn, n) %>% 
  mutate(churn_percent_total = round(Yes/(nrow(filter(churn_df, churn == "Yes")))*100, digits = 1), churn_percent_segment = round(Yes/(Yes + No)*100, digits = 1))

churn_table <- churn_segment_summary %>% select(tenure_segment, churn_percent_segment, churn_percent_total) %>% rename(`Tenure Segment` = tenure_segment, `% Churn by Segment` = churn_percent_segment, `% Churn by Total Churn` = churn_percent_total)

```

```{r, echo = F}
# Generate churn % summary data segmented by monthly segments
churn_billing_summary <- churn_df %>% 
  group_by(churn, tenure_segment, contract) %>% 
  summarise(n = n()) %>% spread(churn, n) %>% 
  mutate(churn_percent_total = round(Yes/(nrow(filter(churn_df, churn == "Yes")))*100, digits = 1), churn_percent_segment = round(Yes/(Yes + No)*100, digits = 1))

```

### Generate summary data
We can see that the vast majority (`r churn_table[1,3] + churn_table[2,3]`%) of the customer churn occurs during the 1st 12 months

```{r, echo = F}
kable(churn_table, format = "html", booktabs = T)
```


Additionally, we can graphically examine the frequency of customer churn as a function of customer tenure interacting with contract type (eg month-to-month vs a one to two year contract).
```{r, echo = F}
(g1 <- ggplot(data = churn_df) + 
   geom_histogram(aes(x = tenure, color = churn, fill = churn), binwidth = 6 , alpha = 0.3) +
   facet_grid(~contract) +
   scale_color_manual(values = c("#1e90ff", "#FF0000", "#999999")) +
   scale_fill_manual(values = c("#1e90ff", "#FF0000", "#999999")))
```

When we break down customer churn by contract type and spending segemts, we can again see that the almost all of the customers who churn are in the month-to-month plan. Additionally, a majority of customers churn during the first 6 months in the low spend (< $40/month) and medium spend (< $80 month), while the high spend (> $80/month) group has churn out to about ~18 months and has a higher ratio of churn to retained customers as well.

```{r, echo = F, message = F}
(g2 <- ggplot(data = churn_df) + 
   geom_histogram(aes(x = tenure, color = churn, fill = churn), binwidth = 6, alpha = 0.3) +
   facet_grid(contract~monthly_segment) +
   scale_color_manual(values = c("#1e90ff", "#FF0000", "#999999")) +
   scale_fill_manual(values = c("#1e90ff", "#FF0000", "#999999")))
```

Paperless billing doesn't appear to have a large effect on customer churn, as the ratio of churn to retained customers is similar across groups. However there are more overall customers in the paperless billing segment with corresponding more churning customers overall.

```{r, echo = F}
(g3 <- ggplot(data = churn_df) + 
   geom_histogram(aes(x = tenure, color = churn, fill = churn), binwidth = 6, alpha = 0.3) +
   facet_grid(contract ~paperless_billing) +
   scale_color_manual(values = c("#1e90ff", "#FF0000", "#999999")) +
   scale_fill_manual(values = c("#1e90ff", "#FF0000", "#999999")))
```

We do have a bit of an interesting trend according to monthly spending, as there is an uptick in churn customers in the $75-100/month range across all the tenure segments, although customer churn even in the high-spend category does go down across customer-tenure. Perhaps customers in the high spending categories are more aggressively examining price comparisons between companies.

```{r, echo = F}
# this is basically the same graph as the following
#(g4 <- ggplot(data = churn_df) + 
  # geom_histogram(aes(x = monthly_charges, color = churn, fill = churn), binwidth = 6, alpha = 0.3) +
  # facet_grid(monthly_segment~tenure_segment))
```

```{r, echo = F}
(g5 <- ggplot(data = churn_df) + 
   geom_bar(aes(x = monthly_segment, color = churn, fill = churn), alpha = 0.3) +
   facet_grid(~tenure_segment) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
   scale_color_manual(values = c("#1e90ff", "#FF0000", "#999999")) +
   scale_fill_manual(values = c("#1e90ff", "#FF0000", "#999999")) +
   xlab("Monthly Spending Segment"))
```

Although less useful than the histograms, we can see the same trend here in the jittered plot - namely that there is less churn as tenure increases, however as spending increases so does churn.

```{r, echo = F}
(g5 <- ggplot(data = churn_df) + 
   geom_jitter(aes(y = tenure, x = churn, color = churn), alpha = 0.3) +
   geom_violin(aes(y = tenure, x = churn, fill = churn, color = churn), alpha = 0.5) +
   facet_wrap(~monthly_segment, ncol = 1) +
   scale_color_manual(values = c("#1e90ff", "#FF0000", "#999999")) +
   scale_fill_manual(values = c("#1e90ff", "#FF0000", "#999999")))
```
  
Paperless billing appears to not be a significant factor in the low or medium spend groups, while there is a distinct uptick across tenure for customers who are both high-spend and paper-less billing. Perhaps digital customers are more likely to "jump ship" and are more price-aware than customers who mail in their checks each month.

```{r, echo = F}
(g6 <- ggplot(data = churn_df) + 
   geom_histogram(aes(x = tenure, color = churn, fill = churn), alpha = 0.3, binwidth = 6) +
   facet_grid(monthly_segment~paperless_billing) +
   scale_color_manual(values = c("#1e90ff", "#FF0000", "#999999")) +
   scale_fill_manual(values = c("#1e90ff", "#FF0000", "#999999")))
```


```{r, echo = F}
#(g7 <- ggplot(data = churn_df) + 
  # geom_histogram(aes(x = tenure, color = churn, fill = churn), alpha = 0.3, binwidth = 1) +
  # facet_grid(monthly_segment ~ .) +
  # xlab("Months as Customer"))
```

