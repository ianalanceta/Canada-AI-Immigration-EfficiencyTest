# Canada-AI-Immigration-EfficiencyTest
An exploration the extent in which (represented by the annual proportion of applications processed) AI improves efficiency in Canada’s immigration intake. 
---
title: "Thesis: Entry AlgorithmsS: AI, Efficiency, and Canadian Immgiration"
author: "235531, Iana Lanceta"
date: "April 28, 2025"
output:
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
```

## Step 1: Load Data & Packages
```{r}
setwd("C:/Users/ianal/OneDrive/Documents/Academic/Hertie MPP/2025 Spring Semester/Thesis/Drafts/Data")

TRV_Apps <- read.csv("C:/Users/ianal/OneDrive/Documents/Academic/Hertie MPP/2025 Spring Semester/Thesis/Drafts/Data/2013-2022 TRVs by Country & Time (Clean).csv")

library(AER)
library(stargazer)
library(ggplot2)
library(GGally)
library(corrplot) 
library(lmtest)  
library(sandwich) 
library(car)  

library(dplyr) 
library(ggplot2) 
library(rdrobust) 
library(readr) 
library(modelsummary) 
library(scales)
library(broom)
library(knitr)

options(scipen = 999)

```


## Step 2: Cleaning Missing Values and Setting Dummy Variables 
```{r}
# Step 2.1: Transforming Categorical to Interval Data
TRV_Apps$total_received <- as.numeric(TRV_Apps$total_received)
TRV_Apps$rec_proc_diff_prop <- as.numeric(TRV_Apps$rec_proc_diff_prop)

# Top 10 Migration Source Countries
top10<- ifelse(TRV_Apps$top_10 == "yes", 1, 0) 
# "yes (in top 10)" is now 1, "no (not in top 10)" is 0.

# Global Development Status (North vs. South)
global_dev <- ifelse(TRV_Apps$global_dev == "GS", 1, 0)
# "GS (global south)" is now 1, "GN (global north)" is 0. 

# Target Levels Score
target_score <- ifelse(TRV_Apps$target_score == "Average", 1, 0)
# An "Average" target level is now 1, a "High" target level is now 0. 

# Party in Leadership
leading_party <- ifelse(TRV_Apps$leading_party == "Liberal", 1, 0)
# When the party in power for the year was "Liberal" the code is 1, 
# When the party in power for the year was "Conservative" the code is 0.  

# Step 2.2: Removing NA values
TRV_Apps_cleaned <- na.omit(TRV_Apps)

```


## Descriptive Statistics
### Figure 8: Figure 8 - Total TRV Applications Processed from 2013-2022
```{r}

# First, aggregate the data 
applications_by_year <- aggregate(total_processed ~ year,
                                   data = TRV_Apps_cleaned,
                                   FUN = sum)

# Create a bar chart using ggplot2
ggplot(applications_by_year, aes(x = year, y = total_processed)) +
  geom_bar(stat = "identity", fill = "#8EAADB") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = unique(floor(applications_by_year$year)),
                     labels = unique(floor(applications_by_year$year))) +
  labs(x = "Year",
       y = "Number of Applications Processed") +
theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5), 
    axis.line = element_line(linewidth = 0.5, color = "black"), 
    axis.text.x = element_text(margin = margin(t = 10)), 
    axis.text.y = element_text(margin = margin(r = 10)), 
    axis.title.x = element_text(margin = margin(t = 15)), 
    axis.title.y = element_text(margin = margin(r = 15))  
  ) +
  scale_y_continuous(labels = scales::comma) 

ggsave("Figure 8.png")

```

### Figure 9: Distribution of TRV Applications Processed by Year
```{r}
ggplot(TRV_Apps_cleaned, aes(x = factor(year), y = rec_proc_diff_prop )) +
  geom_boxplot(width = 0.1, fill = "white", color = "black") + 
  scale_y_continuous(labels = percent) +
  scale_x_discrete(name = "Year") +
  labs(
    y = "Proportion of TRV Applications Processed"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(margin = margin(t = 10)), 
    axis.text.y = element_text(margin = margin(r = 10)), 
   axis.title.x = element_text(margin = margin(t = 15)), 
    axis.title.y = element_text(margin = margin(r = 15))  
  ) +
  scale_y_continuous(labels = scales::comma) 

ggsave("Figure 9.png")

```

### Figure 10: Median Processing Percentage by Year and Country Type
```{r}
yearly_top10_median <- TRV_Apps_cleaned %>%
  group_by(year, top_10) %>%
  summarize(median_proportion = median(rec_proc_diff_prop, na.rm = TRUE))

ggplot(yearly_top10_median, aes(x = factor(year), y = median_proportion, fill = factor(top_10))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#8EAADB", "#F08080"), 
                    labels = c("Other", "Top 10"),
                    name = "Immigration Country Type") +
  scale_y_continuous(labels = percent) +
  labs(x = "Year", y = "Median Percentage Processed") +
  theme_bw()

ggsave("Figure 10.png")

```

### Figure 11: External Factor Analysis: Country Type, Global Development Status, and Application Response
```{r}

# Median Positive Responses
countrytype_dev_approved <- TRV_Apps_cleaned %>%
  group_by(top_10, global_dev) %>%
  summarize(med_approved = median(status_approved, na.rm = TRUE),
            .groups = "drop")

# Median Negative Responses
countrytype_dev_negative <- TRV_Apps_cleaned %>%
  group_by(top_10, global_dev) %>%
  summarize(med_negative = median(status_negative, na.rm = TRUE),
            .groups = "drop")

# Creating a Faceted Heat Map
heatmap_data_combined <- bind_rows(
  countrytype_dev_approved %>% rename(value = med_approved) %>% mutate(metric = "Median Approved"),
  countrytype_dev_negative %>% rename(value = med_negative) %>% mutate(metric = "Median Refused/Withdrawn")
)

ggplot(heatmap_data_combined, aes(x = factor(top_10), y = factor(global_dev), fill = value)) +
  geom_tile() +
  scale_x_discrete(labels = c("Other", "Top 10"), name = "Immigration Country Type") +
  scale_y_discrete(labels = c("Global North", "Global South"), name = "Global Development Status") +
  facet_wrap(~ metric, ncol = 3, scales = "free_y") +  
  scale_fill_gradient(low = "#8EAADB", high = "#F08080",
                      limits = c(min(heatmap_data_combined$value[heatmap_data_combined$metric != "Median Proportion"]),
                                 max(heatmap_data_combined$value[heatmap_data_combined$metric != "Median Proportion"])),
                      name = "Count") +
  labs(x = "Country Type", y = "Global Dev. Status") +
  theme_bw() +
  theme(
  plot.title = element_text(hjust = 0.5), 
  axis.title.x = element_text(margin = margin(t = 15, b = 15)),
  axis.title.y = element_text(margin = margin(r = 15, l = 15))
)

ggsave("Figure 11.png")

```


## Results: Regression Models
### Figure 12: Simple Linear Regression Model: Impact of Year on Proportion of Applications Processed

```{r}
# 2017 is the intervening year, so the effect this year has on proportion of applications processed will be examined by first converting year into a factor.

TRV_Apps_cleaned$year_factor <- as.factor(TRV_Apps_cleaned$year)

# Next a simple (i.e., without control variables) linear regression is run. 
model1 <- lm(rec_proc_diff_prop ~ year_factor, data = TRV_Apps_cleaned)
summary(model1)

```

The above summary is then generated as a table to be put into the document. 
```{r} 

M1covariate_labels <- c(
  "2014",
  "2015",
  "2016",
  "2017",
  "2018",
  "2019",
  "2020",
  "2021",
  "2022"
)
suppressWarnings({
  stargazer(model1,
            type = "text",
            dep.var.labels = "Proportion of Applications Processed",
            covariate.labels = M1covariate_labels,
            out = "model_output.txt")
})
```


### Figure 13: Multivariate Regression Model: Impact of Year and External Factors on PAPs
```{r}
# Since the effect of target levels and leading party is hard to isolate by years (as they are considered in years), interactive models are employed. 

model2_interact_score <- lm(rec_proc_diff_prop ~ year_factor + global_dev * target_score * top_10 * leading_party, data = TRV_Apps_cleaned)

summary(model2_interact_score)

```

Again, the above summary is then generated as a table to be put into the document. 
```{r}
M3covariate_labels <- c(
  "2014",
  "2015",
  "2016",
  "2017",
  "2018",
  "2019",
  "2020",
  "2021",
  "2022",
  "Top 10 Migration Source Country (Top 10)",
  "Leading Political Party",
  "Global Development Status (GDS)",
  "GDS x Target Score",
  "Target Score x Top 10",
  "GDSs x Leading Political Party",
  "Target Score x Leading Political Party",
  "Top 10 x Leading Political Party",
  "GDS x Target Score x Top 10",
  "GDS x Target Score x Leading Political Party",
  "GDS x Top 10 x Leading Political Party",
  "Target Score x Top 10 x Leading Political Party",
  "GDS x Target Score x Top 10 x Leading Political Party",
  "Target Score",
  "Constant"
)
suppressWarnings({
  stargazer(model2_interact_score,
            type = "text",
            dep.var.labels = "Proportion of Applications Processed",
            covariate.labels = M3covariate_labels,
            out = "model_output.txt",
            single.row = TRUE,
            no.space = TRUE,
            font.size = "small"
            )
})
```
