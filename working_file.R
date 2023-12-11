rm(list = ls())
library(tidyverse)
library(ggplot2)
library(plm)
library(ggrepel)
library(cowplot)

vars_to_int = c("year", "num_ODA_projects", "num_OOF_projects", "rec_taiwan")
vars_to_num = c("sum_ODA_value", "sum_CNY_OOF_value", "sum_USD_OOF_value",
                "sum_EUR_OOF_value", "sum_other_OOF_value", "corruption_index",
                "gnipc", "gdppc", "world_ODA_value", "OOF_avg_interest_rate") 
setwd("/Users/michaelalisky/Library/CloudStorage/OneDrive-Stanford/china_lending")

data <- read.csv("chinese_lending.csv") %>% 
  mutate_at(vars(vars_to_int), as.integer) %>%
  mutate_at(vars(vars_to_num), as.numeric) %>%
  mutate(income_status = ifelse(income_status == 0.0, NA, as.character(income_status)))

ggplot(data, aes(x = world_ODA_value, y = num_ODA_projects)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(title = "Figure 6: Chinese ODA versus Rest of the World",
       x = "Total ODA received (current US$ millions)",
       y = "Number of Chinese ODA Projects") +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


ggplot(data, aes(x = world_ODA_value, y = sum_ODA_value)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(title = "Figure 4: Chinese ODA versus Rest of the World",
       x = "Total ODA received (current US$ millions)",
       y = "Value of Chinese ODA Projects (current US$ millions)") +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplot(data, aes(x = gnipc, y = num_ODA_projects, label = paste(name, year))) +
  geom_point() +
  labs(x = "gnipc", y = "number of ODA projects received in a given year", title = 
         "Figure 2: # of ODA projects vs GNI per capita") + 
  geom_smooth(method = lm, se = FALSE)


data %>% filter(rec_taiwan == 1) %>% 
  ggplot(aes(x = gnipc, y = num_ODA_projects, label = paste(name, year))) +
  geom_label(size=2) +
  labs(x = "GNI per capita", y = "number of ODA projects received in a given year", title = 
         "Figure 7c: Countries that Recognize Taiwan") + 
  geom_smooth(method = lm, se = FALSE) + 
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

data_taiwan <- data %>% filter(rec_taiwan == 1) %>%
  group_by(name) %>% summarize(agg_proj = sum(num_ODA_projects),
                               year = year,
                               income_status = income_status)


data.tab <- data %>% group_by(year) %>% 
  summarize(CNY = sum(sum_CNY_OOF_value), 
            USD = sum(sum_USD_OOF_value),
            other = sum(sum_other_OOF_value),
            EUR = sum(sum_EUR_OOF_value)) 

data_long <- tidyr::pivot_longer(data.tab, cols = -year, names_to = "Currency", values_to = "Value")

# Create a stacked bar graph
ggplot(data_long, aes(x = year, y = Value, fill = Currency)) +
  geom_bar(stat = "identity") +
  labs(title = "Figure 1: Chinese Lending Currency Denomination",
       x = "Year",
       y = "Value",
       fill = "Currency") + 
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

data_increase_total <- data %>%
  group_by(name) %>%
  summarize(
    agg_CNY = sum(sum_CNY_OOF_value)
  ) %>%
  arrange(desc(agg_CNY)) %>%
  mutate(name = ifelse(row_number() <= 15, as.character(name), "other")) %>%
  group_by(name) %>%
  summarize(
    agg_CNY = sum(agg_CNY)
  ) %>%
  arrange(desc(agg_CNY))

ggplot(data_increase_total, aes(x = reorder(name, -agg_CNY), y = agg_CNY, fill = name)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Figure 2: Total CNY received by Country, 2000—2021",
       x = "Country",
       y = "Value of CNY received (in 2021 $US)") +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(fill = FALSE)

data <- data %>% mutate(total_OOF = sum_CNY_OOF_value + sum_USD_OOF_value + sum_EUR_OOF_value + sum_other_OOF_value,
                        pct_CNY = sum_CNY_OOF_value / total_OOF, 
                        pct_USD = sum_USD_OOF_value / total_OOF, 
                        pct_EUR = sum_USD_OOF_value / total_OOF,
                        pct_other = sum_other_OOF_value / total_OOF)

ggplot(data, aes(x = gnipc, y = sum_CNY_OOF_value)) +
  geom_point() +
  labs(x = "gnipc", y = "Value of CNY-denominated loans in a given year", title = 
         "Figure 6: Value of CNY-denominated loans vs GNI per capita") + 
  geom_smooth(method = lm, se = FALSE)

ggplot(data, aes(x = gnipc, y = sum_USD_OOF_value)) +
  geom_point() +
  labs(x = "gnipc", y = "Value of USD-denominated loans in a given year", title = 
         "Figure 7: Value of USD-denominated loans vs GNI per capita") + 
  geom_smooth(method = lm, se = FALSE)

ggplot(data, aes(x = gnipc, y = sum_other_OOF_value)) +
  geom_point() +
  labs(x = "gnipc", y = "Value of local-denominated loans in a given year", title = 
         "Figure 8: Value of local-denominated loans vs GNI per capita") + 
  geom_smooth(method = lm, se = FALSE)

ggplot(data, aes(x = gnipc, y = sum_EUR_OOF_value)) +
  geom_point() +
  labs(x = "gnipc", y = "Value of EUR-denominated loans in a given year", title = 
         "Figure 9: Value of EUR-denominated loans vs GNI per capita") + 
  geom_smooth(method = lm, se = FALSE)

data_income <- data %>% group_by(income_status) %>% 
  summarize(total_ODA_projects = sum(num_ODA_projects), 
            aggregate_value = sum(sum_ODA_value), number_observations = n(),
            average_ODA_value = aggregate_value / total_ODA_projects)

data %>% ggplot(aes(x = gdppc, y = num_ODA_projects)) +
  geom_point() + geom_smooth(method = lm, se = FALSE, color = "blue") +
  theme_minimal() + ylim(0, 80) +
  labs(x = "GDP per capita",
       y = "Number of ODA projects received",
       title = "Figure 1: GDP per capita vs number of ODA projects") +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

data <- data %>% mutate(sum_OOF_value = as.integer(sum_CNY_OOF_value + sum_USD_OOF_value + sum_EUR_OOF_value + sum_other_OOF_value))

panel_data <- pdata.frame(data, index = c("name", "year"))

model_ODA_num <- plm(num_ODA_projects ~ gnipc + gdppc + rec_taiwan + corruption_index, data = panel_data, model = "within")
model_ODA_val <- plm(sum_ODA_value ~ gnipc + gdppc + rec_taiwan + corruption_index, data = panel_data, model = "within")

model_CNY <- plm(sum_CNY_OOF_value ~ gnipc + gdppc + rec_taiwan + corruption_index, data = panel_data, model = "within")
model_USD <- plm(sum_USD_OOF_value ~ gnipc + gdppc + rec_taiwan + corruption_index, data = panel_data, model = "within")
model_other <- plm(sum_other_OOF_value ~ gnipc + gdppc + rec_taiwan + corruption_index, data = panel_data, model = "within")

model_OOF <- plm(sum_OOF_value ~ gnipc + gdppc + rec_taiwan + corruption_index, data = panel_data, model = "within")
summary(model_OOF)
stargazer(model_ODA_num, model_OOF, model_CNY, model_USD, model_other, type = "text")

model_basic <- plm(num_ODA_projects ~ gnipc + world_ODA_value, data = panel_data, model = "within")
summary(model_basic)
stargazer(model_basic, type="text")


data %>% ggplot(aes(x = corruption_index, y = num_ODA_projects)) +
  geom_point() + geom_smooth(method = lm, se = FALSE, color = "blue") +
  theme_minimal() + ylim(0, 80) +
  labs(x = "Corruption Index",
       y = "Number of ODA projects received",
       title = "Figure 5: Corruption vs number of ODA projects") +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


data %>% group_by(name) %>% summarize(avg_ODA_proj = mean(num_ODA_projects)) %>% summarize(avg_avg = mean(avg_ODA_proj))
