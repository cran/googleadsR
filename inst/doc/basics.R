## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

load(file = "my_data_googleads.rda")

## ---- eval = FALSE------------------------------------------------------------
#  library(googleadsR)
#  my_data_googleads <-
#    windsor_fetch_googleAds(
#    api_key = "your api key",
#    date_preset = "last_7d",
#    fields = c("source", "campaign", "clicks",
#               "medium", "sessions", "spend")
#  )

## ---- echo = FALSE------------------------------------------------------------
library(googleadsR)

## -----------------------------------------------------------------------------
str(my_data_googleads)

## -----------------------------------------------------------------------------
library(dplyr)
library(ggplot2)

top_10 <- 
  my_data_googleads %>% 
  filter(clicks > 0) %>% 
  group_by(campaign) %>% 
  summarise(n_clicks = sum(clicks)) %>% 
  ungroup %>% 
  arrange(desc(n_clicks)) %>% 
  slice_head(n = 10)

knitr::kable(top_10)

## ---- fig.width=7-------------------------------------------------------------
ggplot(top_10, aes(x = n_clicks, y = campaign)) +
  geom_col()

## ---- fig.width= 7------------------------------------------------------------
my_data_googleads %>%
  filter(clicks > 0) %>%
  group_by(campaign) %>%
  summarise(sum_spend = sum(as.numeric(spend))) %>%
  ungroup %>%
  arrange(desc(sum_spend)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = sum_spend, y = campaign)) +
  geom_col()

## ---- fig.width=10------------------------------------------------------------
library(tidyr)

my_data_googleads %>%
  filter(clicks > 0) %>%
  group_by(campaign) %>%
  summarise(n_clicks = sum(clicks), sum_spend = sum(as.numeric(spend))) %>%
  arrange(desc(sum_spend)) %>%
  slice_head(n = 10) %>%
  pivot_longer(cols = c("n_clicks", "sum_spend"), names_to = "aggreg", values_to = "values") %>%  
  ggplot(aes(x = values, y = campaign, fill = aggreg)) +
  geom_col() +
  facet_wrap("aggreg", ncol = 2) + 
  theme(legend.position="bottom")

