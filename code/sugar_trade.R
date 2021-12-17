#Sugar trade dataviz by Amanda Fanelli, december 2021

#Loading packages ----
library(tidyverse)
library(countrycode)
library(ggflags)
library(ggtext)
library(showtext)
library(patchwork)
library(grid)

#Analyzing data about raw sugar production ----

#Reading data from FAO stats
#Data was downloaded from <https://www.fao.org/faostat/en/#data/TCL> 
#Countries,Elements,Years > Select all, Items > Sugar crops nes, Sugar nes, Sugar non-centrifugal, Sugar Raw Centrifugal, Sugar refined
sugar <- read_csv("input_data/FAOSTAT_sugar_trade.csv")

sugar %>% count(Item)

#Selecting sugar raw centrifugal
raw_sugar <- sugar %>%
  filter(Item == "Sugar Raw Centrifugal") 

#Counting how many countries
View(raw_sugar %>% filter(Year == 2019) %>% count(Area))

#Selecting raw sugar from 2019 and only relevant data
#Removing China because it was counted twice( all China, mainland, Taiwan, Macao,HK)
raw_sugar_2019 <- raw_sugar %>%
  filter(Year == 2019) %>%
  filter(Area != "China") %>%
  select(Area, Year, Element, Year, Value, Flag, `Flag Description`) %>%
  mutate(Value = Value * 10^-6) %>%
  mutate(Area = str_replace(Area, "United States of America", "United States")) 
  


#Total export and import value
imp_value_2019 <- raw_sugar_2019 %>%
  filter(Element == "Import Value") %>%
  arrange(desc(Value))

total_imp_value <- sum(imp_value_2019$Value)

exp_value_2019 <- raw_sugar_2019 %>%
  filter(Element == "Export Value") %>%
  arrange(desc(Value))

total_exp_value <- sum(exp_value_2019$Value)

#Export and Import quantity

#Top sugar exporters
export <- raw_sugar_2019 %>%
  filter(Element == "Export Quantity") %>%
  top_n(n = 5, wt = Value) %>%
  arrange(desc(Value)) %>%
  filter(!is.na(Value)) %>%
  mutate(Area = fct_reorder(factor(Area), Value)) 

#Top sugar importers
import <- raw_sugar_2019 %>%
  filter(Element == "Import Quantity") %>%
  top_n(n = 5, wt = Value) %>%
  arrange(desc(Value)) %>%
  mutate(Area = str_replace(Area,"China, mainland", "China")) %>%
  mutate(Area = fct_reorder(factor(Area), Value)) 

#Adding iso2 codes for each country (required by ggflags, needs to be minuscle)
export <- export %>%
  mutate(iso2 = countrycode(export$Area, "country.name", "iso2c")) %>%
  mutate(iso2 = tolower(iso2))

import <- import %>%
  mutate(iso2 = countrycode(import$Area, "country.name", "iso2c")) %>%
  mutate(iso2 = tolower(iso2))

#plots ----
#adding fonts
font_add_google("Roboto", family = "roboto")
font_add_google("Source Sans Pro", family = "sourcesans")
showtext_auto()

#export plot for top 5 exporters 
p1 <- ggplot(export, aes(x = Value, y = Area)) +
  geom_col(fill = "royalblue", alpha = 0.8, color = "royalblue") +
  geom_flag(x = 0, aes(country = iso2), size = 7) +
  labs(y = NULL, x = "Export Quantity (Million tonnes)",
       title = "Top 5 <span style = 'color:royalblue;'>**exporters**</span> of raw centrifuged sugar in 2019") +
  theme_minimal() + theme(panel.grid.major.y = element_blank(),
                          panel.grid.minor.y = element_blank(),
                          axis.text = element_text(color = "black", family = "sourcesans", 
                                                   size = 6),
                          axis.title = element_text(family = "sourcesans", size = 6),
                          plot.title = element_markdown(family = "roboto", size = 8))

#import plot for top 5 importers 

p2 <- ggplot(import, aes(x = Value, y = Area)) +
  geom_col(fill = "sandybrown", alpha = 0.8, color = "sandybrown") +
  geom_flag(x = 0, aes(country = iso2), size = 7) +
  labs(y = NULL, x = "Import Quantity (Million tonnes)",
       title = "Top 5 <span style = 'color:sandybrown;'>**importers**</span> of raw centrifuged sugar in 2019") +
  theme_minimal() + theme(panel.grid.major.y = element_blank(),
                          panel.grid.minor.y = element_blank(),
                          axis.text = element_text(color = "black", family = "sourcesans", 
                                                   size = 6),
                          axis.title = element_text(family = "sourcesans", size = 6),
                          plot.title = element_markdown(family = "roboto", size = 8))


#Combining plots ----
p <- p1 + p2 


p + plot_annotation(title = "**Where does the sugar come from?**", 
                    subtitle = "The main sources of sugar are **sugarcane** and **sugar beet**.<br>
                    Considering both sources, the international trade of raw sugar moved around 12 bi dollars in 2019.<br>
                    <span style = 'color:royalblue;'>**Brazil**</span> is the **top exporter**, while <span style = 'color:sandybrown;'>**Indonesia**</span> is the **top importer**.",
                    caption = "Data from FAO, text and visualization by Amanda Fanelli (twitter @amandafanelli).",
                    theme = theme(plot.title = element_markdown(family = "roboto", size = 12, 
                                                                color = "darkblue"),
                                  plot.subtitle = element_markdown(family = "sourcesans", size = 7),
                                  plot.caption = element_text(family = "roboto", size = 6)))


ggsave("final_visualizations/sugar_trade.pdf", height = 8, width = 17, units = "cm")
