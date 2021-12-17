#Sugarcane dataviz by Amanda Fanelli, december 2021

#Loading packages ----
library(tidyverse)
library(rnaturalearth)
library(RColorBrewer)
library(countrycode)
library(ggrepel)
library(ggtext)
library(showtext)
library(jpeg)
library(patchwork)
library(grid)
library(gridtext)

#Reading sugarcane data from FAO stats and selecting info----
#data was downloaded from <https://www.fao.org/faostat/en/#data/QCL>
#Countries,Elements,Year > Select all, Items > Sugar cane

sugarcane <- read_csv("input_data/FAOSTAT_sugarcane.csv")

#Checking how many elements, and the respective code. See the description of each on the website
sugarcane %>%
  count(`Element Code`, `Element`) #The elements are the data types (yield, production, area)

#Checking to make sure that all domain codes are QCL
sugarcane %>%
  filter(`Domain Code` != "QCL") #Yes, so don't need this column 

#Selecting only relevant info
sugarcane <- sugarcane %>%
  select(Area, Year, Element, Year, Unit, Value, Flag, `Flag Description`)

#Analyzing data about sugarcane production ----

#Selecting production values and transforming the units
production <- sugarcane %>%
  filter(Element == "Production") %>%
  mutate(Value = Value * 10^-6) %>%
  mutate(Unit = str_replace(Unit, "tonnes", "Megatonnes"))

#Selecting 2019 production values
production_2019 <- production %>%
  filter(Year == "2019") %>%
  arrange(desc(Value))

#Total world production (removing China because it is counted twice(mainland and all china))
production_2019 <- production_2019 %>%
  filter(Area != "China")

total <- sum(production_2019$Value, na.rm = TRUE)

#Adding % world production column and adjusting country names to match rnaturalearth package before building a map
production_2019 <- production_2019 %>%
  mutate(percent_total = round((Value/total) *100,2)) %>%
  mutate(Area = str_remove(Area, ",.*$")) %>%
  mutate(Area = str_remove(Area, "\\(.*$")) %>%
  mutate(Area = str_trim(Area)) %>%
  mutate(Area = str_replace(Area,"Viet Nam", "Vietnam")) %>%
  mutate(Area = str_replace(Area,"United States of America", "United States")) %>%
  mutate(Area = str_replace(Area,"Eswatini", "Swaziland")) %>%
  mutate(Area = str_replace(Area,"Dominican Republic", "Dominican Rep.")) %>%
  mutate(Area = str_replace(Area,"United Republic of Tanzania", "Tanzania")) %>%
  mutate(Area = str_replace(Area,"Democratic Republic of the Congo", "Congo")) %>%
  mutate(Area = str_replace(Area,"Lao People's Democratic Republic", "Lao PDR")) %>%
  mutate(Area = str_replace(Area,"Central African Republic", "Central African Rep.")) %>%
  mutate(Area = str_replace(Area,"Cabo Verde", "Cape Verde")) %>%
  mutate(Area = str_replace(Area,"Saint Vincent and the Grenadines", "St. Vin. and Gren.")) %>%
  mutate(Area = str_replace(Area,"French Polynesia", "Fr. Polynesia")) %>%
  filter(!is.na(Value)) 

#Production Map ----
#adding fonts
font_add_google("Roboto", family = "roboto")
font_add_google("Source Sans Pro", family = "sourcesans")
showtext_auto()

#world map
world <- ne_countries(scale = "medium", returnclass = "sf") 

#Checking if the country names in production_2019 and world match
production_2019 %>% filter(!(Area %in% world$name))

#Joining world and production_2019 in one tibble for ploting
world_sugarcane_prod <- left_join(world, production_2019, by = c("name"="Area"))

#Creating a tibble for labels including country name and percent of world production
world_label <- ne_countries(returnclass = "sf") %>%
  filter(name %in% c("Brazil", "India", "Thailand", "China","Pakistan")) %>%
  mutate(name = case_when(name == "Brazil" ~ "Brazil : 39%",
                          name == "India" ~ "India : 21%",
                          name == "Thailand" ~ "Thailand: 7%",
                          name == "China" ~ "China : 6%",
                          name == "Pakistan" ~ "Pakistan : 3%"))

#ploting the map
set.seed(1000)#makes sure it runs consistently, as ggrepel is random
p1 <- ggplot(data = world_sugarcane_prod) +
  geom_sf(aes(fill = Value, color = ""), size = 0.3) +
  geom_label_repel(data = world_label, aes(geometry = geometry, label = name), 
                   stat = "sf_coordinates",
                   family = "sourcesans",
                   segment.curvature = 1e-20, segment.color = "blue", 
                   segment.size = 0.3, 
                   size = 2, 
                   force = 150) +
  geom_point(data = world_label,
             aes(geometry = geometry), stat = "sf_coordinates", 
             color = "blue", size = 0.5) +
  scale_fill_distiller(name = "Production<br>(million ton)", 
                       palette = "Greens", direction = 1, limits = c(0,800),
                       na.value = "lemonchiffon2", labels = scales::comma) +
  scale_color_manual(values = NA) +              
  guides(color = guide_legend("No data", override.aes=list(color="black", 
                                                           fill = "lemonchiffon2"))) +
  labs(title = "Where in the world is sugarcane produced?", 
       subtitle = "The **Top 5 sugarcane producers** are labeled with the percent contribution to the world's total production. <br> 
       <span style = 'color:darkgreen;'>**Brazil**</span> is the largest producer, followed by 
       <span style = 'color:chartreuse4;'>**India**</span>. Together, they are responsible for more than half of the world's production.")+
  theme(panel.background = element_rect("steelblue"),
        axis.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_markdown(family = "roboto", size = 10, 
                                      color = "darkgreen", face = "bold"),
        plot.subtitle = element_markdown(family = "sourcesans", size = 8),
        legend.title = element_markdown(family = "sourcesans", size = 5),
        legend.text = element_markdown(family = "sourcesans", size = 5),
        legend.key.width = unit(1,"cm"),
        legend.key.height = unit(0.4, "cm"))


ggsave("final_visualizations/map.pdf", height = 10, width = 16, unit = "cm")
#Analyzing Surface and yield data ----

#Trends since 1990
area_yield_trend <- sugarcane %>%
  filter(Element == "Area harvested" | Element == "Yield") %>%
  pivot_wider(names_from = Element,
              values_from = Value) %>%
  group_by(Year) %>%
  summarise(Area = sum(`Area harvested`, na.rm = TRUE),
            Yield = sum(Yield, na.rm = TRUE)) %>%
  filter(Year >= 1990)

#Percent difference from 1990
area_90 <- filter(area_yield_trend, Year == 1990)$Area
yield_90 <- filter(area_yield_trend, Year == 1990)$Yield

area_yield_percent <- area_yield_trend %>%
  mutate(Area = ((Area - area_90)/area_90)) %>%
  mutate(Yield = ((Yield - yield_90)/yield_90))



#Surface and yield trends plot ----
#tibble to add percent difference labels 
labels <- tibble( y = seq(-0.1, 0.5, by = 0.2), 
                  x = rep(1989,4), 
                  label = c("-10%", "+10%", "+30%", "+50%"))
#plot
p2 <- ggplot(area_yield_percent, aes(x = Year)) +
  geom_point(aes(y = Area), color = "chartreuse4") +
  geom_line(aes(y = Area), color = "chartreuse4", size = 0.6) +
  geom_text(aes(x = 2016, y = 0.5), label = "Area harvested (ha)", 
            color = "chartreuse4", family = "sourcesans", size = 2.5) +
  geom_point(aes(y = Yield), color = "darkblue") +
  geom_line(aes(y = Yield), color = "darkblue", size = 0.6) +
  geom_text(aes(x = 2015, y = 0), label = "Yield (hg/ha)", 
            color = "darkblue", family = "sourcesans", size = 2.5) +
  geom_text(data = labels, aes(x=x, y=y, label=label), size = 2, vjust = -1) +
  geom_text(aes(x = 1994, y = 0.6), label = "% Difference from 1990", size = 2.5) +
  labs(x = NULL, y = NULL,
       title = "The sugarcane area harvested increased since 1990, but not the yield",
       subtitle = "The <span style = 'color:darkgreen;'>**area harvested**</span> corresponds to the total area in the world from which sugarcane was gathered, <br>
       whereas the <span style = 'color:darkblue;'>**yield**</span> is the production per unit of harvested area.") +
  scale_y_continuous(limits = c(-0.1,0.6), breaks = seq(-0.1, 0.5, by = 0.2)) +
  theme_minimal() + theme(panel.grid.major.x = element_blank(), 
                          panel.grid.minor.x = element_blank(),
                          panel.grid.minor.y = element_blank(),
                          axis.line.y = element_blank(),
                          axis.ticks.y = element_blank(),
                          axis.text.y = element_blank(),
                          plot.title = element_text(color = "darkgreen",
                                                    family = "roboto",
                                                    face = "bold",
                                                    size = 10),
                          plot.subtitle = element_markdown(family = "sourcesans", size = 8),
                          axis.ticks.x = element_line(color = "black"),
                          axis.text = element_text(color = "black", 
                                                   family = "sourcesans",
                                                   size = 5))
#Adding text to plot

p3 <- richtext_grob("Trends from 1990 to 2019 show a **decrease** in the sugarcane<br>
production **yield**.<br><br>
The world is producing more sugarcane by **expanding** the <br>
**cultivation area**. In 1990, the total harvested area was<br>
181 Mha, and in 2019 it was 282 Mha.<br><br>
To increase the production **sustainably**, we need to produce<br>
more using less land.<br><br>
To increase biomass and fiber contents, some varieties, <br>
named **energy cane**, have been developed. In general, <br>
they are more suitable for energy production from fibers, <br>
not for sugar, although some varieties can be used for both.<br><br>
Another way of **increasing productivity** can be using more<br>
parts of the plant (the leaves, straw, and bagasse) to <br>
**generate products and fuels**. Biomass yield, as shown <br>
here, would not change, but more products could be<br>
generated from the same amount of biomass.", 
                    gp = gpar(fontfamily = "sans", fontsize = 6.5), 
                              x = unit(0.05,"npc"),
                              halign = 0, valign = 0,
                              hjust = 0, vjust = 0.6) 



p4 <- p2 + p3+
  plot_layout(widths = c(1.2,1))

ggsave("final_visualizations/area_yield.pdf", height = 8, width = 16.5, units = "cm")

#Plot with figure and text ----

#reading sugarcane picture jpeg
sugarcane_field <- readJPEG("input_data/sugarcane_field.jpg", native = TRUE)

#plot with text
graph <- ggplot() +
  scale_x_continuous(limits = c(0,8)) +
  scale_y_continuous(limits = c(3.25, 3.8), breaks = seq(3.25,4, by = 0.25)) +
  geom_richtext(aes(x = 1.4, y = 3.6, 
                    label = "**Sugarcane** is a large perennial **grass**, cultivated in tropical and sub-tropical areas. It accumulates<br> 
high levels of sucrose in the stems and is largely used as **food and bioenergy source**. Sugarcane <br> 
is one of the most efficient crops in converting solar energy into sucrose and biomass. It has a high <br> 
economic value, as it is highly productive and can be locally converted into **valued products** such <br> 
as sugar, ethanol, chemicals, and energy. Therefore, it is a **primary trade commodity** in the<br>
countries where it is grown."),
                size = 2.5,
                fill= NA,
                text.color = "black",
                color = NA,
                hjust = 0, vjust = 0.5,
                family = "sans") +
  theme(aspect.ratio = 2/5,
        plot.margin = margin(0,0,1,0)) +
  theme_void()

graph  

#combining picuture and text
image_graph <- graph +
  inset_element(p = sugarcane_field,
                left = 0,bottom = 0.1,
                right = 0.2, top = 1)
image_graph


#Combining  plots ----

#combining image plot and map
figure1 <- (image_graph/p1) + plot_layout(heights = c(2,5)) + 
  plot_annotation(title = "**A sweet and powerful crop**", 
                  theme = theme(plot.title = element_markdown(family = "roboto", size = 16, 
                                                               color = "darkgreen")))



ggsave("final_visualizations/sugarcane_and_map.pdf", height = 14, width = 16, units = "cm")

#final plot with all 3 plots
set.seed(1000)
final <- (image_graph/p1/p4) + 
  plot_layout(heights = c(3,4,4), widths = c(8,8,8)) + 
  plot_annotation(title = "**A sweet and powerful crop**", 
                  caption = "Data from FAO, text and visualization by Amanda Fanelli (twitter @amandafanelli).",
                  theme = theme(plot.title = element_markdown(family = "roboto", size = 16, 
                                                              color = "darkgreen"),
                                plot.caption = element_text(family = "roboto", size = 6))) 


ggsave("final_visualizations/sugarcane_final.pdf", height = 24, width = 16, units = "cm")


