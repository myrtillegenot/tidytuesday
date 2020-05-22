# Author : Myrtille Genot
# Code : Tidy Tuesday // Week 1 // Volcanoes
# Date : 20 May 2020



# Load libraries
library('dplyr')
library('ggplot')


##   1: Data 
#Load volcano data

volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')
#events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/events.csv')
#tree_rings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/tree_rings.csv')
#sulfur <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/sulfur.csv')


#Load tile grid data

worldtilegrid <- read.csv("https://gist.githubusercontent.com/maartenzam/787498bbc07ae06b637447dbd430ea0a/raw/9a9dafafb44d8990f85243a9c7ca349acd3a0d07/worldtilegrid.csv",  stringsAsFactors = FALSE) %>% 
  rename(country = name)

worldtilegrid$alpha.2 <- ifelse(worldtilegrid$country == "Namibia", "NA", worldtilegrid$alpha.2)
worldtilegrid$country <- ifelse(worldtilegrid$alpha.2 == "GB", "United Kingdom", worldtilegrid$country)


#Fuzzy string match for country names

library(fuzzyjoin)
merge1 <- stringdist_join(volcano, worldtilegrid, 
                by = "country",
                mode = "full",
                method = "jw", 
                max_dist = 99, 
                distance_col = "dist") %>%
  group_by(country.x) %>%
  top_n(1, -dist) %>% 
  filter(! dist > 0.3) %>% 
  left_join(eruptions, by ="volcano_name") %>% 
  distinct() %>% 
  select(5,2,28,36,37,40)


countriesfs <- merge1 %>% select(country.x, country.y, dist) %>% distinct()


# Get volcano eruption count per country

manip1 <- merge1 %>% 
  full_join(worldtilegrid) %>% 
  group_by(country) %>% 
  mutate(n_volcanoes = n_distinct(volcano_name)) %>% 
  mutate(eruption = n())

#Replace NAs
manip1$n_volcanoes <- ifelse(is.na(manip1$volcano_name), 0, manip1$n_volcanoes)
manip1$eruption <- ifelse(manip1$n_volcanoes == 0 , 0, manip1$eruption)


# Discrete values 

manip1$discrete_volcanoes <- ifelse(manip1$n_volcanoes == 0, "None", 0)
manip1$discrete_volcanoes <- ifelse(manip1$n_volcanoes <= 10  & manip1$n_volcanoes > 0 , "Under 10", manip1$discrete_volcanoes)
manip1$discrete_volcanoes <- ifelse(manip1$n_volcanoes > 10 & manip1$n_volcanoes < 40 , "Over 10", manip1$discrete_volcanoes)
manip1$discrete_volcanoes <- ifelse(manip1$n_volcanoes > 40 & manip1$n_volcanoes < 90, "Over 40", manip1$discrete_volcanoes)
manip1$discrete_volcanoes <- ifelse(manip1$n_volcanoes > 90, "Over 90", manip1$discrete_volcanoes)

# Level factors 
manip1$discrete_volcanoes <- factor(manip1$discrete_volcanoes, levels = c("None", "Under 10","Over 10","Over 40", "Over 90" ))



##   2 : Plot 
# Colour 
group.colors <- c("None" = "grey", "Under 10" = "snow4", "Over 10" ="#7ebdc2", "Over 40" = "#f6aa1c", "Over 90" = "#bc3808")

# Graph 

volcano <-ggplot(manip1, aes(xmin = x, ymin = y, xmax = x + 1, ymax = y + 1, fill = discrete_volcanoes)) +
  #base graph
  geom_rect() +
  geom_text(aes(x = x, y = y, label = alpha.2), color = "#ffffff", nudge_x = 0.5, nudge_y = -0.5, size = 2) +
  scale_y_reverse() +
  coord_fixed(ratio = 0.55) +
  #annotate arrows
  annotate(geom = "curve",
           x = c(2.5, 26.5, 28.5 ),
           y = c(2.5, 13.5, 6.5 ),
           xend = c(3.5, 27.5, 29 ),
           yend = c(0.8, 14, 5),
           curvature = .3, arrow = arrow(length = unit(1, "mm"))) +
  #annotate - text
  annotate(geom = "text", size = 2.5, family = 'Roboto Black', 
           x = c(3, 28, 27.5),
           y = c(0, 14, 4),
           label = c("USA : 99","Indonesia :\n  94 ","Japan : 104"),
           hjust = "left") +
  #colours : manual
  scale_fill_manual(values = group.colors,labels = c("None" , "Under\n10", "Over\n10", "Over\n40","Over\n90" )) +
  #labels
  labs(title = "Volcanoes : A Global Mosaic",
       subtitle = "Countries with the most volcanoes\n",
       caption = '\nSource: The Smithsonian Institute',
       fill = 'Volcanoes') +
  #legend
  guides(fill = guide_legend(title.position = "top", 
                             title.hjust = 0.5,
                             label.position = "bottom")) +
  theme(#plot text
        plot.title = element_text(margin = margin(t = 20),
                                  face="bold", 
                                  vjust = 2, 
                                  family = 'Roboto Black', 
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 10,
                                     family ="Source Sans Pro",
                                     hjust = 0.5),
        plot.caption = element_text(hjust = 0, 
                                    size =6),
        #axis
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        #panels
        panel.background = element_blank(),
        panel.grid = element_blank(),
        #legend 
        legend.position = "bottom",
        legend.title = element_text(size = 8, 
                                    family ="Source Sans Pro", 
                                    face="bold"),
        legend.text = element_text(size = 6),
        legend.key.width=unit(0.5, "cm"),
        legend.key.size = unit(0.5,"cm"))

#save 

ggsave("~/Desktop/DataViz/TidyTuesday/Graphs/Volcanoes.png", width=6, height=6)


  