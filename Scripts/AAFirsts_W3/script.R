#Author : Myrtille Genot
#Tidy Tuesday : BLM 
#11 June 2020 

#load libraries
library(sf)
library(ggplot2)
library(tmaptools)
library(leaflet)
library(dplyr)
library(tidyverse)

#read data
firsts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/firsts.csv')
science <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/science.csv')

#  -------- 1. Clean Gender column 

#Male 
firsts$sex <- "Male"
#Female
firsts$sex<- ifelse(grepl("Female",firsts$gender), "Female", firsts$sex)

# --------- 2. Clean accomplishment column 

#Remove everything before "African-American". 

firsts$accomplishment <- gsub("^First African-American?", "", firsts$accomplishment)
firsts$accomplishment <- gsub("^First?", "", firsts$accomplishment)

# Clean 
firsts$accomplishment <- gsub("^-?", "", firsts$accomplishment)
firsts$accomplishment <- gsub("^, And?", "", firsts$accomplishment)
firsts$accomplishment <- gsub("U.s", "U.S", firsts$accomplishment)
firsts$accomplishment <- gsub("N.b.a", "N.B.A", firsts$accomplishment)
firsts$accomplishment <- gsub("Nba", "N.B.A", firsts$accomplishment)
firsts$accomplishment <- gsub("Ceo", "CEO", firsts$accomplishment)

#Make title 
firsts$accomplishment <- str_to_title(firsts$accomplishment)

# ----------- 3. Clean Person column really extensively

#Remove everything after first , 
firsts$person <- gsub(",*", "", firsts$person)
firsts$person <- gsub(";*", "", firsts$person) 

#Remove everything in parenthesis 
firsts$person <- gsub( " *\\[.*?\\] *", "", firsts$person)
firsts$person <- gsub( " *\\(.*?\\) *", "", firsts$person)
firsts$person <- gsub( " \\(See also", "", firsts$person)
firsts$person <- gsub( " \\(.*", "", firsts$person)


# ---------- 4. Make Date column
firsts$year <- lubridate::ymd(firsts$year, truncated = 2L)

# Give all observations an ID
firsts$observation <- 1:nrow(firsts) 

# Put name next to accomplishment 

firsts$person_accomplishment <- paste(firsts$person, "-", firsts$accomplishment)




# Make graph -- vertical line with year one side then accomplishment and person underneath italicised. 
#coolors : #EDF2F4



ggplot(firsts, aes(year, observation)) +
  #geoms
  geom_text(aes(label = person_accomplishment), check_overlap = TRUE, hjust = 0, size =0.4, family = "Lato", colour ="white") +
  #axis scxales
  scale_x_date(date_breaks = "5 years", expand=c(0,0), date_labels = "%Y") +
  scale_y_continuous(limits =c(0, 480), expand=c(0,0)) +
  #flip axis
  coord_cartesian(clip = "off") +
  #labels
  labs(title = "The Rate of Change\nAfrican American Firsts Over Time",
       subtitle = "Still I Rise\n") +
  #annotate 
  annotate(geom = "text", size = 1, family = 'Lato', colour = "#EDF2F4",
           x = as.Date("1738-01-01"),
           y = c(8),
           label = ("1738"),
           hjust = "left") +
  #discrepancy in dates here is on purpose -- 1960 label still shows 1960, just to avoid overlap/ 
  annotate(geom = "text", size = 1, family = 'Lato', colour = "#EDF2F4",
           x = as.Date("1950-01-01"),
           y = c(208),
           label = ("1960"),
           hjust = "left") +
  annotate(geom = "text", size = 1, family = 'Lato', colour = "#EDF2F4",
           x = as.Date("2011-01-01"),
           y = c(478),
           label = ("2019"),
           hjust = "left") +
  #theme
  theme(text = element_text(family = "Roboto Black", size = 1),
        #blank background
        plot.background=element_rect(fill = "#1a1f29", color = "#1a1f29"),
        plot.margin = unit(c(1,2,1,1),"cm"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(linetype = "dashed", size = 0.03),
        panel.grid.minor.y = element_blank(),
        #text
        plot.title = element_text(size =10, hjust = 0.5, color = "#E1DEE3"),
        plot.subtitle = element_text(size =8, family = "Times New Roman", face ="italic", hjust =0.5, colour = "#E1DEE3"),
        # axis lines
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        #axis ticks
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        #axis text
        axis.text.x = element_text(size =1 , angle=90, color = "white"),
        #axis title 
        axis.title.x = element_blank()) 

 ggsave("~/Desktop/DataViz/TidyTuesday/Graphs/firsts.pdf", width=9, height=12)
