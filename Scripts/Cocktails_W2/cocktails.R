#Author : Myrtille Genot
#Date : 26/05/2020
#Intent: Tidy Tuesday Week 2


library('dplyr')
library('ggplot2')
library('readxl')
library('fuzzyjoin')

cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')
boston_cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')


# Ingredients : 333 unique 
unique(cocktails$ingredient)



# --------- Get Calories 

# Calories 1 
input <- paste0("http://getdrunknotfat.com/") 
chart_page <- xml2::read_html(input) 


ingredient <- rvest::html_table(chart_page, header= TRUE, fill = TRUE)[[1]] %>% 
  select(1,5,6) %>% 
  rename(ingredient =1,
         serving_size = 2, 
         calories =3)

# Calories 2
condiment <- read_excel("~/Downloads/myfoodapediadata/lu_Condiment_Food_Table.xlsx") %>% 
  select(2,3,21) %>% 
  rename(ingredient =1,
         serving_size = 2, 
         calories =3)

#Calories 3 
food <- read_excel("~/Downloads/myfoodapediadata/Food_Display_Table.xlsx")  %>% 
  select(2,5,25) %>% 
  rename(ingredient =1,
         serving_size = 2, 
         calories =3)

# calories 4 : from https://www.calories.info/food/ice-cream

ingredients2 <- read_excel("~/Desktop/DataViz/TidyTuesday/Scripts/Cocktails_W2/data/ingredients2.xlsx")

# Merge 

ingredients <- ingredient %>% 
  rbind(condiment) %>% 
  rbind(food) %>% 
  rbind(ingredients2)%>% 
  distinct()




# fuzzy string
merge1 <- stringdist_join(cocktails, ingredients, 
                          by = "ingredient",
                          mode = "full",
                          method = "jw", 
                          max_dist = 99, 
                          distance_col = "dist") %>%
  group_by(ingredient.x) %>%
  top_n(1, -dist)

merge2 <- merge1%>% 
  select(2,5,6,8,11,12,14,13,15,16,17) %>% 
  distinct() %>% 
  filter(!dist > 0.194) 
  

# Missing 
missing <- anti_join(cocktails, merge2)

ingredient_missing <- missing %>% select(ingredient) %>% distinct()
  

merge3 <- stringdist_join(missing, ingredient, 
                          by = "ingredient",
                          mode = "full",
                          method = "jw", 
                          max_dist = 99, 
                          distance_col = "dist") %>%
  group_by(ingredient.x) %>%
  top_n(1, -dist) %>% 
  select(12,14,17)

