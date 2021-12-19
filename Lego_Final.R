library(tidyverse)
library(dplyr)
colors <- read_csv("Desktop/Math 250 (Tutorial) - FALL 2021-22/Lego/colors.csv.gz")
view(colors)

minifigs <- read_csv("Desktop/Math 250 (Tutorial) - FALL 2021-22/Lego/minifigs.csv.gz")
view(minifigs)

elements <- read_csv("Desktop/Math 250 (Tutorial) - FALL 2021-22/Lego/elements.csv.gz")
view(elements)

themes <- read_csv("Desktop/Math 250 (Tutorial) - FALL 2021-22/Lego/themes.csv.gz")
view(themes)

inventory_minifigs <- read_csv("Desktop/Math 250 (Tutorial) - FALL 2021-22/Lego/inventory_minifigs.csv.gz")
view(inventory_minifigs)

inventory_parts <- read_csv("Desktop/Math 250 (Tutorial) - FALL 2021-22/Lego/inventory_parts.csv.gz")
view(inventory_parts)

sets <- read_csv("Desktop/Math 250 (Tutorial) - FALL 2021-22/Lego/sets.csv.gz")
view(sets)

inventory_sets <- read_csv("Desktop/Math 250 (Tutorial) - FALL 2021-22/Lego/inventory_sets.csv.gz")
view(inventory_sets)

part_categories <- read_csv("Desktop/Math 250 (Tutorial) - FALL 2021-22/Lego/part_categories.csv.gz")
view(part_categories)

inventories <- read_csv("Desktop/Math 250 (Tutorial) - FALL 2021-22/Lego/inventories.csv.gz")
view(inventories)

part_categories <- read_csv("Desktop/Math 250 (Tutorial) - FALL 2021-22/Lego/part_categories.csv.gz")
view(part_categories)

parts <- read_csv("Desktop/Math 250 (Tutorial) - FALL 2021-22/Lego/parts.csv.gz")
view(parts)

head(themes)
head(sets)

#Comobining themes and sets, which mostly allows us to display average number of parts in a set by year
themes_plus_sets <- inner_join(sets, themes, by = c("theme_id" = "id"))
view(themes_plus_sets)
head(themes_plus_sets)

themes_plus_sets <- themes_plus_sets %>%
  group_by(year) %>%
  mutate(NumPartsPerYear = sum(num_parts)) %>%
  mutate(SetsPerYear = n_distinct(theme_id)) %>%
  mutate(AvgPartsPerYear = NumPartsPerYear / SetsPerYear)
view(themes_plus_sets)
themes_plus_sets$year <- as.character(themes_plus_sets$year)
themes_plus_sets


themes_sets_summarized <-themes_plus_sets %>%
  group_by(year) %>%
  summarize(AvgPartsPerYear = unique(AvgPartsPerYear))
themes_sets_summarized

#Visualizing the avg number of parts per Lego set per year (geom_line)
ggplot(data = themes_plus_sets, mapping = aes(x = year, y = AvgPartsPerYear, group = year)) +
  geom_bar(stat = "summary") + 
  labs(x = "Year", y = "Average Number of Parts per Set") + 
  theme_minimal()

ggplot(data = themes_sets_summarized, mapping = aes(x = year, y = AvgPartsPerYear)) +
  geom_line(stat = "identity") + 
  labs(x = "Year", y = "Average Number of Parts per Set") + 
  theme_minimal()

#Combining inventory_minifigs and minifigs data
inventory_minifigs_plus_minifigs <- inner_join(inventory_minifigs, minifigs, by = c("fig_num" = "fig_num"))
view(inventory_minifigs_plus_minifigs)

#Combining part_categories and parts, which allows us to group by categories, and visualize materials
part_categories_plus_parts <- inner_join(parts, part_categories, by = c("part_cat_id" = "id"))
part_categories_plus_parts$part_cat_id <- as.integer(part_categories_plus_parts$part_cat_id)

ggplot(data = part_categories_plus_parts, aes(x = part_material, y = str_count(part_material), fill = part_material)) +
  geom_col() +
  labs(x = "Part Material", y = "Count") +
  theme_minimal()

  
view(part_categories_plus_parts)
summary(part_categories_plus_parts)

#Combining inventory_parts and colors to try to get a visualization of most popular colors
inventory_parts_plus_colors <- inner_join(inventory_parts, colors, by = c("color_id" = "id"))

inventory_parts_plus_colors_filtered <- inventory_parts_plus_colors %>%
  filter(name == "Red" | name == "Blue" | name == "White" | name == "Black"
         | name == "Yellow" | name == "Orange")
view(inventory_parts_plus_colors_filtered)

ggplot(inventory_parts_plus_colors_filtered, aes(x = name, y = quantity, fill = name)) +
  geom_col() + 
  scale_fill_manual(values = c("Black", "Blue", "Orange", "Red", "Gray", "Yellow")) + 
  labs(x = "Lego Color", y = "Quantity") +
  theme_minimal()

#Creating a data table that only contains spare pieces
spare_pieces <- inventory_parts %>%
  filter(is_spare == 'TRUE')

view(spare_pieces)

#Seeing if there are any interesting comparisons between the color of the piece and if it is spare
spare_pieces_colors <- inner_join(spare_pieces, colors, by = c("color_id" = "id"))

spare_pieces_colors <- spare_pieces_colors %>%
  filter(name == "Red" | name == "Blue" | name == "White" | name == "Black"
         | name == "Yellow" | name == "Orange")
view(spare_pieces_colors)
spare_pieces_colors

ggplot(spare_pieces_colors, aes(x = name, y = quantity, fill = name)) +
  geom_col() + 
  scale_fill_manual(values = c("Black", "Blue", "Orange", "Red", "Gray", "Yellow")) + 
  labs(x = "Lego Color", y = "Number of Spare Pieces") +
  theme_minimal()

## POTENTIAL QUESTION: RELATIONSHIP BETWEEN YEAR AND NUMBER OF PIECES, INCLUDE THEME(?)
# ANOVA test on to see if the theme of the lego set effects the number of pieces in the set
# ANOVA test to see if the year the Lego set was created effects the number of pieces in the set

#This works!
lego_pieces_aov <- aov(formula = NumPartsPerYear ~ year, data = themes_plus_sets)
summary(lego_pieces_aov)

#ANOVA test seeing if there is a relationship between the color of a piece and if it is a spare
color_spare_aov <- aov(formula = quantity ~ name, data = spare_pieces_colors)
summary(color_spare_aov)
#Given the results of this ANOVA test, we have evidence to reject the null hypothesis (That is, 
#the average amount of spare pieces for each color of Lego will be the same.), we can conclude that
#the color of a Lego piece does have a correlation to whether or not it is spare based on this data

test_aov <- aov(formula = num_parts ~ year, data = themes_plus_sets)
summary(test_aov)


t.test(themes_plus_sets$num_parts, themes_plus_sets$year, paired = T)

#Interesting statistical observations







