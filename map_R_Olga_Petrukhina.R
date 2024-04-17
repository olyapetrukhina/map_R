#https://www.kaggle.com/datasets/bhavikjikadara/mental-health-dataset

# 1. Install Required Packages and load libraries: ----------------------------

#install.packages("tidyverse")
#install.packages("ggrepel")

library(tidyverse)
library(ggrepel)

# 2. Read the Data --------------------------------------------------------

my_data <- read.csv("/Users/olgapetrukhina/Downloads/Mental_Health_Dataset.csv")
my_data


# 3. Prepare World Map Data: Get world map data and filter out Antarctica -----
world_map <- map_data("world") 

world_map <- world_map %>%
  filter(region != "Antarctica")

# 4. Creating a background map onto which I'll plot my data ---------------

ggplot() + 
  geom_map( 
    data = world_map, map = world_map, 
    aes(long, lat, map_id = region), 
    colour = "white", fill = "lightpink", size = 0.2
  )

# 5. Data Preprocessing: Standardize country names in my_data --------

my_data$Country <- str_replace_all(my_data$Country, "United States", "USA")
my_data$Country <- str_replace_all(my_data$Country, "United Kingdom", "UK")

# 6. Count the number of respondents per country. -------------------------

country_counts <- my_data %>%
  group_by(Country) %>%
  summarize(People_Count = n())

print(country_counts, n = 100)


# 7. Merge the world map data with country counts. ------------------------

merged_data <- left_join(world_map, country_counts, by = c("region" = "Country"))
merged_data

# 8. Identify the top 15 countries based on the number of responde --------

top_countries <- country_counts %>%
  arrange(desc(People_Count)) %>%
  slice(1:15)

top_countries

# 9. Calculate the center coordinates for labeling the top countries --------

country_labels <- merged_data %>%
  filter(region %in% top_countries$Country) %>%
  group_by(region) %>%
  summarize(long = mean(long), lat = mean(lat))

country_labels

# 10. Generate the map plot with country labels  --------------------------

ggplot() +
  geom_map(data = merged_data, map = merged_data,
           aes(x = long, y = lat, map_id = region, fill = People_Count),
           color = "white") +
  geom_text_repel(data = country_labels,
                  aes(x = long, y = lat, label = region),
                  color = "black",
                  size = 3,
                  box.padding = 0.5,
                  force = 0.5,
                  check_overlap = TRUE) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey50", name = "People Count") +
  theme_void() + 
  ggtitle("Number of respondents living in a certain country") + 
  theme(plot.title = element_text(color = "darkblue", size = 10, face = "bold", family = "serif", hjust = 0.5))
