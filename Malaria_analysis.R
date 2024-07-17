setwd(('C:/Users/sputhuma/Documents/HJF'))
# Load necessary libraries
library(tidyverse)
library(readr)
library(ggplot2)
library(summarytools)
library(ggcorrplot)
library(sf)
library(plotly)
library(tmap)
library(viridis)
library(readr)
library(gridExtra)

# Load the datasets
estimated_numbers <- read_csv("estimated_numbers.csv")
incidence_per_1000_pop_at_risk <- read_csv("incidence_per_1000_pop_at_risk.csv")
reported_numbers <- read_csv("reported_numbers.csv")


# View the first few rows of each dataset
head(estimated_numbers)
head(incidence_per_1000_pop_at_risk)
head(reported_numbers)

# Load geospatial data (world map)
world <- st_read("ne_10m_admin_0_countries.shp")


# Inspect the geospatial data
head(world)

# Inspect column names of the geospatial data
names(world)



# Inspect the renamed data
head(reported_numbers)
names(reported_numbers)


print("Column names in reported_numbers:")
print(names(reported_numbers))


reported_numbers <- reported_numbers %>%
  rename(NAME = `NAME`, `Reported Cases` = `No. of cases`, `Reported Deaths` = `No. of deaths`)

# Inspect the renamed data
print("After renaming, column names in reported_numbers:")
print(names(reported_numbers))
print("First few rows of renamed reported_numbers:")
print(head(reported_numbers))

# Merge the malaria data with the geospatial data
malaria_map_data <- left_join(world, reported_numbers %>% filter(Year == max(Year)), by = "NAME")

# Verify the merged data
print("Column names in merged data:")
print(names(malaria_map_data))
print("First few rows of merged data:")
print(head(malaria_map_data))

# Check if 'Reported Cases' column exists
if(!"Reported Cases" %in% names(malaria_map_data)) {
  stop("Column 'Reported Cases' does not exist in the merged data.")
}


# Handle any NAs in the merged data
malaria_map_data <- malaria_map_data %>% drop_na(`Reported Cases`)

# Fix invalid geometries
malaria_map_data <- st_make_valid(malaria_map_data)

# Set tmap options to check and fix polygons
tmap_options(check.and.fix = TRUE)

# Visualize the map using tmap
tm_shape(malaria_map_data) +
  tm_polygons("Reported Cases", style = "quantile", palette = "viridis", title = "Reported Malaria Cases") +
  tm_layout(title = "Global Distribution of Reported Malaria Cases",
            title.position = c("left", "top"),
            legend.outside = TRUE)





# Alternatively, using ggplot2 for geospatial visualization
ggplot(data = malaria_map_data) +
  geom_sf(aes(fill = `Reported Cases`)) +
  scale_fill_viridis_c() +
  labs(title = "Global Distribution of Reported Malaria Cases",
       fill = "Reported Cases") +
  theme_minimal()

# Interactive map with tmap
tmap_mode("view")

tm_shape(malaria_map_data) +
  tm_polygons("Reported Cases", style = "quantile", palette = "viridis", title = "Reported Malaria Cases", 
              id = "NAME", popup.vars = c("Country Name" = "NAME", "Reported Cases" = "Reported Cases")) +
  tm_layout(title = "Global Distribution of Reported Malaria Cases")


# Provide detailed insights

# Insight 1: Top regions affected
top_regions <- malaria_map_data %>%
  st_drop_geometry() %>%
  arrange(desc(`Reported Cases`)) %>%
  head(10)

print("Top 10 regions affected by malaria:")
print(top_regions)



# Rename columns in incidence_per_1000_pop_at_risk to match those in world
incidence_per_1000_pop_at_risk <- incidence_per_1000_pop_at_risk %>%
  rename(NAME = `NAME`, `Incidence per 1000 population` = `Incidence per 1000 population at risk`)

# Merge with the world data
incidence_map_data <- left_join(world, incidence_per_1000_pop_at_risk %>% filter(Year == max(Year)), by = "NAME")

# Verify the merged data
print("Column names in incidence_map_data:")
print(names(incidence_map_data))
print("First few rows of incidence_map_data:")
print(head(incidence_map_data))

# Handle any NAs in the merged data
incidence_map_data <- incidence_map_data %>% drop_na(`Incidence per 1000 population`)

# Visualize incidence data
tm_shape(incidence_map_data) +
  tm_polygons("Incidence per 1000 population", style = "quantile", palette = "magma", title = "Malaria Incidence per 1000 Population at Risk") +
  tm_layout(title = "Global Malaria Incidence",
            title.position = c("left", "top"),
            legend.outside = TRUE)

# Insight 3: Comparison of cases and deaths
case_death_comparison <- reported_numbers %>%
  filter(Year == max(Year)) %>%
  select(NAME, `Reported Cases`, `Reported Deaths`) %>%
  arrange(desc(`Reported Cases`))





# Filter top 50 countries by reported cases and deaths
top_50_cases <- case_death_comparison %>%
  arrange(desc(`Reported Cases`)) %>%
  head(50)

top_50_deaths <- case_death_comparison %>%
  arrange(desc(`Reported Deaths`)) %>%
  head()

# Plot the top 50 reported cases
plot_cases <- ggplot(top_50_cases, aes(x = reorder(NAME, `Reported Cases`), y = `Reported Cases`)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Top 50 Reported Malaria Cases in Latest Year",
       x = "Country",
       y = "Reported Cases") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8)) +  # Adjust text size for country names
  theme(plot.margin = margin(1, 1, 1, 10, "cm"))  # Adjust the left margin for longer labels

# Plot the top 50 reported deaths
plot_deaths <- ggplot(top_50_deaths, aes(x = reorder(NAME, `Reported Deaths`), y = `Reported Deaths`)) +
  geom_bar(stat = "identity", fill = "red") +
  coord_flip() +
  labs(title = "Top 50 Reported Malaria Deaths in Latest Year",
       x = "Country",
       y = "Reported Deaths") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8)) +  # Adjust text size for country names
  theme(plot.margin = margin(1, 1, 1, 4, "cm"))  # Adjust the left margin for longer labels

# Save the plots with increased size
ggsave("Top_50_Reported_Malaria_Cases.png", plot_cases, width = 12, height = 16)
ggsave("Top_50_Reported_Malaria_Deaths.png", plot_deaths, width = 12, height = 16)

# Print both plots
print(plot_cases)
print(plot_deaths)


grid.arrange(plot_deaths, nrow = 1, top = "Top 50 Reported Malaria Deaths in Latest Year")



# Install necessary packages if not already installed
if (!require(treemap)) install.packages("treemap")

# Load the treemap package
library(treemap)

# Treemap for Reported Cases
treemap(top_50_cases,
        index = "NAME",
        vSize = "Reported Cases",
        title = "Top 50 Reported Malaria Cases in Latest Year",
        palette = "Blues",
        border.col = "white")

# Treemap for Reported Deaths
treemap(top_50_deaths,
        index = "NAME",
        vSize = "Reported Deaths",
        title = "Top 50 Reported Malaria Deaths in Latest Year",
        palette = "Reds",
        border.col = "white")





# Create a data frame for plotting
cases_data <- top_50_cases %>%
  mutate(text = paste("Country: ", NAME, "<br>Reported Cases: ", `Reported Cases`))

deaths_data <- top_50_deaths %>%
  mutate(text = paste("Country: ", NAME, "<br>Reported Deaths: ", `Reported Deaths`))



# Interactive treemap for Reported Cases with unique colors for each country
plot_cases <- plot_ly(
  data = cases_data,
  type = "treemap",
  ids = ~NAME,
  labels = ~NAME,
  parents = "",
  values = ~`Reported Cases`,
  textinfo = "label+value+percent entry",
  hoverinfo = "text",
  text = ~text,
  marker = list(colors = ~NAME, colorscale = "Set3", showscale = FALSE)
) %>%
  layout(title = "Top 50 Countries with Reported Malaria Cases (2000-2018)")




# Interactive treemap for Reported Deaths
plot_deaths <- plot_ly(
  data = deaths_data,
  type = "treemap",
  ids = ~NAME,
  labels = ~NAME,
  parents = "",
  values = ~`Reported Deaths`,
  textinfo = "label+value+percent entry",
  hoverinfo = "text",
  text = ~text,
  marker = list(colors = ~`Reported Deaths`, colorscale = "Reds", showscale = TRUE)
) %>%
  layout(title = "Top 50 Countries with Reported Malaria Deaths (2000-2018)")

# Print the interactive plots
plot_cases
plot_deaths









