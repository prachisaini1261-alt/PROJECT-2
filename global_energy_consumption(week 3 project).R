# Global energy consumption


# Importing data

library(readxl)
df <- read_excel("C:/Users/Puneet/Downloads/global energy used.xlsx")
View(df)

head(df)

# Data Structure
str(df)

# Statistical summary of data
summary(df)

# all column name in data set
colnames(df)

# number of rows
nrow(df)

# number of columns
ncol(df)

# ------------------------------------ visualization-----------------------------

library(dplyr)
library(ggplot2)

# --------------------- Top 10 Countries by Total Energy Consumption (Latest Year)---------

# Summarize total energy consumption by country for the latest year
energy_summary <- df %>%
  filter(Year == max(Year)) %>%   # latest year
  group_by(Country) %>%
  summarise(Total_Consumption = sum(Total_Energy_Consumption, na.rm = TRUE)) %>%
  arrange(desc(Total_Consumption)) %>%
  slice_head(n = 10)  # top 10 countries

# Plot as a horizontal bar chart
ggplot(energy_summary, aes(x = reorder(Country, Total_Consumption), y = Total_Consumption)) +
  geom_col(fill = "blue") +
  coord_flip() +  # horizontal bars
  labs(
    title = "Top 10 Countries by Total Energy Consumption (Latest Year)",
    x = "Country",
    y = "Total Energy Consumption"
  ) +
  theme_minimal()

 # --------------------Total Energy Consumption Over Years--------------------------------

# Summarize total energy consumption per year
energy_over_time <- df %>%
  group_by(Year) %>%
  summarise(Total_Consumption = sum(Total_Energy_Consumption, na.rm = TRUE)) %>%
  arrange(Year)

# Plot line chart
ggplot(energy_over_time, aes(x = Year, y = Total_Consumption)) +
  geom_line(color = "darkgreen", size = 1.2) +
  geom_point(color = "darkred", size = 2) +
  labs(
    title = "Total Energy Consumption Over Years",
    x = "Year",
    y = "Total Energy Consumption"
  ) +
  theme_minimal()

# ------------Scatter plot of Energy Consumption vs Carbon Emissions-------------

ggplot(df, aes(x = Total_Energy_Consumption, y = Carbon_Emissions)) +
  geom_point(color = "steelblue", size = 2, alpha = 0.7) +  # semi-transparent points
  geom_smooth(method = "lm", color = "red", se = TRUE) +    # add linear regression line
  labs(
    title = "Relationship Between Energy Consumption and Carbon Emissions",
    x = "Total Energy Consumption",
    y = "Carbon Emissions"
  ) +
  theme_minimal()


# --------------------Renewable Energy Share by Country (Latest Year)-------------

# Select latest year
renewable_latest <- df %>%
  filter(Year == max(Year)) %>%
  select(Country, Renewable_Energy_Share) %>%
  arrange(desc(Renewable_Energy_Share))

# Bar chart
ggplot(renewable_latest, aes(x = reorder(Country, Renewable_Energy_Share), y = Renewable_Energy_Share)) +
  geom_col(fill = "forestgreen") +
  coord_flip() +  # horizontal bars
  labs(
    title = "Renewable Energy Share by Country (Latest Year)",
    x = "Country",
    y = "Renewable Energy Share (%)"
  ) +
  theme_minimal()


#-------------------------Fossil Fuel Use Share by Country (Latest Year)-------------

# Filter latest year
fossil_latest <- df %>%
  filter(Year == max(Year)) %>%
  select(Country, Fossil_Fuel_Dependency) %>%
  arrange(desc(Fossil_Fuel_Dependency))

# Bar chart
ggplot(fossil_latest, aes(x = reorder(Country, Fossil_Fuel_Dependency), y = Fossil_Fuel_Dependency)) +
  geom_col(fill = "firebrick") +
  coord_flip() +  # horizontal bars for readability
  labs(
    title = "Fossil Fuel Use Share by Country (Latest Year)",
    x = "Country",
    y = "Fossil Fuel Share (%)"
  ) +
  theme_minimal()
# ----------------------------------- or ---------------------------------------------

# Filter latest year and select top 6 countries
fossil_top6 <- df %>%
  filter(Year == max(Year)) %>%
  select(Country, Fossil_Fuel_Dependency) %>%
  arrange(desc(Fossil_Fuel_Dependency)) %>%
  slice_head(n = 6) %>%
  mutate(Percentage = Fossil_Fuel_Dependency / sum(Fossil_Fuel_Dependency) * 100,
         Label = paste0(Country, " (", round(Percentage, 1), "%)"))

# Pie chart
ggplot(fossil_top6, aes(x = "", y = Percentage, fill = Country)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 4) +
  labs(
    title = "Top 6 Countries by Fossil Fuel Use Share (Latest Year)",
    x = NULL,
    y = NULL
  ) +
  theme_void() +
  scale_fill_brewer(palette = "Set2")


# -------------------------top 10 countries by per capita energy use-------------

# Filter latest year and get top 10 countries by per capita energy use
top10_per_capita <- df %>%
  filter(Year == max(Year)) %>%
  select(Country, Per_Capita_Energy_Use) %>%
  arrange(desc(Per_Capita_Energy_Use)) %>%
  slice_head(n = 10)

# Plot horizontal bar chart
ggplot(top10_per_capita, aes(x = reorder(Country, Per_Capita_Energy_Use), y = Per_Capita_Energy_Use)) +
  geom_col(fill = "darkorange") +
  coord_flip() +  # horizontal bars
  labs(
    title = "Top 10 Countries by Per Capita Energy Use (Latest Year)",
    x = "Country",
    y = "Per Capita Energy Use"
  ) +
  theme_minimal()


# -------------------Global Renewable Energy Share Over Years----------------------

# Calculate global average renewable share per year
renewable_trend <- df %>%
  group_by(Year) %>%
  summarise(Global_Avg_Renewable = mean(Renewable_Energy_Share, na.rm = TRUE)) %>%
  arrange(Year)

# Line chart
ggplot(renewable_trend, aes(x = Year, y = Global_Avg_Renewable)) +
  geom_line(color = "forestgreen", size = 1.2) +
  geom_point(color = "darkgreen", size = 2) +
  labs(
    title = "Global Renewable Energy Share Over Years",
    x = "Year",
    y = "Renewable Energy Share (%)"
  ) +
  theme_minimal()

# --------------------------- Distribution of Carbon Emissions------------------------

ggplot(df, aes(x = Carbon_Emissions)) +
  geom_histogram(binwidth = 5000, fill = "darkred", color = "white", alpha = 0.7) +
  labs(
    title = "Distribution of Carbon Emissions",
    x = "Carbon Emissions",
    y = "Frequency"
  ) +
  theme_minimal()

 #--------------------------------- or ----------------------------------------------
ggplot(df, aes(x = "", y = Carbon_Emissions)) +
  geom_boxplot(fill = "salmon", outlier.color = "red", outlier.shape = 16) +
  labs(
    title = "Spread of Carbon Emissions",
    x = "",
    y = "Carbon Emissions"
  ) +
  theme_minimal()

# -------------Top 10 Countries by Industrial Energy Use (Latest Year)--------------------

# Filter latest year and get top 10 countries by Industrial Energy Use
top10_industrial <- df %>%
  filter(Year == max(Year)) %>%
  select(Country, Industrial_Energy_Use) %>%
  arrange(desc(Industrial_Energy_Use)) %>%
  slice_head(n = 10)

# Plot horizontal bar chart
ggplot(top10_industrial, aes(x = reorder(Country, Industrial_Energy_Use), y = Industrial_Energy_Use)) +
  geom_col(fill = "pink") +
  coord_flip() +  # horizontal bars
  labs(
    title = "Top 10 Countries by Industrial Energy Use (Latest Year)",
    x = "Country",
    y = "Industrial Energy Use"
  ) +
  theme_minimal()

# --------------------------- Average Energy Price Index Over Years-------------------

# Summarize average Energy Price Index per year (in case of multiple countries)
energy_price_trend <- df %>%
  group_by(Year) %>%
  summarise(Average_Energy_Price = mean(Energy_Price_Index, na.rm = TRUE)) %>%
  arrange(Year)

# Line chart
ggplot(energy_price_trend, aes(x = Year, y = Average_Energy_Price)) +
  geom_line(color = "darkblue", size = 1.2) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "Average Energy Price Index Over Years",
    x = "Year",
    y = "Energy Price Index"
  ) +
  theme_minimal()


#---------Energy Price Index by Country (Latest Year)----------------------

# Filter latest year and select countries with their Energy Price Index
energy_price_latest <- df %>%
  filter(Year == max(Year)) %>%
  select(Country, Energy_Price_Index) %>%
  arrange(desc(Energy_Price_Index))

# Plot horizontal bar chart
ggplot(energy_price_latest, aes(x = reorder(Country, Energy_Price_Index), y = Energy_Price_Index)) +
  geom_col(fill = "darkblue") +
  coord_flip() +  # horizontal bars
  labs(
    title = "Energy Price Index by Country (Latest Year)",
    x = "Country",
    y = "Energy Price Index"
  ) +
  theme_minimal()