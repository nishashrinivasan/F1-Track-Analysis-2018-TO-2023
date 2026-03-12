# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)
library(RColorBrewer)

# Set the working directory and load the data
setwd("~/Downloads/STATISTICS")
f1_data <- read.csv("Formula1_2023_2024_calendar.csv")

# Convert Race Date to Date format
f1_data$Race_Date <- as.Date(f1_data$Race.Date)
f1_data$Year <- year(f1_data$Race_Date)

# Convert character columns to factors
f1_data$Country <- as.factor(f1_data$Country)
f1_data$GP.Name <- as.factor(f1_data$GP.Name)
f1_data$Season <- as.factor(f1_data$Season)

# Clean the race names for better display
clean_name <- function(name) {
  gsub("^.*\\s|GP$", "", name)
}

f1_data$Short_Name <- sapply(as.character(f1_data$GP.Name), clean_name)

# Custom theme for consistency
theme_f1 <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(size = 14),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10)
    )
}

# VISUALIZATION 1: Circuit Length Comparison
ggplot(f1_data, aes(x = reorder(Short_Name, Circuit.Length.km.), y = Circuit.Length.km., fill = Season)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "F1 Circuit Lengths Comparison (2023-2024)",
       x = "Grand Prix",
       y = "Circuit Length (km)",
       fill = "Season") +
  theme_f1()

# VISUALIZATION 2: Race Calendar Timeline
ggplot(f1_data, aes(x = Race_Date, y = Short_Name, color = Season)) +
  geom_point(size = 3) +
  geom_line(aes(group = Season)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_color_manual(values = c("red", "blue")) +
  labs(title = "F1 Race Calendar Timeline (2023-2024)",
       x = "Date",
       y = "Grand Prix",
       color = "Season") +
  theme_f1()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# VISUALIZATION 3: Number of Laps vs Circuit Length
ggplot(f1_data, aes(x = Circuit.Length.km., y = Number.of.Laps, color = Country)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_text(aes(label = Short_Name), size = 2.8, vjust = -1) +
  scale_color_viridis_d() +
  labs(title = "Relationship Between Circuit Length and Number of Laps in F1 Circuits",
       subtitle = "Each point represents a race track; color = country, label = circuit short name",
       x = "Circuit Length (in kilometers)",
       y = "Total Number of Laps",
       color = "Host Country") +
  theme_f1()

# VISUALIZATION 4: DRS Zones Analysis
drs_summary <- f1_data %>%
  group_by(Season, DRS.Zones) %>%
  summarize(count = n(), .groups = 'drop')

ggplot(drs_summary, aes(x = as.factor(DRS.Zones), y = count, fill = Season)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Number of Circuits by DRS Zone Count",
       x = "Number of DRS Zones",
       y = "Count of Circuits",
       fill = "Season") +
  theme_f1()

# VISUALIZATION 5: Track Evolution - Lap Records
f1_data$Record_Year_Diff <- f1_data$Year - as.numeric(f1_data$Record.Year)

ggplot(f1_data, aes(x = reorder(Short_Name, -Record_Year_Diff), y = Record_Year_Diff, fill = Record.Owner)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_brewer(palette = "Spectral") +
  labs(title = "Age of Lap Records by Circuit",
       subtitle = "How many years old is each circuit's lap record?",
       x = "",
       y = "Years since lap record was set",
       fill = "Record Owner") +
  theme_f1()

# VISUALIZATION 6: Turn Count vs Circuit Length Analysis
ggplot(f1_data, aes(x = Circuit.Length.km., y = Turns)) +
  geom_point(aes(color = Country, size = Race.Distance.km.), alpha = 0.7) +
  geom_text(aes(label = Short_Name), size = 2.8, vjust = -1) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "gray40") +
  scale_color_viridis_d() +
  labs(title = "Turn Count vs Circuit Length in Formula 1 Circuits",
       subtitle = "Each point represents a race track; color = host country, size = total race distance",
       x = "Circuit Length (in kilometers)",
       y = "Number of Turns",
       color = "Host Country",
       size = "Total Race Distance (km)") +
  theme_f1()

# VISUALIZATION 7: First GP Year Distribution
f1_data$First_GP_Decade <- floor(as.numeric(f1_data$First.GP) / 10) * 10
f1_data$First_GP_Decade_Label <- paste0(f1_data$First_GP_Decade, "s")

decade_counts <- f1_data %>%
  group_by(First_GP_Decade, First_GP_Decade_Label) %>%
  summarize(count = n_distinct(Circuit.Name), .groups = 'drop')

ggplot(decade_counts, aes(x = reorder(First_GP_Decade_Label, First_GP_Decade), y = count, fill = First_GP_Decade_Label)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "YlOrRd") +
  labs(title = "Distribution of F1 Circuits by Decade of First Grand Prix",
       x = "Decade",
       y = "Number of Circuits") +
  theme_f1() +
  theme(legend.position = "none")

# VISUALIZATION 8: Race Distance Comparison
ggplot(f1_data, aes(x = Race.Distance.km., fill = Season)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("red", "blue")) +
  labs(title = "Distribution of Race Distances",
       x = "Race Distance (km)",
       y = "Density",
       fill = "Season") +
  theme_f1()

# VISUALIZATION 9: Country Distribution
country_counts <- f1_data %>%
  group_by(Season, Country) %>%
  summarize(count = n(), .groups = 'drop')

ggplot(country_counts, aes(x = reorder(Country, -count), y = count, fill = Season)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  scale_fill_manual(values = c("red", "blue")) +
  labs(title = "Number of Grand Prix Held by Country",
       x = "Country",
       y = "Number of Grand Prix",
       fill = "Season") +
  theme_f1()
