install.packages("ggplot2")
install.packages("dplyr")
install.packages("reshape2")
install.packages("ggthemes")
install.packages("viridis")
install.packages("HistData")
install.packages('plotly')
library(ggplot2)
library(dplyr)
library(reshape2)
library(ggthemes)
library(viridis)

install.packages("HistData")
library(HistData)
data("Nightingale", package = "HistData")

# Mutate Year-Month and Event columns for better visualization
df <- Nightingale %>% 
  mutate(Year_Month = paste(Year, Month, sep = "-")) %>% 
  mutate(Event = case_when(
    Date == as.Date("1854-11-04") ~ "Nightingale Arrives in Scutari",
    Date == as.Date("1855-03-01") ~ "Sanitary Commission Arrives",
    Date == as.Date("1855-09-08") ~ "End of Siege of Sevastopol",
    TRUE ~ NA_character_
  ))

# Convert Year_Month to Date format
df$Date <- as.Date(paste(df$Year, df$Month, "01", sep = "-"), "%Y-%b-%d")

# 1. Stacked Bar Graph of Deaths
hist_data <- melt(df, id.vars = c("Date", "Event", "Year_Month"), measure.vars = c("Disease", "Wounds", "Other"))

stacked_bar <- ggplot(hist_data, aes(x = Date, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(title = "Stacked Bar Graph of Deaths by Cause",
       subtitle = "A visual representation of deaths due to Disease, Wounds, and Other causes",
       x = "Year-Month",
       y = "Number of Deaths",
       fill = "Cause of Death") +
  theme_minimal()

print(stacked_bar)

# 2. Heatmap of Deaths by Diseases
heatmap_data <- df %>% select(Date, Disease, Event)

heatmap <- ggplot(heatmap_data, aes(x = Date, y = 1, fill = Disease)) +
  geom_tile() +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  scale_fill_gradientn(colors = viridis(4), breaks = seq(500, 2000, by = 500), name = "Deaths by Disease") +
  labs(title = "Heatmap of Deaths by Disease",
       subtitle = "Heatmap showing the number of deaths due to Disease with key events highlighted",
       x = "Year-Month",
       y = "",
       fill = "Number of Deaths")+
  theme_minimal() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

print(heatmap)

# 3. Area Graph of Deaths by Cause Highlighting Events
# Plot
area_graph <- ggplot(hist_data, aes(x = Date, y = value, fill = variable)) +
  geom_area(position = "stack", alpha = 0.7) +
  labs(title = "Area Graph of Deaths by Cause",
       subtitle = "Stacked area graph showing deaths due to Disease, Wounds, and Other causes over time",
       x = "Year-Month",
       y = "Number of Deaths",
       fill = "Cause of Death")+
  theme_minimal()

print(area_graph)

# 4 Pie Chart
disease_totals <- df %>% summarise(Disease = sum(Disease), Wounds = sum(Wounds), Other = sum(Other))
pie_data <- melt(disease_totals, measure.vars = c("Disease", "Wounds", "Other"))

# Plot Pie Chart
pie_chart <- ggplot(pie_data, aes(x = "", y = value, fill = variable)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("Disease" = "#FF6F61", "Wounds" = "#6BAED6", "Other" = "#A1D99B"),
                    name = "Cause of Death") +
  labs(title = "Pie Chart of Deaths by Cause",
       subtitle = "Distribution of deaths due to Disease, Wounds, and Other causes") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

print(pie_chart)

# 5. Modern Box Plot
disease_data_long <- melt(df, id.vars = c("Year", "Month"), measure.vars = c("Disease", "Wounds", "Other"))

# Plot Box Plot
box_plot <- ggplot(disease_data_long, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("Disease" = "#FF6F61", "Wounds" = "#6BAED6", "Other" = "#A1D99B"),
                    name = "Cause of Death") +
  labs(title = "Box Plot of Deaths by Cause",
       subtitle = "Box plot showing the distribution of deaths due to Disease, Wounds, and Other causes",
       x = "Cause of Death",
       y = "Number of Deaths") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 10, face = "bold"))

print(box_plot)
#26-10-2024
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)      # Use tidyr for pivot_longer()
library(viridis)    # Ensure viridis package is loaded
library(HistData)

# Prepare Data for the Coxcomb Chart
df <- Nightingale %>% 
  mutate(Year_Month = paste(Year, Month, sep = "-")) %>% 
  mutate(Event = case_when(
    Date == as.Date("1854-11-04") ~ "Nightingale Arrives in Scutari",
    Date == as.Date("1855-03-01") ~ "Sanitary Commission Arrives",
    Date == as.Date("1855-09-08") ~ "End of Siege of Sevastopol",
    TRUE ~ NA_character_
  )) %>%
  select(Year, Month, Year_Month, Date, Disease, Wounds, Other, Event) %>%
  pivot_longer(cols = c(Disease, Wounds, Other), names_to = "Cause", values_to = "Count")

# Convert Year_Month to Date format for ordering purposes
df$Date <- as.Date(paste(df$Year, df$Month, "01", sep = "-"), "%Y-%b-%d")
df$Year_Month <- factor(df$Year_Month, levels = unique(df$Year_Month[order(df$Date)]))

# Create the Windrose/Coxcomb Chart
ggplot(df, aes(x = Year_Month, y = Count, fill = Cause)) +
  geom_bar(stat = "identity", color = "black", position = "stack") +
  coord_polar(start = 0) + 
  scale_fill_viridis_d(option = "D") +   # Use viridis_d for discrete scale
  theme_minimal() + 
  labs(title = "Coxcomb Chart of Deaths by Cause (Diseases, Wounds, Other)",
       subtitle = "Across Year-Month",
       x = "Year-Month",
       y = "Number of Deaths",
       fill = "Cause of Death") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  
  # Add Vertical Lines for Key Events
  geom_vline(aes(xintercept = which(levels(df$Year_Month) == "1854-Nov")), 
             color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = which(levels(df$Year_Month) == "1855-Mar")), 
             color = "blue", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = which(levels(df$Year_Month) == "1855-Sep")), 
             color = "green", linetype = "dashed", size = 1) +
  
  # Add Annotations for Key Events
  annotate("text", x = which(levels(df$Year_Month) == "1854-Nov"), y = max(df$Count) + 10, 
           label = "Nightingale Arrives in Scutari", angle = 90, vjust = -0.5, color = "red") +
  annotate("text", x = which(levels(df$Year_Month) == "1855-Mar"), y = max(df$Count) + 10, 
           label = "Sanitary Commission Arrives", angle = 90, vjust = -0.5, color = "blue") +
  annotate("text", x = which(levels(df$Year_Month) == "1855-Sep"), y = max(df$Count) + 10, 
           label = "End of Siege of Sevastopol", angle = 90, vjust = -0.5, color = "green")




