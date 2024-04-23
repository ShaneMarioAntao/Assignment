install.packages("tidyverse")
install.packages("rnaturalearth")
install.packages("RColorBrewer")
install.packages("ggthemes")
install.packages("patchwork")
install.packages("scales")
install.packages("leaflet")
install.packages("ggiraph")
install.packages("gganimate")
install.packages("rworldmap")
install.packages("plotly")
install.packages("ggplot2")
install.packages("downlit")
install.packages("xml2")
library(plotly)
library(scales)
library(patchwork)
library(leaflet)
library(ggiraph)
library(RColorBrewer)
library(tidyverse)
library(dplyr)
library(rnaturalearth)
library(ggthemes)
library(gganimate)
unicef_indicator_1 <- read_csv("unicef_indicator_1.csv")
unicef_metadata <- read_csv("unicef_metadata.csv")






world_average_data %>%
  st_transform(crs = "+proj=robin") %>% 
  ggplot() +
  geom_sf(color = "darkgrey", fill = "gray85") +  
  geom_sf(aes(fill = average_obs_value)) +  
  scale_fill_gradientn(colours = color_palette, 
                       name = "% Unimproved Water") + 
  theme_bw() +                                    
  theme(plot.background = element_rect(fill = "gray85"),    
        panel.background = element_rect(fill = "gray85"), 
        legend.background = element_rect(fill = "gray85"),  
        plot.title = element_text(face = "bold"),
        axis.text.x = element_blank(), 
        legend.position = "bottom") +
  labs(title = "Global Clean Water Access (2000-2022)",
       subtitle = "Average Unimproved Water Source Usage", 
       x = NULL, y = NULL )





data_for_plot <- unicef_indicator_1 %>% 
  group_by(country, time_period, region) %>%                 
  summarize(avg_unimproved = mean(obs_value, na.rm = TRUE), time_period = time_period[1]) 

n_regions <- length(unique(data_for_plot$region)) 
color_palette <- brewer.pal(n = n_regions, name = "RdBu") 
color_palette <- rev(color_palette)

p <- ggplot(data_for_plot, aes(x = time_period, y = avg_unimproved, group = country, color = region)) +
  geom_line() +  
  theme_bw() +                                    
  theme(plot.background = element_rect(fill = "gray85"),    
        panel.background = element_rect(fill = "gray85"), 
        legend.background = element_rect(fill = "gray85"),
        legend.position = "bottom", 
        legend.box = "horizontal") + 
  labs(title = "Average Unimproved Water Access by Region",
       x = "Time Period", y = "Average Unimproved Water Access (%)",
       color = "Region")  +
  scale_color_manual(values = color_palette) 

ggplotly(p, tooltip = c("region", "time_period")) %>%
  config(displayModeBar = FALSE) %>%
  layout(hoverlabel = list(bgcolor = "gray85",  
                           font = list(family = "Arial", size = 12))) 

plot_data <- unicef_indicator_1 %>% 
  filter(time_period >= 2000 & time_period <= 2022) %>%  
  group_by(time_period) %>% 
  summarize(obs_value_mean = mean(obs_value, na.rm = TRUE)) 
plot_data$label_text <- as.character(round(plot_data$obs_value_mean, 2)) 
predicted_values <- predict(lm(obs_value_mean ~ time_period, data = plot_data))
p <- ggplot(plot_data, aes(x = time_period, y = obs_value_mean)) +
  geom_smooth(method = 'lm', color = "#FF9900", span = 3.0) +  #
  geom_point(color = "#005580", size = 3.5) +        
  geom_area(aes(ymin = Inf, ymax = predicted_values), 
            fill = "gray85", alpha = 0.2) +
  geom_text(aes(label = label_text), size = 3, vjust = +2.0) + 
  theme_bw() +
  theme(plot.background = element_rect(fill = "gray85", color = NA),
        panel.background = element_rect(fill = "gray85", color = NA)) +
  labs(title = "World Unimproved Water Sources Over the Years (2000-2022)",
       subtitle = "Unimproved Water Sources Scatter Plot With Regression Line",
       caption = "Source: Unicef", x = "Year", y = "Unimproved Water Sources") +
  scale_y_continuous(breaks = seq(0, ceiling(max(plot_data$obs_value_mean)), by = 1)) 

print(p)



data_for_plot <- unicef_indicator_1 %>%
  filter(time_period %in% c(2000, 2022)) %>%  
  group_by(region, time_period) %>%      
  mutate(avg_unimproved_change = c(NA, diff(obs_value))) %>%  
  summarize(avg_unimproved = mean(obs_value, na.rm = TRUE)) 
n_regions <- length(unique(data_for_plot$region))  
color_palette <- brewer.pal(n = n_regions, name = "RdBu") 
color_palette <- rev(color_palette) 
p <- ggplot(data_for_plot, aes(x = avg_unimproved, y = region, color = region, size = abs(avg_unimproved))) +
  geom_point() +  
  facet_grid(. ~ time_period) +  
  theme_bw() +                                    
  theme(panel.background = element_rect(fill = "gray85"),    
        plot.background = element_rect(fill = "gray85"),    
        legend.position = "bottom",
        legend.background = element_rect(fill = "gray85"),
        plot.title = element_text(hjust = 0.5)) + 
  labs(title = "Regional Unimproved Water Access: 2000 vs. 2022",
       x = "Average Unimproved Water Access (%)", y = "Region",
       color = "Region",
       size = "") + 
  scale_color_manual(values = color_palette)  

ggplotly(p, tooltip = c("avg_unimproved")) %>%  
  config(displayModeBar = FALSE)





data_for_histogram <- unicef_indicator_1 %>%
  filter(time_period %in% c(2000, 2022)) %>%
  group_by(region, obs_value) %>%  
  summarize(country_count = n()) %>%
  mutate(region = as.factor(region)) 
n_regions <- length(unique(data_for_histogram$region)) 
color_palette <- brewer.pal(n = n_regions, name = "RdBu") 
color_palette <- rev(color_palette)
p <- ggplot(data_for_histogram, aes(x = obs_value, fill = region)) + 
  geom_histogram(color = "black", binwidth = 2) + 
  labs(title = "Distribution of Unimproved Water Access",
       x = "Average Unimproved Water Access (%)",
       y = "Count of Countries",
       fill = "Region") +
  theme_bw() +   
  theme(panel.background = element_rect(fill = "gray85"),    
        plot.background = element_rect(fill = "gray85"), 
        legend.position = "bottom",
        legend.background = element_rect(fill = "gray85"), 
        legend.margin = margin(t = 0, r = 20, b = 0, l = 0), 
        plot.caption = element_text(hjust = 1, vjust = -1)) + 
  labs(caption = "Source: Unicef") + 
  scale_x_continuous(breaks = seq(0, 100, by = 5)) +  
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  scale_fill_manual(values = color_palette)

interactive_plot <- ggplotly(p, tooltip = c("region", "country_count"))  
interactive_plot



category_labels <- c("0-5%", "5-10%", "10-15%", "15%+")
category_breaks <- c(0, 5, 10, 15, Inf)
data_2000 <- unicef_indicator_1 %>%
  filter(time_period == 2000) %>%
  mutate(category = cut(obs_value, breaks = category_breaks, labels = category_labels, include.lowest = TRUE))
data_2022 <- unicef_indicator_1 %>%
  filter(time_period == 2022) %>%
  mutate(category = cut(obs_value, breaks = category_breaks, labels = category_labels, include.lowest = TRUE))
count_2000 <- data_2000 %>%
  group_by(category) %>%
  summarize(count = n())
count_2022 <- data_2022 %>%
  group_by(category) %>%
  summarize(count = n())
count_combined <- rbind(
  data.frame(Year = rep("2000", nrow(count_2000)), count_2000),
  data.frame(Year = rep("2022", nrow(count_2022)), count_2022)
)
p <- ggplot(count_combined, aes(x = category, y = count, fill = Year, text = paste("Year:", Year, "<br>Count of Countries:", count))) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Comparison of Country Counts by Observed Value Category",
       x = "Observed Value Category",
       y = "Count of Countries",
       fill = "Year") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "gray85", color = NA),
        plot.background = element_rect(fill = "gray85", color = NA),
        legend.position = "top") +
  scale_fill_manual(values = c("2000" = "red3", "2022" = "skyblue")) +
  theme(plot.title = element_text(hjust = 0.5))

ggplotly(p, tooltip = "text") %>%
  style(hoverinfo = "text") %>%
  config(displayModeBar = FALSE)





gdp_data <- unicef_metadata %>%
  filter(year >= 2000 & year <= 2022) %>%
  select(year, gdp_per_capita = `GDP per capita (constant 2015 US$)`, country) 
p <- ggplot(gdp_data, aes(x = year, y = gdp_per_capita, text = paste("Country:", country, "<br>GDP Per Capita:", gdp_per_capita))) +
  geom_point(aes(color = gdp_per_capita), size = 2.5) +  
  labs(title = "GDP Per Capita Over the Years (2000-2022)",
       subtitle = "GDP Per Capita Time Series",
       caption = "Source: Unicef Metadata",
       x = "Year", y = "GDP Per Capita (constant 2015 US$)",
       color = "GDP Per Capita") +  # Adjust legend title
  scale_x_continuous(breaks = seq(2000, 2022, by = 5)) +
  scale_color_gradient(low = "lightgreen", high = "darkgreen") +
  theme_bw() + 
  theme(plot.background = element_rect(fill = "grey85", color = NA),
        panel.background = element_rect(fill = "grey85", color = NA),
        legend.background = element_rect(fill = "grey85", color = NA))

ggplotly(p, tooltip = "text")





joined_data <- unicef_metadata %>%
  inner_join(unicef_indicator_1, by = "country") %>%
  filter(year >= 2000 & year <= 2022) %>%
  group_by(country, region, year) %>%
  summarize(avg_gdp_per_capita = mean(`GDP per capita (constant 2015 US$)`, na.rm = TRUE),
            avg_obs_value = mean(obs_value, na.rm = TRUE), 
            .groups = "drop")
joined_data$region <- as.factor(joined_data$region)
n_regions <- length(unique(joined_data$region)) 
color_palette <- brewer.pal(n = n_regions, name = "RdBu")
color_palette <- rev(color_palette)  
p <- ggplot(joined_data, aes(x = avg_obs_value, y = avg_gdp_per_capita, color = region, fill = region, text = paste("Year:", year, "<br>Region:", region, "<br>Unimproved Water Access (%):", avg_obs_value))) +
  geom_point(shape = 21, size = 3, color = "black", stroke = 0.2) +
  labs(title = "GDP per Capita vs. Unimproved Water Access by Region",
       x = "Unimproved Water Access (%)",
       y = "GDP per Capita (constant 2015 US$)",
       color = "Region",
       fill = "Region") + 
  scale_fill_manual(values = color_palette) +
  theme_bw() +
  theme(plot.background = element_rect(fill = "gray85", color = NA),
        panel.background = element_rect(fill = "gray85", color = NA),
        legend.background = element_rect(fill = "gray85"),
        legend.position = "bottom",
        legend.box = "horizontal") +
  scale_x_continuous(breaks = seq(-5, 50, by = 5)) +
  scale_y_continuous(breaks = seq(0, 300000, by = 50000))

ggplotly(p, tooltip = "text")

