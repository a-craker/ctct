library(scales)
library(tidyverse)

alphanumeric_waves <- paste0(rep(1:8, each = 6), LETTERS[1:6])

seeds <- c("$", "@", "&", alphanumeric_waves, "Dh", "Ds")

plot_data <- cycle_tour %>%
  filter(time != "", 
         time != "Not started") %>% 
  mutate(
    time_seconds = as.numeric(hms(time)),
    # Use the new seeds vector here to force the X-axis order
    wave = factor(wave, levels = seeds) 
  ) %>%
  filter(!is.na(wave))


hour_breaks <- seq(0, 36000, by = 3600)

# 3. Create the plot
boxplot <- ggplot(plot_data, aes(x = wave, y = time_seconds)) +
  geom_boxplot(
    fill = "#A6CEE3", 
    color = "#1F78B4", 
    outlier.alpha = 0.2, 
    outlier.size = 3
  ) +
  scale_y_time(breaks = hour_breaks) +
  labs(
    title = "Cape Town Cycle Tour 2026",
    x = "Seed",
    y = "Time (Hours)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),              
    axis.text.x = element_text(angle = 90, vjust = 0.5, size = 12),
    axis.text.y = element_text(size = 10),
    panel.grid.major.y = element_line(color = "grey"), 
    panel.grid.major.x = element_line(color = "grey"), 
    panel.grid.minor.y = element_blank()
  )

ggsave("output/ctct26_boxplot.png", boxplot, width = 24, height = 10, dpi = 300)


# -------------------------------------------------------------------------

theme_clean_sans <- theme_void() +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 10, family = "sans", face = "bold"),
    # axis.title.x = element_text(size = 11, family = "sans"),
    axis.title.x = element_blank(),
    plot.title = element_text(size = 13, family = "sans", face = "bold", hjust = 0.5),
    plot.margin = margin(5, 5, 5, 5)
  )

sub3 <- plot_data %>%
  mutate(sub3 = if_else(
    time_seconds < 10800, 1, 0
  )) %>% 
  filter(sub3 == 1) %>%
  group_by(wave) %>% 
  summarise(riders = n_distinct(race_no), .groups = "drop") %>% 
  arrange(riders) %>% 
  ggplot(aes(y = riders, x = wave)) +
  geom_col(fill = "#1F78B4") +
  geom_text(aes(label = comma(riders)), vjust = -0.5, family = "sans") +
  scale_x_discrete() +
  labs(title = "Sub 3 Riders by Seed", x = "Seed", y = "Riders") +
  theme_clean_sans
  
  
ggsave("output/ctct26_sub3.png", sub3, width = 8, height = 4.5, dpi = 300)
  

# -------------------------------------------------------------------------

plot_data %>%
  filter(!wave %in% c("$", "@", "&")) %>% 
  mutate(sub3 = if_else(
    time_seconds < 10800, 1, 0
  )) %>% group_by(sub3) %>% 
  summarise(n = n_distinct(race_no))


plot_data %>%
  # filter(!wave %in% c("$", "@", "&")) %>% 
  mutate(sub3 = if_else(
    time_seconds < 10800, 1, 0
  )) %>% 
  filter(sub3 == 1) %>% 
  group_by(wave) %>% 
  summarise(n = n_distinct(race_no))
