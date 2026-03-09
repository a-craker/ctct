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
    outlier.size = 0.5
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
    axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8),
    panel.grid.major.y = element_line(color = "grey"), 
    panel.grid.major.x = element_line(color = "grey"), 
    panel.grid.minor.y = element_blank()
  )

ggsave("output/ctct26_boxplot.png", boxplot, width = 24, height = 10, dpi = 300)
