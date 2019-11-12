my_overall_plot <- function(x) {
  x %>%
    group_by(state, mode) %>%
    summarise(mean_percent = mean(percent)) %>%
    ungroup() %>%
    filter(mode == "Walk") %>%
    arrange(-mean_percent) %>%
    top_n(10, mean_percent) %>%
    ggplot(aes(x = fct_reorder(state, mean_percent, median), 
               y = mean_percent, 
               fill = state)) +
    geom_col() +
    guides(fill = FALSE) +
    coord_flip() +
    labs(x = "State", 
         y = "Percentage of Walkers", 
         title = "States with the Highest Percentage of Walkers") +
    theme(text = element_text(size = 10)) +
    theme_minimal()
}

my_walk_plot <- function(x) {
  x %>%
    filter(mode == "Walk") %>%
    group_by(city_size) %>%
    ggplot(aes(x = city_size, y = percent, colour = city_size)) +
    geom_jitter(width = .1, size = 3, alpha = .25) +
    guides(colour = FALSE) +
    labs(title = "% of Walkers by City Size",
         x = "City Size",
         y = "Percent of Walkers") +
    theme(text = element_text(size = 12)) +
    coord_flip()
}

desc_stats <- function(x) {
  x %>% 
    filter(mode == "Walk") %>%
    group_by(city_size) %>% 
    summarise(mean_walk = mean(percent), sd_walk = sd(percent))
}