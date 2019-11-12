my_plan <- drake_plan(
  commute_mode = read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-05/commute.csv"),
  show_overall_plot = my_overall_plot(commute_mode),
  show_walk_plot = my_walk_plot(commute_mode),
  show_stats = desc_stats(commute_mode),
  fit = lm(percent ~ city_size, data = filter(commute_mode, mode == "Walk")),
  summary_fit = summary(fit)
)