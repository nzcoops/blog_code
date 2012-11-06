# github code


library(knitr)
setwd("/Users/mcooper/Documents/git/blog_code")
knit("petrol_prices.Rmd")
system("pandoc -s petrol_prices.md -o petrol_prices.html")
knit("ggplot_post_text_example.Rmd")
system("pandoc -s ggplot_post_text_example.md -o ggplot_post_text_example.html")
knit("nfl_team_picker_example.Rmd")
system("pandoc -s nfl_team_picker_example.md -o nfl_team_picker_example.html")
knit("forest_plot.Rmd")
system("pandoc -s forest_plot.md -o forest_plot.html")
