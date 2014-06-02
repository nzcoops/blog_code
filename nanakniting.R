# github code
# copy into blog folder, 'add', put in a comment, then push
# pop-mac-a1138:blog_code mcooper$ cp forest_plot.Rmd blog_code/
# cp nanakniting.R blog_code/
# cd blog_code/
# git add forest_plot.Rmd 
# git add nanakniting.R 
# git commit -m 'fifth crack'
# git push https://github.com/nzcoops/blog_code

library(knitr)
setwd("/Users/mcooper/Documents/documents_random/git/blog_code")
knit("petrol_prices.Rmd")
system("pandoc -s petrol_prices.md -o petrol_prices.html")
knit("ggplot_post_text_example.Rmd")
system("pandoc -s ggplot_post_text_example.md -o ggplot_post_text_example.html")
knit("nfl_team_picker_example.Rmd")
system("pandoc -s nfl_team_picker_example.md -o nfl_team_picker_example.html")
knit("forest_plot.Rmd")
system("pandoc -s forest_plot.md -o forest_plot.html")
knit("shading_between_the_lines.Rmd")
system("pandoc -s shading_between_the_lines.md -o shading_between_the_lines.html")
