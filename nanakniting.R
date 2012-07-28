library(knitr)

knit("petrol_prices.Rmd")
system("pandoc -s petrol_prices.md -o petrol_prices.html")