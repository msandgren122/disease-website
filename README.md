# disease-website
Shiny{} app for disease data from Summer 2016 Math Research

##Live Version
[The current version is live here](https://msandgren.shinyapps.io/Diseases_2/). It's hosted on a free `shinyapps.io` server, but is limited to 25 up hours per month. Therefore, if you have `R` installed, it would be helpful to run there, as to save server time.

##Instructions
You'll need to install Shiny in `R` to begin: 
```R
install.packages('shiny')
library(shiny)
```

To use in R, create a folder in your working directory named `disease_app`. Place `server.R` and `ui.R` in this main level. Next, place the `data` and `www` inside `disease_app`. 

To run the app, open `server.R` and `ui.R` in R, and in the console, type: 
```R
runApp('disease_app')
```
