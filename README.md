# disease-website
Shiny{} app for disease data from Summer 2016 Math Research

###Live Version
[The current version is live here](https://msandgren.shinyapps.io/Diseases_2/). It's hosted on a free `shinyapps.io` server, but is limited to 25 up hours per month. Therefore, if you have `R` installed, it would be helpful to run there.

###Instructions
You'll need to install Shiny in R to begin with: 

```R
install.packages('shiny')
library(shiny)
```

To use in R, create a folder in your working directory named <disease_app>. Place `server.R` and `ui.R` in this main level. Next, create two folders inside `disease_app`, named `data` and `www`. The CSS files go in `www`, and the CSV's go in `data`. 

To run the app, open `server.R` and `ui.R` in R, and in the console, type: `runApp('disease_app')`. 
