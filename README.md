# disease-website
Shiny{} app for disease data from Summer 2016 Math Research

You'll need to install Shiny in R to begin with: `install.packages('Shiny')`, followed by, of course, `library(Shiny)`.

To use in R, create a folder in your working directory named <disease_app>. Place `server.R` and `ui.R` in this main level. Next, create two folders inside `disease_app`, named `data` and `www`. The CSS files go in `www`, and the CSV's go in `data`. 

To run the app, open `server.R` and `ui.R` in R, and in the console, type: `runApp('disease_app')`. 
