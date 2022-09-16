# PsychTopics – A Shiny App for Exploring and Analyzing Research Topics in Psychology

![alt text](https://github.com/leibniz-psychology/psychtopics/blob/main/screenshot.png?raw=true)

## Launch via shinyapps.io
[https://abitter.shinyapps.io/psychtopics/](https://abitter.shinyapps.io/psychtopics/)

## Running the app locally

1. Download the files using the green "Code" button
2. Unzip 
3. Start RStudio by opening `psychtopics.Rproj`
4. Open the `app.R` script file
5. Run the code or use the "Run App" button in RStudio
6. Important: In the new RStudio window, click "Open in Browser" in the top menu
7. The app should be up and running in your browser!

## Packages

You may also need to the install following packages:
```
install.packages(c('devtools','config','echarts4r','golem','htmlwidgets','reactable','shiny.fluent','shiny.react','shiny.router','tsbox'))
devtools::install_github("Appsilon/shiny.fluent")
```
