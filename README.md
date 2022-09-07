# PsychTopics – A Shiny App for Exploring and Analyzing Research Topics in Psychology

## Running the app  

There are two ways to run the app.  

### First way  

Install the app from GitHub and run!  

```{r eval=FALSE}

## install from github
devtools::install_github("leibniz-psychology/psychtopics")

## if the installation is successful, the app should run with this command!
psychtopics::run_app()

```

### Second way  

1. Download using the green Code-Button and unzip 
2. Start RStudio by opening `psychtopics.Rproj`
3. Open the `app.R` script file
4. Run the code or use the "Run App" button in RStudio
5. Important: In the new RStudio window, click "Open in Browser" in the top menu
6. The app should be up and running in your browser!

## Packages

You may also need to the install following packages:
```
install.packages(c('config','echarts4r','golem','htmlwidgets','reactable','shiny.fluent','shiny.react','shiny.router','tsbox'))
devtools::install_github("Appsilon/shiny.fluent")
```
