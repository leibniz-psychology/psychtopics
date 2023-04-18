# PsychTopics – A Shiny App for Exploring and Analyzing Research Topics in Psychology

![alt text](https://github.com/leibniz-psychology/psychtopics/blob/main/screenshot.png?raw=true)

Find the research paper here: [Bittermann & Rieger (2022): "Finding Scientific Topics in Continuously Growing Text Corpora"](https://aclanthology.org/2022.sdp-1.2/)

## Launch via shinyapps.io
[https://abitter.shinyapps.io/psychtopics/](https://abitter.shinyapps.io/psychtopics/)

## Running the app locally

1. Start R
2. Run `shiny::runGitHub('psychtopics','leibniz-psychology')`
3. Important when using RStudio: In the new RStudio window, click "Open in Browser" in the top menu
4. The app should be up and running in your browser!

## Packages

You may also need to the install following packages:
```
install.packages(c('devtools','config','echarts4r','golem','htmlwidgets','reactable', 'shiny', 'shiny.react','shiny.router','tsbox'))
devtools::install_github('Appsilon/shiny.fluent')
```
