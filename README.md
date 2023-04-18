# PsychTopics – A Shiny App for Exploring and Analyzing Research Topics in Psychology

![alt text](https://github.com/leibniz-psychology/psychtopics/blob/main/screenshot.png?raw=true)

The increasing pace of digitalization and globalization has led to a significant increase in the amount of information available in the field of science, including psychology. To help researchers and other interested parties navigate this information overload, we developed _PsychTopics_, an online tool that uses machine learning to identify research topics and trends in psychology literature from the German-speaking countries (www.psyndex.de/en). The topics are continuously detected with [RollingLDA](https://github.com/JonasRieger/rollinglda) – a variant of topic modeling that enables sequential modeling of dynamically growing corpora and ensures time consistency of resulting time series.

:scroll: **Find the research paper here:**  
Bittermann, A. & Rieger, J. (2022). Finding Scientific Topics in Continuously Growing Text Corpora. In _Proceedings of the Third Workshop on Scholarly Document Processing_, pages 7–18, Gyeongju, Republic of Korea. Association for Computational Linguistics. https://aclanthology.org/2022.sdp-1.2

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
