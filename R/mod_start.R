#' start UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_start_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(
      class = "two-cards",
      makeCard(
        size = 12,
        title = "About PsychTopics",
        style = "background-color: #c6cf78ff",
        content = tagList(
          bodyText(
            tagList("With this tool, you can explore current and past ",
                    tags$b("research trends in psychology"),
                    " from the ",  tags$b("German-speaking countries."),
                    " Topics are identified in ", tags$a("PSYNDEX", href = "http://www.psyndex.de/en/", target = "_blank"),
                    ", the comprehensive literature database produced by the ",
                    tags$a("Leibniz Institute for Psychology (ZPID)", href = "https://www.leibniz-psychology.org/en/", target = "_blank"),
                    "."
            )
          ),
          br(),
          bodyText(uiOutput(ns("last_update"))),
          br(),
          bodyText(
            # "PsychTopics is ", tags$b("open-source software"), ".", br(),
            "PsychTopics is open-source software.", br(),
            " See the ",
            tags$a("GitHub repo", href = "https://github.com/leibniz-psychology/psychtopics", target = "_blank"),
            " for a list of contributors and the code."
          ),
          br(),
          bodyText(
            tagList(
              "How to cite: ",
              shiny.fluent::TooltipHost(
                content = tagList(
                  tags$div(
                    style = "margin: 11px",
                    shiny.fluent::Text("Bittermann, A. & Rieger, J. (2022). Finding Scientific Topics in Continuously Growing Text Corpora.",
                                       br(),
                                       "In A. Cohan et al. (Eds.), ",
                                       tags$i("Proceedings of the Third Workshop on Scholarly Document Processing"), 
                                       " (7–18), Gyeongju, Republic of Korea. Association for Computational Linguistics."),
                    br(),
                    tags$a("https://aclanthology.org/2022.sdp-1.2/", href = "https://aclanthology.org/2022.sdp-1.2/", target = "_blank")
                  )
                ),
                delay = 0,
                tags$a("Bittermann & Rieger (2022)")

              )
            )
          )
        )
      ),
      div(
        
      ),
      makeCard(
        size = 12,
        title = tagList(
          div(
            style = "float:right",
            shiny.fluent::TooltipHost(
              delay = 0,
              content = div(
                style = "margin: 13px",
                shiny.fluent::Text(
                  "Throughout PsychTopics, you will find more of these information boxes.",
                  br(),
                  "Hovering over the ", tags$i("info buttons"),
                  #shiny.fluent::Icon(iconName = "Info"),
                  " should open the box."
                )
              ),
              shiny.fluent::IconButton.shinyInput(inputId = ns("help1"), iconProps = list(iconName = "Info", className = "icon-help"), class = "button-help-green")
            )
          ),
          "How to Use PsychTopics"
        ),
        style = "background-color: #c6cf78ff",
        content = tagList(
          
          
          bodyText(
            tags$ol(tags$b(
              tags$li("Use the menu for different topic views."),
              tags$li(
                "Click on the icons in the top right corner of the boxes for more information."
              ),
              tags$li("Draw conclusions carefully*")
            )),
            br(),
            "*PsychTopics is designed for exploratory purposes.
            Topics are derived from scientific publications ", tags$i("automatically"),
            " using machine learning algorithms.
            Thus, ", tags$b("PsychTopics makes no claim to completeness and cannot replace specific search strategies."), 
            " For more information, see “Methods” in the menu."
            
            
          )
          
        )
      )
    ),
    
    #br(),
    
    div(
      class = "two-cards",
      style = "margin-bottom: 0;",
      makeCard(
        size = 12,
        style = "background-color: #e9ecefff",
        title = title_with_help(
          id = ns("help2"),
          title = uiOutput(ns("title_box3")),
          content = tagList(
            shiny.fluent::Text(
              "These are the - ", tags$b("preliminary"), " - most popular topics in the current year.",
              br(),
              br(),
              "Each topic has a numeric id. See ", tags$i("Browse Topics"), " in the menu for more topic details.",
              br(),
              br(),
              "The larger the bar, the more publications address the topic.",
              br(),
              "A publication in counted as addressing a topic, if at least 50% of its contents are related to this topic.",
              br(),
              br(),
              tags$b("Please note: "), " These preliminary topics might change with updates throughout the year,", br(),
              " since publications of the current year may not be recorded yet.", br(),
              " Moreover, journals, books, and reports on specific topics may be published in waves (e.g., quarterly issues)."
            )
            
          )
        ),
          
        content = tagList(
          
          div(
            class = "grid-p1-b3-b4",
            div(
              class = "text",
              style = "margin-top: 11px",
              bodyText(text = "Please note that these topics are preliminary!")
            ),
            div(
              class = "dropdown",
              
              shiny.fluent::Dropdown.shinyInput(
                inputId = ns("dropdown_most_popular1"),
                style = list(textAlign = "center"),
                label = "Show top",
                options = list(
                  list(key = 5, text = "5"),
                  list(key = 10, text = "10"),
                  list(key = 15, text = "15"),
                  list(key = 20, text = "20")
                ),
                value = 10
              ),
            )
          ),
          
          br(),
          echarts4r::echarts4rOutput(ns("plot_box3"), height = 550)
          #highcharter::highchartOutput(ns("plot_box3"), height = 650)
        )
      ),
      
      div(
        
      ),
      
      makeCard(
        size = 12,
        style = "background-color: #e9ecefff",
        title = title_with_help(
          id = ns("help3"),
          title = "Overall Most Popular Topics in PSYNDEX",
          content = tagList(
            shiny.fluent::Text(
              "These are the most popular topics in PSYNDEX across all years since 1980.",
              br(),
              br(),
              "Each topic has a numeric id. See ", tags$i("Browse Topics"), " in the menu for more details on topics.",
              br(),
              br(),
              "The larger the bar, the more publications address the topic.",
              br(),
              br(),
              "A publication in counted as addressing a topic,", br(),
              " if at least 50% of its contents are related to this topic."
            )
          )
        ),
          
        #   
        #   tagList(
        #   div(
        #     style = "float:right",
        #     shiny.fluent::IconButton.shinyInput(inputId = ns("help3"), iconProps = list(iconName = "Info", className = "icon-help-grey"), class = "button-help-grey")
        #   ),
        #   "Overall Most Popular Topics in PSYNDEX"
        # ),
        content = tagList(
          
          div(
            class = "grid-p1-b3-b4",
            div(
              class = "text"
              #style = "margin-top: 11px",
              #bodyText(text = "Please note that these topics are preliminary!")
            ),
            div(
              class = "dropdown",
              
              shiny.fluent::Dropdown.shinyInput(
                inputId = ns("dropdown_most_popular2"),
                #url = "https://abitter.shinyapps.io/psychtopics/_w_f422542b/#!/browse-topics",
                style = list(textAlign = "center"),
                label = "Show top",
                options = list(
                  list(key = 5, text = "5"),
                  list(key = 10, text = "10"),
                  list(key = 15, text = "15"),
                  list(key = 20, text = "20")
                ),
                value = 10
              )
            )
          ),
          br(),
          echarts4r::echarts4rOutput(ns("plot_box4"), height = 550)
          #highcharter::highchartOutput(ns("plot_box4"), height = 650)
          #plotOutput(ns("plot_box4"))
        )
      )
    ),
  spsGoTop(
    id = "gotop",
    icon = icon("arrow-up-long", "fa-solid"),
    right = "2rem",
    bottom = "5rem",
    color = "#953386"
  )  
  )
}
    
#' start Server Functions
#' 
#' @import echarts4r
#'
#' @noRd 
mod_start_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$last_update = renderUI({
      req(r$last_updated)

      glue::glue("Last update (quarterly): {r$last_updated}")
    })
    # 
    # observeEvent(input$dropdown_most_popular1, {
    #   shiny.fluent::updateDropdown.shinyInput(inputId = "dropdown_most_popular2", value = 5)
    # }, once = TRUE)
    
    output$title_box3 = renderUI({
      req(r$current_year)
      #x = 2019
      glue::glue("Popular PSYNDEX Topics in {r$current_year}")
    })
    
    

    output$plot_box3 = echarts4r::renderEcharts4r({
      req(r$n_doc_year, r$topic, r$current_year, input$dropdown_most_popular1, r$topic_evo_concatenated)

      # d1 = as.data.frame(as.table(r$n_doc_year)) %>%
      #   dplyr::mutate(year = as.numeric(as.character(Var1)), label = Var2)
      #print(str(d1))
      
      
      d1 = r$n_doc_year
      
      color <- "#953386"

      topics = r$topic %>% 
        dplyr::mutate(
          topic_evo_year = r$topic_evo_concatenated
        )
      

      top = input$dropdown_most_popular1

      df = d1 %>%

        #dplyr::arrange(-Freq) %>%
        #dplyr::slice_head(n = top) %>%
        #dplyr::mutate(Freq = round(Freq * 100, 2)) %>%
        dplyr::left_join(topics, by = c("id" = "ID")) %>% 
        dplyr::filter(year == r$current_year) %>%
        dplyr::arrange(-Freq) %>% 
        dplyr::slice_head(n = top) %>% 
        dplyr::mutate(
          search = "", # for using Top Terms of selected years. See below.
          id2 = as.factor(id),
          tooltip = glue::glue("{topic_evo_year};{r$current_year};{Label};{as.numeric(colnames(r$topic_evo[[1]])[1])}")
        )
      
      
      #print(str(df))
      
      df %>% 
        echarts4r::e_charts(id2) %>% 
        #echarts4r::e_bar(Freq, name = "n-docs", bind = tooltip) %>% 
        # echarts4r::e_bar(Freq, name = "N docs", bind = tooltip, selectedMode = TRUE, select = list(itemStyle = list(color = "#a2b21e"))) %>%
        echarts4r::e_bar(Freq, name = "N docs", bind = tooltip, selectedMode = FALSE) %>% 
        echarts4r::e_title(text = glue::glue("Popular topics in {r$latest_year}")) %>% 
        echarts4r::e_flip_coords() %>% 
        echarts4r::e_x_axis(name = "essential publications", nameLocation = "center", nameGap = 27) %>% 
        echarts4r::e_y_axis(name = "ID", nameLocation = "center", nameRotate = 0, nameGap = 35, inverse = TRUE, show = FALSE) %>% 
        echarts4r::e_tooltip(
          confine = TRUE,
          formatter = htmlwidgets::JS("
            function(params){
              var vals = params.name.split(';');
              year = vals[1];
              min_year = vals[3];
              top_terms = year <= min_year ? vals[0].match(min_year + '.*')[0].replace(min_year, '') : vals[0].match(year + '.*')[0].replace(year, '');
              return('ID: ' + params.value[1] + 
                      '<br/> Label: ' + vals[2] +
                      '<br/> Essential Publications: ' + params.value[0]) +
                      '<br/> Year: ' + year + 
                      '<br/> Evolution Terms' + top_terms
                      }
          ")
          #textstyle.overflow = "break"
        ) %>% 
        echarts4r::e_labels(
          position = "insideLeft",
          fontSize = 12,
          color = "#000",
          formatter = htmlwidgets::JS("
            function(params){
              return(params.name.split(';')[2])
              }
          ")
        ) %>% 
        echarts4r::e_color(color = color) %>% 
        echarts4r::e_legend(show = FALSE)
      
      
    })
    
    output$plot_box4 = echarts4r::renderEcharts4r({
      req(r$n_doc_year, r$topic, input$dropdown_most_popular2)

      # d1 = as.data.frame(as.table(r$n_doc_year)) %>%
      #   dplyr::mutate(year = as.numeric(as.character(Var1)), label = Var2)
      #print(str(d1))
      
      d1 = r$n_doc_year
      
      color <- "#953386"

      top = input$dropdown_most_popular2
      


      df = r$topic %>%
        #dplyr::filter(year == 2019) %>%
        #dplyr::arrange(-Freq) %>%
        #dplyr::slice_head(n = top) %>%
        #dplyr::mutate(Freq = round(Freq * 100, 2)) %>%
        #dplyr::left_join(r$topic, by = c("id" = "ID")) %>%
        #tibble::glimpse(.) %>% 
        dplyr::arrange(-n_docs) %>% 
        dplyr::slice_head(n = top) %>% 
        #tibble::glimpse(.) %>% 
        dplyr::mutate(
          id2 = as.factor(ID),
          tooltip = glue::glue("{TopTerms};{Label}"),
        )
      
      #print(tail(df))
      
      #r$browse_top_3 = unique(df$id)[1:3]

      #print(str(df))
      
      df %>% 
        echarts4r::e_charts(id2, reorder = FALSE) %>% 
        echarts4r::e_bar(n_docs, name = "n-docs", bind = tooltip) %>% 
        # echarts4r::e_title(text = "Popular topics overall") %>% 
        echarts4r::e_flip_coords() %>% 
        echarts4r::e_x_axis(name = "essential publications", nameLocation = "center", nameGap = 27) %>% 
        echarts4r::e_y_axis(name = "ID", nameLocation = "center", nameRotate = 0, nameGap = 35, inverse = TRUE, show = FALSE) %>% 
        echarts4r::e_tooltip(
          confine = TRUE,
          formatter = htmlwidgets::JS("
            function(params){
              var vals = params.name.split(';');
              return('ID: ' + params.value[1] +
                      '<br/> Label: ' + vals[1] +
                      '<br/> Essential Publications: ' + params.value[0]) + 
                      '<br/> Top Terms: ' + vals[0]
                      }
          ")
        ) %>% 
        echarts4r::e_labels(
          position = "insideLeft",
          fontSize = 15,
          color = "#000",
          #overflow = "break",
          formatter = htmlwidgets::JS("
            function(params){
              return(params.name.split(';')[1])
              }
          ")
        ) %>% 
        echarts4r::e_color(color = color) %>% 
        echarts4r::e_legend(show = FALSE)
      

      # hch2 = df %>%
      #   highcharter::hchart(
      #     "bar",
      #     highcharter::hcaes(x = "id2", y = "Freq", topic = "Thema", topicSplit = "topic_split", id = "id", year = "year"),
      #     name = "Prevalence",
      #     #colorByPoint = TRUE,
      #     borderColor = "black",
      #     dataLabels = list(
      #       enabled = TRUE,
      #       align = "right",
      #       x = -33,
      #       color = "#fff",
      #       style = list(fontSize = 13),
      #       formatter = JS('
      #       function() {
      #         return this.point.topicSplit.slice(0, 2);
      #       }'
      #       )
      #     )
      #   ) %>%
      #   highcharter::hc_chart(
      #     plotBorderColor = "#aaa",
      #     plotBorderWidth = 2
      #   ) %>%
      #   highcharter::hc_colors(color) %>%
      #   highcharter::hc_xAxis(title = list(text = ""), labels = list(style = list(fontSize = "17px")), gridLineColor = 'transparent') %>%
      #   highcharter::hc_yAxis(title = list(text = "Prevalence"), gridLineColor = 'transparent') %>%
      #   #highcharter::hc_add_theme(highcharter::hc_theme_google()) %>%
      #   highcharter::hc_title(text = glue::glue("Popular topics overall"), style = list(fontSize = "21px")) %>%
      #   highcharter::hc_tooltip(
      #     pointFormat = "ID: {point.id} <br/> Year: {point.year} <br/> Prevalence: {point.y} <br/> Topic: {point.topic}",
      #     headerFormat = "",
      #     style = list(fontSize = "15px", opacity = 1),
      #     borderWidth = 2,
      #     backgroundColor = "#fff",
      #     hideDelay = 333
      #   ) %>%
      #   highcharter::hc_size(height = height)



    })
    
    # output$plot_box4 = renderPlot({
    #   shinipsum::random_ggplot()
    # })
    # 
    
  })
}
    
## To be copied in the UI
# mod_start_ui("start_ui_1")
    
## To be copied in the server
# mod_start_server("start_ui_1")
