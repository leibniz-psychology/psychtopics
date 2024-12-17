#' popular_by_year UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_hot_topics_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    div(
      #class = "three-cards",
      class = "two-cards-33-66",
      
      makeCard(
        title = "OSF & PsychArchives Preregistrations by Year and Month",
        size = 12,
        style = "background-color: #c6cf78ff",
        content = tagList(
          
          bodyText("Explore the most popular topics in a specific year and month."),
          br(),
          br(),
          shiny.fluent::Stack(
            horizontal = TRUE,
            div(
              class = glue("ms-Grid-col ms-sm{12} ms-xl{12}"),
              style = "text-align: center",
              uiOutput(ns("ui_select_year"))
            ),
            div(
              class = glue("ms-Grid-col ms-sm{12} ms-xl{12}"),
              style = "text-align: center",
              uiOutput(ns("ui_select_month"))
            )
            
          ),
          
          br(),
          br(),
          uiOutput(ns("box1_text")),
          br(),
          uiOutput(ns("last_updated"))
        )
        
      ),
      
      div(),
      
      makeCard(
        style = "background-color: #e9ecefff",
        title = title_with_help(
          id = ns("help2"),
          title = uiOutput(ns("title_box2")),
          content = tagList(
            shiny.fluent::Text(
              "These are the most popular topics in paper registrations in OSF as well as PsychArchives in the selected year and month.",
              br(),
              br(),
              "The larger the bar, the more publications address the topic.",
              br(),
              br(),
              "A registration is assigned to a particular topic using BERTopic in Python which is a topic modeling technique that uses BERT embeddings."
            )
          )
        ),
        size = 12,
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
                inputId = ns("dropdown_most_popular"),
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
          
          echarts4r::echarts4rOutput(ns("plot_box2"), height = 430)
        )
       )
      ),          
      
#      div(),
#      
#      makeCard(
#        title = title_with_help(
#          id = ns("help2"),
#          title = uiOutput(ns("title_box2")),
#          content = tagList(
#            shiny.fluent::Text(
#              "These are the most popular topics in PSYNDEX in the selected year.",
#              br(),
#              br(),
#              "Each topic has a numeric id. See the table below for more topic details.",
#              br(),
#              br(),
#              "The larger the bar, the more publications address the topic.",
#              br(),
#              br(),
#              "A publication is counted as addressing a topic, if at least 50% of this publication’s content is related to this topic."
#            )
#          )
#        ),
#        size = 12,
#        content = tagList(
#          
#          div(
#            class = "grid-p1-b3-b4",
#            div(
#              class = "text"
#              #style = "margin-top: 11px",
#              #bodyText(text = "Please note that these topics are preliminary!")
#            ),
#            div(
#              class = "dropdown",
#              
#              shiny.fluent::Dropdown.shinyInput(
#                inputId = ns("dropdown_most_popular"),
#                style = list(textAlign = "center"),
#                label = "Show top",
#                options = list(
#                  list(key = 5, text = "5"),
#                  list(key = 10, text = "10"),
#                  list(key = 15, text = "15"),
#                  list(key = 20, text = "20")
#                ),
#                value = 10
#              )
#            )
#          ),
#          
#          echarts4r::echarts4rOutput(ns("plot_box2"), height = 430)
#        )
#      )
#    ),
#
#    
#    
#    
#    div(
#     class = "two-cards-33-66",
#      
#      makeCard(
#        title = "Popular PSYNDEX Topics by Year",
#        size = 12,
#        style = "background-color: #c6cf78ff",
#       content = tagList(
#          
#          bodyText("Explore the most popular topics in a specific year."),
#          br(),
#          br(),
#          shiny.fluent::Stack(
#            horizontal = TRUE,
#            div(
#             class = glue("ms-Grid-col ms-sm{12} ms-xl{12}"),
#              style = "text-align: center",
#              uiOutput(ns("ui_select_year"))
#            )
#            
#          ),
#          
#          br(),
#          br(),
#          uiOutput(ns("box1_text")),
#          br(),
#          uiOutput(ns("last_updated"))
#        )
#        
#      ),
#      
#      div(),
#      
#      makeCard(
#        title = title_with_help(
#          id = ns("help2"),
#          title = uiOutput(ns("title_box2")),
#          content = tagList(
#            shiny.fluent::Text(
#              "These are the most popular topics in PSYNDEX in the selected year.",
#              br(),
#              br(),
#              "Each topic has a numeric id. See the table below for more topic details.",
#              br(),
#              br(),
#              "The larger the bar, the more publications address the topic.",
#              br(),
#              br(),
#              "A publication is counted as addressing a topic, if at least 50% of this publication’s content is related to this topic."
#            )
#          )
#        ),
#        size = 12,
#        content = tagList(
#          
#          div(
#            class = "grid-p1-b3-b4",
#            div(
#              class = "text"
#              #style = "margin-top: 11px",
#              #bodyText(text = "Please note that these topics are preliminary!")
#            ),
#            div(
#              class = "dropdown",
#              
#              shiny.fluent::Dropdown.shinyInput(
#                inputId = ns("dropdown_most_popular"),
#                style = list(textAlign = "center"),
#                label = "Show top",
#                options = list(
#                  list(key = 5, text = "5"),
#                  list(key = 10, text = "10"),
#                  list(key = 15, text = "15"),
#                  list(key = 20, text = "20")
#                ),
#                value = 10
#              )
#            )
#          ),
#          
#         echarts4r::echarts4rOutput(ns("plot_box2"), height = 430)
#        )
#      )
#    ),
    
    div(
      class = "one-card",
      style = "margin-bottom: 0",
      makeCard(
        title = title_with_help(
          id = ns("help3"),
          title = uiOutput(ns("title_box3")),
          content = tagList(
            shiny.fluent::Text(
              "The topics are sorted in decreasing order according to the number of associated papers.",
              br(),
              br(),
              "Basically, a topic is a group of words that are frequently used together in registrations ",
              tags$b("(= Representation)"), ". These terms are found automatically by the algorithm.
              For better interpretation, ", tags$b("Names"), " or Labels were assigned using the R package topiclabels.",
              br(),
              br(),
              "Here, the ", tags$b("Representations"), " are reported. These are the most characteristic terms of the topic in the given year and month.",              
              br(),
              br(),
              "The ", tags$b("Count"), " or number of essential documents for all months is determined by counting all registrations
              that mainly address the topic.",
              br(),
              br(),
              "With ", tags$b("Search OSF"), " and ", tags$b("Search PsychArchives"), " you can explore topic-related articles in osf.oi and PsychArchives.org.
              The search query is generated from the top terms."
            )
          )
        ),
        size = 12,
        content = tagList(
          reactable::reactableOutput(ns("topics_table"))
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

    
#' popular_by_year Server Functions
#'
#' @noRd 
mod_hot_topics_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    opened <- reactiveVal(FALSE)
    observe({
      # Set `opened` reactive to indicate whether this page has been opened
      # It runs only once, after page has been opened for the first time
      if (!opened()) {
        opened(shiny.router::get_page() == "hot-topics")
      }
    })
    
    ## reactiveValues for this mod
    r_mod_pby = reactiveValues()
    
    output$ui_select_year = renderUI({
      req(r$current_year, r$new_data_year, opened())
      shiny.fluent::Dropdown.shinyInput(
        inputId = ns("selected_year"),
        style = list(textAlign = "center", width = "100%"),
        calloutProps = list(directionalHintFixed = TRUE, calloutMaxHeight = 350),
        label = "Select year",
        options = lapply(sort(r$new_data_year, decreasing = TRUE), function(x) list(key = x, text = glue::glue("{x}"))),
        value = r$current_year
      )
    })
    output$ui_select_month = renderUI({
      req(r$current_year, r$new_data_month, opened())
      shiny.fluent::Dropdown.shinyInput(
        inputId = ns("selected_month"),
        style = list(textAlign = "center", width = "100%"),
        calloutProps = list(directionalHintFixed = TRUE, calloutMaxHeight = 350),
        label = "Select month",
        options = lapply(sort(r$new_data_month, decreasing = TRUE), function(x) list(key = x, text = glue::glue("{x}"))),
        value = r$new_data_month[[1]]
      )
    })
    
    output$title_box2 = renderUI({
      req(input$selected_year, opened())
      #x = 2019
      glue::glue("Popular Topics of {input$selected_year}")
    })
    
    output$title_box3 = renderUI({
      req(input$selected_year, opened())
      #x = 2019
      glue::glue("Details for Popular Topics {input$selected_year}")
    })
    
    
    # observeEvent(input$selected_year, {
    #   req(input$selected_year, opened())
    #   
    #   shiny.fluent::updateDropdown.shinyInput(
    #     inputId = "selected_year",
    #     options = lapply(sort(r$start_year:r$current_year, decreasing = TRUE), function(x) list(key = x, text = glue::glue("{x}"))),
    #     value = r$current_year
    #   )
    # })
    
    output$box1_text = renderUI({
      req(input$selected_year, opened())
      print(class(input$selected_year))
      req(input$selected_year == r$current_year)
      
      #bodyText(shiny.fluent::Icon(iconName = "WarningSolid", style = list(fontSize = 33)), glue::glue("  Topics of {r$current_year} are preliminary, as journals, books, and reports on specific topics are published in waves throughout the year."))
      bodyText(tags$b(""), glue::glue(""))
      
    })
    
    # output$last_updated = renderUI({
    #   req(r$last_updated)
    #   bodyText(glue::glue("Last Updated: {r$last_updated}"))
    # })
    
    output$plot_box2 = echarts4r::renderEcharts4r({
      req(input$selected_year, input$selected_month, input$dropdown_most_popular, r$new_data, opened())
      
      color <- "#95339680"
      topics = r$topic %>% 
        dplyr::mutate(
          topic_evo_year = r$topic_evo_concatenated
        )
      top = input$dropdown_most_popular
      df = r$new_data %>%
        dplyr::select(-Document) %>% 
        # as_tibble() %>% 
        dplyr::filter(Year == input$selected_year) %>%
        dplyr::filter(Month == input$selected_month) %>%
        dplyr::arrange(-Count) %>%
        #tibble::glimpse(.) %>% 
        dplyr::slice_head(n = top) %>% 
        dplyr::mutate(
          search = "", # for using Representation of selected month and year for OSF. See below.
          search_2 = "",
          #search = createLink_OSF(Representation),
        tooltip = glue::glue("{input$selected_year};{Name};{Representation};{Count}")
        )
        # dplyr::left_join(topics, by = c("id" = "ID")) %>% 
        #dplyr::mutate(Freq = round(Freq * 100, 2)) %>%
        #dplyr::left_join(r$topic, by = c("id" = "Nr..")) %>% 
      
      r_mod_pby$df = df
      
      #print(str(df))
      
      df %>%
        #dplyr::mutate(colors = c(color, rep("red", 4))) %>% 
        echarts4r::e_charts(Name) %>% 
        # echarts4r::e_bar(Freq, name = "N docs", bind = tooltip, selectedMode = TRUE, select = list(itemStyle = list(color = "#a2b21e"))) %>%
        echarts4r::e_bar(Count, name = "N docs", bind = tooltip, selectedMode = FALSE) %>% 
        #echarts4r::e_title(text = glue::glue("Popular topics in {input$selected_year}")) %>% 
        echarts4r::e_flip_coords() %>% 
        echarts4r::e_x_axis(name = "Count", nameLocation = "center", nameGap = 27) %>% 
        echarts4r::e_y_axis(name = "ID", nameLocation = "center", nameRotate = 0, inverse = TRUE, show = FALSE) %>% 
        echarts4r::e_tooltip(
          #show = FALSE,
          #show.content = FALSE,
          trigger = 'item',
          triggerOn = 'click',
          confine = TRUE,
          formatter = htmlwidgets::JS("
            function(params){
              var vals = params.name.split(';');
              return('Label: ' + vals[1] +
                      '<br/> Representation: ' + vals[2] +
                      '<br/> Year: ' + vals[0] + 
                      '<br/> Count: ' + vals[3])
                      }
          ")
        ) %>% 
        echarts4r::e_labels(
          position = "insideLeft",
          fontSize = 13,
          color = "#000",
          formatter = htmlwidgets::JS("
            function(params){
              return(params.name.split(';')[1])
              }
          ")
        ) %>% 
        echarts4r::e_color(color = color) %>% 
        #echarts4r::e_show_loading() %>% 
        echarts4r::e_legend(show = FALSE)
        #echarts4r::e_highlight(series_index = 0, dataIndex = 2)
        #echarts4r::e_add("itemStyle", colors)
        #echarts4r::e_add("dataIndex", 1:5)
      
    })  ## end plot_box2
    
    observeEvent(selected(), {
      proxy = echarts4r::echarts4rProxy(ns("plot_box2"))
      
      if (is.null(selected())) {
        proxy %>% 
          echarts4r::e_dispatch_action_p("select", dataIndex = NULL)
      } else {
        proxy %>% 
          echarts4r::e_dispatch_action_p("select", dataIndex = (selected() - 1))
      }

    }, ignoreNULL = FALSE)
    
    # observeEvent(input$plot_box2_clicked_data, {
    #   print(input$plot_box2_clicked_data)
    # })
    
    output$topics_table = reactable::renderReactable({
      req(r_mod_pby$df, opened())
      
      # min_year_topic_evo = as.numeric(colnames(r$topic_evo[[1]])[1])
      # selected_year = ifelse(input$selected_year <= min_year_topic_evo, min_year_topic_evo, input$selected_year)
      
      r_mod_pby$df %>% 
        #dplyr::select(Name, Year, Month, Representation, Count) %>%
        dplyr::select(Name, Year, Month, Representation, Count, search, search_2) %>% 
        dplyr::mutate(
          search = createLink_OSF(Representation),
          search_2 = createLink_PsychArchives(Representation)
          ) %>%
        # dplyr::mutate(
        #   topic_evo_year = topic_evo_year %>%
        #     stringr::str_extract(glue::glue("{selected_year}.*")) %>% 
        #     stringr::str_remove(glue::glue("{selected_year}: ")),
        #   search = createLink_evo(topic_evo_year, r$booster)
        # ) %>% 
        reactable::reactable(
          rownames = FALSE,
          searchable = TRUE,
          sortable = FALSE,
          resizable = TRUE,
          #selection = "single",
          defaultSelected = 1,
          defaultPageSize = 20,
          #showPageSizeOptions = TRUE,
          #pageSizeOptions = c(5, 10, 15, 20),
          #onClick = "select",
          # theme = reactable::reactableTheme(
          #   rowSelectedStyle = list(backgroundColor = "#c6cf78ff", boxShadow = "inset 2px 0 0 0 #ffa62d")
          # ),
          columns = list(
            # id = reactable::colDef(
            #   name = "ID"
            # ),
            # TopTerms = reactable::colDef(
            #   name = "Top Terms"
            # ),
            Name = reactable::colDef(
              name = glue::glue("Name")
            ),
            Year = reactable::colDef(
              name = "Year", 
              show = FALSE
            ),
            Month = reactable::colDef(
              name = "Month", 
              show = FALSE
            ),
            Representation = reactable::colDef(
              name = glue::glue("Representation")
            ),
            Count = reactable::colDef(
              name = "Count",
              html = TRUE
            ),
            search = reactable::colDef(
              name = "OSF",
              html = TRUE
            ),
            search_2 = reactable::colDef(
              name = "PsychArchives",
              html = TRUE
            )
            # URLs = reactable::colDef(
            #   name = "Urls"#,
            #   #format = reactable::colFormat(digits = 2)
            # )#,
            # .selection = reactable::colDef(
            #   show = TRUE,
            #   headerClass = "hide-checkbox"
            # )
          )
          
        )
    })  ## end topics_table
    
    selected <- reactive(reactable::getReactableState("topics_table", "selected"))
    
    
    
  })
}
    
## To be copied in the UI
# mod_hot_topics_ui("hot_topoics_ui_1")
    
## To be copied in the server
# mod_popular_by_year_server("popular_by_year_ui_1")
