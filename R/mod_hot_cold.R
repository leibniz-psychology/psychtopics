#' hot_cold UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_hot_cold_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    div(
      class = "three-cards",
      
      makeCard(
        title = "Hot and Cold Topics in PSYNDEX",
        style = "background-color: #c6cf78ff",
        content = tagList(
          
          bodyText("Hot topics show the highest increase in number of publications over the years,
                   while cold topics are characterized by a decrease."),
          br(),
          br(),
          #bodyText("You can set the range of years here"),
          
          shiny.fluent::Stack(
            horizontal = TRUE,
            div(
              class = "ms-Grid-col ms-sm10 ms-xl10",
              
              ## may need to be changed when https://github.com/Appsilon/shiny.fluent/issues/63 is solved
              # shiny.fluent::Slider(
              #   onChange = shiny.fluent::setInput(ns("slider"), 2),
              #   ranged = TRUE,
              #   label = "Select the range of years",
              #   min = 1980,
              #   max = 2019,
              #   defaultValue = 2019,
              #   defaultLowerValue = 2015,
              #   snapToStep = TRUE
              # )
              
              uiOutput(ns("slider_input")),
              
            ),
            div(
              class = "ms-Grid-col ms-sm1 ms-xl1",
              br(),
              shiny.fluent::IconButton.shinyInput(
                inputId = ns("go"),
                iconProps = list(iconName = "Forward"),
                className = "buttons-tab2",
                disabled = TRUE
              )
            )
          ),
          
          
          br(),
          br(),
          uiOutput(ns("cur_year_text"))
        ),
        
        
        
        size = 12
      ),
      
      div(),
      
      makeCard(
        title = title_with_help(
          id = ns("help2"),
          title = uiOutput(ns("title_box2")),
          content = tagList(
            shiny.fluent::Text(
              "Hot topics show the strongest ", tags$b("increase in publications"), " during the specified time span.",
              br(), br(),
              "The top three hot topics are shown by default. You can add more topics from the table below.",
              br(), br(),
              "The increase is determined using slopes of a linear regression model."
            )
          )
        ),
        size = 12,
        content = tagList(
          echarts4r::echarts4rOutput(ns("hot_plot")),
          shiny.fluent::Stack(
            horizontal = TRUE,
            div(class = "ms-Grid-col ms-sm4 ms-xl4"),
            div(
              class = "ms-Grid-col ms-sm4 ms-xl4",
              shiny.fluent::DefaultButton.shinyInput(inputId = ns("clear_plot_hot"), text = "Clear Plot")
            )
          )
        )
      ),
      
      div(),
      
      makeCard(
        title = title_with_help(
          id = ns("help3"),
          title = uiOutput(ns("title_box3")),
          content = tagList(
            shiny.fluent::Text(
              "Cold topics show the strongest ", tags$b("decrease in publications"), " during the specified time span.",
              br(), br(),
              "The top three cold topics are shown by default. You can add more topics from the table below.",
              br(), br(),
              "The decrease is determined using slopes of a linear regression model."
            )
          )
        ),
        size = 12,
        content = tagList(
          tagList(
            echarts4r::echarts4rOutput(ns("cold_plot")),
            shiny.fluent::Stack(
              horizontal = TRUE,
              div(class = "ms-Grid-col ms-sm4 ms-xl4"),
              div(
                class = "ms-Grid-col ms-sm4 ms-xl4",
                shiny.fluent::DefaultButton.shinyInput(inputId = ns("clear_plot_cold"), text = "Clear Plot")
              )
            )
          )
        )
      )
    ),
    
    
    div(
      class = "two-cards",
      style = "margin-bottom: 0",
      makeCard(
        title = title_with_help(
          id = ns("help4"),
          title = uiOutput(ns("title_box4")),
          content = tagList(
            shiny.fluent::Text(
              "The topics are sorted by their linear trend (highest increase at the top).",
              br(), br(),
              "With ", tags$b("Search PSYNDEX"), ", you can explore topic-related articles in PubPsych.eu."
              #The search query is generated with the ", tags$b("Evolution Terms"), ", which are the most
              #characteristic terms of the topic in the given year."
            )
          )
        ),
        size = 12,
        content = tagList(
          tagList(
            reactable::reactableOutput(ns("hot_table"))
          )
        )
      ),
      
      div(),
      
      makeCard(
        title = title_with_help(
          id = ns("help5"),
          title = uiOutput(ns("title_box5")),
          content = tagList(
            shiny.fluent::Text(
              "The topics are sorted by their linear trend (highest decrease at the top).",
              br(), br(),
              "With ", tags$b("Search PSYNDEX"), ", you can explore topic-related articles in PubPsych.eu."
              #The search query is generated with the ", tags$b("Evolution Terms"), ", which are the most
              #characteristic terms of the topic in the given year."
            )
          )
        ),
        size = 12,
        content = tagList(
          tagList(
            reactable::reactableOutput(ns("cold_table"))
          )
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

#' hot_cold Server Functions
#'
#' @noRd 
mod_hot_cold_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    opened <- reactiveVal(FALSE)
    observe({
      # Set `opened` reactive to indicate whether this page has been opened
      # It runs only once, after page has been opened for the first time
      if (!opened()) {
        opened(shiny.router::get_page() == "hot-cold")
      }
    })
    
    r_mod_hot_cold = reactiveValues(
      lower = NULL,
      upper = NULL
    )
    
    output$slider_input = renderUI({
      
      req(r$current_year, r$start_year, opened())
      
      shiny.fluent::Slider(
        onChange = shiny.fluent::setInput(ns("slider"), 2),
        ranged = TRUE,
        label = "Select the range of years",
        min = r$start_year,
        max = (r$current_year - 1),
        defaultValue = (r$current_year - 1),
        defaultLowerValue = (r$current_year - 3),
        snapToStep = TRUE
      )
    })
    #sliderInput("range", "Select the range of years",
    #            min = r$start_year, max = (r$current_year - 1),
    #           value = c((r$current_year - 3), (r$current_year - 1))))
    #})
    
    
    output$cur_year_text = renderUI({
      req(r$current_year, opened())
      bodyText(glue::glue("For trends, only records from 1980 to {r$current_year - 1} are included,
               since publications of the current year may not be fully covered yet. The records are always updated after the first quarter of the following year, i.e. in April {r$current_year + 1}."))
    })
    
    observeEvent(opened(), {
      req(opened(), r$current_year)
      golem::invoke_js("setSlider", list = list(id = ns("slider"), vals = c((r$current_year - 3), r$current_year-1)))
    })
    
    observeEvent(input$slider, {
      #req(r_mod_hot_cold$lower)

      req(opened())

      if (!is.null(r_mod_hot_cold$lower)) {
        #print("slider is null")

        if (r_mod_hot_cold$lower != input$slider[1] | r_mod_hot_cold$upper != input$slider[2]) {
          shiny.fluent::updateIconButton.shinyInput(inputId = "go", disabled = FALSE)
        } else {
          shiny.fluent::updateIconButton.shinyInput(inputId = "go", disabled = TRUE)
        }


      } else {
        shiny.fluent::updateIconButton.shinyInput(inputId = "go", disabled = FALSE)
        golem::invoke_js("clickGo", list = list(button = ns("go")))
      }
    })
    
    output$title_box2 = renderUI({
      
      req(opened())
      
      if (is.null(r_mod_hot_cold$lower)) {
        HTML("Hot Topics Plot")
      } else {
        HTML("Hot Topics from", r_mod_hot_cold$lower, "to", r_mod_hot_cold$upper)
      }
      
    })
    
    output$title_box3 = renderUI({
      
      req(opened())
      
      if (is.null(r_mod_hot_cold$lower)) {
        HTML("Cold Topics Plot")
      } else {
        HTML("Cold Topics from", r_mod_hot_cold$lower, "to", r_mod_hot_cold$upper)
      }
      
    })
    
    output$title_box4 = renderUI({
      
      req(opened())
      
      if (is.null(r_mod_hot_cold$lower)) {
        HTML("Hot Topics Table")
      } else {
        HTML("Hot Topics from", r_mod_hot_cold$lower, "to", r_mod_hot_cold$upper)
      }
      
    })
    
    output$title_box5 = renderUI({
      
      req(opened())
      
      if (is.null(r_mod_hot_cold$lower)) {
        HTML("Cold Topics Table")
      } else {
        HTML("Cold Topics from", r_mod_hot_cold$lower, "to", r_mod_hot_cold$upper)
      }
      
    })
    
    # trends function
    trends <- reactive({
      req(r_mod_hot_cold$lower, r_mod_hot_cold$upper, opened())
      
      trends.ab(r_mod_hot_cold$lower-1979, r_mod_hot_cold$upper-1979, 
                r$n_docs_year_0, r$n_docs_time,
                r$n_docs_ts, r$years, r$topic)
      
    })
    
    observeEvent(input$go, {
      req(input$slider, opened())

      shiny.fluent::updateIconButton.shinyInput(inputId = "go", disabled = TRUE)
      if (input$slider[1] == input$slider[2]) {
        r_mod_hot_cold$lower = NULL
        r_mod_hot_cold$upper = NULL
      } else {
        r_mod_hot_cold$lower = input$slider[1]
        r_mod_hot_cold$upper = input$slider[2]
      }

    })
    # observeEvent(input$slider, {
    #   req(opened())
    #     if (input$slider[1] == input$slider[2]) {
    #       r_mod_hot_cold$lower = NULL
    #       r_mod_hot_cold$upper = NULL
    #     } else {
    #       r_mod_hot_cold$lower = input$slider[1]
    #       r_mod_hot_cold$upper = input$slider[2]
    #     }
    # })
    
    
    output$hot_plot = echarts4r::renderEcharts4r({
      # req(input$go, r_mod_hot_cold$lower, r$topic_evo_concatenated, opened())
      req(r_mod_hot_cold$lower, r$topic_evo_concatenated, opened())
      
      topics = r$topic %>% 
        dplyr::mutate(
          topic_evo_year = r$topic_evo_concatenated
        )
      
      trends()[[3]] %>% tsbox::ts_data.frame() %>% 
        dplyr::mutate(
          year = format(time, "%Y") %>% as.character(),
          id = as.numeric(id)
        ) %>% 
        dplyr::select(-time) %>% 
        dplyr::filter(id %in% id_selected_hot()) %>%
        dplyr::left_join(topics, by = c("id" = "ID")) %>%
        dplyr::group_by(Label) %>% 
        dplyr::mutate(
          tooltip = glue::glue("{topic_evo_year};{id};{Label};{as.numeric(colnames(r$topic_evo[[1]])[1])}")
          #tooltip = glue::glue("{TopTerms};{id};{Label}")#,
          #value = round(value * 100, 2) # used for theta only
        ) %>% 
        echarts4r::e_charts(year, reorder = FALSE) %>% 
        echarts4r::e_line(value, bind = tooltip) %>% 
        echarts4r::e_x_axis(name = "Year", nameLocation = "center", nameGap = 27, axisPointer = list(snap = TRUE)) %>% 
        echarts4r::e_y_axis(name = "essential publications", nameLocation = "center", nameGap = 30) %>% 
        echarts4r::e_tooltip(
          confine = TRUE,
          appendToBody = TRUE,
          textStyle = list(width = 50, overflow = "break"),
          axisPointer = list(type = "cross"),
          formatter = htmlwidgets::JS("
            function(params){
              var vals = params.name.split(';');
              year = params.value[0];
              min_year = vals[3];
              top_terms = year <= min_year ? vals[0].match(min_year + '.*')[0].replace(min_year, '') : vals[0].match(year + '.*')[0].replace(year, '');
              return('ID: ' + vals[1] + 
                      '<br/> Label: ' + vals[2] + 
                      '<br/> Essential Publications: ' + params.value[1]) +
                      '<br/> Year: ' + year + 
                      '<br/> Evolution Terms' + top_terms
                      }
          ")
        )
      
      
    })  ## hot_plot
    
    output$cold_plot = echarts4r::renderEcharts4r({
      # req(input$go, r_mod_hot_cold$lower, r$topic_evo_concatenated, opened())
      req(r_mod_hot_cold$lower, r$topic_evo_concatenated, opened())
      
      topics = r$topic %>% 
        dplyr::mutate(
          topic_evo_year = r$topic_evo_concatenated
        )
      
      trends()[[4]] %>% tsbox::ts_data.frame() %>% 
        dplyr::mutate(
          year = format(time, "%Y") %>% as.character(),
          id = as.numeric(id)
        ) %>% 
        dplyr::select(-time) %>% 
        dplyr::filter(id %in% id_selected_cold()) %>%
        dplyr::left_join(topics, by = c("id" = "ID")) %>%
        dplyr::group_by(Label) %>% 
        dplyr::mutate(
          tooltip = glue::glue("{topic_evo_year};{id};{Label};{as.numeric(colnames(r$topic_evo[[1]])[1])}")
          #tooltip = glue::glue("{TopTerms};{id};{Label}")#,
          #value = round(value * 100, 2) # used for theta only
        ) %>% 
        echarts4r::e_charts(year, reorder = FALSE) %>% 
        echarts4r::e_line(value, bind = tooltip) %>% 
        echarts4r::e_x_axis(name = "Year", nameLocation = "center", nameGap = 27, axisPointer = list(snap = TRUE)) %>% 
        echarts4r::e_y_axis(name = "essential publications", nameLocation = "center", nameGap = 30) %>% 
        echarts4r::e_tooltip(
          confine = TRUE,
          appendToBody = TRUE,
          textStyle = list(width = 50, overflow = "break"),
          axisPointer = list(type = "cross"),
          formatter = htmlwidgets::JS("
            function(params){
              var vals = params.name.split(';');
              year = params.value[0];
              min_year = vals[3];
              top_terms = year <= min_year ? vals[0].match(min_year + '.*')[0].replace(min_year, '') : vals[0].match(year + '.*')[0].replace(year, '');
              return('ID: ' + vals[1] + 
                      '<br/> Label: ' + vals[2] + 
                      '<br/> Essential Publications: ' + params.value[1]) +
                      '<br/> Year: ' + year + 
                      '<br/> Evolution Terms' + top_terms
                      }
          ")
        )
      
      
    })
    
    
    
    output$hot_table = reactable::renderReactable({
      req(input$slider, r_mod_hot_cold$lower, r$topic_evo, r$topic_evo_concatenated, opened())
      
      min_year_topic_evo = as.numeric(colnames(r$topic_evo[[1]])[1])
      selected_year = ifelse(input$slider[2] <= min_year_topic_evo, min_year_topic_evo, input$slider[2])
      # selected_year = input$slider[2]
      
      topics = r$topic %>% 
        dplyr::mutate(
          topic_evo_year = r$topic_evo_concatenated
        )
      
      trends()[[1]] %>%
        dplyr::select(ID = NR) %>% 
        dplyr::left_join(topics, by = "ID") %>% 
        dplyr::select(ID, Label, TopTerms, topic_evo_year, Empirical) %>% 
        dplyr::mutate(
          topic_evo_year = topic_evo_year %>%
            stringr::str_extract(glue::glue("{selected_year}.*")) %>% 
            stringr::str_remove(glue::glue("{selected_year}: ")),
          # search = createLink_evo(topic_evo_year, r$booster)
          search = createLink_evo(TopTerms, r$booster)
        ) %>% 
        reactable::reactable(
          rownames = FALSE,
          compact = TRUE,
          searchable = TRUE,
          sortable = FALSE,
          resizable = TRUE,
          fullWidth = FALSE,
          defaultPageSize = 5,
          selection = "multiple",
          defaultSelected = 1:3,
          onClick = "select",
          style = list(
            width = "100%"
          ),
          theme = reactable::reactableTheme(
            rowSelectedStyle = list(backgroundColor = "#c6cf78ff", boxShadow = "inset 2px 0 0 0 #ffa62d")
          ),
          columns = list(
            search = reactable::colDef(
              name = "Publications",
              html = TRUE
            ),
            topic_evo_year = reactable::colDef(
              name = glue::glue("Evolution Terms {input$slider[2]}"),
              show = FALSE
            ),
            Empirical = reactable::colDef(
              name = "Empirical %"
            ),
            .selection = reactable::colDef(
              show = TRUE,
              headerClass = "hide-checkbox"
            ),
            ID = reactable::colDef(
              show = FALSE)
            ,
            TopTerms = reactable::colDef(
              show = TRUE
            )
          )
          
        )
    })  ## hot_table
    
    
    output$cold_table = reactable::renderReactable({
      # req(input$go, opened())
      req(input$slider, r_mod_hot_cold$lower, r$topic_evo, r$topic_evo_concatenated, opened())
      
      min_year_topic_evo = as.numeric(colnames(r$topic_evo[[1]])[1])
      selected_year = ifelse(input$slider[2] <= min_year_topic_evo, min_year_topic_evo, input$slider[2])
      # selected_year = input$slider[2]
      
      topics = r$topic %>% 
        dplyr::mutate(
          topic_evo_year = r$topic_evo_concatenated
        )
      
      trends()[[2]] %>%
        dplyr::select(ID = NR) %>% 
        dplyr::left_join(topics, by = "ID") %>% 
        dplyr::select(ID, Label, TopTerms, topic_evo_year, Empirical) %>% 
        dplyr::mutate(
          topic_evo_year = topic_evo_year %>%
            stringr::str_extract(glue::glue("{selected_year}.*")) %>% 
            stringr::str_remove(glue::glue("{selected_year}: ")),
          # search = createLink_evo(topic_evo_year, r$booster)
          search = createLink_evo(TopTerms, r$booster)
        ) %>% 
        reactable::reactable(
          rownames = FALSE,
          compact = TRUE,
          searchable = TRUE,
          sortable = FALSE,
          resizable = TRUE,
          fullWidth = FALSE,
          defaultPageSize = 5,
          selection = "multiple",
          defaultSelected = 1:3,
          onClick = "select",
          style = list(
            width = "100%"
          ),
          theme = reactable::reactableTheme(
            rowSelectedStyle = list(backgroundColor = "#c6cf78ff", boxShadow = "inset 2px 0 0 0 #ffa62d")
          ),
          columns = list(
            search = reactable::colDef(
              name = "Publications",
              html = TRUE
            ),
            topic_evo_year = reactable::colDef(
              name = glue::glue("Evolution Terms {input$slider[2]}"),
              show = FALSE
            ),
            Empirical = reactable::colDef(
              name = "Empirical %"
            ),
            .selection = reactable::colDef(
              show = TRUE,
              headerClass = "hide-checkbox"
            ),
            ID = reactable::colDef(
              show = FALSE)
            ,
            TopTerms = reactable::colDef(
              show = TRUE
            )
          )
          
        )
    })  ## cold_table
    
    ## get selections from table
    selected_hot <- reactive(reactable::getReactableState("hot_table", "selected"))
    selected_cold <- reactive(reactable::getReactableState("cold_table", "selected"))
    
    id_selected_hot = reactive({
      trends()[[1]][selected_hot(), ] %>%
        dplyr::select(NR) %>%  
        dplyr::pull()
    })
    
    id_selected_cold = reactive({
      trends()[[2]][selected_cold(), ] %>%
        dplyr::select(NR) %>%  
        dplyr::pull()
    })
    
    observeEvent(input$clear_plot_hot, {
      reactable::updateReactable("hot_table", selected = NA)
    })
    
    observeEvent(input$clear_plot_cold, {
      reactable::updateReactable("cold_table", selected = NA)
    })
    
    
  })  ## module_server  
}

