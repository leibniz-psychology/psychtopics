#' topic_evol UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_topic_evol_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(
      class = "two-cards",
      
      makeCard(
        title = "Evolution of PSYNDEX Topics",
        size = 12,
        style = "background-color: #c6cf78ff",
        content = tagList(
          bodyText("Here, you can explore how topic contents change over time."),
          br(),
          br(),
          
          shiny.fluent::Stack(
            horizontal = TRUE,
            div(
              class = "ms-Grid-col ms-sm2 ms-xl2 mod-evol-search1"
            ),
            div(
              class = "ms-Grid-col ms-sm5 ms-xl5 mod-evol-search2",
              shiny.fluent::Label("Select a Topic"),
              # shiny.fluent::NormalPeoplePicker.shinyInput(
              #   inputId = ns("search"),
              #   options = 1:10,
              #   itemLimit = 1
              # ),
              
              uiOutput(ns("tagPicker"))
              
            )
          ),
          
          br(),
          br(),
          uiOutput(ns("cur_year_text"))
        )
      ),
      
      div(),
      
      makeCard(
        title = title_with_help(
          id = ns("help2"),
          title = uiOutput(ns("title_box2")),
          content = tagList(
            shiny.fluent::Text(
              "A topic's number of ", tags$b("Essential Publications"),
              " is determined by counting all publications that mainly address the topic 
              (i.e., at least 50% of a publications’ content is related to the topic)."
            )
          )
        ),
        size = 12,
        content = tagList(
          echarts4r::echarts4rOutput(ns("plot"))
        )
      )
    ),
    
    div(
      class = "one-card",
      makeCard(
        title = title_with_help(
          id = ns("help3"),
          title = uiOutput(ns("title_box3")),
          content = tagList(
            shiny.fluent::Text(
              "Basically, a topic is a group of words that are frequently used together in publications.
              These word groups are found automatically by the algorithm.",
              br(), br(),
              "This table shows the ", tags$b("ten most characterizing words of the topic"),
              " (sorted from top to bottom), ", tags$b("and how they change over years."),
              br(), br(),
              "These ", tags$b("Evolution Terms"), " are weighted, if they occur often in the present topic, but less often in average in all other topics.",
              br(), br(),
              "The underlying topic identification method adds new documents every year, letting the topics evolve over time."
            )
          )
        ),
        size = 12,
        content = tagList(
          
          br(),
          shiny.fluent::Stack(
            horizontal = TRUE,
            div(class = "ms-Grid-col ms-sm4 ms-xl4 mod-evol-slider-col1"),
            div(
              class = "ms-Grid-col ms-sm3 ms-xl3 mod-evol-slider-col2",
              
              ## may need to be changed when https://github.com/Appsilon/shiny.fluent/issues/63 is solved
              # shiny.fluent::Slider(
              #   onChange = shiny.fluent::setInput(ns("slider"), 2),
              #   ranged = TRUE,
              #   label = "Select the range of years",
              #   min = 1999,
              #   max = 2019,
              #   defaultValue = 2019,
              #   defaultLowerValue = 2015,
              #   snapToStep = TRUE
              # )
              uiOutput(ns("slider_input"))
            ),
            div(
              class = "ms-Grid-col ms-sm1 ms-xl1",
              br(),
              # shiny.fluent::IconButton.shinyInput(
              #   inputId = ns("go"),
              #   iconProps = list(iconName = "Forward"),
              #   className = "buttons-tab2",
              #   disabled = TRUE
              # )
            )
          ),
          
          br(),
          reactable::reactableOutput(ns("table"))
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
    
#' topic_eval Server Functions
#'
#' @noRd 
mod_topic_evol_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    opened <- reactiveVal(FALSE)
    observe({
      # Set `opened` reactive to indicate whether this page has been opened
      # It runs only once, after page has been opened for the first time
      if (!opened()) {
        opened(shiny.router::get_page() == "topic-evolution")
      }
    })
    
    # reactive values for slider input
    r_mod_topic_eval = reactiveValues(
       lower = NULL,
       upper = NULL
    )

    output$slider_input = renderUI({
      
      req(r$current_year, r$start_evo, r$topic_evo_firsts, r$topic_evo_lasts, input$search, opened())
      
      searched <- input$search[1] %>% as.numeric()
      
      # use last five years or less
      default_lower <- ifelse((r$topic_evo_lasts[searched] - r$topic_evo_firsts[searched]) >= 5,
                              r$topic_evo_lasts[searched] - 5,
                              r$topic_evo_firsts[searched])
      
      
      shiny.fluent::Slider(
        onChange = shiny.fluent::setInput(ns("slider"), 2),
        ranged = TRUE,
        label = "Select the range of years",
        # min = r$start_evo,
        # max = r$current_year,
        # defaultValue = r$current_year,
        # defaultLowerValue = (r$current_year - 5),
        min = r$topic_evo_firsts[searched],
        max = r$topic_evo_lasts[searched],
        defaultValue = r$topic_evo_lasts[searched],
        defaultLowerValue = default_lower,
        snapToStep = TRUE
      )
    })
    
    output$tagPicker = renderUI({
      req(r$topic, r$topic_evo_firsts, r$topic_evo_lasts, opened())
      
      ## update the topicIds in javascript
      golem::invoke_js("updateTopicIds", list = list(values = r$topic$Label))                                          
      
      ## set the slider for the first run, this actually imitates an initial click by the user through javascript
      golem::invoke_js("setSlider", list = list(id = ns("slider"), vals = c((r$current_year - 5), r$current_year)))
      
      
      ## the search input for the topic ids, lots of javascript involved!
      TagPicker(
        defaultSelectedItems = JS("topicIds.slice(0, 1)"),
        onResolveSuggestions = JS("filterSuggestedTags"),
        onEmptyInputFocus = JS("function(tagList) { return topicIds.filter(tag => !listContainsTagList(tag, tagList)); }"),
        getTextFromItem = JS("function(item) { return item.text }"),
        pickerSuggestionsProps = list(suggestionsHeaderText = 'Suggested topic(s)', noResultsFoundText = 'No topic found'),
        itemLimit = 1,
        onChange = JS("function(selection) { Shiny.setInputValue('topic_evol-search', selection) }")
      )
    })
    
    output$cur_year_text = renderUI({
      req(r$current_year, opened())
      bodyText(glue::glue("For trends, only records from 1980 to {r$current_year - 1} are included,
               since publications of the current year may not be fully covered yet. The records are always updated after the first quarter of the following year, i.e. in March {r$current_year + 1}."))
    })
    
   # GO BUTTON DISABLED 
    # observeEvent(input$slider, {
    #   req(opened())
    # 
    #   
    #   if (!is.null(r_mod_topic_eval$lower)) {
    #     # print("slider is null")
    #     
    #     if (r_mod_topic_eval$lower != input$slider[1] | r_mod_topic_eval$upper != input$slider[2]) {
    #       shiny.fluent::updateIconButton.shinyInput(inputId = "go", disabled = FALSE)
    #     } else {
    #       shiny.fluent::updateIconButton.shinyInput(inputId = "go", disabled = TRUE)
    #       
    #     }
    #     
    #     
    #   } else {
    #     shiny.fluent::updateIconButton.shinyInput(inputId = "go", disabled = FALSE)
    #     golem::invoke_js("clickGo", list = list(button = ns("go")))
    #   }
    # })
    
    
    output$title_box2 = renderUI({
      req(opened())
      
      if (is.null(input$search)) {
        HTML("Trend Plot")
      } else {
        searched = input$search[1] %>% as.numeric()
        HTML("Trend of Topic: ", r$topic$Label[r$topic$ID == searched])
      }
      
    })
    
    
    output$title_box3 = renderUI({
      
      if (is.null(input$search)) {
        HTML("Change of Terms Table")
      } else {
        searched = input$search[1] %>% as.numeric()
        HTML("Evolution Terms for Topic: ", r$topic$Label[r$topic$ID == searched])
      }
      
    })
    
    # GO BUTTON DISABLED 
    # observeEvent(input$go, {
    #   req(input$slider, opened())
    #   
    #   shiny.fluent::updateIconButton.shinyInput(inputId = "go", disabled = TRUE)
    #   if (input$slider[1] == input$slider[2]) {
    #     r_mod_topic_eval$lower = NULL
    #     r_mod_topic_eval$upper = NULL
    #   } else {
    #     r_mod_topic_eval$lower = input$slider[1] %>% as.character()
    #     r_mod_topic_eval$upper = input$slider[2] %>% as.character()
    #   }
    #   
    # })
    
    # Update table immediately
    observeEvent(input$slider, {
      req(opened())
      
      # don't show table if one year is selected:
      # if (input$slider[1] == input$slider[2]) {
      #   r_mod_topic_eval$lower = NULL
      #   r_mod_topic_eval$upper = NULL
      # } else {
        r_mod_topic_eval$lower = input$slider[1] %>% as.character()
        r_mod_topic_eval$upper = input$slider[2] %>% as.character()
      # }

    })
    
    
    # observeEvent(input$search, {
    #   print(input$search)
    # 
    #   r$topic %>%
    #     dplyr::filter(ID == input$search) %>%
    #     print()
    # 
    #   r$topic_evo[[input$search]] %>% print()
    # 
    # })
    

    output$table = reactable::renderReactable({
      # req(r$topic_evo_search, input$search, r_mod_topic_eval$lower, opened())
      req(r$topic_evo_search, input$search, r_mod_topic_eval$lower, r_mod_topic_eval$upper, opened())
      
      searched = input$search[1] %>% as.numeric()
      
      col_names <- names(as.data.frame(r$topic_evo_search[[searched]]))
      lower <- r_mod_topic_eval$lower
      upper <- r_mod_topic_eval$upper
      
      if(!(lower %in% col_names)){
        lower <- col_names[1]
      }
      if(!(upper %in% col_names)){
        upper <- col_names[length(col_names)]
      }
      
      r$topic_evo_search[[searched]] %>% 
        as.data.frame() %>% 
        # dplyr::select(r_mod_topic_eval$lower:r_mod_topic_eval$upper) %>%
        dplyr::select(lower:upper) %>%
        reactable::reactable(
          defaultColDef = reactable::colDef(html = TRUE),
          rownames = FALSE,
          compact = TRUE,
          striped = TRUE,
          searchable = FALSE,
          sortable = FALSE,
          resizable = TRUE,
          fullWidth = TRUE,
          defaultPageSize = 11,
          # selection = "multiple",
          # defaultSelected = 1:3,
          # onClick = "select",
          # style = list(
          #   width = "100%"
          # ),
          theme = reactable::reactableTheme(
            rowSelectedStyle = list(backgroundColor = "#c6cf78ff", boxShadow = "inset 2px 0 0 0 #ffa62d")
          )
          # columns = list(
          #    search = reactable::colDef(
          #      name = "2021",
          #      html = TRUE
          #    ),
          #   .selection = reactable::colDef(
          #     show = TRUE,
          #     headerClass = "hide-checkbox"
          #   ),
          #   TopTerms = reactable::colDef(
          #     show = FALSE
          #   )
          #)
          
        )
      
    })
    
    output$plot = echarts4r::renderEcharts4r({
      req(r$topic, input$search, r$start_year, r$current_year, opened())
      
      searched = input$search[1] %>% as.numeric()
      
      topics = r$topic %>% 
        dplyr::mutate(
          topic_evo_year = r$topic_evo_concatenated
        )
      
      r$n_doc_year %>%
        dplyr::filter(id == searched) %>% 
        dplyr::left_join(topics, by = c("id" = "ID")) %>%
        dplyr::group_by(Label) %>% 
        dplyr::mutate(
          tooltip = glue::glue("{topic_evo_year};{id};{Label};{as.numeric(colnames(r$topic_evo[[1]])[1])}"),
          year = as.character(year)
        ) %>% 
        dplyr::filter(year %in% (r$start_year):(r$current_year-1)) %>% # leave out current year (last row)
        echarts4r::e_charts(year, reorder = FALSE) %>% 
        echarts4r::e_line(Freq, bind = tooltip) %>% 
        echarts4r::e_x_axis(name = "Year", nameLocation = "center", nameGap = 27, axisPointer = list(snap = TRUE)) %>% 
        echarts4r::e_datazoom() %>%
        echarts4r::e_y_axis(name = "essential publications", nameLocation = "center", nameGap = 38) %>% 
        echarts4r::e_tooltip(
          confine = TRUE,
          appendToBody = TRUE,
          textStyle = list(width = 50, overflow = "truncate"),
          axisPointer = list(type = "cross"),
          formatter = htmlwidgets::JS("
            function(params){
              var vals = params.name.split(';');
              year = params.value[0];
              min_year = vals[3];
              top_terms = year <= min_year ? vals[0].match(min_year + '.*')[0].replace(min_year, '') : vals[0].match(year + '.*')[0].replace(year, '');
              // 'ID: ' + vals[1] +
              return('Label: ' + vals[2] + 
                      '<br/> Essential Publications: ' + params.value[1]) +
                      '<br/> Year: ' + year + 
                      '<br/> Evolution Terms' + top_terms
                      }
          ")
        )
    })
    
    
  })
}
    
## To be copied in the UI
# mod_topic_eval_ui("topic_eval_ui_1")
    
## To be copied in the server
# mod_topic_eval_server("topic_eval_ui_1")
