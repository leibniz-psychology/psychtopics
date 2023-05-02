#' browse-topics UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_browse_topics_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    div(
      class = "three-cards",

      makeCard(
        title = "Browse PSYNDEX Topics",
        style = "background-color: #c6cf78ff",
        size = 12,
        content = tagList(
          
          
          bodyText("Here you can browse all topics included in the model."),
          br(),
          bodyText("Select topics in the ", tags$b("table below"), " to add them to the plots."),
          br(),
          uiOutput(ns("cur_year_text"))
        )
      ),
      
      div(),
      
      makeCard(
        title = title_with_help(
          id = ns("help2"),
          title = "Topic Trends",
          content = tagList(
            shiny.fluent::Text(
              "A topic's ", tags$b("Essential Publications"), " is determined by counting all publications that mainly address
              the topic (i.e., at least 50% of a publications' content is related to the topic)."
            )
          )
        ),
        size = 12,
        content = tagList(
          echarts4r::echarts4rOutput(ns("plot_box2")),
          
          shiny.fluent::Stack(
            horizontal = TRUE,
            div(class = "ms-Grid-col ms-sm4 ms-xl4"),
            div(
              class = "ms-Grid-col ms-sm4 ms-xl4",
              shiny.fluent::DefaultButton.shinyInput(inputId = ns("clear_plot"), text = "Clear Plot")
            )
          )

        )
      ),
      
      div(),
      
      makeCard(
        title = title_with_help(
          id = ns("help3"),
          title = "Share of Empirical Research",
          content = tagList(
            shiny.fluent::Text(
              "The share of empirical research is the relative frequency of topic-related publications
              with an empirical study methodology.",
              br(),
              br(),
              "Some topics may address theoretical issues or conceptual work.
              Some topics might be characterized by a large share of empirical research.
              And some topics might shift from mostly theoretical publications in its early years
              to an increasing investigation of empirical evidence."
            )
          )
        ),
        size = 12,
        content = tagList(
          
          echarts4r::echarts4rOutput(ns("plot_box3")),
          shiny.fluent::Stack(
            horizontal = TRUE,
            div(class = "ms-Grid-col ms-sm4 ms-xl4"),
            div(
              class = "ms-Grid-col ms-sm4 ms-xl4",
              shiny.fluent::DefaultButton.shinyInput(inputId = ns("clear_plot2"), text = "Clear Plot")
            )
          )
        )
          
         
      )
    ),
    
    div(
      class = "one-card",
      style = "margin-bottom: 0",
      makeCard(
        title = title_with_help(
          id = ns("help2"),
          title = "Topic Details",
          content = tagList(
            shiny.fluent::Text(
              "The topics are sorted in decreasing order according to the number of associated papers.",
              br(),
              br(),
              "Basically, a topic is a group of words that are frequently used together in publications ",
              tags$b("(= Top Terms)"), ". These terms are found automatically by the algorithm.
              For better interpretation, the PsychTopics team assigned topic ", tags$b("Labels."),
              br(),
              br(),
              "The ", tags$b("Essential Publications"), " across all years is determined by counting all publications
              that mainly address the topic (i.e., at least 50% of a publications’ content is related to the topic).",
              br(),
              br(),
              "The share of ", tags$b("Empirical Research"), " is the relative frequency of these publications with a empirical study methodology.",
              br(),
              br(),
              "The ", tags$b("Journals"), " column shows the three most frequent journals that publish articles related to the topic.",
              br(),
              br(),
              "With ", tags$b("Search PSYNDEX"), ", you can explore topic-related articles in PubPsych.eu.
              The search query is generated from the top terms.",
              br(),
              br(),
              "If you enter a search query, the ", tags$b("Evolution Terms"), " show the last year in which the entered term occurs.",
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
    
#use_gotop(
#  src = "fas fa-chevron-up",
#  width = 45,
#  opacity = 0.7,
#  place = "right",
#  color = "",
#  appear = 100,
#  scrolltime = 800,
#  fadein = 500,
#  fadeout = 500,
#  marginX = 5,
#  marginY = 2,
#  container = "",
#  zIndex = 9
#)
    
  )
}
    
#' browse-topics Server Functions
#'
#' @noRd 
mod_browse_topics_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    opened <- reactiveVal(FALSE)
    observe({
      # Set `opened` reactive to indicate whether this page has been opened
      # It runs only once, after page has been opened for the first time
      if (!opened()) {
        opened(shiny.router::get_page() == "browse-topics")
      }
    })
    

    
    output$cur_year_text = renderUI({
      req(r$current_year, opened())
      bodyText(glue::glue("For trends, only records from 1980 to {r$current_year - 1} are included,
               since publications of the current year may not be recorded yet 
               (journals, books, and reports on specific topics are published in waves throughout the year). 
               The records are always updated after the first quarter of the following year, i.e. in April {r$current_year + 1}.")
      )
    })
    
    ## send all the topic_evo terms to javascript
    observeEvent(reactable::getReactableState("topics_table", "pages"), {
      req(r$topic_evo_concatenated)
      

      golem::invoke_js("setTopicEvoTerms", list(terms = r$topic_evo_concatenated))
      golem::invoke_js("initiateWordEmbeddings", list(id = "id"))
    }, once = TRUE)
    
    ## the data for the table
    topic = reactive({
      
      # make_tooltip = function(topic_evo_string) {
      #   
      #   text_to_display = stringr::str_split(topic_evo_string, " \n ")[[1]][1]
      #   
      #   shiny.fluent::TooltipHost(
      #     content = topic_evo_string,
      #     delay = 0,
      #     HTML(text_to_display)
      #   ) %>% as.character()
      # }
      
      req(r$topic_evo_concatenated)
      
      # make_topic_evo_string = function(each) {
      #   #years = attributes(each)$dimnames[[2]]
      #   
      #   each = as.data.frame(each)
      #   
      #   years = names(each)
      #   
      #   get_all_strings = function(year) {
      #     strings = glue::glue_collapse(each[[year]], sep = ", ")
      #     glue::glue("{year}: {strings}")
      #   }
      #   
      #   all_strings = sapply(years, get_all_strings)
      #   glue::glue_collapse(all_strings, sep = "\n")
      #   
      # }
      
      # topics = sapply(r$topic_evo, make_topic_evo_string)
      
      r$topic %>% 
        dplyr::mutate(
          search = createLink(TopTerms, r$booster, ID),
          topic_evo = r$topic_evo_concatenated
        ) %>% 
        dplyr::arrange(-n_docs)
    })
    
    observeEvent(input$clear_plot, {
      reactable::updateReactable("topics_table", selected = NA)
    })
    
    observeEvent(input$clear_plot2, {
      reactable::updateReactable("topics_table", selected = NA)
    })
    
    
    output$plot_box2 = echarts4r::renderEcharts4r({
      req(r$n_doc_year, r$topic, id_selected(), r$start_year, r$current_year, r$topic_evo_concatenated, opened())
      
      label1 <- list(
        formatter = htmlwidgets::JS(
          'function(value, index){
            return value;
          }'
        )
      )
      
      topics = r$topic %>% 
        dplyr::mutate(
          topic_evo_year = r$topic_evo_concatenated
        )
      
      r$n_doc_year %>%
        dplyr::left_join(topics, by = c("id" = "ID")) %>%
        dplyr::filter(id %in% id_selected()) %>% 

        #tibble::glimpse(.) %>% 
        dplyr::mutate(
          # tooltip = glue::glue("{TopTerms};{id};{Label}"),
          tooltip = glue::glue("{topic_evo_year};{id};{Label};{as.numeric(colnames(r$topic_evo[[1]])[1])}"),
          year = as.character(year),
          Label = factor(Label)
        ) %>% 
        dplyr::group_by(Label) %>% 
        dplyr::filter(year %in% (r$start_year):(r$current_year-1)) %>% # leave out current year (last row)
        
        echarts4r::e_charts(year, reorder = FALSE) %>% 
        echarts4r::e_line(Freq, bind = tooltip) %>% 
        echarts4r::e_x_axis(name = "Year", nameLocation = "center", nameGap = 27, axisPointer = list(snap = TRUE)) %>% 
        echarts4r::e_y_axis(name = "essential publications", nameLocation = "center", nameGap = 31) %>% 
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
      
    })  ## plot_box2
    
    
    output$plot_box3 = echarts4r::renderEcharts4r({
      req(r$empirical, r$topic, id_selected(), r$topic_evo_concatenated, r$start_year, r$current_year, opened())
      
      topics = r$topic %>% 
        dplyr::mutate(
          topic_evo_year = r$topic_evo_concatenated
        )

      
      r$empirical %>%
        dplyr::left_join(topics, by = c("id" = "ID")) %>%
        dplyr::filter(id %in% id_selected()) %>% 
        
        #tibble::glimpse(.) %>% 
        dplyr::group_by(Label) %>% 
        dplyr::mutate(
          tooltip = glue::glue("{topic_evo_year};{id};{Label};{as.numeric(colnames(r$topic_evo[[1]])[1])}"),
          year = as.character(year)
        ) %>% 
        
        dplyr::filter(year %in% (r$start_year):(r$current_year-1)) %>% # leave out current year (last row)
        
        echarts4r::e_charts(year) %>% 
        echarts4r::e_line(Freq, bind = tooltip) %>% 
        echarts4r::e_x_axis(name = "Year", nameLocation = "center", nameGap = 27, axisPointer = list(snap = TRUE)) %>% 
        echarts4r::e_y_axis(name = "%", nameLocation = "center", nameGap = 27, nameRotate = 0) %>% 
        echarts4r::e_tooltip(
          confine = TRUE,
          axisPointer = list(type = "cross"),
          
          formatter = htmlwidgets::JS("
            function(params){
              var vals = params.name.split(';');
              year = params.value[0];
              min_year = vals[3];
              top_terms = year <= min_year ? vals[0].match(min_year + '.*')[0].replace(min_year, '') : vals[0].match(year + '.*')[0].replace(year, '');
              return('ID: ' + vals[1] + 
                      '<br/> Label: ' + vals[2] + 
                      '<br/> % Empirical: ' + params.value[1]) +
                      '<br/> Year: ' + year + 
                      '<br/> Evolution Terms' + top_terms
                      }
          ")
        )
    })  ## plot_box3
    
    
    output$topics_table = reactable::renderReactable({
      
      req(opened())
      
      #htmltools::browsable(
      #  tagList(
      #    tags$button("Download as CSV", onclick = "Reactable.downloadDataCSV('topics-table')"),
      
      topic() %>% 
        reactable::reactable(
          elementId = "topics-table",
          rownames = FALSE,
          compact = TRUE,
          searchable = TRUE,
          sortable = TRUE,
          showSortable = TRUE,
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
            # id = reactable::colDef(
            #   name = "ID"
            # ),
            search = reactable::colDef(
              name = "Publications",
              html = TRUE
            ),
            TopTerms = reactable::colDef(
              name = "Top Terms"
            ),
            n_docs = reactable::colDef(
              name = "Essential Publications"
            ),
            Empirical = reactable::colDef(
              name = "Empirical %"#,
              #format = reactable::colFormat(digits = 2)
            ),
            root = reactable::colDef(
              name = "root topic",
              show = FALSE
            ),
            topic_evo = reactable::colDef(
              name = "Evolution Terms",
              show = TRUE,
              html = TRUE,
              
              cell = htmlwidgets::JS('
                function(cellInfo, state) {
                  // input:
                  //  - cellInfo, an object containing cell info
                  //  - state, an object containing the table state (optional, new in v0.2.3.9000)
                  //
                  // output:
                  //  - content to render (e.g. an HTML string)
                  
                  all_years = cellInfo.value.split("\\n")
                  n = all_years.length - 1
                  
                  if (!state.searchValue) {
                    show = all_years[n]
                  } else {
                    x = all_years.filter(s => s.includes(state.searchValue))
                    if (x.length == 0) {
                      show = "No Match"
                    } else if (x.length == 1) {
                      show = x
                    } else if (x.length > 1) {
                      show = x[(x.length - 1)]
                    }
                  }
                

                  // console.log(show)
                  
                
                  // return "<div id = " + cellInfo.index + ">" + show + "</div>"
                  return show
                  
                }
              '),
              
              
              # cell = reactable::JS(
              #   'function(value) {
              #     console.log(value);
              #     if (value) {
              #       valueArray = value.split(" \n ");
              #       console.log(valueArray);
              #       searched = $("#browse-topics_table .rt-search").val();
              #       console.log(searched);
              #       x = valueArray.findIndex(s => s.includes(searched));
              #       return x;
              #     } else {
              #       return "this is a cell";
              #     }
              #   
              # 
              #   }'
              # )
              #style = "overflow: hidden"
            ),
            # freq = reactable::colDef(
            #   name = "Prevalence"
            # ),
            .selection = reactable::colDef(
              show = TRUE,
              headerClass = "hide-checkbox"
            )
          )
          #)
        )
    })
    
    selected <- reactive(reactable::getReactableState("topics_table", "selected"))
    
    id_selected = reactive({
      topic()[selected(), ] %>%
        dplyr::select(ID) %>%  
        dplyr::pull()
    })
    
    
  })
}
    
## To be copied in the UI
# mod_browse-topics_ui("browse-topics_ui_1")
    
## To be copied in the server
# mod_browse-topics_server("browse-topics_ui_1")
