#' methods UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_methods_ui <- function(id){
  ns <- NS(id)
  tagList(
    

    
    div(
      class = "two-cols",

      div(
        class = "methods-card1",
        makeCard(
          title = "Topic Identification",
          size = 12,
          content = tagList(
            bodyText(
              # tags$b("PsychTopics employs "),
              bodyText("PsychTopics employs "),
              tags$a("topic modeling", href = "https://en.wikipedia.org/wiki/Topic_model", target = "_blank"),
              ", an unsupervised machine learning method.
              The results of topic modeling are words that are frequently used together in publications (see the TopTerms in the Browse Topics table). 
              These word clusters are referred to as 'topics'.
              The topic labels were assigned by the PsychTopics team based on inspecting the most representative publications and TopTerms of each topic. 
              In addition, topic contents were validated using the ",
              tags$a("APA classification system", href = "https://www.apa.org/pubs/databases/training/class-codes", target = "_blank"), "."
            ),
            br(),
            bodyText(tags$b("The specific topic identification procedure is:")),
            bodyText(
              tags$ol(
                tags$li("Build a text corpus using English language titles and abstracts of PSYNDEX records (or English translations)."),
                tags$li(
                  "Remove ",
                  shiny.fluent::TooltipHost(
                    content = tagList(
                      tags$div(
                        style = "margin: 9px",
                        shiny.fluent::Text(
                          "Besides standard stopwords of the quanteda R package, we used:",
                          br(),
                          tags$ul(
                            tags$li("Christ et al. (2019): ", tags$a("http://dx.doi.org/10.23668/psycharchives.2613", href = "http://dx.doi.org/10.23668/psycharchives.2613", target = "_blank")),
                            tags$li("Bittermann & Klos (2019): "), tags$a("http://dx.doi.org/10.23668/psycharchives.2499", href = "http://dx.doi.org/10.23668/psycharchives.2499", target = "_blank")
                          ),
                          br(),
                          "The German stopwords by Bittermann & Klos were translated using DeepL (", tags$a("https://www.deepl.com/translator", href = "https://www.deepl.com/translator", target = "_blank"), ")"
                          
                        )
                      )
                    ),
                    delay = 0,
                    tags$a("stopwords")
                  ),
                  ", punctuation, symbols, etc."
                ),
                tags$li(
                  uiOutput(ns("li3"))
                ),
                tags$li(
                  "Update the initiation model year by year (using ",
                  tags$a("RollingLDA", href = "https://github.com/JonasRieger/rollinglda", target = "_blank"),
                  " with last year as memory), allowing topics to evolve over time."
                )
              )
            ),
            br(),
            bodyText(tags$b("PsychTopics’ topic modeling settings are:")),
            bodyText(
              tags$ul(
                tags$li("Number of topics (K) = 200"),
                tags$li("alpha = 0.0001 (only few topics per publication)"),
                tags$li("eta = 1/K"),
                tags$li("Number of iterations = 500"),
                tags$li("Number of prototypes = 25"),
				        tags$li("Memory parameter: 100 % of last year's publications"),
                tags$li("Limit for new vocabulary: word appears at least 10 times")
              )
            ),
            
            #br(),
            #bodyText(tags$a(tags$b("A research paper with more details on the development of PsychTopics is in preparation.")))
            
            
          )  ## end tagList
          
          
        )
      ),
      
      div(
        class = "methods-gap"
      ),

      div(
        class = "methods-card2 one-card",
        makeCard(
          title = "Interpreting PsychTopics",
          size = 12,
          content = tagList(
            bodyText(
              "Please note:",
              tags$ul(
                tags$li("PsychTopics is ", tags$b("designed for exploring"),
                        " psychological research topics, not for defining the complete research landscape."),
                tags$li("PsychTopics is ", tags$b("best used along with specific search strategies"),
                        " (e.g., check the number of documents for your topic of interest using a respective search strategy
                        in your literature database of choice)."),
                tags$li("PsychTopics and the underlying methods are ", tags$b("research in progress"), "."),
                tags$li("If you feel that relevant topics are missing you can leave a quick note in the ",
                        tags$b(tags$a("feedback form", href = "https://forms.gle/bzsC6AJdTTBY3RDH8", target = "_blank")),
                        " and help us improve.")
              )
            )
          )
        )
      ),
      div(
        class = "methods-card3 one-card",
        makeCard(
          title = "Related Publications",
          size = 12,
          content = tagList(
            bodyText(
              "Bittermann, A. & Rieger, J. (2022).
              Finding Scientific Topics in Continuously Growing Text Corpora. Preprint available at ", tags$i("PsychArchives"), br(),
              tags$a("http://dx.doi.org/10.23668/psycharchives.8168", href = "http://dx.doi.org/10.23668/psycharchives.8168", target = "_blank"),
              br(),
              br(),
              "Rieger, J., Jentsch, C., & Rahnenführer, J. (2021).
              RollingLDA: An Update Algorithm of Latent Dirichlet Allocation to Construct Consistent Time Series from Textual Data. In ",
              tags$i("Findings of the Association for Computational Linguistics: EMNLP 2021"), " (2337-2347).", br(),
              tags$a("https://doi.org/10.18653/v1/2021.findings-emnlp.201", href = "https://doi.org/10.18653/v1/2021.findings-emnlp.201", target = "_blank"),
              br(),
              br(),
              "Rieger, J., Rahnenführer, J. und Jentsch, C. (2020).
              Improving Latent Dirichlet Allocation: On Reliability of the Novel Method LDAPrototype. In ",
              tags$i("Natural Language Processing and Information Systems, NLDB 2020. LNCS 12089"), " (118-125).", br(),
              tags$a("https://doi.org/10.1007/978-3-030-51310-8_11", href = "https://doi.org/10.1007/978-3-030-51310-8_11", target = "_blank"),
              br(),
              br(),
              "Bittermann, A. (2019). Development of a user-friendly app for exploring and analyzing research topics in psychology.
              In G. Catalano, C. Daraio, M. Gregori, H. F. Moed & G. Ruocco (Eds.), ",
              tags$i("Proceedings of the 17th Conference of the International Society for Scientometrics and Informetrics"), " (2634–2635).
              Rom: Edizioni Efesto. ", br(),
              tags$a("http://dx.doi.org/10.23668/psycharchives.2521", href = "http://dx.doi.org/10.23668/psycharchives.2521", target = "_blank"),
              br(),
              br(),
              "Bittermann, A. & Fischer, A. (2018).
              How to identify hot topics in psychology using topic modeling. ", tags$i("Zeitschrift für Psychologie, 226"), ", 3–13.", br(),
              tags$a("https://doi.org/10.1027/2151-2604/a000318", href = "https://doi.org/10.1027/2151-2604/a000318", target = "_blank")
            )
          )
        )
      )
    )
    
    
    # shiny.fluent::Stack(
    #   horizontal = TRUE,
    #   div(
    #     makeCard(
    #       title = "Topic Identification",
    #       size = 6,
    #       content = tagList(
    # 
    #       )
    #     )
    #   ),
    # 
    #   div(
    #     class = "ms-Grid-col ms-sm6 ms-xl6",
    #     shiny.fluent::Stack(
    #       div(
    #         makeCard(
    #           title = "Card 2",
    #           size = 12,
    #           content = tagList(
    # 
    #           )
    #         )
    #       ),
    #       div(
    #         makeCard(
    #           title = "Topic Card 3",
    #           size = 12,
    #           content = tagList(
    # 
    #           )
    #         )
    #       )
    #     )
    #   )
    # ) 
    
  ),
    use_gotop(
    	src = "fas fa-chevron-up",
    	width = 45,
    	opacity = 0.7,
    	place = "right",
    	color = "",
    	appear = 100,
    	scrolltime = 800,
    	fadein = 500,
    	fadeout = 500,
    	marginX = 5,
    	marginY = 2,
    	container = "",
    	zIndex = 9
  	)
}
    

#' methods Server Functions
#'
#' @noRd 
mod_methods_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$li3 = renderUI({
      req(r$current_year)

      tagList(
        "Run the initiation model (a ", 
        tags$a("ldaPrototype", href = "https://github.com/JonasRieger/ldaPrototype/", target = "_blank"),
        glue::glue(" for the years 1980-2009")
		# glue::glue(" for the years 1980-{r$current_year})")
      )
    })
 
  })
}
    
## To be copied in the UI
# mod_methods_ui("methods_ui_1")
    
## To be copied in the server
# mod_methods_server("methods_ui_1")
