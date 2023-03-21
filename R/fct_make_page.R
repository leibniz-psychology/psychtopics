#' make_page 
#'
#' @description A fct function
#' 
#' @import shiny.fluent shiny.router
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

library(shiny.fluent)
library(glue)
library(shiny.router)

makePage <- function (title, subtitle, contents) {
  tagList(div(
    class = "page-title",
    span(title, class = "ms-fontSize-32 ms-fontWeight-semibold", style = "color: #323130"),
    span(subtitle, class = "ms-fontSize-14 ms-fontWeight-regular", style = "color: #605E5C; margin: 14px;")
  ),
  contents)
}

makeCard <- function(title, content, size = 12, style = "") {
  div(
    class = glue("card ms-depth-8 ms-sm{size} ms-xl{size}"),
    style = style,
    Text(variant = "xxLarge", title, block = TRUE, styles = list(root = list(color = '#953386'))),
    br(),
    content
  )
}

bodyText = function(text, ...) {
  Text(text, styles = list(root = list(fontSize = 17)), ...)
}


title_with_help = function(id, title, content) {
  tagList(
    div(
      style = "float:right",
      shiny.fluent::TooltipHost(
        content = div(
          style = "margin: 15px",
          content
        ),
        delay = 0,
        shiny.fluent::IconButton.shinyInput(inputId = id, iconProps = list(iconName = "Info", className = "icon-help-grey"), class = "button-help-grey")
      )
    ),
    title
  )
}


header <- tagList(
  
  CommandBar(
    items = tagList(

      CommandBarItem(text = tags$img(src = "www/logo.png", class = "logo"), href = "https://www.leibniz-psychology.org/", target = "_blank"),
    #),
    
      CommandBarItem("Start", href = '#!/', target = "_self", key = 'home'),
      
      CommandBarItem("Browse Topics", href = '#!/browse-topics', target = "_self", key = 'browse'),
      
      CommandBarItem("Popular by Year", href = '#!/popular', target = "_self", key = 'popular'),
      
      CommandBarItem("Hot/Cold", href = '#!/hot-cold', target = "_self", key = 'hot-cold'),
      
      CommandBarItem("Topic Evolution", href = '#!/topic-evolution', target = "_self", key = 'topic-evolution'),
      
      CommandBarItem("Methods", href = '#!/methods', target = "_self", key = 'methods')
    ),
      
    farItems = list(
      CommandBarItem("Contact", href = "https://psyndex.de/en/trends/psychtopics/", target = "_blank")
    )
  )
)

title2 = tagList(
  div("PsychTopics", class = "ms-fontSize-32 ms-fontWeight-semibold", style = "color: #fff; font-size: 3rem; padding-left: 33px"),
)



#navigation <- Nav(
  
#  IconButton.shinyInput("menu"),
  
#  groups = list(
#    list(links = list(
#      list(name = 'Start', url = '#!/', key = 'home'),
#      list(name = 'Browse Topics', url = '#!/browse-topics', key = 'browse'),
#      list(name = 'Popular by Year', url = '#!/popular', key = 'popular'),
#      list(name = 'Hot/Cold', url = '#!/hot-cold', key = 'hot-cold'),
#      list(name = 'Topic Evolution', url = '#!/topic-evolution', key = 'topic-evolution'),
#      list(name = 'Methods', url = '#!/methods', key = 'methods')
#    ))
#  ),
#  initialSelectedKey = 'home',
#  styles = list(
#    linkText = list(
#      fontWeight = 600,
#      fontSize = 15,
#      color = "white",
#      ":hover" = list(
#        color = "black"
#      )
#    ),
#    root = list(
#      height = '100%',
#      width = "100%",
#     #position = "sticky",
#      boxSizing = 'border-box',
#      overflowY = 'auto',
#      overflowX = "hidden"
      
      #transition: 'width 0.3s ease-in-out',
      #selectors: {
      #              ':hover':{
      #                  width: '100%'
      #                }
      #           }
      # ABC
      #transition = 'width 0.3s ease-in-out',
      #selectors = list(
      #  ":hover" = list(
      #    width = "100%"
      # )
      #)
#    )
#  )
#)

#menu = IconButton.shinyInput(
#  "menu",
#  iconProps = list(
#    iconName = "CollapseMenu",
#    styles = list(
#      root = list(
#        color = "#241b3e",
#        #width = "0px",
#        fontWeight = 600
#      )
#    )
#  )
#)


# footer <- Stack(
#   horizontal = TRUE,
#   horizontalAlign = 'space-between',
#   tokens = list(childrenGap = 20),
#   Text(variant = "medium", "Built with ❤ by Appsilon", block=TRUE),
#   Text(variant = "medium", nowrap = FALSE, "If you'd like to learn more, reach out to us at hello@appsilon.com"),
#   Text(variant = "medium", nowrap = FALSE, "All rights reserved.")
# )



layout <- function(mainUI){
  div(class = "grid-container",
      div(class = "header", header),
      div(class = "title2", title2),
      #div(class = "sidenav", navigation),
      div(class = "main", mainUI),
      #div(class = "menu", menu)
      #div(class = "footer", footer)
  )
}

card1 <- makeCard(
  "Welcome to shiny.fluent demo!",
  div(
    Text("shiny.fluent is a package that allows you to build Shiny apps using Microsoft's Fluent UI."),
    Text("Use the menu on the left to explore live demos of all available components.")
  ))

card2 <- makeCard(
  "shiny.react makes it easy to use React libraries in Shiny apps.",
  div(
    Text("To make a React library convenient to use from Shiny, we need to write an R package that wraps it - for example, a shiny.fluent package for Microsoft's Fluent UI, or shiny.blueprint for Palantir's Blueprint.js."),
    Text("Communication and other issues in integrating Shiny and React are solved and standardized in shiny.react package."),
    Text("shiny.react strives to do as much as possible automatically, but there's no free lunch here, so in all cases except trivial ones you'll need to do some amount of manual work. The more work you put into a wrapper package, the less work your users will have to do while using it.")
  ))

home_page <- makePage(
  "This is a Fluent UI app built in Shiny",
  "shiny.react + Fluent UI = shiny.fluent",
  div(card1, card2)
)


