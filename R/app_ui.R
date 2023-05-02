#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny shiny.fluent
#' @noRd
install.packages("gotop")
library(gotop)

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    shiny.fluent::fluentPage(
      layout(router$ui),
    )
  )
}


#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny shiny.fluent
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  add_resource_path("shiny.router", system.file("www", package = "shiny.router"))
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'psychtopics'
    ),
    
    ## set viewport for mobile scaling
    tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
    
    ## js code for shiny.router
    tags$script(type = "text/javascript", src = file.path("shiny.router", "shiny.router.js")),
    
    #tags$script(type = "text/javascript", src = "https://code.jquery.com/ui/1.13.0/jquery-ui.js")
    #tags$script(type = "text/javascript", src = "https://unpkg.com/compromise"),
    
    ## add matomo tracking
    HTML(
      "<script>
      var _paq = window._paq = window._paq || [];
      /* tracker methods like 'setCustomDimension' should be called before 'trackPageView' */
      _paq.push(['disableCookies']);
      _paq.push(['trackPageView']);
      _paq.push(['enableLinkTracking']);
      (function() {
        var u='https://mtm.leibniz-psychology.org/';
        _paq.push(['setTrackerUrl', u+'matomo.php']);
        _paq.push(['setSiteId', '26']);
        var d=document, g=d.createElement('script'), s=d.getElementsByTagName('script')[0];
        g.async=true; g.src=u+'matomo.js'; s.parentNode.insertBefore(g,s);
      })();
    </script>"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

use_gotop()
