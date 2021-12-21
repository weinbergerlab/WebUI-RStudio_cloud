library(magrittr)
library(plotly)
library(shinyjs)
library(future)
library(parallel)
library(promises)
library(jsonlite, exclude=c("validate"))
library(shiny)
library(InterventionEvaluatR)
library(uuid)
library(ggplot2)
library(shinyBS)
library(shinyWidgets)
library(lubridate)
library(plyr)
library(dplyr)
library(htmltools)
library(brew)
library(tools)
library(rlang)
library(stringr)

source("common.R")
source("mdbootstrap.R")
source("analysis.R")
source("setup.R")
source("results.R")
source("worker.R")

plan(sequential)

ui <- md_page(
  id="page",
  useShinyjs(),
  singleton(tags$head(
    tags$script(src = "js/app.js")
  )),
  div(
    class="fixed-top",
    id="header",
    md_navbar(
      title="InterventionEvaluatR",
      tags$ul(
        class="navbar-nav nav mx-auto justify-content-center",
        role="tablist",
        id="nav-main",
        tags$li(
          tags$a(
            class="nav-link active", 
            id="nav-setup-tab",
            "data-toggle"="tab",
            href="#nav-setup",
            role="tab",
            "aria-controls"="nav-setup",
            "aria-selected"="true",
            "Setup"
          )
        ),
        tags$li(
          tags$a(
            class="nav-link", 
            id="nav-results-tab",
            "data-toggle"="tab",
            href="#nav-results",
            role="tab",
            "aria-controls"="nav-results",
            "aria-selected"="true",
            "Results"
          )
        )
      ),
      tags$ul(
        class="navbar-nav justify-content-end",
        tags$li(
          class="nav-item", id="help-button",
          tags$a(class="nav-link", icon("question-circle"))
        )
      )
    ) %>% tagAppendAttributes(class="navbar-expand-sm")
  ),
  div(
    class="main-content tab-content",
    id="nav-main-content",
    setup.ui(),
    results.ui()
  ),
  div(
    id="help-toggle", 
    div(
      id="help-container",
      div(
        id="help",
        class="card",
        setup.help(),
        results.help()
      )
    )
  )
)

server = function(input, output, session) {
  setup = setup.server(input, output, session)

  analysisState = results.server(input, output, session, setup)

  ############################################################
  # Shiny idle timeout
  ############################################################
  # We need to shut down the server automatically when the 
  # User is idle, to free up resources for other users
  # Front end reports user activity once per minute in 
  # input$lastUserActivity. We check for user activity once
  # a minute and if we detect none for 5 minutes we stop the
  # server

  previousAnalysisState = reactiveVal("")
  autoStop = reactiveVal(as.numeric(Sys.time()))
  
  observe({
    validate(need(input$lastUserActivity, FALSE))
    autoStop(as.numeric(Sys.time()))
  })

  observe({
    # Invalidate every minute
    invalidateLater(60000, session)

    # Analysis state transition counts as activity (to prevent immediate stop when analysis completes)
    if (previousAnalysisState() != analysisState()) {
      autoStop(as.numeric(Sys.time()))
      previousAnalysisState(analysisState())
    }

    # Check time since last activity
    idleTimeout = 5 * 60
    if (as.numeric(Sys.time()) > autoStop() + idleTimeout) {
      # Don't shut down if we're running analysis
      if (analysisState() != ANALYSIS_RUNNING && !getOption("ie.worker.local", default=TRUE)) {
        system("sh ./stop-server.sh")
      }
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
