# library(remotes)
# 
# if(require(INLA)==F){
#   remotes::install_github("hrue/r-inla@stable", subdir = "rinla", upgrade='never')  # 21 hours old
# }
# 
# if(require(InterventionEvaluatR)==F){
#   remotes::install_github("weinbergerlab/InterventionEvaluatR@InterventionEvaluatR-lite", upgrade='never')  # 21 hours old
# }
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

d2 <- read.csv('RSA.csv')
d2$date<-as.Date(d2$date,"%Y-%m-%d")
exclude_covar <- c("denom", "A20_B99_excl_bac", "A16", "A17", "A18", "A19", "R00_R09", "R10_R19", "R20_R39", "R40_R49", "R50_R69", "R70_R94", "R95_R99", "D50_D89")      

d2 <- d2[,-which(names(d2) %in% exclude_covar)]

d2 <-
  d2[, c(
    'date',
    'age',
    'Pneum',
    "A16_A19",
    "A20_A48",
    "A39",
    "A50_A79",
    "A80_B34",
    "B05_B06",
    "B20_B24",
    "B35_B49" ,
    "B45",
    "B50_B89",
    "B99" ,
    "C00_D49" ,
    "A20_B99_a_D50_D89" ,
    "E00_E89"      ,
    "E10_E14"  ,
    "E40_E46",
    "F01_F99"   ,
    "G05_G99"   ,
    "H00_H99_excl_cj_om",
    "I00_I99"  ,
    "I60_I64"   ,
    "K00_K95"   ,
    "K35"  ,
    "K80"   ,
    "L00_L99"       ,
    "M00_M99"   ,
    "N00_N99"  ,
    "N39"     ,
    "O00_O99"       ,
    "P00_P15" ,
    "P05_P07",
    "Q00_Q99"      ,
    "R00_R99"   ,
    "S00_T88"  ,
    "V01_Y99"
  )]
SAfrica <- d2[d2$age %in% c('1-11 months', '1-4 years','65-79 years'),]
SAfrica$one = 1

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
