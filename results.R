# Create results UI
results.ui = function() {
  div(
    class="tab-pane fade show",
    id="nav-results",
    role="tabpanel",
    "aria-labelledby"="nav-results-tab",
    div(
      class="container",
      md_row(
        md_column(
          tagList(
            uiOutput("resultsPendingUI"),
            uiOutput("resultsUI")
          )
        )
      )
    )
  )
}

# Create results UI help
results.help = function() {
  renderHTML("markdown/help-results.md")
}

ANALYSIS_READY = "ready"
ANALYSIS_RUNNING = "running"
ANALYSIS_DONE = "done"
ANALYSIS_FAILED = "failed"
ANALYSIS_CANCELED = "canceled"

# Server-side handling of results UI
results.server = function(input, output, session, setup) {
  ############################################################
  # Analysis
  ############################################################
  
  analysisStatus = reactiveVal(ANALYSIS_READY)
  completedAnalysis = reactiveVal(NULL)
  reformattedAnalysis = reactiveVal(NULL)
  saveData = reactiveVal(NULL)
  
  output$analysisStatus = renderText({
    analysisStatus()
  }) 
  outputOptions(output, 'analysisStatus', suspendWhenHidden=FALSE)
  
  output$resultsUnivariate = reactive({})
  outputOptions(output, 'resultsUnivariate', suspendWhenHidden=FALSE)
  
  output$resultsPendingUI = renderUI({
    tags$section(
      id="results-pending",
      div(
        class="navbar results-heading mb-3 mt-3 justify-content-center primary-color",
        p(class="h3 p-2 m-0 text-white", "Analysis in progress…"),
        md_spinner("spinner-results") %>% tagAppendAttributes(class="text-white")
      ),
      div(
        tags$ul(
          id="analysis-progress",
          class="list-group"
        )
      )
    )
  })
  outputOptions(output, 'resultsPendingUI', suspendWhenHidden=FALSE)
  
  observeEvent(input$analyze, {
    # Loading progress UI
    shinyjs::show("results-pending")
    
    withLogErrors({
      session$sendCustomMessage("activate_tab", list(tab="nav-results-tab"))
      if (analysisStatus() == ANALYSIS_RUNNING) {
        return()
      } else {
        analysisStatus(ANALYSIS_RUNNING)
        
        fullAnalysisData = setup$preparedData()
        
        if (checkNeed(input$analysisGroups)) {
          analysisData = fullAnalysisData %>% filter_at(input$groupCol, function(group) group %in% input$analysisGroups)
          groups = input$analysisGroups
        } else {
          groups = NULL
        }
        
        params = setup$analysisParams()
        
        print("Analysis setup:")
        print(params)
        
        fullParams = c(
          params,
          list(
            data=analysisData
          )
        )

        # Need to pull these out of reactives in order to use them inside a future
        if (checkNeed(setup$precomputedAnalysis())) {
          precomputedAnalysis = setup$precomputedAnalysis()
        } else {
          precomputedAnalysis = NULL
          userInput = setup$userInput()
          userInput$info$analysisDate = now()
          setup$userInput(userInput)
        }
        info = setup$userInput()$info
        
        analysisTypes = input$analysisTypes
        
        progress = function(...) {
          update_progress(session, ...)
        }
        
        # All the future nonsense in here is actually unnecessary because I had to switch to sequential futures to get any kind of useful progress feedback, and it should be rewritten to just use regular function calls.
        future({
          withLogErrors({
            # If the user uploaded precomputed results, and their current analysis settings are compatible with them, use them
            if (checkNeed(precomputedAnalysis)) {
              precomputedAnalysis
            } 
            # Otherwise set up the computation worker and run the analysis
            else {

              performAnalysis(fullParams, analysisTypes, progress)
            }
          })
        }) %...>% (function(analysis) {
          print("Analysis done")
          analysisStatus(ANALYSIS_DONE)
          completedAnalysis(analysis)
          
          reformatted = reformatAnalysis(analysis, analysisTypes, info)
          reformattedAnalysis(reformatted)

          # This is the data that is saved to Results.Rds on download. If you change the format of this data or anything included in it, you need to increment SAVE_VERSION_CURRENT and SAVE_VERSION_COMPATIBLE.
          saveData(list(
            version = SAVE_VERSION_CURRENT,
            data = fullAnalysisData,
            params = params,
            info = info,
            analysis = analysis
          ))
          results.server.show(input, output, session, reformatted)
        }) %...!% (function(error) {
          print("Analysis failed")
          analysisStatus(ANALYSIS_FAILED)
          print(error$message)
          print(error$call)
          results.server.showError(input, output, session, error)
        })
        
        # By constructing a future but returning NULL, shiny server will continue updating the UI while the future is being computed, which allows us to give progress updates
        NULL
      }
    })
  })

  ############################################################
  # Download analysis results
  ############################################################
  
  output$downloadResults <- downloadHandler(
    filename = function() {
      sprintf("InterventionEvaluatR Report %s.zip", Sys.Date())
    },
    content = function(file) {
      # Output files to a temporary directory
      tempDir = sprintf("%s/%s", tempdir(), UUIDgenerate())
      dir.create(tempDir, recursive=TRUE)
      
      withLogErrors({
        # Save data in RDS
        # Note: save all data even if only a subset of groups was analyzed, so that we can come back later and analyze other groups
        saveRDS(saveData(), sprintf("%s/Results.rds", tempDir))
        
        # Render each plot to a PDF file
        analysis = reformattedAnalysis()
        for (idx in seq_along(analysis$results$groups)) {
          group = analysis$results$groups[[idx]]
          groupName = group$name
          groupFileName = gsub("[^a-zA-Z0-9_.-]", "-", groupName)
          dir.create(sprintf("%s/Plots/%s", tempDir, groupFileName), recursive=TRUE)
          ggsave(
            sprintf("%s/Plots/%s/prevented-cases.pdf", tempDir, groupFileName), 
            group$plots$prevented,
            width = 4, height = 3
          )
          ggsave(
            sprintf("%s/Plots/%s/cases-yearly.pdf", tempDir, groupFileName), 
            group$plots$tsYearly,
            width = 4, height = 3
          )
          ggsave(
            sprintf("%s/Plots/%s/cases-monthly.pdf", tempDir, groupFileName), 
            group$plots$tsMonthly,
            width = 4, height = 3
          )
          ggsave(
            sprintf("%s/Plots/%s/covariate-comparison.pdf", tempDir, groupFileName), 
            group$plots$univariate,
            width = 4, height = 3
          )
        }
        
        message(sprintf("Running brew in %s", tempDir))
  
        # Using brew -> latex because rmarkdown is currently unable to output code blocks from a loop, and we need to loop over groups
        brew(
          file="Report.template.tex",
          output=sprintf("%s/Report.tex", tempDir),
          envir=new_environment(data=list(
            # LaTeX template needs analysis data
            analysis=analysis,
            # and some helpers
            renderLaTeX=renderLaTeX,
            new_environment=new_environment,
            rmd.if=rmd.if,
            rmd.endif=rmd.endif,
            rmd.foreach=rmd.foreach
          ), parent=baseenv())
        )
  
        # Change workdir when running LaTeX so its temporary files can be deleted
        oldWD <- getwd()
        # Clean up temporary directory on exit
        on.exit({
          setwd(oldWD)
          unlink(tempDir, recursive = TRUE, force = TRUE)
        })
        setwd(tempDir)
        
        message(sprintf("Running texi2pdf in %s", tempDir))
  
        Sys.setenv(PDFLATEX="xelatex")
        texi2pdf(
          file="Report.tex"
        )
        
      })
      # Zip what we want (the rest is LaTeX garbage)
      zip(file, c("Plots", "Report.pdf", "Report.tex", "Results.rds"))
    }
  )
  outputOptions(output, 'downloadResults', suspendWhenHidden=FALSE)
  
  update_progress(session)
  return(analysisStatus)
}

# Server-side update of results when analysis is complete
results.server.show = function(input, output, session, analysis) {
  setup = analysis$setup
  results = analysis$results
  withLogErrors({
    output$resultsUI = renderUI({
      # One section for each analysis group
      tagList(
        tags$section(
          div(
            class="navbar results-heading mt-3 mb-3 justify-content-center primary-color",
            p(class="h3 p-2 m-0 text-white", "Analysis Summary")
          ),
          md_accordion(
            id="acc-results-overview",
            md_accordion_card(
              "acc-results-summary",
              "Summary",
              renderHTML(
                "markdown/results-summary.Rmd", envir=new_environment(data=list(
                  setup = setup,
                  dataIssues = analysis$dataIssues,
                  # Also some helpers
                  rmd.if = rmd.if,
                  rmd.endif = rmd.endif,
                  rmd.foreach = rmd.foreach
                ), parent=baseenv())
              ),
              expanded=TRUE
            )
          ) %>% tagAppendAttributes(class="mb-3 mt-3 col-12")
        ),
        tagList(llply(seq_along(results$groups), function(idx) {
          group = results$groups[[idx]]
          groupName = group$name
          
          plots = group$plots
          prevented = group$prevented
          
          # item=tableOutput(visId("rateRatios", idx)) %>% tagAppendAttributes(class="table-wrap"),
          
          tags$section(
            div(
              class="navbar results-heading mt-3 mb-3 justify-content-center primary-color",
              p(class="h3 p-2 m-0 text-white", groupName)
            ),
            md_accordion(
              id=sprintf("acc-results-group-%s", idx),
              results_text_panel(
                idx, "summary", "Summary", "markdown/results-group-summary.Rmd", list(setup=setup, group=group), expanded=TRUE
              ),
              results_plot_panel(
                idx, "prevented", "Prevented cases", "markdown/results-explainer-prevented.md"
              ),
              results_plot_panel(
                idx, "tsYearly", "Total cases (yearly)", "markdown/results-explainer-yearly.md"
              ),
              results_plot_panel(
                idx, "univariate", "Covariate comparison", "markdown/results-explainer-univariate.md"
              ),
              md_accordion_card(
                visId("card-supplemental", idx),
                body.class=NULL,
                "Supplemental information",
                div(
                  class="d-flex justify-content-center", 
                  md_accordion(
                    id=sprintf("acc-supplemental-group-%s", idx),
                    supplemental_results_plot_panel(
                      idx, "tsMonthly", "Total cases (monthly)", "markdown/results-explainer-monthly.md"
                    )
                  ) %>% tagAppendAttributes(class="supplemental")
                )
              )
            ) %>% tagAppendAttributes(class="mb-3 mt-3 col-12")
          )
        })),
        tags$section(
          div(
            class="navbar results-heading justify-content-center primary-color",
            p(class="h3 p-2 m-0 text-white", "Download results")
          ),
          div(
            class="col-12 mb-3 mt-3",
            downloadButton('downloadResults', "Download results"),
            p("Includes:"),
            tags$ul(
              tags$li("Report with analysis results (PDF). It contains the same information you see on this page, in a form that you can easily share with others."),
              tags$li("Individual plots (PDF). You can use these in your own reports and presentations."),
              tags$li("Data file with analysis results (RDS). Advanced users can import this into RStudio for additional analysis or to generate additional plots.")
            )
          ),
          HTML("<script>$(function() { $('#downloadResults')[0].click() })</script>")
        )
      )
    })
    
    for(idx in seq_along(results$groups)) {
      # Need separate environment because renderPlotly is lazy and therefore without a separate environment all plots end up being evaluated in the last group
      plotlyEnv = env(
        plots=results$groups[[idx]]$plots
      )
      
      if ("univariate" %in% analysis$setup$analysisTypes) {
        output[[visId("univariate", idx)]] = renderPlotly(
          ggplotly(plots$univariate) %>% plotlyOptions(staticPlot=TRUE),
          env=plotlyEnv
        )
        outputOptions(output, visId("univariate", idx), suspendWhenHidden=FALSE)
      }
      
      if ("impact" %in% analysis$setup$analysisTypes) {
        output[[visId("rateRatios", idx)]] = renderTable(
          results$rateRatios[[idx]]
        )
        outputOptions(output, visId("rateRatios", idx), suspendWhenHidden=FALSE)
        
        output[[visId("tsMonthly", idx)]] = renderPlotly(
          ggplotly(plots$tsMonthly) %>% plotlyOptions(),
          env=plotlyEnv
        )
        outputOptions(output, visId("tsMonthly", idx), suspendWhenHidden=FALSE)
        
        output[[visId("tsYearly", idx)]] = renderPlotly(
          ggplotly(plots$tsYearly) %>% plotlyOptions(),
          env=plotlyEnv
        )
        outputOptions(output, visId("tsYearly", idx), suspendWhenHidden=FALSE)
        
        output[[visId("prevented", idx)]] = renderPlotly(
          ggplotly(plots$prevented) %>% plotlyOptions(),
          env=plotlyEnv
        )
        outputOptions(output, visId("prevented", idx), suspendWhenHidden=FALSE)
      }
    }
    output$resultsPendingUI = renderUI({
    })
  })
}

# Display error that occurred during analysis
results.server.showError = function(input, output, session, error) {
  withLogErrors({
    output$resultsUI = renderUI({
      # One section for each analysis group
      tagList(
        tags$section(
          div(
            class="navbar results-heading mb-3 mt-3 justify-content-center primary-color",
            p(class="h3 p-2 m-0 text-danger", "Analysis Failed")
          ),
          md_accordion(
            id="acc-results-overview",
            md_accordion_card(
              "acc-results-error",
              span(class="text-danger", "Error"),
              p(error$message),
              expanded=TRUE
            )
          ) %>% tagAppendAttributes(class="mb-3 mt-3 col-12")
        )
      )
    })
    
    output$resultsPendingUI = renderUI({
    })
  })
}

visId = function(type, idx) {
  sprintf("%sResults%d", type, idx)
}

analysisStatusDetail <- function(text) {
  print(text)
}

results_text_panel = function(idx, id, title, template, substitutions, expanded=FALSE) {
  md_accordion_card(
    visId(id, idx),
    title,
    div(
      class="d-flex justify-content-between", 
      div(
        id = visId(id, idx),
        renderHTML(
          template, envir=new_environment(data=substitutions, parent=baseenv())
        )
      )
    ), expanded=expanded
  )
}

results_plot_panel = function(idx, id, title, explainer, expanded=FALSE) {
  md_accordion_card(
    visId(id, idx),
    title,
    div(
      class="d-flex justify-content-between", 
      plotlyOutput(visId(id, idx), width="800px"),
      div(
        class="explainer card border-light mb-3",
        div(
          class="card-body text-muted",
          renderHTML(explainer)
        )
      )
    ), expanded=expanded
  )
}

supplemental_results_text_panel = function(idx, id, title, template, substitutions, expanded=FALSE) {
  md_accordion_card(
    visId(id, idx),
    span(class="ml-3", title),
    div(
      class="d-flex justify-content-between ml-3", 
      div(
        id = visId(id, idx),
        renderHTML(
          template, envir=new_environment(data=substitutions, parent=baseenv())
        )
      )
    ), expanded=expanded
  )
}

supplemental_results_plot_panel = function(idx, id, title, explainer, expanded=FALSE) {
  md_accordion_card(
    visId(id, idx),
    span(class="ml-3", title),
    div(
      class="d-flex justify-content-between ml-3", 
      plotlyOutput(visId(id, idx), width="800px"),
      div(
        class="explainer card border-light mb-3",
        div(
          class="card-body text-muted",
          renderHTML(explainer)
        )
      )
    ), expanded=expanded
  )
}

# Progress for us is basically a checklist. This function takes a named list for each checklist item. The value of each checklist item is one of: list(name="Display Name") for a new item that hasn't started yet, list(done=FALSE) for an item that has started but isn't done, or list(done=TRUE) for an item that is done. The first two can be combined. For example:
#
# updateProgress(a=list(name="First progress item", done=FALSE), b=list(name="Second progress item"))
#
# sets up two items on the progress checklist, of which the first is in progress and the second isn't yet started, and
#
# updateProgress(a=list(done=TRUE), b=list(done=TRUE))
#
# marks both as done. 

update_progress = function(session, ...) {
  items = list(...) %>% llply(function(item) {
    # Items that are bare TRUE/FALSE are wrapped in a list(done=X)
    if (!is.list(item)) {
      list(done=item)
    } else {
      item
    }
  })

  # There's a bug in Shiny which causes R to crash with a segfault if we do this a happy way, so kludge it is
  # https://community.rstudio.com/t/sendcustommessage-segfault-failing-to-work-around-it/39993
  # session$sendCustomMessage("update_analysis_progress", list(items=items))
  
  # Instead, write progress info into a downloadable file and have the client poll it
  progressState <<- updateState(progressState, session, items)
  sessionProgress = progressState[[session$token]] %>% toJSON(auto_unbox=TRUE)
  
  d = sprintf("www/.session-data/%s", session$token)
  dir.create(d, showWarnings = TRUE, recursive = TRUE)
  write(sessionProgress, sprintf("%s/progress.json", d))
}

# Kludge from above continues
progressState = list()

updateState = function(state, session, items) {
  sessionID = session$token

  sessionState = state[[sessionID]]
  if (is.null(sessionState)) {
    sessionState = list(items=list(), order=c())
  }

  for (itemID in names(items)) {
    item = items[[itemID]]

    itemState = sessionState$items[[itemID]]
    if (is.null(itemState)) {
      itemState = list()
      # We also need to decide where this item will appear in the list
      # Its predecessor will be the last item that is currently in the list that is not waiting
      insertAfter = sessionState$order %>% laply(function(id) {
        if (!is.null(sessionState$items[[id]]$done)) {
          id
        } else {
          NA
        }
      }) %>% na.omit()

      if (length(insertAfter)) {
        sessionState$order = c(sessionState$order[1:length(insertAfter)], itemID, sessionState$order[(length(insertAfter) + 1):length(sessionState$order)])
      } else {
        sessionState$order = c(sessionState$order, itemID)
      }
    }

    for (propertyID in names(item)) {
      itemState[[propertyID]] = item[[propertyID]]
    }

    sessionState$items[[itemID]] = itemState
  }

  state[[sessionID]] = sessionState
  state
}
