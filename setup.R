# Create setup UI
hiddenIf = function(hidden, ui) {
  if(!hidden) {
    return(ui)
  } else {
    return(hidden(ui))
  }
}

setup.ui = function() {
  div(
    class="tab-pane fade show active",
    id="nav-setup",
    role="tabpanel",
    "aria-labelledby"="nav-setup-tab",
    div(
      id="plot-container",
      class="container-fluid",
      div(
        md_row(
          md_column(
            id="plotColumn",
            plotlyOutput("previewPlot", height="200px"),
            md_spinner("plotSpinner")
          )
        )
      )
    ),
    div(
      class="container",
      md_row(
        class="",
        md_column(
          md_stepper_vertical(
            id="steps",
            selected="load",
            md_stepper_step(
              title="Load Data",
              value="load",
              div(
                class="file-input",
                fileInput(
                  inputId = "userDataset",
                  label = "Load your data:",
                  buttonLabel = "Choose a file"
                )
              ),
              selectInput(
                inputId = "stockDataset",
                label = "Or load stock data:",
                choices = c("", stockDatasets)
              ),
              nextButton("nextDate", "loadSpinner"),
              summary=textOutput("loadSummary"),
              enabled=TRUE
            ),
            md_stepper_step(
              title="Select Time Variable",
              value="date",
              uiOutput("dateColUI"),
              uiOutput("dateFormatUI"),
              nextButton("nextOutcome", "dateSpinner"),
              summary=uiOutput("dateSummary")
            ),
            md_stepper_step(
              title="Select Outcome Variable",
              value="outcome",
              uiOutput("outcomeColUI"),
              uiOutput("groupColUI"),
              uiOutput("denomColUI"),
              nextButton("nextPeriods", "outcomeSpinner"),
              summary=uiOutput("outcomeSummary")
            ),
            md_stepper_step(
              title="Select Analysis Periods",
              value="periods",
              uiOutput("introDateUI"),
              selectInput(
                inputId = "postDuration",
                label = "How long after its introduction did the vaccine become established in the population?",
                choices=postDurations,
                selected=24
              ),
              nextButton("nextAnalysis", "periodsSpinner"),
              summary=uiOutput("periodsSummary")
            ),
            md_stepper_step(
              title="Analyze",
              value="analysis",
              hidden(checkboxGroupInput(
                "analysisTypes",
                "Which types of analysis do you want to perform?",
                c(
                  "Univariate Poisson regression"="univariate",
                  "Impact analysis"="impact"
                ),
                selected = c("univariate", "impact"),
                inline=TRUE
              )),
              hiddenIf(lite, radioButtons(
                "impactType",
                "Which type of impact analysis do you want to perform?",
                c(
                  "Bayesian (ridge) analysis — faster"="ridge",
                  "Frequentist (Markov chain Monte Carlo) analysis — slower"="mcmc"
                ),
                selected = c("ridge"),
                inline=FALSE
              )),
              uiOutput("analysisGroupsUI"),
              uiOutput("analyzeButtonUI")
            )
          )
        )
      )
    )
  )
}

# Create setup UI help
setup.help = function() {
  renderHTML("markdown/help-setup.md")
}

# Server-side handling of setup UI
setup.server = function(input, output, session) {
  ############################################################
  # Set up reactive data inputs
  ############################################################
  
  setup = new_environment()
  
  setup$userInput = reactiveVal() # Will be list(name, input, params, results)
  
  observe({
    validate(need(input$stockDataset, FALSE))
    md_update_spinner(session, "loadSpinner", visible=TRUE)
    setup$userInput(list(
      info=list(
        name=names(which(stockDatasets == input$stockDataset))
      ),
      data=switch(
        input$stockDataset,
        pnas_brazil = {
          data("pnas_brazil", package="InterventionEvaluatR")
          pnas_brazil
        },
        SAfrica
      )
    ))
  })
  
  userInputRDS = function(upload) {
    input = c(
      readRDS(upload$datapath)
    )
    input$info$name = upload$name
    if (!is.numeric(input$version) || input$version < SAVE_VERSION_COMPATIBLE) {
      input$analysis = NULL
    }
    
    # In RDS prior to version 15, ridge = FALSE is implicit
    if (is.null(input$params$impactType)) {
      input$params$ridge = FALSE;
      input$analysis$ridge = FALSE;
    }
    
    input
  }
  
  userInputCSV = function(upload) {
    list(
      data=read.csv(upload$datapath),
      info=list(
        name=upload$name
      )
    )
  }
  
  observe({
    validate(need(input$userDataset, FALSE))
    md_update_spinner(session, "loadSpinner", visible=TRUE)
    updateSelectInput(session, "stockDataset", selected="")
    upload = input$userDataset[1,]
    # We accept rds and csv input. Try rds first.
    tryCatch(
      setup$userInput(userInputRDS(upload)),
      error = function(e) {
        setup$userInput(userInputCSV(upload))
      }
    )
  })
  
  # This is the input data for analysis  
  inputData = reactive({
    validate(need(setup$userInput(), FALSE))
    setup$userInput()$data
  })
  
  # Pre-computed results, if uploaded by the user
  inputAnalysis = reactive({
    validate(need(setup$userInput(), FALSE))
    setup$userInput()$analysis
  })
  
  # Params of pre-computed results, if uploaded by the user
  inputParams = reactive({
    validate(need(setup$userInput(), FALSE))
    setup$userInput()$params
  })
  
  dataDateColumns = reactive({
    dateColumns(inputData())
  })
  
  dataTime = reactive({
    validate(need(input$dateCol, FALSE))
    validate(need(input$dateFormat, FALSE))
    
    as.Date(inputData()[[input$dateCol]], format=input$dateFormat) 
  })
  
  dataOutcome = reactive({
    validate(need(input$outcomeCol, FALSE))
    
    if (checkNeed(input$denomCol)) {
      inputData()[[input$outcomeCol]] / inputData()[[input$denomCol]]
    } else {
      inputData()[[input$outcomeCol]]
    }
  })
  
  dataGroup = reactive({
    if (!is.null(input$groupCol)) {
      inputData()[[input$groupCol]]
    }
  })
  
  dataNeedsGroup = reactive({
    length(unique(dataTime())) < length(dataTime())
  })
  
  dataGroupValues = reactive({
    validate(need(dataGroup, FALSE))
    factor(dataGroup())
  })
  
  dataPostStart = reactive({
    validate(need(input$postStart, FALSE))
    date = as.Date(input$postStart, "%Y-%m-%d")
    day(date) = 1
    date
  })
  
  dataEvalStart = reactive({
    validate(need(input$postDuration, FALSE))
    date = dataPostStart() %m+% months(as.numeric(input$postDuration))
    day(date) = 1
    date
  })
  
  setup$preparedData = reactive({
    validate(need(inputData(), dataTime(), FALSE))
    data = inputData()
    data[[input$dateCol]] = dataTime()
    data
  })
  
  ############################################################
  # Set up reactive data display
  ############################################################
  
  output$previewPlot = renderPlotly({
    periods = function() {
      if (checkNeed(input$postStart) && checkNeed(input$postDuration)) {
        df = data.frame(
          xmin=c(min(dataTime()), dataEvalStart()),
          xmax=c(dataPostStart(), max(dataTime())),
          y=rep(max(dataOutcome()) * 1.1, 2)
        )
        c(
          geom_segment(data=df, aes(x=xmin, xend=xmax, y=y, yend=y)),
          geom_point(data=df, aes(x=xmin, y=y)),
          geom_point(data=df, aes(x=xmax, y=y))
        )
      } else {
        df = data.frame(
          xmin=min(dataTime()),
          xmax=max(dataTime()),
          y=max(dataOutcome()) * 1.1
        )
        c(
          geom_segment(data=df, aes(x=xmin, xend=xmax, y=y, yend=y), color="#FFFFFF00"),
          geom_point(data=df, aes(x=xmin, y=y), color="#FFFFFF00"),
          geom_point(data=df, aes(x=xmax, y=y), color="#FFFFFF00")
        )
      }
    }
    
    if (!is.null(dataGroup())) {
      ggplotly(ggplot(
        data.frame(y=dataOutcome(), t=dataTime(), g=dataGroup()) %>% arrange(t)
      ) +
        geom_line(aes(x=t, y=y, group=g), size=0.1) +
        periods() + 
        labs(x=NULL, y=NULL) +
        theme_minimal()
      ) %>% plotlyOptions()
    } else if (dataNeedsGroup()) {
      data = data.frame(y=dataOutcome(), t=dataTime()) %>% arrange(t)
      data %<>% group_by(t) %>% summarize(ymin=min(y), ymax=max(y))
      ggplotly(
        ggplot(data) +
          geom_ribbon(aes(x=t, ymin=ymin, ymax=ymax), size=0.1, fill="grey75") +
          periods() + 
          labs(x=NULL, y=NULL) +
          theme_minimal()
      ) %>% plotlyOptions()
    } else {
      data = data.frame(y=dataOutcome(), t=dataTime()) %>% arrange(t)
      ggplotly(
        ggplot(data) +
          geom_line(aes(x=t, y=y), size=0.1) +
          periods() + 
          labs(x=NULL, y=NULL) +
          theme_minimal()
      ) %>% plotlyOptions()
    }
  })
  outputOptions(output, 'previewPlot', suspendWhenHidden=FALSE)
  
  output$showPreviewPlot = reactive({
    show = checkNeed(input$dateCol) && checkNeed(input$dateFormat) && checkNeed(input$outcomeCol)
    toggleClass(id="page", class="plot-on", condition=show)
    show
  })
  outputOptions(output, 'showPreviewPlot', suspendWhenHidden=FALSE)
  
  ############################################################
  # Precomputed results
  ############################################################
  
  setup$analysisParams = reactive({
    validate(need(dataTime(), FALSE))
    validate(need(dataPostStart(), FALSE))
    validate(need(dataEvalStart(), FALSE))
    validate(need(input$groupCol, FALSE))
    validate(need(input$dateCol, FALSE))
    validate(need(input$outcomeCol, FALSE))
    validate(need(input$denomCol, FALSE))
    # Detect whether we are using monthly or quarterly observations by looking at the average interval between observations
    obsPerYear = 365 / as.numeric(diff(range(dataTime()))) * length(unique(dataTime()))
    obsPerYear = ifelse(obsPerYear > 8, 12, 4)
    
    list(
      country="Placeholder",
      post_period_start=dataPostStart(),
      eval_period_start=dataEvalStart(),
      eval_period_end=max(dataTime()),
      n_seasons=obsPerYear,
      year_def="cal_year",
      group_name=input$groupCol,
      date_name=input$dateCol,
      outcome_name=input$outcomeCol,
      denom_name=input$denomCol,
      ridge=(input$impactType == "ridge")
    )    
  })
  
  setup$precomputedAnalysis = reactive({
    # Precomputed results are only valid if inputParams match current analysis params and if groups previously analyzed include all groups currently selected
    validate(need(inputAnalysis(), FALSE))
    validate(need(inputParams(), FALSE))
    validate(need(setup$analysisParams(), FALSE))
    
    missingGroups = setdiff(input$analysisGroups, inputAnalysis()$groups)
    
    paramsAgree = setup$analysisParams() %>% as.data.frame() %>% 
      rbind(inputParams() %>% as.data.frame()) %>%
      summarize_all(function(col) length(unique(col)) == 1) %>%
      as.logical() %>%
      all()
    
    if (paramsAgree && length(missingGroups) == 0) {
      inputAnalysis()
    }
  })
  
  ############################################################
  # Set up reactive input controls
  ############################################################
  
  output$dateColUI <- renderUI({
    choices = names(dateColumns(inputData()))
    if (length(choices) > 1) {
      choices = c("", choices)
    }
    
    if (length(choices) > 0) {
      selectInput(
        inputId = "dateCol",
        label = "Which variable in your data represents time?",
        choices = choices
      )
    } else {
      div("Your data doesn't contain a valid time variable. The time variable must be formatted as either ", code("year-month-day"), " or ", code("year/month/day"), "and it must be either monthly or quarterly, with no skipped time periods.")
    }
  })
  outputOptions(output, 'dateColUI', suspendWhenHidden=FALSE)
  
  output$dateFormatUI <- renderUI({
    validate(need(input$dateCol, FALSE))
    
    choices = (inputData() %>% dateColumns())[[input$dateCol]]
    select = selectInput(
      inputId = "dateFormat",
      label = "Date Format:",
      choices = choices
    )
    
    if (length(choices) < 2) {
      select = hidden(select)
    }
    
    select
  })
  outputOptions(output, 'dateFormatUI', suspendWhenHidden=FALSE)
  
  output$outcomeColUI <- renderUI({
    selectInput(
      inputId = "outcomeCol",
      label = "Which is the outcome variable in your data? This should be a count for number of cases/hospitalizations/death per unit time",
      choices = c("", setdiff(names(inputData()), names(dateColumns(inputData()))))
    )
  })
  outputOptions(output, 'outcomeColUI', suspendWhenHidden=FALSE)
  
  output$denomColUI <- renderUI({
    selectInput(
      inputId = "denomCol",
      label = "Which is the denominator variable in your data? This could be population size, total number of hospitalizations, etc",
      choices = c("", setdiff(names(inputData()), names(dateColumns(inputData()))))
    )
  })
  outputOptions(output, 'denomColUI', suspendWhenHidden=FALSE)
  
  output$groupColUI <- renderUI({
    if (dataNeedsGroup()) {
      selectInput(
        inputId = "groupCol",
        label = "Your data contains multiple observations for each point in time. Which is the grouping variable in your data? (e.g., age group, region)",
        choices = c(`No grouping`="", setdiff(names(inputData()), names(dateColumns(inputData()))))
      )
    }
  })
  outputOptions(output, 'groupColUI', suspendWhenHidden=FALSE)
  
  output$introDateUI <- renderUI({
    airMonthpickerInput(
      inputId = "postStart",
      label = "When was the vaccine introduced?",
      view="months",
      minView="months",
      minDate=min(dataTime()),
      maxDate=max(dataTime()),
      addon="none",
      autoClose=TRUE#,
      #value=oldValue 
    )
  })
  outputOptions(output, 'introDateUI', suspendWhenHidden=FALSE)
  
  output$analysisGroupsUI = renderUI({
    validate(need(dataGroupValues(), FALSE), need(input$groupCol, FALSE))
    groupValues = levels(dataGroupValues())
    groupNames = sprintf("%s %s", input$groupCol, groupValues)
    
    checkboxGroupInput(
      "analysisGroups",
      "Which groups do you want to include in analysis?",
      choiceNames = groupNames,
      choiceValues = groupValues,
      selected = groupValues,
      inline = TRUE
    )
  })
  outputOptions(output, 'analysisGroupsUI', suspendWhenHidden=FALSE)
  
  output$analyzeButtonUI = renderUI({
    with(list(analyzeAvailable=checkNeed(input$analysisTypes)), {
      if (checkNeed(setup$precomputedAnalysis())) {
        nextButton("analyze", "analyzeSpinner", title="Show Results", disabled=!analyzeAvailable)
      } else {
        nextButton("analyze", "analyzeSpinner", title="Analyze", disabled=!analyzeAvailable)
      }
    })
  })
  outputOptions(output, 'analyzeButtonUI', suspendWhenHidden=FALSE)
  
  ############################################################
  # Set up step enabled / disabled state and next buttons
  ############################################################
  
  dateCols = reactive({
    if (!is.null(inputData())) {
      dateColumns(inputData())
    }
  })
  
  observe({
    with(list(dateAvailable=checkNeed(dateCols())), {
      updateButton(session, "nextDate", disabled=!dateAvailable)
      md_update_stepper_step(session, "steps", "date", enabled=dateAvailable)
    })
  })
  
  observeEvent(input$nextDate, {
    md_update_stepper(session, "steps", value="date")
  })
  
  observe({
    with(list(outcomeAvailable=!is.null(dataTime())), {
      updateButton(session, "nextOutcome", disabled=!outcomeAvailable)
      md_update_stepper_step(session, "steps", "outcome", enabled=outcomeAvailable)
    })
  })
  
  observeEvent(input$nextOutcome, {
    md_update_stepper(session, "steps", value="outcome")
  })
  
  observe({
    with(list(periodsAvailable=checkNeed(dataOutcome()) && checkNeed(input$denomCol) && (!dataNeedsGroup() || checkNeed(dataGroup()))), {
      updateButton(session, "nextPeriods", disabled=!periodsAvailable)
      md_update_stepper_step(session, "steps", "periods", enabled=periodsAvailable)
    })
  })
  
  observeEvent(input$nextPeriods, {
    md_update_stepper(session, "steps", value="periods")
  })
  
  observe({
    with(list(analysisAvailable=checkNeed(dataPostStart()) && checkNeed(dataEvalStart())), {
      updateButton(session, "nextAnalysis", disabled=!analysisAvailable)
      md_update_stepper_step(session, "steps", "analysis", enabled=analysisAvailable)
    })
  })
  
  observeEvent(input$nextAnalysis, {
    md_update_stepper(session, "steps", value="analysis")
  })
  
  observe({
    with(list(analyzeAvailable=checkNeed(input$analysisTypes)), {
      updateButton(session, "analyze", disabled=!analyzeAvailable)
    })
  })
  
  ############################################################
  # Set up step summaries
  ############################################################
  
  output$loadSummary = reactive({
    validate(need(setup$userInput()$info$name, FALSE))
    md_update_spinner(session, "loadSpinner", hidden=checkNeed(setup$userInput()$info$name))
    setup$userInput()$info$name
  })
  
  output$dateSummary = renderUI({
    validate(need(input$dateCol, FALSE))
    tags$code(input$dateCol)
  })
  
  output$outcomeSummary = renderUI({
    validate(need(dataOutcome(), FALSE))
    if (checkNeed(input$denomCol) && checkNeed(input$groupCol)) {
      span(
        tags$code(input$outcomeCol),
        " / ",
        tags$code(input$denomCol),
        " by ",
        tags$code(input$groupCol)
      )
    } else if (checkNeed(input$denomCol)) {
      span(
        tags$code(input$outcomeCol),
        " / ",
        tags$code(input$denomCol)
      )
    } else if (checkNeed(input$groupCol)) {
      span(
        tags$code(input$outcomeCol),
        " by ",
        tags$code(input$groupCol)
      )
    } else {
      tags$code(input$outcomeCol)
    }
  })
  
  output$periodsSummary = renderUI({
    validate(need(dataPostStart(), FALSE), need(dataEvalStart(), FALSE))
    span(
      span(
        class="pre-period",
        strftime(min(dataTime()), "%b %Y"), 
        "—",
        strftime(dataPostStart(), "%b %Y")
      ),
      "vs.",
      span(
        class="post-period",
        strftime(dataEvalStart(), "%b %Y"), 
        "—",
        strftime(max(dataTime()), "%b %Y")
      )
    )
  })
  
  setup
}
