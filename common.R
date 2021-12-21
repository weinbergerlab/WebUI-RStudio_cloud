library(plyr)
library(rmarkdown)

lite = tryCatch(InterventionEvaluatR:::lite, error=function(err){FALSE})

dateFormats = list(
  `YYYY-MM-DD`="%Y-%m-%d",
  `YYYY-DD-MM`="%Y-%d-%m",
  `MM-DD-YYYY`="%m-%d-%Y",
  `DD-MM-YYYY`="%d-%m-%Y",
  `YYYY/MM/DD`="%Y/%m/%d",
  `YYYY/DD/MM`="%Y/%d/%m",
  `MM/DD/YYYY`="%m/%d/%Y",
  `DD/MM/YYYY`="%d/%m/%Y"
)

postDurations = list(
  `6 months`=6,
  `12 months`=12,
  `18 months`=18,
  `24 months`=24
)

stockDatasets = list(
  `Pneumonia in Brazil, 2003-2013`="pnas_brazil"  
)

# TRUE if format is valid date format for v
# Format is valid if the vector can be converted to dates in that format and if differences between nearby dates are >5
validFormat = function(v, format) {
  dates = as.Date(as.character(v), format)
  if(all(!is.na(dates))) {
    # This gets all the deltas between distinct adjacent dates
    diffs = dates %>% sort() %>% unique() %>% diff() %>% unique()
    # Of which none should be <20 or >200, because we're expecting monthly or quarterly data
    all(diffs > 20 & diffs < 200)
  } else {
    FALSE
  }
}

# Auto-detect viable time columns and their formats. Empty list if none are found, NULL if data is NULL
dateColumns = function(data) {
  names(data) %>% 
    sapply(function(name) {
      # List of viable formats for named column; NULL if none
      dateFormats %>% 
        lapply(function(format) {
          if(validFormat(data[[name]], format)) {
            format
          }
        }) %>% 
        compact() %>%
        (function(x) {
          if (length(x) > 0) {
            x
          }
        })
    }, simplify = FALSE, USE.NAMES = TRUE) %>%
    compact() 
}

# True if expr is valid according to the same criteria as shiny::need
checkNeed = function(expr) {
  tryCatch(
    is.null(need(expr, FALSE)), 
    error=function(e) if (!inherits(e, "shiny.silent.error")) {
      stop(e)
    } else {
      FALSE
    }
  )
}

nextButton = function(buttonId, spinnerId, title="Next", disabled=TRUE) {
  div(
    class="button-next",
    md_button(
      buttonId,
      span(class="title", title), 
      md_button_spinner(spinnerId), 
      style="primary", disabled=disabled
    )
  )
}

# Render using Rmarkdown, then read into shiny HTML
renderToString = function(input, output_file, output_format, ...) {
  # rmarkdown normally outputs to a file, and creates a complete (standalone) HTML document
  # But we want to return the output as an object, and it needs to be an HTML fragment
  args = c(
    list(...),
    list(
      input = input,
      output_file = output_file,
      output_format = output_format,
      quiet = TRUE
    )
  )
  do.call(render, args)
  
  # Then read the file back
  paste(readLines(args$output_file), collapse="\n")
}

renderHTML = function(input, ...) {
  HTML(
    renderToString(input, tempfile("renderHTML", fileext=".html"), html_fragment(), ...)
  )
}

renderLaTeX = function(input, ...) {
  renderToString(input, tempfile("renderLaTeX", fileext=".tex"), latex_fragment(), ...)
}

renderMarkdown = function(input, ...) {
  renderToString(input, tempfile("renderMarkdown", fileext=".md"), md_document(), ...)
}

# Default plotly options for all plots
plotlyOptions = function(plot, staticPlot=FALSE, hovermode="x") {
  plot %>% plotly::config(
    staticPlot=staticPlot,
    editable=FALSE,
    scrollZoom=FALSE,
    doubleClick=FALSE,
    showAxisDragHandles=FALSE,
    showLink=FALSE,
    displayModeBar=FALSE,
    showSendToCloud=FALSE,
    displaylogo=FALSE
  ) %>% plotly::layout(
    hovermode=hovermode
  )
}

# These utilities allow use of rmd.if / rmd.endif conditionals inside rmarkdown. See results-summary.Rmd for an example.
rmd.if.state = c()
rmd.if = function(cond) {
  beginHide = all(rmd.if.state) && !cond
  rmd.if.state <<- c(rmd.if.state, cond)
  if (beginHide) {
    "\n<!--\n"
  }
}
rmd.endif = function() {
  cond = tail(rmd.if.state, 1)
  rmd.if.state <<- rmd.if.state[1:length(rmd.if.state)-1]
  endHide = all(rmd.if.state) && !cond
  if (endHide) {
    "\n-->\n"
  }
}

# This lets you write a loop in your rmarkdown. See results-summary.Rmd for an example.
rmd.foreach = function(eltName, list, template) {
  paste(llply(list, function(elt) {
    env = new_environment(list(), baseenv())
    env[[eltName]] = elt
    renderMarkdown(template, envir=env)
  }), collapse="\n")
}
