library(plyr)
library(shinyBS)
library(magrittr)

md_page = function(...) {
  tags$html(
    singleton(tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "mdb/css/bootstrap.min.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "mdb/css/mdb.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/app.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/mdbootstrap.css")
    )),
    tags$body(
      div(...),
      tags$script(src = "mdb/js/mdb.js"),
      tags$script(src = "mdb/js/bootstrap.js"),
      tags$script(src = "js/mdbootstrap.js")
    )
  )
}

md_navbar = function(..., title=NULL, class=NULL) {
  result = tags$nav(
    tags$a(
      title,
      class="navbar-brand",
      href="#"
    ),
    ...,
    class="navbar navbar-dark primary-color-dark"
  )
  if (!is.null(class)) {
    result %>% tagAppendAttributes(class=class)
  }
  result
}

md_row = function(...) {
  div(..., class="row")
}

md_column = function(...) {
  div(..., class="col")
}

md_stepper_vertical = function(..., id, selected) {
  steps = list(...)
  tags$ul(
    lapply(seq_along(steps), function(idx) {
      step = steps[[idx]]
      tags$li(
        a(
          span(as.character(idx), class="circle"),
          span(step$title, class="label"),
          span(step$summary, class="label summary"),
          href="#!"
        ),
        div(
          class="step-body",
          div(
            step$content,
            class="step-content"
          )
        ),
        id=step$value,
        class=ifelse(step$enabled, "completed", "")
      )
    }),
    class="stepper stepper-vertical",
    id=id,
    "data-stepper-selected"=selected
  )
}

md_stepper_step = function(title, ..., value, summary=NULL, enabled=FALSE) {
  list(title=title, value=value, content=list(...), summary=summary, enabled=enabled)
}

md_update_stepper = function(session, stepper, value=NULL) {
  session$sendCustomMessage("md_update_stepper", list(stepper=stepper, value=value) %>% compact())
}

md_update_stepper_step = function(session, stepper, step, enabled=NULL) {
  session$sendCustomMessage("md_update_stepper_step", list(
    stepper=stepper, step=step, enabled=enabled
  ) %>% compact())
}

# All unnamed arguments except for the first one are treated as content
md_button = function(id, ...) {
  args = list(...)
  named = args[names(args) != ""]
  unnamed = args[names(args) == ""]
  do.call(bsButton, c(list(inputId=id, label=tagList(unnamed)), named))
}

md_spinner = function(id) {
  div(
    class="spinner-border", role="status", id=id,
    span(class="sr-only", "Loading…")
  )
}

md_button_spinner = function(id, visible=FALSE) {
  span(
    class="spinner-border spinner-border-sm", role="status", id=id, "aria-hidden"="true"
  ) %>% tagAppendAttributes(class=ifelse(visible, "", "invisible"))
}

# hidden vs visible = CSS display vs CSS visible
md_update_spinner = function(session, spinner, hidden=NULL, visible=NULL) {
  session$sendCustomMessage("md_update_spinner", list(spinner=spinner, hidden=hidden, visible=visible) %>% compact())
}

md_carousel = function(id, items) {
  div(
    id=id,
    class="carousel slide",
    "data-interval"="false",
    tags$ol(
      class="carousel-indicators",
      tagList(llply(seq_along(items), function(idx) {
        content = tags$li(
          class="primary-color",
          "data-target"=sprintf("#%s", id),
          "data-slide-to"=idx-1
        )
        if (idx == 1) {
          content %<>% tagAppendAttributes(class="active")
        }
        content
      }))
    ),
    div(
      class="carousel-inner",
      role="listbox",
      tagList(llply(seq_along(items), function(idx) {
        item = items[[idx]]
        if (class(item) != "list") {
          item = list(caption="", "item"=item)
        }

        content = div(
          class="d-block w-100",
          item$item
        )

        if (checkNeed(item$caption)) {
          content = div(
            class="view",
            content,
            div(
              class="carousel-caption",
              p(item$caption)
            )
          )
        }
        
        content = div(
          class="carousel-item",
          content
        )

        if (idx == 1) {
          content %<>% tagAppendAttributes(class="active")
        }
        content
      }))
    ),
    tags$a(
      class="carousel-control-prev",
      href=sprintf("#%s", id),
      role="button",
      "data-slide"="prev",
      span(
        class="carousel-control-prev-icon primary-color",
        "aria-hidden"="true"
      ),
      span(
        class="sr-only",
        "Previous"
      )
    ),
    tags$a(
      class="carousel-control-next",
      href=sprintf("#%s", id),
      role="button",
      "data-slide"="next",
      span(
        class="carousel-control-next-icon primary-color",
        "aria-hidden"="true"
      ),
      span(
        class="sr-only",
        "Next"
      )
    )
  )
}

md_accordion_card = function(id, title, content, expanded=FALSE, body.class="card-body") {
  list(id=id, body.class=body.class, title=title, content=content, expanded=expanded)
}

md_accordion = function(id, ...) {
  cards = list(...)

  div(
    class="accordion",
    id=id,
    tagList(lapply(seq_along(cards), function(idx) {
      card = cards[[idx]]
      buttonId = sprintf("%s-button", card$id)
      targetId = sprintf("%s-target", card$id)
      
      if (card$expanded) {
        buttonClass = ""
        ariaExpanded = "true"
        targetClass = "collapse show"
      } else {
        buttonClass = "collapsed"
        ariaExpanded = "false"
        targetClass = "collapse"
      }
      
      div(
        class="card",
        div(
          class="card-header",
          id=buttonId,
          a(
            "aria-controls"=targetId,
            "aria-expanded"=ariaExpanded,
            class=buttonClass,
            "data-parent"=sprintf("#%s", id),
            "data-toggle"="collapse",
            href=sprintf("#%s", targetId),
            tags$h5(
              class="mb-0",
              card$title,
              icon("angle-up"),
              icon("angle-down")
            )
          )
        ),
        div(
          id=targetId,
          class=targetClass,
          "aria-labelledby"=buttonId,
          "data-parent"=sprintf("#%s", id),
          div(
            class=card$body.class,
            card$content
          )
        )
      )
    }))
  )
}