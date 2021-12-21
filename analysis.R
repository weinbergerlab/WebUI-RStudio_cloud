library(plyr)
library(InterventionEvaluatR)
library(ggplot2)
library(tibble)

# Run the relevant pieces of evaluatr analysis. This calls InterventionEvaluatR, but it doesn't do anything to rearrange the results into a form that is useful in the web interface
performAnalysis = function(params, analysisTypes, progress) {
  # return(readRDS("/tmp/app.plot.rds")$analysis) # For debugging
  dataCheckWarnings = list()
  withCallingHandlers({
    progress(setup=list(name="Preparing to run analysis"))
    progress(init=list(name="Initializing analysis"))
    
    if ('univariate' %in% analysisTypes) {
      progress(univariate=list(name="Running univariate analysis"))
    }
    
    if ('impact' %in% analysisTypes) {
      progress(impact=list(name="Running impact analysis"))
    }

    progress(finish=list(name="Finishing analysis"))
    
    # Helper function that turns InterventionEvaluatR progress information (done, total) into the format expected by the progress callback
    analysisProgress = function(part) {
      function(analysis, done, names, before) {
        message(sprintf("%s: %d / %d", part, done, length(names)))
        items = seq_along(names) %>% llply(
          function(idx) list(name=names[[idx]], done=(idx <= done))
        ) %>% setNames(
          sprintf("analysis-%s-%s", part, 1:length(names))
        )
        do.call(progress, items)
      }
    }

    progress(setup=FALSE)
    worker = setupWorker()
    
    on.exit({
      dismissWorker(worker)
    }, add=TRUE)
    progress(setup=TRUE)

    call.params = params    
    # Lite version only does ridge (and has no ridge param in init)
    if (lite) {
      params$ridge = TRUE
      call.params$ridge = NULL
    }
    
    progress(init=FALSE)
    analysis = do.call(
      evaluatr.init,
      call.params
    )
    progress(init=TRUE)

    if ('univariate' %in% analysisTypes) {
      progress(univariate=FALSE)
      InterventionEvaluatR:::evaluatr.initParallel(analysis, worker$startCluster, worker$stopCluster, analysisProgress("univariate"))
      univariateResults = evaluatr.univariate(analysis)
      progress(univariate=TRUE)
    }
    
    if ('impact' %in% analysisTypes) {
      progress(impact=FALSE)
      InterventionEvaluatR:::evaluatr.initParallel(analysis, worker$startCluster, worker$stopCluster, analysisProgress("impact"))
      impactResults = evaluatr.impact(analysis)
      progress(impact=TRUE)
    }
    
    # Lite version only does ridge
    if (lite) {
      analysis$ridge = TRUE
    }

    # Only keep what we need so we aren't shipping large amounts of never-to-be-used data between worker and UI
    progress(finish=FALSE)
    evaluatr.prune(analysis)
    analysis$dataCheckWarnings = dataCheckWarnings
    progress(finish=TRUE)
    analysis
  }, evaluatr.dataCheck=function(warning) {
    dataCheckWarnings <<- c(dataCheckWarnings, list(warning))
    invokeRestart("muffleWarning")
  })
}

# Take the output of InterventionEvaluatR and rearrange it into a form that is better suited for what the Web UI needs to do with it
reformatAnalysis = function(analysis, analysisTypes, info) {
  # saveRDS(list(analysis=analysis, analysisTypes=analysisTypes), "/tmp/app.plot.rds") # For debugging
  reformatted = list(
    setup = list(
      dataName = info$name,
      analysisDate.value = info$analysisDate,
      dateCol = analysis$date_name,
      outcomeCol = analysis$outcome_name,
      denomCol = analysis$denom_name,
      groupCol = analysis$group_name,
      preStart.value = analysis$pre_period[1],
      postStart.value = analysis$post_period[1],
      postEnd.value = analysis$post_period[2],
      analysisTypes = analysisTypes,
      groups = analysis$groups
    ),
    results = list(
      groups = llply(analysis$groups, function(group) {
        list(name=sprintf("%s %s", analysis$group_name, group))
      })
    )
  )
  
  # "Month year" with non-breaking space
  reformatted$setup$preStart = reformatted$setup$preStart.value %>% strftime(format="%B\U00A0%Y")
  reformatted$setup$postStart = reformatted$setup$postStart.value %>% strftime(format="%B\U00A0%Y")
  reformatted$setup$postEnd = reformatted$setup$postEnd.value %>% strftime(format="%B\U00A0%Y")

    # "Month day, year" with non-breaking space
  reformatted$setup$analysisDate = reformatted$setup$analysisDate.value %>% strftime(format="%B\U00A0%e,\U00A0%Y") %>% str_replace_all(" ", "")
  
  if ('univariate' %in% analysisTypes) {
    for(idx in seq_along(analysis$groups)) {
      reformatted$results$groups[[idx]]$plots$univariate = evaluatr.univariate.plot(analysis$results$univariate[[idx]], plot.labs=NULL)
    }
  }
  
  if ('impact' %in% analysisTypes) {
    impactPlots = evaluatr.plots(analysis)
    for(idx in seq_along(analysis$groups)) {
      if (!analysis$ridge) {
        reformatted$results$groups[[idx]]$plots$tsMonthly = impactPlots$groups[[idx]]$pred_best + ggtitle(NULL)
        reformatted$results$groups[[idx]]$plots$tsYearly = impactPlots$groups[[idx]]$pred_best_agg + ggtitle(NULL)
        reformatted$results$groups[[idx]]$best = analysis$results$impact$best$variant[[idx]]
        
        prevented = as.data.frame(analysis$results$impact$best$cumsum_prevented[, , idx])
      } else {
        reformatted$results$groups[[idx]]$plots$tsMonthly = impactPlots$groups[[idx]]$pred_full + ggtitle(NULL)
        reformatted$results$groups[[idx]]$plots$tsYearly = impactPlots$groups[[idx]]$pred_full_agg + ggtitle(NULL)
        reformatted$results$groups[[idx]]$best = "full"
        
        prevented = as.data.frame(analysis$results$impact$full$cumsum_prevented[, , idx])
      }

      reformatted$results$groups[[idx]]$plots$prevented = impactPlots$groups[[idx]]$cumsum_prevented + ggtitle(NULL) + theme(panel.grid.major.y=element_line(color="lightgrey"))

      prevented = prevented[nrow(prevented),] %>% round() %>% signif(3)
      reformatted$results$groups[[idx]]$prevented.value = list(
        median=prevented[["50%"]],
        lcl=prevented[["2.5%"]],
        ucl=prevented[["97.5%"]]
      )
      reformatted$results$groups[[idx]]$prevented = llply(reformatted$results$groups[[idx]]$prevented.value, function(val) {
        format(val, big.mark = "\U205F", scientific=FALSE) # "Medium mathematical space", no scientific notation
      })
      
    }
  }
  
  if ("impact" %in% analysisTypes) {
    normRR = function(rr, variant) {
      rr %>% data.frame() %>% setNames(c("lcl", "median", "ucl")) %>% rownames_to_column("group") %>% mutate(variant=variant)
    }
    
    rr = rbind(
      analysis$results$impact$full$rr_mean %>% normRR(variant="full"),
      analysis$results$impact$time$rr_mean %>% normRR(variant="time"), 
      analysis$results$impact$its$rr_end %>% normRR(variant="its")
    )
    
    if (!analysis$ridge) {
      rr = rbind(
        rr, 
        analysis$results$impact$time_no_offset$rr_mean %>% normRR(variant="time_no_offset"), 
        analysis$results$impact$pca$rr_mean %>% normRR(variant="pca"),
        analysis$results$impact$best$rr_mean %>% normRR(variant="best")
      )
    }

    reformatted$results$rateRatios = llply(seq_along(analysis$groups), function(idx) {
      rr %>% filter(group==analysis$groups[[idx]]) %>% mutate(
        rr=sprintf("%.2f (%.2f - %.2f)", median, lcl, ucl),
        variant.name=variant
      ) %>%
      select(variant.name, rr)
    })
  }
  
  reformatted$dataIssues = llply(analysis$dataCheckWarnings, function(warning) {
    list(type = "warning", description = warning$message)
  })
  
  reformatted
}

# This is the version number for the "download results" rds file. Change if making incompatible changes.
SAVE_VERSION_CURRENT = 15
# This is oldest version number for the "download results" rds file that we still accept. Change when dropping support for loading older files.
SAVE_VERSION_COMPATIBLE = 14
