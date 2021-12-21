# Contributing

## Getting started

* Get a copy of InterventionEvaluatR-WebUI using your favorite git tools
* Open WebUI.Rproj in RStudio

InterventionEvaluatR WebUI uses packrat for package management. The first time you open the project in RStudio, you will need to install all the required packages. Use `packrat::restore()` to accomplish this.

To run the web UI, open `WebUI/app.R` in RStudio and click "Run App".

## Organization

The code is roughly divided into:

* `app.R`: The main skeleton of the app (main navigation bar, help button)
* `setup.R`: The analysis setup UI and logic. This is where all the magic happens before the user starts the analysis — loading of data, selection of analysis parameters, etc. The corresponding help content is in `markdown/help-setup.md`.
* `results.R`: Display of results UI and logic. This is everything that happens in response to the Analyze button — preparing the data for analysis, running the analysis, displaying progress, displaying the results, and saving the results. The corresponding help content is in `markdown/help-results.md`. Other relevant templates are in `markdown/results-*.Rmd` and `Report.template.tex`.
* `analysis.R`: This is the code that interfaces directly with the InterventionEvaluatR package. Its main purpose is to turn user's selection of parameters into calls to `evaluatr.*` and to turn the results of `evaluatr.*` into something that's suited for being displayed in the web UI. 
* `worker.R`: This is low-level code responsible for setting up cloud-compute resources to run the analysis. 
* `common.R`: Some utilities used by various other code. 

## Text updates

Text in the app roughly breaks down into large chunks of text (such as the analysis summary or the help content) and small bits of text (such as menu titles).

Large chunks of text come from the templates in the `markdown` folder. Templates named `*.md` have fixed text (as in help content), whereas templates named `*.Rmd` have dynamic text that is processed using the standard Rmarkdown rules for R code in markdown. The dynamic text in `*.Rmd` is used both in the web-based display of results, and to generate the PDF that is part of "Download results"; if you make changes there, check both the web and the PDF to make sure your changes look the way you want them to.

Small bits of text are scattered through `app.R`, `setup.R`, and `results.R`. This will need to change when we want to translate the web UI into other languages (probably using the `shiny.i18n` internationalization package). 

There is also some text that comes directly from InterventionEvaluatR package (mainly in plot content), which will also need to be revisited for internationalization.

## InterventionEvaluatR updates

The web UI is *intentionally* pinned to a specific revision of the main InterventionEvaluatR package. Updating InterventionEvaluatR on your computer does *not* automatically make that new version of InterventionEvaluatR available to the web UI. This is good because it ensures that any untested changes to InterventionEvaluatR will not immediately break the web UI, but it also means that you need to take extra steps to update the InterventionEvaluatR in use by the web UI when you want to do that.

The current process for updating InterventionEvaluatR used by web UI is:

1. Merge the changes you are interested in into the `web-ui` branch in InterventionEvaluatR repository
2. With the WebUI.Rproj open, update InterventionEvaluatR to the latest on the `web-ui` branch using `devtools::install_github("weinbergerlab/InterventionEvaluatR", "web-ui")`. Note that these versions work: InterventionEvaluatR-WebUI commit 47fa32cb; InterventionEvaluatR commit 6acc2c07 ; 
3. also run this to update packages: 
devtools::install_deps()
devtools::install_version("future", version = "1.19.1", repos = "http://cran.us.r-project.org")

3. You can now run the web UI locally with the updated InterventionEvaluatR. 
4. When you are satisfied with the results, run `packrat::snapshot()` to record the new version InterventionEvaluatoR R in packrat
5. Commit the changes to `packrat.lock` that were made by running `packrat::snapshot()`
