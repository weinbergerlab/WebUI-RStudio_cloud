:::: { #help-load .help-section .card-body }

# Loading data

## Stock data

If you don't have your own data, you can explore InterventionEvaluatR by using a stock dataset.

## Your data

You can also load your own data into InterventionEvaluatR.

### Data Format

Your data must be a CSV file with the following columns:

* One column containing the date of each observation. The date must be in one of the following formats: YYYY-MM-DD, YYYY-DD-MM, MM-DD-YYYY, or DD-MM-YYYY.
* One column containing the outcome variable (usually incidence or incidence rate.
* If your data contains multiple observations for each date (for example, for different age groups), then each observation has to be assigned to a group; the group assignment is in its own column
* If the outcome variable is incidence, then a separate column containing the denominator should be included.
* Any other relevant variables you want to include as predictors in the analysis.

::::

:::: { #help-date .help-section .card-body }

# Selecting time variable

Choose which variable in your data contains the date of the observations. Only variables that contain valid dates are shown here.

::::

:::: { #help-outcome .help-section .card-body }

# Selecting outcome

Choose which variable in your data contains the outcome variable — usually, incidence or incidence rate.

If your data is grouped (for example, by age), then you must specify which variable lists the group for each observation.

If the outcome variable you selected above is incidence (rather than incidence rate), then you also need to select the denominator variable.

::::

:::: { #help-periods .help-section .card-body }

# Selecting analysis periods

Specify the month in which the vaccine was introduced.

Also specify how long it took the vaccine to become established in the population. If you are unsure, 24 months is a good initial choice.

::::

:::: { #help-analysis .help-section .card-body }

# Running the analysis

If you specified a group variable above, then you can choose to exclude some groups from the analysis. The analysis takes several minutes for each group, so excluding unneeded groups is definitely a good idea.

::::
