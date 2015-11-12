# American Red Cross Smoke Alarm Project

### Contents

* [Big picture](#big-picture)
* [How to contribute](#how-to-contribute)
* [Modeling ideas](#modeling-ideas)
* [Structure of repo](#structure-of-repo)

### Big picture
Help Red Cross Home Fire Preparedness Campaign target areas for smoke alarm installs.  Check out the repo [wiki](https://github.com/brooksandrew/arc_smoke_alarm/wiki) for more background information.

### How to contribute

We'd like to try using GitHub issues to manage modeling progress and tasks for this project.  We intend to use "issues" liberally to include things such as exploratory analysis, modeling, research and non-coding tasks such as reaching out to our Red Cross contacts to clarify data issues when needed.

  1. **Find a task:** Search the [repo issues](https://github.com/home-fire-risk/smoke_alarm_models/issues) for models that have been previously scoped with the Red Cross team and assign yourself to it.  Or start a new issue (path of analysis) if you have a modeling idea and assign yourself to it.
  2. **Code**: Create a folder within the root directory named something like "model_XXX" where _XXX is a short description of what you're working on.  We've started a couple of these already (/model_1a_RC_homevisit, for example).
  3. **Explain** Add a README.md to your modeling folder briefly explaining what you're doing, dependencies (data/tools) and a quick note about how to run your code if it's not clear.
  4. **Results**: Save your results (census tract level if possible) in a /results folder within your modeling folder as results_tract.csv with one column of predictions/risk scores and one column of corresponding 11 digit census tract codes (or appropiate geography)

### Modeling ideas (working doc)
Currently in [this Google Doc](https://docs.google.com/document/d/1oJN-QwLVqFHOvrRNtW2KEAkNZ-PuFiqTwa8y3iXx1Sg).  We'd like to migrate these and future ideas directly to [this repo's issues page](https://github.com/home-fire-risk/smoke_alarm_models/issues).

### Structure of repo

##### 1. `/model_Xx` 
Contains exploration and modeling code to produce risk scores for census tracts.  Each model or indicator has it's own folder.  The suffix of each folder name (_1a, _1c, _3a, etc) correspond to the naming convention in this [model scoping working doc]... which is subject to change.   Each folder should have the code necessary to generate the predictions/risk scores and a .csv of the risk scores themselves for each census tract.  Most model input data is too large to store on Github.  It lives on Google Drive or should be pointed to with a README.md if it's publicly available on the web.  This repo is meant to allow for data science tinkering.  The data for the visualization is pulled from here, aggregated and stored in `aggregate_risk/data/risk_tract.R`

##### 2. `/aggregate_risk` 
`aggregate_risk/aggregate_models.R` pulls the .csv output from each individual model folder and creates an aggregated risk score at the census tract level which will be used in the [viz repo].

**Note:** the structure of this repo was simply initalized as it is now.  Improvements and structural enhancements/shifts as the project progresses are welcomed!

**Note:** Results from these models will be visualized in the [visualization repo](https://github.com/home-fire-risk/smoke_alarm_map)

[model scoping working doc]: https://docs.google.com/document/d/1oJN-QwLVqFHOvrRNtW2KEAkNZ-PuFiqTwa8y3iXx1Sg/edit
[viz repo]: https://github.com/home-fire-risk/smoke_alarm_map


