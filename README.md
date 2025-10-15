# Health & Rights Observatory

## Background
This repository hosts the code and files used to develop the "Health & Rights Observatory", accessible at:  
<https://cehdi-har.share.connect.posit.cloud/>

## Install

All of the code is executed in R, using the RStudio IDE. Ensure that the following packages are installed before running the scripts:

```
install.packages("pacman")
install.packages("shiny")
install.packages("rsconnect")
```

## Usage
The Shiny script to run the dashboard is contained within the `app.R` file, found in the main directory.  

It's important to note that for the dashboard to properly deploy, we need to make sure that any new data files that are needed by the app are pushed to GitHub so they can be accessed online. Additionally, the `manifest.json` file should be routinely updated to account for package and other dependencies by running the below script and then committing and pushing the updated `manifest.json` file.
```
rsconnect::writeManifest()
```
