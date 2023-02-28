#General idea of PAGEDOWN: https://pagedown.rbind.io/

#Understanding PAGEDOWN: https://slides.yihui.org/2019-rstudio-conf-pagedown.html#1

# CSS: https://www.pagedmedia.org/about-paged-media/pagedjs-cheatsheet/

# How to create a CV with PAGEDOWN: https://livefreeordichotomize.com/2019/09/04/building_a_data_driven_cv_with_r/#want-to-build-your-cv-this-way

#Images in axis: https://stackoverflow.com/questions/54247880/image-as-axis-tick-ggplot

#Usig emojis as icons in ggplot: https://www.statworx.com/ch/blog/using-emojis-and-png-as-icons-in-your-ggplot/

# Create stickers with R: https://cran.r-project.org/web/packages/magick/vignettes/intro.html

#<br><br> make separations in the web page

# install.packages("usethis")
# install.packages('devtools')
# devtools::install_github("laresbernardo/lares") #NOW IT IS IN CRAN

#library(pagedown) package to create our CV
#library(magick) package to import images
#library(scales) package to use the function percent that allow to conver decimals to percentages
#library(cowplot) package to change axis to images
#library(lares) package to use theme_lares2()  for plots

########################################################

# <br><br> representa espacio entre dos partes

# "At the en of the main part is going to be an aside part."

#######################################################

# RECOMENDATIONS TO USE PAGEDOWN:

#1. Install miktex (laTEX distribution for Windows) that helps us to create pdf´s cientific papers.
#2. Install correct version of pandoc, to use pagedown it is necessary that Pandoc >= 2.2.3
# 2.1 Check version on computer rmarkdown::pandoc_version()
# 2.2 Correct version 2.7.3 (install on https://www.npackd.org/p/pandoc64/2.7.3)
# 2.3 If we are using Anaconda distribution, uninstall its pandoc version (so we can be able to use ours)
# and delete the one on rstudio (in a r-studio windows folder)
# and after that reinstall again jupyter and spyder (conda install jupyter)

# rmarkdown::render("path/to/document.Rmd")  rmarkdown::render("RESUME.Rmd")
# psycModel::html_to_pdf(file_path = "RESUME.html")




## Sys.setenv(R_CONFIG_ACTIVE = "db_cv") --
#Sys.getenv()
## config <- config::get(file = "config.yml") --

# udeploy::mongo_manipulation(
#   mongo_choice = "push",
#   push_record = map(jsonlite::fromJSON("RESUME_IMAGES/PERSONAL_INFORMATION.json"),
#                     ~ .x %>% as.data.table())
# )
## udeploy::mongo_manipulation(mongo_choice = "pull") --