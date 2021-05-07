
# 1.0 Load Packages -------------------------------------------------------
rm(list = ls())

library(purrr)

map(c("udeploy", "config", "jsonlite", 
      "mongolite", "data.table", "stringr",
      "lubridate", "dplyr", "glue", 
      "ggplot2", "lares", "scales",
      "cowplot", "pagedown"), 
    require, character.only = TRUE)

# 2.0 Read Data -----------------------------------------------------------
Sys.setenv(R_CONFIG_ACTIVE = "db_cv")
#Sys.getenv()
config <- config::get(file = "config.yml")

# udeploy::mongo_manipulation(
#   mongo_choice = "push",
#   push_record = map(jsonlite::fromJSON("RESUME_IMAGES/PERSONAL_INFORMATION.json"),
#                     ~ .x %>% as.data.table())
# )
udeploy::mongo_manipulation(mongo_choice = "pull")

#' @Regex: define the possible regex to get a text from string (an observation)
#' @Useful: in  HTTP_LINK and CREATING_DFS functions
regex <- map2(list("\\[", c("\\[", "\\]"), "\\)"), #everything before [, everything between [], #everything after )
              c("before_pattern", "between_pattern", "after_pattern"),
              ~udeploy::obtain_regex(pattern = .x, return_regex = .y)) %>% 
  set_names("regex_left", "regex_inside", "regex_after")

bracket_regex <- udeploy::obtain_regex(pattern = pluck(regex, "regex_inside"),  #start with [text]
                                       return_regex = "starts_with_pattern") 
join_regex <- str_glue("{regex$regex_left}\\[{regex$regex_inside}\\]\\(.*\\){regex$regex_after}") #"text"["text"]("text")"no text"|"text"

# 3.0 Dataset Creation ----------------------------------------------------

#' @description
#' 1.pull_info_dfs: dataset of each topic/TYPE where is obtain from a mongodb.
#' - if an observation = "today" of date variables, then we put the date of today.
#' - Adjust the uppercase with the help of the function *HTTP_LINK*
#' -new variable LABEL_POS is the mean from the START and the END date, with the 
#' intention that in the timeline plot (of the TIME_LINE_DF dataset) we can put
#' the INSTITUTION variable in the central point of the bar of each topic(type)
#' - This dataset is the reference to define the next two important datasets.
#' 
#' 2.PAGEDOWN_DF: 
#' - get the year of START and END because in pagedown we are only going to show 
#' from which year to which year 
#' - arrange by END to get the information in order from the last year from the 
#' lowest to last in all the dataset
#' - define the variable TIMELINE, it is important because in the cv we are going
#' to show only the year and if the start year is the same as the end year for 
#' each observation then only show the end year, and we only want that column 
#' that is why we delete START and END
#' 
#' 3.TIME_LINE_DF: 
#' - First we replicate this dataset (because of the variable TIME that we are
#' going to create), after that in the variables TITLE and INSTITUTION we are 
#' going to put in each observation that is [text](link) only text 
#' - POS is the sequence from 1 to the nrow of the dataset before and after the 
#' duplicated 1,2,..,N,1,2,...,N
#' -TIME we replicated the dataset because of this variable to put first the 
#' start dates of INFO_DF and then the final dates of INFO_DF, this with the 
#' intention that the bar in the plot shows the start and the end of each 
#' TITLE 
#' @return list of two datasets
#' 2.PAGEDOWN_DF: Dataset that represent all the necessary information in the
#' correct format to create our pagedown cv with the help of the function 
#' [SECTION_PAGEDOWN]
#' 3.TIME_LINE_DF: Dataset that represent all the necessary information in the 
#' correct format to create the plot of time line with the help of the function
#' [TIME_LINE_PLOT]
CREATING_DFS <- function(){
  
  pull_info_dfs <- pull_info[,-"MYSELF"]
  
  pull_info_dfs <- map(names(pull_info_dfs), 
                       ~ pull_info_dfs[,eval(parse(text = .x))] %>% pluck(1) %>% 
                         as.data.table() %>% .[,TYPE:= .x]) %>% 
    rbindlist()
  
  vars_time <- c("START","END")
  vars_factor <- c("TYPE", "INSTITUTION")
  
  walk(vars_time, 
       ~ pull_info_dfs[,(.x):=str_replace_all(string = eval(parse(text = .x)),
                                              pattern = "today",
                                              replacement = as.character(Sys.Date()))] %>% 
         setnames(toupper(names(.))) %>% 
         .[,lapply(.SD, as.character), .SDcols = names(.)]
  ) 
  walk(names(pull_info_dfs), 
       ~ pull_info_dfs[,(.x):=HTTP_LINK(df_variable = eval(parse(text = .x)))])
  
  pull_info_dfs <-
    pull_info_dfs[,(vars_time):=lapply(.SD, as.Date), .SDcols = vars_time] %>%
    .[,(vars_factor):=lapply(.SD, as.factor), .SDcols = vars_factor] %>%
    .[,LABEL_POS:=START + floor((END - START)/2)] #to create the label in each line of time line plot
  
  PAGEDOWN_DF <- copy(pull_info_dfs)[,-c("LABEL_POS")] %>%
    .[,':='(START = year(START), END = year(END))] %>% 
    setorder(END) %>% 
    .[,TIMELINE:=ifelse(test = START == END,
                        yes = END,
                        no = str_glue('{START} - {END}'))] %>% 
    .[,-c("START", "END")]
  
  vars_timeline <- c("TYPE", "TITLE", "INSTITUTION", "LABEL_POS")
  vars_replacements <- vars_timeline[2:3]
  
  TIME_LINE_DF <- copy(pull_info_dfs)[,lapply(.SD, rep, 2), .SDcols = vars_timeline] %>% #only variables in .SDcols and replicate observations
    .[,(vars_replacements):=lapply(.SD, as.character),.SDcols = vars_replacements]
  
  walk(vars_replacements, function(i){
    TIME_LINE_DF[,(i):= map(pluck(regex, "regex_inside"), 
                            ~ list(.x, udeploy::obtain_regex(pattern = .x, 
                                                             return_regex = "not_contains_pattern"))) %>% 
                   unlist() %>% udeploy::obtain_regex(return_regex = "or") %>% 
                   str_extract(string = TIME_LINE_DF[,eval(parse(text = i))])
    ] %>% .[,':='(POS = rep(seq(from = 1, to = pull_info_dfs[,.N], by = 1), 2),
                  TIME = c(pull_info_dfs$START, pull_info_dfs$END))] # replicated before because of this part
  })
  
  list(PAGEDOWN_DF = PAGEDOWN_DF, TIME_LINE_DF = TIME_LINE_DF) %>%
    return()
}
#' @SUBFUNCTION
#' @description The intention of this function is to select where to put an 
#' upper case in each (thanks to the loop map_chr) observation of a variable 
#' 1. map_chr loop over all observations not only one observation (string),
#' iterating in each observation
#' 2. define the replacement of the information with the *regex list* that is going to be
#' the information that represent the regex in the string but in *upper case* of 
#' each observation.
#' 3. Make the replacements
#' 3.1 If is a link ([string](link)) then we only put in upper case everything 
#' between [] to make the link work
#' 3.2 If we have some information and then [string](link) and possibly text
#' after that then we put in uppercase everything except between ()
#' 3.3 in other case put the observation or string in upper case
#' @Note: we separate in three regex because it is not simple to combine regex 
#' (like AND operator) applied into a string  
#' @param df_variable variable to make observations in uppercase
#' @return observation of a variable of the dataset (of the topic selected) in
#' uppercase based on restrictive conditions
HTTP_LINK <- function(df_variable){
  
  map_chr(df_variable, function(df_variable){
    
    uppercase <- map(regex,
                     ~str_extract(string = df_variable, pattern = .x)) %>% 
      map(~toupper(.x)) 
    
    if(str_detect(string = df_variable, 
                  pattern = bracket_regex)){
      
      df_variable <- str_replace_all(string = df_variable,
                                     pattern = regex$regex_inside,
                                     replacement = uppercase$regex_inside)
      
    }else if(str_detect(string = df_variable, 
                        pattern = join_regex)){
      
      df_variable <- str_replace(string = df_variable, 
                                 pattern = regex$regex_left,
                                 replacement = uppercase$regex_left) %>% 
        str_replace(pattern = regex$regex_inside,
                    replacement = uppercase$regex_inside) %>% 
        str_replace(pattern = regex$regex_after,
                    replacement = uppercase$regex_after)
      
      
    }else df_variable <- toupper(df_variable)
  }) %>% return()
}
#HTTP_LINK(df_variable = df$DESCRIPTION)

# 4.0 Pagedown Format -----------------------------------------------------

#' @description we filter the topic that we want from PAGEDOWN_DF dataset.
#' @param df dataset PAGEDOWN_DF from the *CREATING_DFS* function
#' @param type topic we want to filter
#' @Note:we use $\n\n$ "salto de linea" to separate the ideas in the page of pagedown
#' @return filter dataset in the pagedown cv format
SECTION_PAGEDOWN <- function(df, type){
  
  df %>% 
    filter(TYPE == type) %>% 
    glue_data(
      "### {TITLE}",
      "\n\n",
      "{INSTITUTION}",
      "\n\n",
      "{LOCATION}",
      "\n\n",
      "{TIMELINE}", 
      "\n\n",
      "{DESCRIPTION}",
      "\n\n\n")%>% return()
}
CREATING_DFS()$PAGEDOWN_DF %>% SECTION_PAGEDOWN(type = 'EDU')

# 5.0 Creating time line plot ---------------------------------------------

#' @description  
#' arguments:
#' x-axis = TIME because we want to see the changes in the time
#' y-axis = reorder(TITLE, -POS) reorder TITLE (each line) of each group (TYPE)
#' based on the sequence POS, to have each group with each one 
#' label = each line represent the INSTITUTION
#' color = based on each group (TYPE)
#' size = 7 size of each line
#' After that we use facet_grid to bring the plots of each topic (type), 
#' Next include the name of the title and subtitle that we want(specified in the 
#' firm of the function), after that theme_lares2 put the title and subtitle in 
#' a beautiful letter.
#' Each line need to have an explicit label = INSTITUTION of each observation
#' with the help of the geom_label located in x = LABEL_POS (that we defined in 
#' the dataset pull_info_dfs, that it is in our TIME_LINE_DF dataset from the function 
#' CREATING_DFS), remember that this variable was created to put INSTITUTION in
#' the central point of each line.
#' Then with geom_vline we add a vertical line at the end of each time in each 
#' group.
#' - the first theme quits the lines and adjust in each facet and remark the axis and the ticks.
#' - the next theme create a black border
#' - the last theme quit the labels
#' @param title that we want of the plot 
#' @param subtitle that we want of the plot
#' @return Creating time line cv plot
TIME_LINE_PLOT <- function(titulo, subtitulo){
  
  options(warn = -1)
  
  df <- pluck(CREATING_DFS(), "TIME_LINE_DF")
  
  df %>% ggplot(aes(x = TIME, y = reorder(TITLE, -POS), label = INSTITUTION)) + 
    geom_line(aes(color = TYPE), size = 7) + 
    
    facet_grid(TYPE ~ ., scale = "free", space = "free") +
    
    labs(title = titulo, 
         subtitle = subtitulo,
         x = NULL,
         y = NULL) + 
    
    lares::theme_lares2()  + #pondra letra bonita
    
    geom_label(aes(x = LABEL_POS), colour = "black", size = 1.8, alpha = 0.3) +  #label in each line
    
    geom_vline(xintercept = max(df$TIME), alpha = 0.8, linetype = "dotted") + #ultima linea vertical del plot
    
    theme(panel.grid = element_blank(),
          axis.line = element_line(color = "black"),
          axis.ticks = element_line(color = "black")) +
    
    theme(plot.background = element_rect(color = "black", fill = "white", size = 2)) + 
    
    theme(legend.position = "none") %>% 
    
    return()
}
# TIME_LINE_PLOT (titulo = "Curriculum Vitae Timeline",
#                 subtitulo = "Jorge Eduardo Trujillo Velázquez")

# 6.0 Creating Skills plot ------------------------------------------------

#' @description 
#' Create a dataset called *DF_SKILLS*, which have two variables:
#' 1. SKILLS: my language abilities
#' 2. ID: sequence from 1 to the end of each SKILL (to the times that SKILLS is 
#' repeated) this with the intention to create a dot plot where each point is 
#' each ID for each SKILL
#' Create a dataset called *DF_PERCENTACE_SKILLS* which have three variables:
#' 1.SKILLS
#' 2.ID
#' 3. PERCENTAGE: percentage to give to each SKILL
#' Create the final dataset *DF* that is a left join between the two previous 
#' datasets to add the PERCENTAGE variable to the first dataset in the last 
#' observation of each SKILL then we arrange the DF based on the SKILLS to show 
#' the dataset in order of the SKILLS.
#' We create the plot with the dataset where each observation is a SKILL where 
#' the Y axis is each image of PIMAGE
#' @return skills plot
SKILLS_PLOT <- function(){
  
  options(warn = -1)
  
  DF_SKILLS <- data.table(
    SKILLS = map2(list(c("SPARK", "OFFICE"), 
                       c("BOOSTRAP", "CSS", "GIT", 
                         "DOCKER", "AWS", "PYTHON", 
                         "SQL", "SHELL", "MONGODB"),
                       c("R", "SHINY", "RMARKDOWN")),
                  3:5, function(i,j)
                    map(i, ~rep(.x, j)) %>% flatten_chr()) %>% flatten_chr() %>% 
      as.factor()) %>% 
    .[,ID:=1:.N, by = "SKILLS"]
  
  DF_PERCENTACE_SKILLS <- DF_SKILLS[,.(ID,PERCENTAGE = .N/5.2), by = "SKILLS"] %>% 
    .[,.SD[.N], by = "SKILLS"] 
  
  DF <- data.table::merge.data.table(DF_SKILLS, 
                                     DF_PERCENTACE_SKILLS, 
                                     by = c("ID", "SKILLS"), all.x = TRUE) %>% 
    setorder(SKILLS) %>% 
    .[,PERCENTAGE:= ifelse(is.na(PERCENTAGE), 
                           as.numeric(PERCENTAGE),
                           as.character(scales::percent(PERCENTAGE)))]
  
  PLOT <- DF %>% ggplot(aes(x = SKILLS, y = ID)) + 
    geom_point(size = 10, shape = 21, fill = "#689fff") + 
    geom_hline(aes(yintercept = 6), linetype = 3, size = 2, col = "#689fff") + 
    geom_text(aes(label = PERCENTAGE), position = position_nudge(y = + 0.8), color = "#142ae8") + #add the text of the percentages
    coord_flip() + #flip the axes
    theme_void() + 
    theme(panel.background = element_rect(fill = '#f7fbff')) #background color
  
  abilities <- c("sql", "spark", "shiny", "shell", "markdown", 
                 "R", "python", "office","mongoDB",  "git", 
                 "docker", "css", "boostrap", "aws")
  
  paths_axis <- str_glue("RESUME_IMAGES/{abilities}.png") %>% as.list() %>% 
    set_names(abilities)
  
  PIMAGE <- cowplot::axis_canvas(plot = PLOT, axis = 'y') +  
    pmap(list(paths_axis,
              c(5.55, 5.15, 4.8, 4.4, 4.0, 
                3.6, 3.2, 2.8, 2.42, 2.07, 
                1.65, 1.25, 0.83, 0.45),
              c(rep(0.4, 2), 0.35, 0.33, 0.35, 
                rep(0.3, 2), 0.25, 0.65, 0.35, 
                0.45, rep(0.35, 2), 0.32)),
         ~ cowplot::draw_image(..1, y = ..2, scale = ..3))
  
  PLOT <- cowplot::ggdraw(plot = insert_yaxis_grob(plot = PLOT, 
                                                   grob = PIMAGE, 
                                                   position = "left")) #add the images in the y-axes
  
  ggsave(filename = "RESUME_IMAGES/plot.png",  #save the plot
         PLOT, width = 5, height = 6, 
         bg = "transparent") %>% return()
  message("Image Saved")
}
#SKILLS_PLOT()