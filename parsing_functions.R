rm(list = ls())

library(purrr)

map(c("jsonlite","stringr", "data.table", "dplyr",
      "glue", "lubridate", "ggplot2","magick", 
      "pagedown", "scales", "cowplot", "usethis",
      "devtools", "lares", "mongolite", "udeploy"), 
    require, character.only = TRUE)


# Read Data ---------------------------------------------------------------

Sys.setenv(R_CONFIG_ACTIVE = "db_cv")
#Sys.getenv()
config <- config::get(file = "config.yml")

# udeploy::mongo_Listread_or_write(do = "write",
#                                  json_to_write = jsonlite::fromJSON("PERSONAL_INFORMATION.json"))

udeploy::mongo_Listread_or_write(do = "read")

#' @Regex: define the possible regex to get a text from string (an observation)
#' @Useful: in  HTTP_LINK and CREATING_DFS functions
regex <- list(
  regex_left = ".*(?=\\[)", #everything before [
  regex_inside = "(?<=\\[)(.*?)(?=\\])", #everything between []
  regex_after = "(?<=\\)).*" #everything after )
)

bracket_regex <- glue("^\\[{regex$regex_inside}\\]") #start with [text]
join_regex <- glue("{regex$regex_left}\\[{regex$regex_inside}\\]\\(.*\\)(?:$|{regex$regex_after})") #text[text](text)no text|text

# Dataset creation  -------------------------------------------------------

vars_pagedown <- str_subset(string = names(json_information), pattern = "^(?!MYSELF).*") #everything except MYSELF

#' @description
#' 1.INFO_DF: with the help of the function *structure_df* that gets the dataset
#' of each topic, in here with the map_df, we bring in tabuilar form all my 
#' personal information extracted from json file, where 
#' -new variable LABEL_POS is the media from the START and the END date, with the 
#' intention that in the timeline plot (of the TIME_LINE_DF dataset) we can put
#' the INSTITUTION variable in the central point of the bar of each topic(type)
#' 
#' 2.PAGEDOWN_DF: first important dataset that will help us to create our pagedown 
#' cv
#' - get the year of START and END because in pagedown we are only going to show 
#' from which year to which year 
#' - arrage by END to get the information in order from the last year from the 
#' lowest to last in all the dataset
#' - define the variable TIMELINE, is important because in the cv we are going
#' to show only the year and if the start year is the same as the end year for 
#' each observation then only show the end year, and we only want that column 
#' that is why we delete START and END
#' 
#' 3.TIME_LINE_DF: second important dataset to create the timeline of our cv
#' First we replicate this dataset because of the variable TIME that we are
#' going to create, after that in the variables TITLE and INSTITUTION we are 
#' going to put in each observation that is [text](link) only text 
#' - POS is the sequence from 1 to the nrow of the dataset before and after the 
#' duplicated 1,2,..,N,1,2,...,N
#' -TIME we replicated the dataset because of this variable to put first the 
#' start dates of INFO_DF and then the final dates of INFO_DF his with the 
#' intention that the bar in the plot shows the start and the end of each 
#' TITLE 
#' 
#' variable is to duplicate observations of the dataset this with the intention to order the plot, 
#' @return list of three datasets
#' 1.INFO_DF: Reference Dataset to define the next two important datasets with
#' the help of the subfunction *structure_df*
#' 2.PAGEDOWN_DF: Dataset that represent all the necessary information in the
#' correct format to create our pagedown cv withe the help of the function 
#' [SECTION_PAGEDOWN]
#' 3.TIME_LINE_DF: Dataset that represent all the necessary information in the 
#' correct format to create the plot of time line with the help of the function
#' [TIME_LINE_PLOT]
CREATING_DFS <- function(){
  
  json_information_pagedown <- 
    map(vars_pagedown, ~ pluck(json_information, .x)) %>% set_names(vars_pagedown)
  
  vars_time <- c("START","END")
  vars_factor <- c("TYPE", "INSTITUTION")
  
  INFO_DF <- map_df(names(json_information_pagedown), structure_df) %>% as.data.table() %>% 
    .[,(vars_time):=lapply(.SD, as.Date), .SDcols = vars_time] %>% 
    .[,(vars_factor):=lapply(.SD, as.factor), .SDcols = vars_factor] %>% 
    .[,LABEL_POS:=START + floor((END - START)/2)] #to create the label in each line of time line plot
  
  PAGEDOWN_DF <- copy(INFO_DF)[,-c("LABEL_POS")] %>%
    .[,':='(START = year(START), END = year(END))] %>% 
    setorder(END) %>% 
    .[,TIMELINE:=ifelse(test = START == END,
                        yes = END,
                        no = glue('{START} - {END}'))] %>% 
    .[,-c("START", "END")]
  
  
  vars_timeline <- c("TYPE", "TITLE", "INSTITUTION", "LABEL_POS")
  vars_replacements <- c("TITLE", "INSTITUTION")
  replacement_regex <- glue("{bracket_regex}(.*)") #[text](text)
  
  TIME_LINE_DF <- copy(INFO_DF)[,lapply(.SD, rep, 2), .SDcols = vars_timeline] %>% #only variables in .SDcols and replicate observations
    .[,(vars_replacements):=lapply(.SD, as.character),.SDcols = vars_replacements]
  
  replacements <- map(list(TIME_LINE_DF$TITLE, TIME_LINE_DF$INSTITUTION),
                      ~str_extract(string = .x, pattern = regex$regex_inside)) %>% 
    set_names(vars_replacements) 
  
  walk2(vars_replacements, replacements, 
        ~TIME_LINE_DF[,(.x):=str_replace_all(string = get(.x),
                                             pattern = replacement_regex,
                                             replacement = .y)])
  TIME_LINE_DF[,':='(POS = rep(seq(from = 1, to = INFO_DF[,.N], by = 1), 2),
                     TIME = c(INFO_DF$START, INFO_DF$END))] # replicated before because of this part
  
  list(INFO_DF, TIME_LINE_DF,  PAGEDOWN_DF) %>% 
    set_names(c("INFO_DF", "TIME_LINE_DF", "PAGEDOWN_DF"))%>% 
    return()
}
#map(names(base_df), ~class(base_df[[.x]]))

#' @SUBFUNCTION
#' @description This function creates a dataset of each topic that we selected
#' (information of each topic is in the JSON file) where:
#' - if an observation = "today" then we put the date of today for the END variable
#' - each variable in upper case
#' - each information of each row in upper case with the help of the function 
#' *HTTP_LINK*
#' @param topic from the JSON file (personal information of each topic)
#' @return A dataset with the information of the topic
structure_df <- function(topic){
  
  json_information_pagedown <- 
    map(vars_pagedown, ~ pluck(json_information, .x)) %>% set_names(vars_pagedown)
  
  json_information_topic <- json_information_pagedown[[topic]]
  
  df <- data.table(
    
    Type = str_subset(string = names(json_information_pagedown), pattern = topic),
    
    Title = json_information_topic[["TITLE"]],
    
    Location = json_information_topic[["LOCATION"]],
    
    Institution = json_information_topic[["INSTITUTION"]],
    
    Description = json_information_topic[["DESCRIPTION"]],
    
    Start = json_information_topic[["START"]],
    
    End = json_information_topic[["END"]]
  )
  
  if(any(str_detect(string = df[,End], pattern = "today"))){
    df[,End:=str_replace_all(string = End, 
                             pattern = "today", 
                             replacement = as.character(Sys.Date()))]
  }else df
  
  df <- df %>% setnames(toupper(names(df))) %>% 
    .[,lapply(.SD, as.character), .SDcols = names(df)] 
  
  walk(names(df), ~df[,(.x):=HTTP_LINK(df_variable = get(.x))])
  
  df <- df
  
  df
}
#df <- structure_df(topic = "CERT")

#' @SUBFUNCTION
#' @description The intention of this function is to select where to put an 
#' upper case in each (thanks to the loop map_chr) observation of a variable 
#' 1. map_chr loop over all observations not only one observation (string)
#' (iterating in each observation)
#' 2. define the replacement of the information in the *regex list* that is going to be
#' the information that represent the regex in the string but in *upper case* defined in the 
#' uppercase list
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

# Format of pagedown ------------------------------------------------------
#' @description we filter the topic that we want from the general dataset 
#' PAGEDOWN_DF with all the information 
#' @param df datset PAGEDOWN_DF from the CREATING_DFS function
#' @param type topic we want to filter from the JSON file
#' @Note:we use $\n\n$ salto de linea to separate the ideas in the page of pagedown
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
#CREATING_DFS()$PAGEDOWN_DF %>% SECTION_PAGEDOWN('EDU')

# Creating time line plot -------------------------------------------------
#' @description  
#' arguments:
#' x-axis = TIME because we want to see the changes in the time
#' y-axis = reorder(TITLE, -POS) reorder TITLE (each line) of each group (TYPE)
#' based on the sequence POS, to have each group with each one 
#' label = INSTITUTION each line represent the INSTITUTION
#' color = based on each group (TYPE)
#' size = 7 size of each line
#' After that we use facet to bring the plots of each topic (type), including the
#' name of the title and subtitle that we want specified in the firm of the 
#' function, after that theme_lares2 put the title and subtitle in a beautiful 
#' letter.
#' Each line need to have explicited the label =INSTITUTION of each observation
#' with the help of the geom_label located in x = LABEL_POS that we defined in 
#' the dataset INFO_DF that is in our TIME_LINE_DF dataset from the function 
#' CREATING_DFS, remember that this variable was created to put INSTITUTION in
#' the central point of each line.
#' Then with geom_vline we add a vertical line at the end of each time in each 
#' group.
#' - the first theme quit the lines of ajust in each facet and remark the axis and the ticks
#' - the next theme create a black border
#' - the last theme quit the labels
#' @param title that we want of the plot 
#' @param subtitle that we want of the plot
#' @return Creating time line cv plot
TIME_LINE_PLOT <- function(titulo, subtitulo){
  
  options(warn = -1)
  
  df <- CREATING_DFS()$TIME_LINE_DF
  
  df %>% ggplot(aes(x = TIME, y = reorder(TITLE, -POS), label = INSTITUTION)) + 
    geom_line(aes(color = TYPE), size = 7) + 
    
    facet_grid(TYPE ~ ., scale = "free", space = "free") +
    
    labs(title = titulo, 
         subtitle = subtitulo,
         x = NULL,
         y = NULL) + 
    
    theme_lares2()  + #pondra letra bonita
    
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

# Creating Skills plot ------------------------------------------------
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
#' We create the plot with the dataset where each plot is each observation of 
#' each SKILL where the Y axis is each image of PIMAGE
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
  
  DF <- merge(DF_SKILLS, DF_PERCENTACE_SKILLS, by = c("ID", "SKILLS"), all.x = TRUE) %>% 
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
  
  paths_axis <- str_c("IMAGES",
                      str_subset(string = list.files("IMAGES"),
                                 pattern = "(?:spark.*|office.*|boostrap.*|css.*|git.*|docker.*|aws.*|python.*|sql.*|shell.*|mongo.*|R.png|shiny.*|markdown.*)") %>%
                        sort(),
                      sep = "/") %>% as.list() %>%
    set_names(c("AWS", "BOOSTRAP", "CSS", "DOCKER", "GIT", "MARKDOWN", "MONGODB",
                "OFFICE", "PYTHON", "R", "SHELL", "SHINY", "SPARK", "SQL"))
  
  PIMAGE <- axis_canvas(plot = PLOT, axis = 'y') +     #each selected image to be the y axis
    draw_image(paths_axis$SQL, y = 5.55, scale = 0.4) + 
    draw_image(paths_axis$SPARK, y = 5.15, scale = 0.4) + 
    draw_image(paths_axis$SHINY, y = 4.8, scale = 0.35) + 
    draw_image(paths_axis$SHELL, y = 4.4, scale = 0.33) + 
    draw_image(paths_axis$MARKDOWN, y = 4.0, scale = 0.35) + 
    draw_image(paths_axis$R, y = 3.6, scale = 0.3) + 
    draw_image(paths_axis$PYTHON, y = 3.2, scale = 0.3) + 
    draw_image(paths_axis$OFFICE, y = 2.8, scale = 0.25) + 
    draw_image(paths_axis$MONGODB, y = 2.42, scale = 0.65) + 
    draw_image(paths_axis$GIT, y = 2.07, scale = 0.35) + 
    draw_image(paths_axis$DOCKER, y = 1.65, scale = 0.45) + 
    draw_image(paths_axis$CSS, y = 1.25, scale = 0.35) + 
    draw_image(paths_axis$BOOSTRAP, y = 0.83, scale = 0.35) +
    draw_image(paths_axis$AWS, y = 0.45, scale = 0.32) 
  
  PLOT <- ggdraw(plot = insert_yaxis_grob(plot = PLOT, grob = PIMAGE, position = "left")) #add the images in the y-axes
  
  ggsave(filename = "IMAGES/plot.png",  #save the plot
         PLOT, width = 5, height = 6, 
         bg = "transparent") %>% return()
}
#SKILLS_PLOT()

