rm(list = ls())

library(purrr)

map(c("jsonlite","stringr", "data.table", "dplyr",
      "glue", "lubridate", "ggplot2","magick", 
      "pagedown", "scales", "cowplot", "usethis",
      "devtools", "lares"), 
    require, character.only = TRUE)



# Dataset creation  -------------------------------------------------------


#'@ The intention of this function is to create a dataset (extracted 
#' from a JSON file) for each topic where the information of that 
#' dataset is  our personal information of that topic, where we put:
#' - if an observation = today then we put the date of toyday for the END variable 
#' - each variable in upper case 
#' - each information of each row in upper case

structure_df <- function(topic){
  
  json_information <- jsonlite::fromJSON("PERSONAL_INFORMATION.json")
  
  json_information_topic <- json_information[[topic]]
  
  df <- data.table(
    
    Type = str_subset(string = names(json_information), pattern = topic),
    
    Title = json_information_topic[["TITLE"]],
    
    Location = json_information_topic[["LOCATION"]],
    
    Institution = json_information_topic[["INSTITUTION"]],
    
    Description = json_information_topic[["DESCRIPTION"]],
    
    Start = json_information_topic[["START"]],
    
    End = json_information_topic[["END"]]
    
    
  ) %>% 
    .[,End:=str_replace_all(string = End, 
                            pattern = "today", 
                            replacement = as.character(Sys.Date()))]  
  
  names(df) <- toupper(names(df))
  
  df <- df[,lapply(.SD, as.character), .SDcols = names(df)] 
  
  walk(names(df), DEFINING_HTTP_LINK, df) 
  
  df2 <- df
  
  df2 %>% return()  
}


#'@ This function is a subfunction of the function structure_df  
#' the intention of this function is to select where to put an upper case 
#' of an observation in  each link variable, this is: 
#' we check each variable of the dataset and if the observation of a variable
#' is a link ([observation](link)) then we only put in upper case everything 
#' between [] the intention of this is that the link works
#' in other case put the observation in upper case 

HTTP_LINK <- function(df_variable){
  
  return <- NA
  
  uppercase_bracket <- str_extract(string = df_variable, 
                                   pattern = "(?<=\\[)(.*?)(?=\\])") %>% keep(~!is.na(.x))
  
  if(str_detect(string = df_variable, pattern = "^\\[")) 
    
    return  <- str_replace_all(string = df_variable, 
                               pattern = "(?<=\\[)(.*?)(?=\\])", 
                               replacement = toupper(uppercase_bracket))
  
  else return <- toupper(df_variable)
  
  return(return)
}

#'@ This function is a subfunction of the function structure_df where we apply 
#' the HTTP_LINK function to a variable of the dataset iterating in each observation
#' with map_chr, we apply this function in a walk function to affect each variable of 
#' the dataset

DEFINING_HTTP_LINK <- function(df_variable, df){
  
  df[,(df_variable):=map_chr(get(df_variable), HTTP_LINK)]
  
}

#'@ The intention of this function is to create thre datasets:
#'  INFO_DF contain in tabular format all my personal information extracted from json file where
#'  - LABEL POS is the media from the START and the END date, with the intention that in the timeline plot
#'  we can put the title in this median point of the bar of each topic(type)

#'  PAGEDOWN_DF is the format of the dataset that contains the necessary information to create our pagedown cv where

#'  - get the year of START and END because in pagedown we are only going to show from which year to which year
#'  - arrage by the last year to get the information in order from lowest year to last year for each topic
#'  - TTIMELINE variable is the representation of interval of years (what we did in each interval) 

#'  TIME_LINE_DF dataset for creating the time line where 

#'  - POS variable is to duplicate observations of the dataset this with the intention to order the plot, 
#'  this is bring the bars of each topic(type) in order
#'  - TIME variable put START dates first and second the END datas as rowwn of TIME this with the intention that 
#'  the bar show the start and the end of each title of each topic(type)


CREATING_DFS <- function(){
  
  json_information <- jsonlite::fromJSON("PERSONAL_INFORMATION.json")
  
  vars_time <- c("START","END")
  vars_factor <- c("TYPE", "INSTITUTION")
  
  INFO_DF <- map_df(names(json_information), structure_df) %>% as.data.table() %>% 
    .[,(vars_time):=lapply(.SD, as.Date), .SDcols = vars_time] %>% 
    .[,(vars_factor):=lapply(.SD, as.factor), .SDcols = vars_factor] %>% 
    .[,LABEL_POS:=START + floor((END - START)/2)] #to create the label in each line of time line plot
  
  PAGEDOWN_DF <- INFO_DF[,-c("LABEL_POS")] %>%
    .[,':='(START = year(START), END = year(END))] %>% 
    setorder(END) %>% 
    .[,TIMELINE:=ifelse(test = START == END,
                        yes = END,
                        no = glue('{START} - {END}'))] %>% 
    .[,-c("START", "END")]
  
  
  vars_timeline <- c("TYPE", "TITLE", "INSTITUTION", "LABEL_POS")
  
  replacements <- c("\\[UNAM].*" = "UNAM",
                    "\\[RITTERDRAGON].*"=  "RITTERDRAGON",
                    "\\[NIELSEN].*"= "NIELSEN")
  
  replacement2 <- c("\\[DIPLOMA IN STATISTICS WITH R].*" = "DIPLOMA IN STATISTICS WITH R",
                     "\\[CARREER TRACK IN DATA SCIENCE WITH R].*"=  "CARREER TRACK IN DATA SCIENCE WITH R",
                     "\\[SQL  FOR DATA SCIENCE].*"= "SQL FOR DATA SCIENCE",
                     "\\[GIT  FOR DATA SCIENCE].*"= "Git FOR DATA SCIENCE")
  
  TIME_LINE_DF <- INFO_DF[,lapply(.SD, rep, 2), .SDcols = vars_timeline] %>% 
    .[,INSTITUTION:=str_replace_all(INSTITUTION, replacements)] %>% 
    .[,TITLE:=str_replace_all(TITLE, replacement2)] %>% 
    .[,POS:= rep(seq(from = 1, to = nrow(INFO_DF), by = 1),2)] %>% 
    .[,TIME:=c(INFO_DF$START, INFO_DF$END)]   # replicated before because of this part
  
  list(INFO_DF, TIME_LINE_DF,  PAGEDOWN_DF) %>% 
    set_names(c("INFO_DF", "TIME_LINE_DF", "PAGEDOWN_DF"))%>% 
    return()
}

#map(names(base_df), ~class(base_df[[.x]]))



# Format of pagedown ------------------------------------------------------

#'@ The intention of this function is to print the format that is required 
#' to print for each topic for pagedown 
#' we use $\n\n$ salto de linea to separate the ideas in the page of pagedown

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


# Creating Skills plot ------------------------------------------------


#'@ First of all we created the dataset DF_SKILLS, with two variables SKILLS and ID
#'this with the intention that ID represent a squence from 1 to the times that SKILLS 
#'is repeated, this with the intention that creat a dot plot where each point is each ID
#'observation of the respective SKILL
#'After that I created DF_PERCENTACE_SKILLS which PERCENTAGE is a percentage for each SKILLS
#'after that we merge it with DF_SKILLS to assign a percentage in the last observation of 
#'each SKILL, the plot is a dot plot where each plot is each observation of each SKILL where 
#'the Y axis is each image of PIMAGE

SKILLS_PLOT <- function(){
  
  options(warn = -1)
  
  DF_SKILLS <- data.table(
    SKILLS = c(rep("R", 5), rep("PYTHON", 4), rep("SQL", 4), rep("GIT", 4),
               rep("MARKDOWN", 5), rep("CSS", 4), rep("OFFICE", 3)) %>% 
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
    geom_text(aes(label = PERCENTAGE), position = position_nudge(y = + 0.8), color = "#142ae8") + 
    coord_flip() +
    theme_void() +
    theme(panel.background = element_rect(fill = '#d3eeff'))

  paths_axis <- str_c(
    
    str_c(getwd(), "IMAGES", sep = "/"),
    
    list.files(str_c(getwd(), "IMAGES", sep = "/")) %>% 
      str_subset(pattern = "css.*|git.*|markdown.*|office.*|python.*|R.*|sql.*") %>% 
      sort(),
    
    sep = "/") %>% as.list() %>% 
    set_names(c("CSS", "GIT", "MARKDOWN", "OFFICE", "PYTHON", "R", "SQL"))
  
  PIMAGE <-  axis_canvas(plot = PLOT, axis = 'y') +
    draw_image(paths_axis$SQL, y = 5.3, scale = 0.8) +
    draw_image(paths_axis$R, y = 4.5, scale = 0.7) +
    draw_image(paths_axis$PYTHON, y = 3.7, scale = 0.6) +
    draw_image(paths_axis$OFFICE, y = 2.9, scale = 0.5) +
    draw_image(paths_axis$MARKDOWN, y = 2.2, scale = 0.8) +
    draw_image(paths_axis$GIT, y = 1.4, scale = 0.6) +
    draw_image(paths_axis$CSS, y = 0.7, scale = 0.6)
  
  PLOT <- gdraw(insert_yaxis_grob(PLOT, PIMAGE, position = "left")) 
  
  ggsave(filename = str_c(getwd(), "IMAGES/plot.png", sep = "/"), 
         PLOT, width = 5, height = 6, 
         bg = "transparent") %>% return()
    
}


# Creating time line plot -------------------------------------------------


#'@ Creating time line cv plot
#'Explaining the function work:
#' arguments of the plot are:
#' - TIME in the x-axis because we want to see the changes in the time
#' - reorder(TITLE, -POS) y-axis is TITLE ordered by POS this will bring us the bars of each topic(type) in order
#' we use geom_line to get lines over time and we facet to bring the plots of each topic (type)
#' after that theme_lares2 put the title and subtitle in a beautiful letter
#' then in each line we put the label = INSTITUTION in the media of the line indicated with de LABEL_POS variable 
#' then we add a line at the end with geom_vline
#' the first theme quit the lines of ajust in each facet and remark the axis and the ticks
#' the next theme create a black border
#' the last theme quit the labels

TIME_LINE_PLOT <- function(df, titulo, subtitulo){
  
  options(warn = -1)
  
  df %>% ggplot(aes(x = TIME, y = reorder(TITLE, -POS), label = INSTITUTION)) + 
    geom_line(aes(color = TYPE), size = 7) + 
    
    facet_grid(TYPE ~ ., scale = "free", space = "free") +
    
    labs(title = titulo, 
         subtitle = subtitulo,
         x = NULL,
         y = NULL) + 
    
    theme_lares2()  + #pondra letra bonita
    
    geom_label(aes(x = LABEL_POS), colour = "black", size = 3, alpha = 0.3) +  #label in each line
    
    geom_vline(xintercept = max(df$Time), alpha = 0.8, linetype = "dotted") + #ultima linea vertical del plot
    
    theme(panel.grid = element_blank(),
          axis.line = element_line(color = "black"),
          axis.ticks = element_line(color = "black")) +
    
    theme(plot.background = element_rect(color = "black", fill = "white", size = 2)) + 
    
    theme(legend.position = "none") %>% 
    
    return()
  
}

