# Loading all the packages ------------------------------------------------

packages <- c("dplyr", "readr", "plyr", "IPtoCountry", "ggplot2", "gridExtra", "reshape2", "tidyr", "data.table",
              "MatchIt", "cobalt", "skimr", "stargazer", "Amelia", "summarytools", "Zelig", "ffbase")
instapack <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}
suppressMessages(suppressWarnings(instapack(packages)))



# Importing coursera data -------------------------------------------------

crsra_import <- function(workdir = "./data/raw_data/databases") {

# This is for disconnecting all databases since there is a max of 16
     dirnames <- list.dirs(path = workdir, full.names = FALSE)
     dirnames <- dirnames[-1] # to remove the first empty folder name (check if this line will be an issue in windows)
#This is to check whether the working directory is pointing at the right folder.
     dircheck <- list.files(file.path(workdir, dirnames), 
                            full.names = FALSE, 
                            recursive = TRUE, 
                            include.dirs = FALSE, 
                            pattern = "course_branch_grades.csv")
     if (length(dircheck) == 0) {
         stop("Please make sure you have set your working directory 
              to where the Coursera data dump is located.")
     }
     numcourses <<- length(dirnames)
########################################################################
  
# creates a list and populates it with each course and all the tables for each course
     populate <- function(x) {
         names = list.files(pattern="*.csv", path = file.path(workdir, x))
         tablenames <<- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(names))
         suppressMessages(suppressWarnings(all <- purrr::map(1:length(tablenames), 
                                                             ~ read_csv(paste(file.path(workdir, x), names[.x], sep="/")))))
         # names(all) <- tablenames
         # all[["peer_review_part_free_responses"]] <- as_data_frame(all[["peer_review_part_free_responses"]]) %>%
         #    dplyr::select("peer_assignment_id", "peer_assignment_review_schema_part_id", "peer_review_id", "peer_review_part_free_response_text")
     }
     all_tables <<- purrr::map(1:numcourses, ~ populate(dirnames[[.x]]))

# this changes the table names and corrects the columns of the table "peer_review_part_free_responses"
     for (i in 1:numcourses) {
         names(all_tables[[i]]) <<- tablenames
         all_tables[[i]][["peer_review_part_free_responses"]] <<- as_data_frame(all_tables[[i]][["peer_review_part_free_responses"]]) %>%
             dplyr::select("peer_assignment_id", 
                           "peer_assignment_review_schema_part_id", 
                           "peer_review_id", 
                           "peer_review_part_free_response_text")
     }
     
     coursenames <<- purrr::map(1:numcourses, ~ all_tables[[.x]][["courses"]]$course_name) # Extracts course names, 85 is the number of table associated with table courses
     names(all_tables) <<- coursenames # Assigns course names to the list courses
     partner_user_id <<- colnames(all_tables[[1]][["users"]])[1]
    
     
     message(" The following courses are loaded:")
     print(paste0(coursenames))
     
 }
 
# imports all the data in a list called call_tables 
crsra_import()


# creating the data frame for each course that contain all the information about progress
progress_calc <- function(x,y){
    
    all_grades <- x %>%
        mutate(pct_passed = round(100*course_grade_overall_passed_items/max(course_grade_overall_passed_items, na.rm = TRUE), 1)) %>% 
        select(jhu_user_id,
               course_passing_state_id,
               pct_passed,
               course_grade_overall,
               course_grade_ts)
    
    all_progress <- y %>% 
        select(-course_id, -course_item_id) %>%
        filter(course_progress_state_type_id==2) %>% 
        dplyr::filter(!is.na(course_progress_ts)) %>%
        dplyr::group_by(jhu_user_id) %>%
        dplyr::summarise(start.time = min(course_progress_ts),
                         stop.time = max(course_progress_ts)) %>% 
        dplyr::mutate(time.taken = round(as.numeric(difftime(stop.time, start.time, units="days")),1))
    
    progress_data <- all_grades %>% 
        left_join(all_progress, by = "jhu_user_id", `copy` = TRUE)
    
    return(progress_data)
}
    
progress_data <- purrr::map(1:numcourses, ~ progress_calc(all_tables[[.x]][["course_grades"]], 
                                                          all_tables[[.x]][["course_progress"]]))
names(progress_data) <- coursenames

# course progress across all the courses, binary completion
df1 <- bind_rows(progress_data, .id = "course") %>% 
    dplyr::filter(course_passing_state_id %in% c(1,2)) %>% 
    dplyr::group_by(jhu_user_id) %>% 
    dplyr::summarise(passed = round(100*n()/10,1))

# calculating a data friame for start and stop time and grade across all courses
df2 <- bind_rows(progress_data, .id = "course") %>% 
    dplyr::group_by(jhu_user_id) %>% 
    dplyr::summarise(start.time.all = min(start.time),
                     stop.time.all = max(stop.time),
                     grade.overall = sum(course_grade_overall, na.rm = TRUE)/n()) %>% 
    dplyr::mutate(time.taken.all = round(as.numeric(difftime(stop.time.all, start.time.all, units="days")),1))

# course progress across all the courses, continious completion
df.all.courses <- bind_rows(progress_data, .id = "course") %>% 
    dplyr::group_by(jhu_user_id) %>% 
    dplyr::summarise(cont.passed = sum(pct_passed, na.rm = TRUE)/10) %>% 
    dplyr::left_join(df1, by="jhu_user_id", `copy`=TRUE) %>% 
    dplyr::left_join(df2, by="jhu_user_id", `copy`=TRUE)


saveRDS(df.all.courses, "./data/tidy_data/passed_courses.rds")
