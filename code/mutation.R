# recoding categorical variables for feeding into the amelia package

 map_age = setNames(c(1,
                      2,
                      3,
                      4,
                      5,
                      6,
                      7,
                      8,
                      9,
                      10,
                      11), 
                    c("17 or younger", 
                      "18-20", 
                      "21-25",
                      "26-30",
                      "31-35",
                      "36-40",
                      "41-45",
                      "46-50",
                      "51-55",
                      "56-60",
                      "61 or older"))
 
# 
 map_edu <- setNames(c(1,
                       2,
                       3,
                       4,
                       5,
                       6,
                       7,
                       8),
                     c("Less than high school degree",
                       "High school degree or equivalent (e.g., GED)",
                       "Some college but no degree",
                       "Associate degree",
                       "Bachelor degree",
                       "Master degree",
                       "Professional degree",
                       "Doctoral degree"))
# 
 map_major <- setNames(c(1,
                         2,
                         3,
                         4,
                         5,
                         6,
                         7,
                         8,
                         9,
                         10),
                       c("Arts",
                         "Business",
                         "Engineering",
                         "Health and medicine",
                         "Humanities",
                         "Law",
                         "Occupational",
                         "Science and math",
                         "Social sicences",
                         "other"))
# 
# map_major_related <- setNames(c(1,
#                                 2,
#                                 3),
#                               c("No, not at all",
#                                 "Yes, somewhat",
#                                 "Yes, very much related"))
# 
 map_exper_ds <- setNames(c(1,
                            2,
                            3),
                          c("Beginner",
                            "Intermediate",
                            "Advanced"))
#x$age <- as.numeric(map_age[unlist(x$age)])
 
#x$education <- as.numeric(map_edu[unlist(x$education)])
# x$major <- as.numeric(map_major[unlist(x$major)])
# x$major.related.ds <- as.numeric(map_major_related[unlist(x$major.related.ds)])

 
mutation <- function(x){
    x$passed <- as.numeric(x$passed)
    x$pass.fail <- as.factor(x$pass.fail)
    x$were.employed <- as.factor(x$were.employed)
    #x$ds.frequency.old <- as.factor(x$ds.frequency.old)
    x$gender <- as.factor(x$gender)
    x$age <- as.factor(x$age)
    x$white <- as.factor(x$white)
    x$education <- as.factor(x$education)
    x$major <- as.factor(x$major)
    x$major.related.ds <- as.factor(x$major.related.ds)
    x$expertise.in.ds <- as.factor(x$expertise.in.ds)
    x$motive.career <- as.factor(x$motive.career)
    x$country <- as.factor(x$country)
    x$only.jhu <- as.factor(x$only.jhu)
    x$new.income.num <- as.numeric(x$new.income.num)
    x$old.income.num <- as.numeric(x$old.income.num)
    
    #x$num.jobs.changed[x$num.jobs.changed == "5+"] <- 5
    #x$num.jobs.changed <- as.numeric(x$num.jobs.changed)
    
    
    if (mutate==TRUE){
        ## one problem of this mutation is that every time it's different
        mute.out <- amelia(x, 
                           m = mute_num, 
                           idvars = c("pass.fail",
                                      "major",
                                      "country",
                                      "white",
                                      "education",
                                      "expertise.in.ds",
                                      "were.employed",
                                      "major.related.ds", 
                                      "gender",
                                      "age",
                                      "motive.career"), # need to be retained but not used in mutation algorithm
                           cs = "respondentID", # identifier variable
                           noms = c("only.jhu")) # the ones used in mutation algorithm
        clean.progress.mutated <- mute.out$imputations[[whichm]] %>% 
            na.omit()
    } else {
        clean.progress.mutated <- x %>% 
            na.omit()
    }
    
    
    clean.progress.mutated <- clean.progress.mutated %>% 
        dplyr::mutate(pass.fail = ifelse(pass.fail=="Not Passed", 0, 1)) %>% 
        dplyr::mutate(cont.pass.fail = ifelse(cont.pass.fail=="Not Passed", 0, 1)) %>% 
        dplyr::mutate(education.level = ifelse(education %in% c("Less than high school degree",
                                                                "High school degree or equivalent (e.g., GED)",
                                                                "Associate degree",
                                                                "Some college but no degree"), "No college",
                                               ifelse(education == "Bachelor degree", "College",
                                                      ifelse(education %in% c("Master degree"), "Masters",
                                                             ifelse(education %in% c("Professional degree",
                                                                                     "Doctoral degree"), "Advanced", NA)))))
    return(clean.progress.mutated)
} 
 

