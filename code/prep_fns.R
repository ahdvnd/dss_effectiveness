# Loading and cleaning data -----------------------------------------------

income_vars <- c("cont.pass.fail", "pass.fail", "passed", "cont.passed", "new.income.num", # do I really need both new income and old income in matching?
                   "old.income.num", "were.employed", #"ds.frequency.old", # removed this variable since it has so many missing
                   "gender", "age", "white", "education", "major", # do I really need major?
                   "major.related.ds", "expertise.in.ds", "motive.career", "only.jhu",
                   "country", "respondentID")

mobility_vars <- c("num.jobs.changed", "looking.jobs", "switch.jobs.1yr", "switch.jobs.2yr", "same.job")

matching_vars <- c(income_vars, mobility_vars)

clean <- readRDS("./data/tidy_data/cleaned_data.rds")
users <- readRDS("./data/tidy_data/r_programming_users.rds")
user_ids <- readRDS("./data/tidy_data/jhu_user_ids.rds")
passed.courses <- readRDS("./data/tidy_data/passed_courses.rds")

clean.progress <- clean %>% 
    dplyr::left_join(user_ids, by="r_programming_user_id", `copy`=TRUE) %>% 
    dplyr::left_join(passed.courses, by="jhu_user_id", `copy`=TRUE) %>% 
    dplyr::mutate(passed = replace(passed, is.na(passed), 0)) %>% 
    dplyr::mutate(cont.passed = replace(cont.passed, is.na(cont.passed), 0)) 
    

clean.users <- clean %>%
    dplyr::left_join(user_ids, by="r_programming_user_id", `copy`=TRUE) %>% 
    dplyr::left_join(users, by = "jhu_user_id", `copy`=TRUE)

ctry_name <- function(nn, incl_india_china){
    if (incl_india_china==FALSE){
        l <- as.data.frame(prop.table(table(clean$country))) %>% 
            dplyr::arrange(-Freq) %>% 
            slice(1:nn) %>% 
            pull(Var1)
        l <- l[! l %in% c("India", "China")]
    } else {
        l <- as.data.frame(prop.table(table(clean$country))) %>% 
            dplyr::arrange(-Freq) %>% 
            slice(1:nn) %>% 
            pull(Var1)
    }
    return(l)
    }

ctry_cutoff <- function(cutoff, nn){
    clean.country.cutoff <- clean.progress %>% 
        dplyr::filter(country %in% ctry_name(nn, incl_india_china=incl_india_china)) %>% 
        dplyr::mutate(pass.fail = ifelse(passed >= cutoff,
                                         "Passed", "Not Passed")) %>% 
        dplyr::mutate(cont.pass.fail = ifelse(cont.passed >= cutoff,
                                         paste0("Cont. >= ", cutoff, 
                                                " %"), "Not Passed"))
    return(clean.country.cutoff)
}

ctry_cutoff_minmax <- function(cutoff, cutoff1, cutoff2, nn){
    clean.country.cutoff <- clean.progress %>% 
        dplyr::filter(country %in% ctry_name(nn, incl_india_china=incl_india_china)) %>% 
        dplyr::mutate(pass.fail = ifelse(passed >= cutoff,
                                         "Passed", "Not Passed")) %>% 
        dplyr::mutate(cont.pass.fail = ifelse(cont.passed >= cutoff,
                                              paste0("Cont. >= ", cutoff, 
                                                     " %"), "Not Passed")) %>% 
        dplyr::mutate(pass.group = case_when(
            passed<= cutoff1 ~ paste(cutoff1, "% of courses", sep = ""),
            passed > cutoff1 & passed<=cutoff2 ~ paste("Between ", cutoff1, " to ", cutoff2, "% of courses", sep = ""),
            passed > cutoff2 ~ paste("More than ", cutoff2, "% of courses", sep = "")))
    return(clean.country.cutoff)
}


ctry_cutoff_weighted <- function(cutoff, cutoff1, cutoff2, nn){
    df <- var_prep_minmax(cutoff = cutoff, cutoff1 = cutoff1, cutoff2 = cutoff2, nn = nn)
    df <- mutation(df) %>% 
        prop_wt() %>% 
        dplyr::mutate(pass.group = case_when(
            passed<= cutoff1 ~ paste(cutoff1, "% of classes"),
            passed > cutoff1 & passed<=cutoff2 ~ paste("Between", cutoff1, "to", cutoff2, "% of classes"),
            passed > cutoff2 ~ paste("More than", cutoff2, "% of classes")))
    return(df)
}

# race_minus_white <- clean %>%
#     select(contains("ethnicity")) %>% 
#     colnames() %>% 
#     setdiff("ethnicity.white")

var_prep <- function(x){

    clean.progress.matching <- x %>% 
        dplyr::mutate(white = ifelse(ethnicity.white == "White / Caucasian", 
                                     "White", "Non-White")) %>% 
        dplyr::mutate(nonwhite = ifelse(!is.na(ethnicity.indian) |
                                            !is.na(ethnicity.asian) |
                                            !is.na(ethnicity.black) | 
                                            !is.na(ethnicity.carribean) | 
                                            !is.na(ethnicity.east.asian) | 
                                            !is.na(ethnicity.hispanic) |
                                            !is.na(ethnicity.mideastern) |
                                            !is.na(ethnicity.north.african) |
                                            !is.na(ethnicity.south.asian) |
                                            !is.na(ethnicity.other), 1, 0)) %>%
        dplyr::mutate(white = replace(white, nonwhite==1, "Non-White")) %>% 
        dplyr::mutate(only.jhu = ifelse(ds.training.only.jhu == "JHU data science specialization was the only data science program I participated in" |
                                            ds.training.online.content == "Youtube videos/Other online resources",
                                        "Only_JHU", "Other Programs")) %>% 
        dplyr::mutate(non.jhu = ifelse(!is.na(ds.training.other.moocs) |
                                           !is.na(ds.training.bootcamps) |
                                           !is.na(ds.training.online.master) | 
                                           !is.na(ds.training.on.campus), 1, 0)) %>% 
        dplyr::mutate(only.jhu = replace(only.jhu, non.jhu==1, "Other Programs")) %>%
        dplyr::mutate(motive.career = ifelse(motivation.career.change == "For career change" | 
                                                 motivation.relevant.job == "Relevant to job",
                                             "Career Related", "Other Reasons")) %>% 
        dplyr::mutate(motive.non.career = ifelse(is.na(motivation.career.change) & 
                                                     is.na(motivation.relevant.job) &
                                                     (!is.na(motivation.personal.growth) |
                                                          !is.na(motivation.gen.interest) |
                                                          !is.na(motivation.earn.cert) | 
                                                          !is.na(motivation.prestigious) | 
                                                          !is.na(motivation.experience.online) | 
                                                          !is.na(motivation.meet.people) |
                                                          !is.na(motivation.relevant.studies) |
                                                          !is.na(motivation.improve.english)), 1, 0)) %>% 
        dplyr::mutate(motive.career = replace(motive.career, motive.non.career==1, "Other Reasons")) %>%
        
        #   dplyr::mutate(gender = ifelse(gender == "Male", 1, ifelse(gender == "Female", 0, NA))) %>%  # Female = 0, Male = 1
        dplyr::filter(gender!="Other") %>% 
        dplyr::select(matching_vars)
    return(clean.progress.matching)
}


var_prep_minmax <- function(cutoff = cutoff, cutoff1 = cutoff1, cutoff2 = cutoff2, nn = nn){
    
    clean.progress.matching <- ctry_cutoff_minmax(cutoff = cutoff, cutoff1 = cutoff1, cutoff2 = cutoff2, nn = nn) %>% 
        dplyr::mutate(white = ifelse(ethnicity.white == "White / Caucasian", 
                                     "White", "Non-White")) %>% 
        dplyr::mutate(nonwhite = ifelse(!is.na(ethnicity.indian) |
                                            !is.na(ethnicity.asian) |
                                            !is.na(ethnicity.black) | 
                                            !is.na(ethnicity.carribean) | 
                                            !is.na(ethnicity.east.asian) | 
                                            !is.na(ethnicity.hispanic) |
                                            !is.na(ethnicity.mideastern) |
                                            !is.na(ethnicity.north.african) |
                                            !is.na(ethnicity.south.asian) |
                                            !is.na(ethnicity.other), 1, 0)) %>%
        dplyr::mutate(white = replace(white, nonwhite==1, "Non-White")) %>% 
        dplyr::mutate(only.jhu = ifelse(ds.training.only.jhu == "JHU data science specialization was the only data science program I participated in" |
                                            ds.training.online.content == "Youtube videos/Other online resources",
                                        "Only_JHU", "Other Programs")) %>% 
        dplyr::mutate(non.jhu = ifelse(!is.na(ds.training.other.moocs) |
                                           !is.na(ds.training.bootcamps) |
                                           !is.na(ds.training.online.master) | 
                                           !is.na(ds.training.on.campus), 1, 0)) %>% 
        dplyr::mutate(only.jhu = replace(only.jhu, non.jhu==1, "Other Programs")) %>%
        dplyr::mutate(motive.career = ifelse(motivation.career.change == "For career change" | 
                                                 motivation.relevant.job == "Relevant to job",
                                             "Career Related", "Other Reasons")) %>% 
        dplyr::mutate(motive.non.career = ifelse(is.na(motivation.career.change) & 
                                                     is.na(motivation.relevant.job) &
                                                     (!is.na(motivation.personal.growth) |
                                                          !is.na(motivation.gen.interest) |
                                                          !is.na(motivation.earn.cert) | 
                                                          !is.na(motivation.prestigious) | 
                                                          !is.na(motivation.experience.online) | 
                                                          !is.na(motivation.meet.people) |
                                                          !is.na(motivation.relevant.studies) |
                                                          !is.na(motivation.improve.english)), 1, 0)) %>% 
        dplyr::mutate(motive.career = replace(motive.career, motive.non.career==1, "Other Reasons")) %>%
        #   dplyr::mutate(gender = ifelse(gender == "Male", 1, ifelse(gender == "Female", 0, NA))) %>%  # Female = 0, Male = 1
        dplyr::filter(gender!="Other") %>% 
        dplyr::select(matching_vars)
    
    return(clean.progress.matching)
}


income_before_after <- function(cutoff, nn, var){
    income.passed <- ctry_cutoff(cutoff=cutoff, nn=nn) %>% 
        dplyr::group_by_(var) %>%
        dplyr::summarise(before=mean(old.income.num, na.rm = TRUE),
                         after=mean(new.income.num, na.rm = TRUE)) %>% 
        `colnames<-`(c("group", "before", "after")) %>% 
        as.data.frame()
    income.passed <- filter(income.passed, !is.na(group))
    return(income.passed)
}


motiv.ment <- function(x, cutoff = cutoff, nn = nn){
    motivation.mentioned <- x %>%
        select(contains("motivation"), contains("mentioned")) %>% 
        colnames()
    for (i in motivation.mentioned){
        x <- ctry_cutoff(cutoff, nn) %>%
            dplyr::group_by_(i) %>% 
            dplyr::summarise(before=mean(old.income.num, na.rm = TRUE), 
                             after=mean(new.income.num, na.rm = TRUE)) %>% 
            `colnames<-`(c("group", "before", "after")) %>% 
            as.data.frame() %>% 
            filter(!is.na(group))
        assign(paste("income.", i, sep = ""), x)
    }
    
    combined.motivation <- bind_rows("Relevant to Job"=income.motivation.relevant.job,
                                     "Personal Growth"=income.motivation.personal.growth,
                                     "Career Change"=income.motivation.career.change,
                                     "General Interst"=income.motivation.gen.interest,
                                     "Earn a Certificate"=income.motivation.earn.cert,
                                     "Offered by Prestigious University"=income.motivation.prestigious,
                                     "Experience Online Education"=income.motivation.experience.online,
                                     "Meet People"=income.motivation.meet.people,
                                     "Relevant to Studies"=income.motivation.relevant.studies,
                                     "Improve My English"=income.motivation.improve.english, .id = "motivation")
    
    combined.mention <- bind_rows("To Manager/Supervisor"=income.mentioned.manager,
                                  "To Colleagues"=income.mentioned.colleagues,
                                  "On Linkedin"=income.mentioned.linkedin,
                                  "On Resume"=income.mentioned.resume,
                                  "During Interviews"=income.mentioned.interview, .id = "wherementioned")
    c <- list(combined.mention, combined.motivation)
    names(c) <- c("mention", "motivation")
    return(c)
}



## for resampling the data set based on weights
logsumexp <- function (x) {
    y = max(x)
    y + log(sum(exp(x - y)))
}

softmax <- function (x) {
    exp(x - logsumexp(x))
}