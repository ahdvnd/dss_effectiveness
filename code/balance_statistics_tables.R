

# Loading all the packages ------------------------------------------------

packages <- c("dplyr", "readr", "plyr", "IPtoCountry", "ggplot2", "gridExtra", "MatchIt", "cobalt", "skimr", "stargazer", "summarytools", "xtable")
instapack <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}
suppressMessages(suppressWarnings(instapack(packages)))


# Loading and cleaning data -----------------------------------------------



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




demog <- c("IPaddress", 
           "gender", 
           "age", 
           "marital.status", 
           "country",
           "ethnicity.indian", 
           "ethnicity.asian", 
           "ethnicity.black", 
           "ethnicity.carribean", 
           "ethnicity.east.asian", 
           "ethnicity.hispanic",
           "ethnicity.mideastern",
           "ethnicity.north.african",
           "ethnicity.south.asian",
           "ethnicity.white",
           "ethnicity.other",
           "were.employed",
           "are.employed",
           "education",
           "major",
           "country_cd",
           "region_cd",
           "profile_language_cd",
           "browser_language_cd",
           "reported_or_inferred_gender",
           "employment_status",
           "educational_attainment",
           "student_status")


sum.stat <- c("gender", 
           "age", 
           "marital.status", 
           "country",
           "were.employed",
           "were.self.employed",
           "old.job",
           "old.income",
           "ds.frequencey.old",
           "are.employed",
           "are.self.employed",
           "new.job",
           "new.income",
           "ds.frequency.new",
           "num.jobs.changed",
           "job.tenure",
           "looking.jobs",
           "switch.jobs.1yr",
           "switch.jobs.2yr",
           "education",
           "major",
           "major.related.ds",
           "expertise.in.ds",
           "dss.improved.skills",         
           "dss.helped.career")


# Balance Tables ------------------------------------------------------


# Gender (Shares in percentages)
rbind(round(100*prop.table(table(clean.users$gender)), 1), round(100*prop.table(table(clean.users$reported_or_inferred_gender)), 1))



# Country based on survey (Shares in percentages)
clean.users %>% 
    dplyr::filter(!is.na(country)) %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(Total=n()) %>%
    dplyr::mutate(Share=round(Total/sum(Total),2)) %>%
    dplyr::arrange(desc(Total))
# Country based on Coursera (Shares in percentages)
clean.users %>% 
    dplyr::filter(!is.na(country_cd)) %>%
    dplyr::group_by(country_cd) %>%
    dplyr::summarise(Total=n()) %>%
    dplyr::mutate(Share=round(Total/sum(Total),2)) %>%
    dplyr::arrange(desc(Total))



# Employment status based on survey (were.employed and NOT are.employed)
clean.users %>% 
    dplyr::filter(!is.na(were.employed)) %>%
    dplyr::group_by(were.employed) %>%
    dplyr::summarise(Total=n()) %>%
    dplyr::mutate(Share=round(Total/sum(Total),2)) %>%
    dplyr::arrange(desc(Total))
# Employment status based on coursera
clean.users %>% 
    dplyr::filter(!is.na(employment_status)) %>%
    dplyr::group_by(employment_status) %>%
    dplyr::summarise(Total=n()) %>%
    dplyr::mutate(Share=round(Total/sum(Total),2)) %>%
    dplyr::arrange(desc(Total))



# Eucation based on survey
clean.users %>% 
    dplyr::filter(!is.na(education)) %>%
    dplyr::group_by(education) %>%
    dplyr::summarise(Total=n()) %>%
    dplyr::mutate(Share=round(Total/sum(Total),2)) %>%
    dplyr::arrange(desc(Total))
# Eucation based on survey
clean.users %>% 
    dplyr::filter(!is.na(educational_attainment)) %>%
    dplyr::group_by(educational_attainment) %>%
    dplyr::summarise(Total=n()) %>%
    dplyr::mutate(Share=round(Total/sum(Total),2)) %>%
    dplyr::arrange(desc(Total))





# Summary statistics ------------------------------------------------------


# Ethnicities (Shares in percentages)
ethnicity.fun <- function(x){
    x = enquo(x)
    denum <- cleaned.data.users %>%
        filter(more=="Yes") %>% 
        filter(!is.na(ethnicity.white) | 
                   !is.na(ethnicity.asian) |
                   !is.na(ethnicity.south.asian) |
                   !is.na(ethnicity.hispanic) |
                   !is.na(ethnicity.east.asian) |
                   !is.na(ethnicity.other) |
                   !is.na(ethnicity.black) |
                   !is.na(ethnicity.mideastern) |
                   !is.na(ethnicity.east.asian) |
                   !is.na(ethnicity.north.african) |
                   !is.na(ethnicity.carribean) |
                   !is.na(ethnicity.indian))
    num <- denum %>%
        filter(!is.na(!!x)) 
    print(round(100*nrow(num)/nrow(denum),1))
}
# White
ethnicity.fun(ethnicity.white)
# Asian
ethnicity.fun(ethnicity.asian)
# South Asian
ethnicity.fun(ethnicity.south.asian)
# Hispanice
ethnicity.fun(ethnicity.hispanic)
# East Asian
ethnicity.fun(ethnicity.east.asian)
# Other
ethnicity.fun(ethnicity.other)
# Black
ethnicity.fun(ethnicity.black)


gh <- cleaned.data.users %>% 
    dplyr::select(sum.stat)
fg <- dfSummary(gh, style='grid', plain.ascii = FALSE, graph.col = FALSE, valid.col = FALSE, na.col = FALSE)
xtable(fg)


## Is coursera's gender inferring algorithm good?
#round(prop.table(table(cleaned.data.users$gender, cleaned.data.users$reported_or_inferred_gender))*100, 2)



