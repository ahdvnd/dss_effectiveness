
packages <- c("dplyr", "readr", "plyr", "IPtoCountry", "crsra", "ggplot2", "gridExtra", "MatchIt")
instapack <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}
suppressMessages(suppressWarnings(instapack(packages)))


## for downloading the most updated data from survey monkey, go to Analyze Results
## then Exports, and All individual responses. In the window that pops up,
## choose XLS (it will be downloaded as csv), and choose the following:
## Original view + Condensed + Actual Answer Text and you should be good!

# Loading and cleaning data -----------------------------------------------

raw.data <- suppressMessages(suppressWarnings(read_csv('./data/raw_data/raw_data_2.csv'))) %>% 
    slice(-1L)
colnames(raw.data) <- c("respondentID",
                        "collectorID",
                        "start.date",
                        "end.date",
                        "IPaddress",
                        "email",
                        "first.name",
                        "last.name",
                        "custdata",
                        "consent",
                        "expertise.in.ds",
                        "were.employed",
                        "were.self.employed",
                        "old.job",
                        "old.job.other",
                        "old.income",
                        "ds.frequency.old",
                        "are.employed",
                        "same.job",
                        "are.self.employed",
                        "new.job",
                        "new.job.other",
                        "num.jobs.changed",
                        "ds.frequency.new",
                        "new.income",
                        "job.tenure",
                        "looking.jobs",
                        "switch.jobs.1yr",
                        "switch.jobs.2yr",
                        "dss.improved.skills",
                        "dss.helped.career",
                        "ds.training.only.jhu",
                        "ds.training.other.moocs",
                        "ds.training.bootcamps",
                        "ds.training.online.master",
                        "ds.training.on.campus",
                        "ds.training.online.content",
                        "ds.training.other",
                        "more",
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
                        "education",
                        "major",
                        "major.related.ds",
                        "motivation.gen.interest",
                        "motivation.relevant.job",
                        "motivation.relevant.studies",
                        "motivation.personal.growth",
                        "motivation.career.change",
                        "motivation.meet.people",
                        "motivation.experience.online",
                        "motivation.earn.cert",
                        "motivation.improve.english",
                        "motivation.prestigious",
                        "mentioned.interview",
                        "mentioned.linkedin",
                        "mentioned.resume",
                        "mentioned.colleagues",
                        "mentioned.manager",
                        "r_programming_user_id")

cleaned.data <- raw.data %>% 
    dplyr::filter(consent=="Yes") %>%
    dplyr::select(-email,
                  -first.name,
                  -last.name,
                  -custdata)

map = setNames(c(5000,
                 15000,
                 25000,
                 35000,
                 45000,
                 55000,
                 65000,
                 75000,
                 85000,
                 95000,
                 105000,
                 115000,
                 125000,
                 135000,
                 145000,
                 155000), 
               c("Under $10,000", 
                 "$10,000 - $20,000", 
                 "$20,000 - $30,000",
                 "$30,000 - $40,000",
                 "$40,000 - $50,000",
                 "$50,000 - $60,000",
                 "$60,000 - $70,000",
                 "$70,000 - $80,000",
                 "$80,000 - $90,000",
                 "$90,000 - $100,000",
                 "$100,000 - $110,000",
                 "$110,000 - $120,000",
                 "$120,000 - $130,000",
                 "$130,000 - $140,000",
                 "$140,000 - $150,000",
                 "Above $150,000"))


cleaned.data$old.income.num <- as.numeric(map[unlist(cleaned.data$old.income)])
cleaned.data$old.income.num[cleaned.data$were.employed=="No"] <- 0
cleaned.data$new.income.num <- as.numeric(map[unlist(cleaned.data$new.income)])
cleaned.data$new.income.num[cleaned.data$are.employed=="No"] <- 0

#-------------------------------------------------------------------------------
## replacing NA values in the old income var if they answered that their current
## is the same exact as their old job.
cleaned.data$new.income.num[!is.na(cleaned.data$same.job) & cleaned.data$same.job=="Yes, exactly"] <- 
    cleaned.data$old.income.num[!is.na(cleaned.data$same.job) & cleaned.data$same.job=="Yes, exactly"]

cleaned.data$num.jobs.changed[!is.na(cleaned.data$same.job) & cleaned.data$same.job=="Yes, exactly"] <- 0

cleaned.data$ds.frequency.new[!is.na(cleaned.data$same.job) & cleaned.data$same.job=="Yes, exactly"] <- 
    cleaned.data$ds.frequency.old[!is.na(cleaned.data$same.job) & cleaned.data$same.job=="Yes, exactly"]

cleaned.data$new.job[!is.na(cleaned.data$same.job) & cleaned.data$same.job=="Yes, exactly"] <- 
    cleaned.data$old.job[!is.na(cleaned.data$same.job) & cleaned.data$same.job=="Yes, exactly"]

# Saving the data ---------------------------------------------------------

saveRDS(cleaned.data, file="./data/tidy_data/cleaned_data.rds")
