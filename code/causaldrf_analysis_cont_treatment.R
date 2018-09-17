
cont.df <- clean.progress.mutated %>% 
    mutate(log.new.income.num = log(new.income.num+1)) %>% 
    as.data.frame()

## hirano-imbens estimator
hi_model <- causaldrf::hi_est(Y = log.new.income.num,
                  treat = passed,
                  treat_formula = passed ~ expertise.in.ds +
                      were.employed +
                      gender +
                      age +
                      white +
                      as.factor(education.level) +
                      major +
                      major.related.ds +
                      motive.career +
                      country +
                      only.jhu,
                  outcome_formula = log.new.income.num ~ passed +
                      gps +
                      as.factor(cont.df$education.level) +
                      cont.df$gender +
                      cont.df$age +
                      cont.df$major +
                      cont.df$country +
                      cont.df$white,
                  data = cont.df,
                  grid_val = seq(10, 100, by = 10),
                  treat_mod = "Normal")

summary(hi_model)


### result for men/women

cont.df <- clean.progress.mutated %>% 
    mutate(log.new.income.num = log(new.income.num+1)) %>% 
    filter(gender=="Male") %>% 
    as.data.frame()

summary(causaldrf::hi_est(Y = new.income.num,
                              treat = passed,
                              treat_formula = passed ~ expertise.in.ds +
                                  were.employed +
                                  age +
                                  white +
                                  as.factor(education.level) +
                                  major +
                                  major.related.ds +
                                  motive.career +
                                  country +
                                  only.jhu,
                              outcome_formula = new.income.num ~ passed +
                                  gps +
                                  as.factor(cont.df$education.level) +
                                  cont.df$major +
                                  cont.df$country +
                                  cont.df$white,
                              data = cont.df,
                              grid_val = seq(10, 100, by = 10),
                              treat_mod = "Normal"))




## linear regression
summary(lm(cont.df$new.income.num ~ cont.df$passed + 
       as.factor(cont.df$education.level) +
       cont.df$gender +
       cont.df$age +
       cont.df$major +
       cont.df$country +
       cont.df$white))


# BART estimator
# this usually TAKES SOOOOOOOO LONG
bart_model <- bart_est(Y = log.new.income.num,
                           treat = passed,
                 outcome_formula = log.new.income.num ~ passed +
                     as.factor(education.level) +
                     gender +
                     age +
                     major +
                     country +
                     white,
                           data = cont.df,
                           grid_val = seq(0, 100, by = 10))

summary(bart_model)

## additive spline estimator
add_spl_estimate <- add_spl_est(Y = new.income.num,
                                treat = passed,
                                treat_formula = passed ~ expertise.in.ds +
                                    were.employed +
                                    gender +
                                    age +
                                    white +
                                    as.factor(education.level) +
                                    major +
                                    major.related.ds +
                                    motive.career +
                                    country +
                                    only.jhu,
                                data = cont.df,
                                grid_val = seq(0, 100, by = 10),
                                knot_num = 3,
                                treat_mod = "Normal",
                                link_function = "inverse")

summary(add_spl_estimate)

## GAM estimator
gam_estimate <- gam_est(Y = new.income.num,
                        treat = passed,
                        treat_formula = passed ~ expertise.in.ds +
                            were.employed +
                            gender +
                            age +
                            white +
                            as.factor(education.level) +
                            major +
                            major.related.ds +
                            motive.career +
                            country +
                            only.jhu,
                        data = cont.df,
                        grid_val = seq(0, 100, by = 10),
                        treat_mod = "Normal",
                        link_function = "inverse")

## The inverse probability of treatment weighting (iptw) estimator
iptw_estimate <- iptw_est(Y = new.income.num,
                          treat = passed,
                          treat_formula =  passed ~ expertise.in.ds +
                              were.employed +
                              gender +
                              age +
                              white +
                              as.factor(education.level) +
                              major +
                              major.related.ds +
                              motive.career +
                              country +
                              only.jhu,
                          numerator_formula = passed ~ 1,
                          data = cont.df,
                          degree = 2,
                          treat_mod = "Normal",
                          link_function = "inverse")



