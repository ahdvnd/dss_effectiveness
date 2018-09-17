prop_match <- function(x){
    # k measures the ratio of control units to treatment units
    # This is to figure out whether to use nearest neighbor or full matching
    # A detailed description of when to use different methods based on the 
    # ratio of control to treatment can be found in the paper by Stuart on matching
    k <- floor(length(x$pass.fail[x$pass.fail==0]) / 
                   length(x$pass.fail[x$pass.fail==1]))
    if(k>=3) {
        m.out <- suppressMessages(suppressWarnings(matchit(pass.fail ~ old.income.num + 
                                                               #new.income.num + # this doesn't have to be added since old.income.num is already included.
                                                               expertise.in.ds +
                                                               were.employed +
                                                               #ds.frequency.old +
                                                               gender +
                                                               age +
                                                               white +
                                                               education.level +
                                                               major +
                                                               major.related.ds +
                                                               only.jhu +
                                                               motive.career,
                                                           data = x,
                                                           method = "nearest",
                                                           exact = x$country,
                                                           ratio = k)))
        cont.m.out <- suppressMessages(suppressWarnings(matchit(cont.pass.fail ~ old.income.num + 
                                                                    #new.income.num + # this doesn't have to be added since old.income.num is already included.
                                                                    expertise.in.ds +
                                                                    were.employed +
                                                                    #ds.frequency.old +
                                                                    gender +
                                                                    age +
                                                                    white +
                                                                    education.level +
                                                                    major +
                                                                    major.related.ds +
                                                                    only.jhu +
                                                                    motive.career,
                                                                data = x,
                                                                method = "nearest",
                                                                exact = x$country,
                                                                ratio = k)))
    } else {
        m.out <- suppressMessages(suppressWarnings(matchit(pass.fail ~ old.income.num + 
                                                               #new.income.num +
                                                               expertise.in.ds +
                                                               were.employed +
                                                               #ds.frequency.old +
                                                               gender +
                                                               age +
                                                               white +
                                                               education.level +
                                                               major +
                                                               major.related.ds +
                                                               motive.career +
                                                               country +
                                                               only.jhu,
                                                           data = x,
                                                           method = "full")))
        cont.m.out <- suppressMessages(suppressWarnings(matchit(cont.pass.fail ~ old.income.num + 
                                                                    #new.income.num +
                                                                    expertise.in.ds +
                                                                    were.employed +
                                                                    #ds.frequency.old +
                                                                    gender +
                                                                    age +
                                                                    white +
                                                                    education.level +
                                                                    major +
                                                                    major.related.ds +
                                                                    motive.career +
                                                                    country +
                                                                    only.jhu,
                                                                data = x,
                                                                method = "full")))
    }
    
    # summary(m.out)
    # summary(m.out.cont)
    # 
    # 
    # match.data(m.out) %>% 
    #     dplyr::group_by(pass.fail) %>% 
    #     dplyr::summarise(avg = mean(new.income.num))
    
    
    s1 <- summary(lm(new.income.num ~ pass.fail + 
                   education.level +
                   gender +
                   age +
                   major +
                   country +
                   white,
               data = match.data(m.out)))
    
    
    s2 <- summary(lm(new.income.num ~ cont.pass.fail + 
                   education.level +
                   gender +
                   age +
                   major +
                   country +
                   white,
               data = match.data(cont.m.out)))
    
    
    ## simple regression of continuous passing on income
    # log of income
    # interaction terms
    s3 <- summary(lm(log(new.income.num+1) ~ cont.passed +
                   #cont.passed * education.level +
                   education.level +
                   gender +
                   age +
                   major +
                   country +
                   white,
               data = x))
    l <- list(pf = s1,
              cont_pf = s2,
              cont.pf_reg = s3)
    return(l)
}
    



