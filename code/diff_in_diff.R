# difference-in-difference estimation -------------------------------------

clean.did <- bind_rows(clean.progress.2, 
                       clean.progress.2, .id = "time") %>% 
    select(new.income.num, old.income.num, pass.fail, time, gender,
           passed, age, white, education, major, major.related.ds,
           expertise.in.ds, country, ds.training.only.jhu) %>% 
    mutate(time = replace(time, time == 1, 0)) %>% 
    mutate(time = replace(time, time == 2, 1)) %>% 
    mutate(income = ifelse(time==0, old.income.num, new.income.num)) %>% 
    mutate(interaction = as.numeric(time) * pass.fail) %>% 
    filter(!is.na(income))

model.1 <- lm(income ~ pass.fail + time + interaction, data = clean.did)
model.2 <- lm(income ~ pass.fail + time + interaction + gender + factor(age) + white, data = clean.did)
model.3 <- lm(income ~ pass.fail + time + interaction + gender + factor(age) + white + factor(education) , data = clean.did)

library(stargazer)

stargazer(model.1, model.2, model.3, 
          title="Difference in Differences Results",
          header=FALSE, keep.stat="n", digits=2,
          report = "vc*",
          align=TRUE)