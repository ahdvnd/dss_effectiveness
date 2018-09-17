prop_wt <- function(x){
    
    final_data <- x %>%
        mutate(
            p = predict(glm(pass.fail ~ expertise.in.ds +
                                were.employed +
                                #old.income.num +
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
                            data = x, family = binomial("logit")),
                        type = "response"), 
            p_0 = 1 - p, 
            p_assign = case_when( ## Predicted probability of being assigned trt
                pass.fail == 1 ~ p,
                pass.fail == 0 ~ p_0
            ),
            atm = pmin(p, p_0) / p_assign,
            ato = 1 - p_assign,
            ate = 1 / p_assign,
            att = p / p_assign,
            atc = p_0 / p_assign
        ) %>% 
        select(new.income.num, pass.fail, passed, education.level, gender, age, major, country, white, atm, ato, ate, att, atc)

    #prop.table(table(clean.progress.mutated$education.level))
    #prop.table(svytable(~education.level, design = clean.progress.weighted))

    return(final_data)
    
}








## creating sample data set

# X <- mvtnorm::rmvnorm(
#     n = 1000,
#     mean = c(0.5, 1),
#     sigma = matrix(c(1, 1, 1, 2), ncol = 2)
# )
# x_1 <- X[, 1]
# x_2 <- X[, 2]
# U <- rnorm(1000, 0, 1)
# V <- rnorm(1000, 0, 1)
# z <- as.numeric(-0.5 + 0.25 * x_1 + 0.25 * x_2 + V > 0)
# y <- 1 + z + x_1 + 2 * x_2 + U
# 
# dat <- tibble(
#     y = y,
#     z = z,
#     x_1 = x_1,
#     x_2 = x_2
# )
