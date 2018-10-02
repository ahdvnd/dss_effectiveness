

# par(mfrow=c(1,3))
# par(cex.lab=1.2, cex.axis=1.2, cex.main=1.2, cex.sub=1.2)
# col=c("gray86", "gray49")


raincloud_theme = theme(
    text = element_text(size = 10),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    legend.title=element_text(size=16),
    legend.text=element_text(size=16),
    legend.position = "right",
    plot.title = element_text(lineheight=.8, face="bold", size = 16),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))




par(family = "serif")
source("code/main_analysis.R")
library(RColorBrewer)


# Demographics ------------------
## -----------------------------------------
demog <- data.frame(
    all_cats = c("Female", "Female", 
                 "Age: 21-40", "Age: 21-40", 
                 "White", "White", 
                 "Reside in the US", "Reside in the US",
                 "College major related to data science", "College major related to data science",
                 "Currenly employed (full time)", "Currenly employed (full time)",
                 "Use data science everyday at work", "Use data science everyday at work",
                 "Married", "Married",
                 "Education: master's degree and above", "Education: master's degree and above",
                 "Only took JHU data science", "Only took JHU data science"),
    prop = c(0.68, 0.32, 0.38, 0.62, 0.51, 0.49, 0.65, 0.35, 
             0.34, 0.66, 0.31, 0.69, 0.68, 0.32, 0.48, 0.52,
             0.35, 0.65, 0.6, 0.4),
    group = c("other", "share", "other", "share", "other", "share", 
              "other", "share", "other", "share", "other", "share",
              "other", "share", "other", "share", "other", "share",
              "other", "share"))

demog_crsra <- data.frame(
    all_cats = c("Female", "Female", 
                 "Age: 21-40", "Age: 21-40", 
                 "White", "White", 
                 "Reside in the US", "Reside in the US",
                 "College major related to data science", "College major related to data science",
                 "Currenly employed (full time)", "Currenly employed (full time)",
                 "Use data science everyday at work", "Use data science everyday at work",
                 "Married", "Married",
                 "Education: master's degree and above", "Education: master's degree and above",
                 "Only took JHU data science", "Only took JHU data science"),
    prop = c(0.71, 0.29, 1, 0, 1, 0, 0.66, 0.34, 
             1, 0, 0.35, 0.65, 1, 0, 1, 0,0.39, 0.61, 1, 0),
    group = c("other", "share", "other2", "share2", "other2", "share2", 
              "other", "share", "other2", "share2", "other", "share",
              "other2", "share2", "other2", "share2", "other", "share",
              "other2", "share2"))


demog$all_cats <- factor(demog$all_cats,levels = c("Only took JHU data science",
                                                   "Use data science everyday at work",
                                                   "College major related to data science",
                                                   "Married",
                                                   "White",
                                                   "Age: 21-40",
                                                   "Currenly employed (full time)",
                                                   "Education: master's degree and above",
                                                   "Reside in the US",
                                                   "Female"))


demog_crsra$all_cats <- factor(demog_crsra$all_cats,levels = c("Only took JHU data science",
                                                   "Use data science everyday at work",
                                                   "College major related to data science",
                                                   "Married",
                                                   "White",
                                                   "Age: 21-40",
                                                   "Currenly employed (full time)",
                                                   "Education: master's degree and above",
                                                   "Reside in the US",
                                                   "Female"))

s1 <- ggplot(demog, aes(x=all_cats, y=prop, fill=group)) +
    scale_y_continuous(labels = percent_format()) +
    geom_bar(stat='identity') +
    geom_hline(yintercept=.5, linetype = 'dashed') +
    coord_flip() +
    theme_bw() +
    ylab('% of learners (Survey)') +
    xlab('') +
    scale_fill_manual(values=c('grey', 'red')) +
    theme(legend.position='none') +
    theme(axis.text.y=element_text(size=13, color="black"),
          axis.text.x=element_text(size=10, color="black"),
          panel.grid.major.y = element_line( size=.1, color="black"))

s2 <- ggplot(demog_crsra, aes(x=all_cats, y=prop, fill=group)) +
    scale_y_continuous(labels = percent_format()) +
    geom_bar(stat='identity') +
    geom_hline(yintercept=.5, linetype = 'dashed') +
    coord_flip() +
    theme_bw() +
    ylab('% of learners (Coursera data)') +
    xlab('') +
    scale_fill_manual(values=c('grey', 'white', 'cornflowerblue', 'white')) +
    theme(legend.position='none') +
    theme(axis.text.x=element_text(size=10, color="black"),
          axis.text.y=element_blank(),
          panel.grid.major.y = element_line( size=.1, color="black"))

demog.graph <- grid.arrange(s1, s2, ncol=2, widths = c(2/3, 1/3))

# figure 1 in paper
ggsave("graphs/comparison.pdf", plot=demog.graph, device = NULL, path = NULL,
       scale = 1, width = 8, height = 4.5, units = c("in"),
       dpi = 300, limitsize = TRUE)


# violin plot --------------------------------------------------------
# unweighted model

# d1 <- ggplot(ctry_cutoff(cutoff = 20, nn = 10), aes(x=pass.fail, y= new.income.num, fill = pass.fail)) + 
#     geom_violin(trim=FALSE)+
#     geom_boxplot(width=0.1, fill=c("#E69F00", "#56B4E9"))+
#     labs(x="Passing threshold at 20% of Courses", y = "Post-graduation Income (in $)") + 
#     scale_fill_manual(values=c("#E69F00", "#56B4E9")) + theme_classic() + guides(fill=FALSE) +
#     theme(axis.text=element_text(size=10)) + ylim(-30000, 200000)

source("code/raw_code/geom_flat_violin.R")
d1 <- ggplot(ctry_cutoff_minmax(cutoff = cutoff, cutoff1 = cutoff1, cutoff2 = cutoff2, nn = 10), aes(x=pass.group, y= new.income.num/1000, fill = pass.group)) +
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .7, trim = FALSE) +
    geom_boxplot(width=0.1, fill=c("#E69F00", "#56B4E9", "springgreen3"), alpha=0.7)+
    labs(x="Different treatment groups", y = "Post-graduation income (thousand $)") +
    scale_fill_manual(values=c("#E69F00", "#56B4E9", "springgreen3")) + theme_classic() + #+ guides(fill=FALSE) +
    scale_y_continuous(labels = dollar) +
    labs(caption="Panel (A)") +
    theme(axis.text.x=element_blank(),
          axis.text.y=element_text(size=10),
          plot.caption = element_text(hjust=0.5, size=rel(1)),
          legend.justification=c(1,1), 
          legend.position=c(1,1),
          legend.background = element_rect(fill=alpha('white', 0.4)),
          panel.grid.major.y = element_line( size=.1, color="grey"))
    
#d <- ctry_cutoff_weighted(cutoff = 50, cutoff1 = 30, cutoff2 = 60, nn = 10)
## weithed model
d <- prop_wt_out %>% 
    dplyr::mutate(`Passing Group` = case_when(
        passed<= cutoff1 ~ "0 courses completed",
        passed > cutoff1 & passed<=cutoff2 ~ paste("Between 1 to ", cutoff2/10, " courses completed", sep = ""),
        passed > cutoff2 ~ paste("More than ", cutoff2/10, " courses completed", sep = "")))
sampled_rows <- sample(1:NROW(d), size = NROW(d), prob = softmax(d$atm), replace = TRUE)
wd <- d[sampled_rows, ]
d2 <- ggplot(wd, aes(x=`Passing Group`, y= new.income.num/1000, fill = `Passing Group`)) +
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .7, trim = FALSE) +
    geom_boxplot(width=0.1, fill=c("#E69F00", "#56B4E9", "springgreen3"), alpha=0.7)+
    labs(x="Different treatment groups (weighted sample)", y = "Post-DSS income (thousand $)") +
    scale_fill_manual(values=c("#E69F00", "#56B4E9", "springgreen3")) + theme_classic() + #+ guides(fill=FALSE) +
    #labs(caption="Panel (B)") +
    theme(axis.text.x=element_blank(),
          axis.text.y=element_text(size=10),
          plot.caption = element_text(hjust=0.5, size=rel(1)),
          legend.justification=c(1,1), 
          legend.position=c(1,1),
          legend.background = element_rect(fill=alpha('white', 0.4)),
          panel.grid.major.y = element_line( size=.1, color="grey"))


violin <- grid.arrange(d1, d2, ncol=2)
ggsave("graphs/violin.pdf", plot=d2, device = NULL, path = NULL,
       scale = 1, width = 5, height = 4, units = c("in"),
       dpi = 300, limitsize = TRUE)

# grid.arrange(d2, contplot, roiplot, ncol=3, widths = c(1/3, 1/3, 1/3))

## plot of the effect of continuous treatment after using causaldrf and hi_model
# --------------------------------

cont.df$yhat <- hi_model$out_mod$fitted.values

ghplot <- cont.df %>% 
    filter(passed!=0) %>% 
    dplyr::group_by(passed) %>% 
    dplyr::summarise(mean = mean(yhat),
                     n=n(),
                     upper = mean + 1.96*sd(yhat)/sqrt(n),
                     lower = mean - 1.96*sd(yhat)/sqrt(n))


v <- hi_model$param
p <- data.frame(pct_increase_income = ghplot$mean,
                pct_completed = seq(10, 100, by = 10),
                upper = ghplot$upper,
                lower = ghplot$lower)


contplot <- ggplot(data=p, aes(x=pct_completed/10, y=pct_increase_income))+
    geom_point(colour = "grey15")+ 
    geom_line(colour = "grey15") + 
    geom_ribbon(aes(ymin=p$lower, ymax=p$upper), linetype=2, alpha=0.2, fill="#43b7ff") +
    labs(x="No. of courses completed", y = "Percent increase in income",
         caption = "Panel (B)") +
    theme_light() +
    scale_x_continuous(breaks = c(2,4,6,8,10)) +
    theme(text = element_text(size=11),
          plot.caption = element_text(hjust=0.5, size=rel(1)))
    #annotate(geom="text", x=20, y=10, label="Linear coefficient = 0.8%", color="black")

ggsave("graphs/income_increase.pdf", plot=contplot, device = NULL, path = NULL,
       scale = 1, width = 4, height = 4, units = c("in"),
       dpi = 300, limitsize = TRUE)


################# Motivation dumble bar #########################

v1 <- motiv.ment(clean, 50, 10)[["motivation"]]
data=data.frame(x=v1$motivation, value1=v1$before/1000, value2=v1$after/1000)
# Reorder data using average?
data = data %>% 
    rowwise() %>%
    mutate( mymean = mean(c(value1,value2))) %>% 
    arrange(mymean) %>% 
    mutate(x=factor(x, x)) %>% 
    dplyr::arrange(value1)
# plot
par(mar = c(2, 2, 2, 2))
# With a bit more style
mot <- ggplot(data) +
    geom_segment( aes(x=x, xend=x, y=value1, yend=value2), color="black") +
    geom_point( aes(x=x, y=value1), color="#E69F00", size=3 ) +
    geom_point( aes(x=x, y=value2), color="#56B4E9", size=3 ) +
    coord_flip()+
    theme_light() +
    theme(
        legend.position = "top",
        panel.border = element_blank(),
        text = element_text(size=14),
        axis.title.y = element_text(colour="black",size=11, family = "Arial"),
        axis.title.x = element_text(colour="black",size=11, family = "Arial"),
        axis.text.y = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust=0.5, size=rel(0.8))) +
    xlab("What motivated them to take classes") +
    ylab("Income (thousand $)")+
    labs(caption = "Panel (B)")


## return to investment (ROI) graph
# --------------------------------------

# numbers are from these papers:
# https://uknowledge.uky.edu/cgi/viewcontent.cgi?article=1061&context=ukcpr_papers
# https://www.newyorkfed.org/medialibrary/media/research/current_issues/ci20-3.pdf
# https://www.chegg.com/homework-help/questions-and-answers/find-best-possible-answer-following-follow-data-presented-graph-select-one--women-always-e-q28606467

roi <- data.frame(group = c("DSS, Male", "DSS, Female", 
                            "Associate, Male", "Associate, Female", 
                            "College, Male", "College, Female",
                            "Advanced, Male", "Advanced, Female"),
                  gender = c("Male", "Female", "Male", "Female",
                            "Male", "Female", "Male", "Female"),
                  return = c(5570, 19140, 1484, 2336, 25896, 15808, 19812, 11440),
                  cost = c(500, 500, 6000, 6000, 101636, 101636, 47800, 47800))

roiplot <- ggplot(roi, aes(x=cost, y=return)) +
    geom_point(alpha=0.8) +
    geom_text(aes(label=group), size=3.5, hjust=1.1, vjust=0.2, colour = "burlywood4") +
    theme_light() +
    labs(y = "Increase in earnings ($ - logarithmic scale)", x = "Cost of program ($ - logarithmic scale)",
         caption = "Panel (C)") +
    #coord_cartesian(xlim = c(0, 200000), ylim = c(0,10000)) +
    #scale_y_continuous(labels = dollar) +
    #scale_x_continuous(labels = dollar) +
    theme(text = element_text(size=11),
          plot.caption = element_text(hjust=0.5, size=rel(1)))+
    scale_x_log10(breaks = c(10, 100, 1000, 10000, 100000),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits = c(10, 200000)) +
    scale_y_log10(breaks = c(100, 1000, 10000, 100000),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits = c(100, 100000)) +
    annotation_logticks() 

ggsave("graphs/roi.pdf", plot=roiplot, device = NULL, path = NULL,
       scale = 1, width = 4, height = 4, units = c("in"),
       dpi = 300, limitsize = TRUE)




## scatter plot countries ----------------------------------------------------

cutoff = 50
lc <- as.data.frame(table(clean$country)) %>% 
         dplyr::arrange(-Freq) %>% 
         slice(1:nc)

ctry_cutoff_2 <- function(cutoff, ctry){
    clean.country.cutoff <- clean.progress %>% 
        dplyr::filter(country == ctry) %>% 
        dplyr::mutate(pass.fail = ifelse(passed >= cutoff,
                                         "Passed", "Not Passed")) %>% 
        dplyr::mutate(cont.pass.fail = ifelse(cont.passed >= cutoff,
                                              paste0("Cont. >= ", cutoff, 
                                                     " %"), "Not Passed"))
    return(clean.country.cutoff)
}

# to run this function, remove the variable country in the regression
# in file prop_wt.R
c = c(0)
nc = 15
for (i in 1:nc){
    a <- var_prep(ctry_cutoff_2(cutoff, lc$Var1[i]))
    if (all(mobility_vars %in% colnames(a))){
        a <- a %>% 
            select(-mobility_vars)
    }
    a.mutated <- mutation(a)
    W.out <- suppressWarnings(weightit(pass.fail ~ expertise.in.ds +
                          were.employed + gender + age + white +
                          education.level + major + major.related.ds +
                          motive.career + only.jhu,
                      data = a.mutated, estimand = "ATE", method = "ps"))
    
    d.w <- svydesign(ids = ~1, weights = get.w(W.out),
                     data = a.mutated)
    fit <- svyglm(log(new.income.num+1) ~  pass.fail + education.level +
                       gender + age + major + white, 
                  design = d.w)
    
    c[i] <- coef(fit)["pass.fail"]
}


sc <- data.frame("country" = lc$Var1[1:10],
                 "share" = c(34.6, 11.0, 3.7, 3.5, 3.5,
                             2.6, 2.5, 2.5, 2.1, 2.0),
                 "effect" = c[1:10],
                 "gdppc" = c(57638.2, 6570.6, 42656.2, 15123.9, 44819.5,
                             48860.5, 15529.1, 50538.6, 36304.9, 46012.3), stringsAsFactors = TRUE)

levels(sc$country) <- c(levels(sc$country), "UK", "US")
sc[3,1] <- "UK"
sc[1,1] <- "US"

ggplot(sc, aes(x=gdppc, y=effect, size=share)) +
    geom_point(alpha=0.8) +
    scale_colour_continuous(guide = FALSE) +
    geom_text(aes(label=country), size=3, nudge_x = 0.15, nudge_y = 0.15, check_overlap = F) +
    geom_hline(yintercept=0, color = "steelblue1") +
    geom_vline(xintercept=30000, color = "steelblue1") +
    theme(panel.grid.major = element_line(colour="grey", size = (0.1)),
          legend.justification=c(0.1,0.1), 
          legend.position=c(0.1,0.1),
          legend.background = element_rect(fill=alpha('white', 0.7)))+
    labs(size = "% all learners", y = "Effect (percentage increase in income)", x = "GDP/capita")
    

# income_before_after_passfail ------------------------------------------------------

#dplyr::bind_rows(income_before_after(0.2, 1, "pass.fail"))

graph_before_after <- function(df){
    colnames(df) <- c("group", "Before", "After")
    left_label <- paste0("$",format(round(df$`Before`), big.mark = ",", scientific = FALSE))
    right_label <- paste0("$",format(round(df$`After`), big.mark = ",", scientific = FALSE))
    df$class <- ifelse(df$group %in% c("Not Passed"), "lightgrey", "darkgrey")
    # Plot
    p <- ggplot(df) + geom_segment(aes(x=1, xend=2, y=`Before`, yend=`After`, col=class), size=.75, show.legend=F) + 
        geom_vline(xintercept=1, linetype="dashed", size=.1) + 
        geom_vline(xintercept=2, linetype="dashed", size=.1) +
        scale_color_manual(labels = c("Up", "Down", "Same"), 
                           values = c("lightgrey"="grey89", "darkgrey"="grey49")) +  # color of lines
        labs(x="", y="Mean Income in $", family="Times New Roman") +  # Axis labels
        xlim(0.5, 2.5) + ylim(40000,90000)  # X and Y axis limits
    
    # Add texts
    p <- p + geom_text(label=left_label, y=df$`Before`, x=rep(1, NROW(df)), hjust=1.1, size=3.5)
    p <- p + geom_text(label=right_label, y=df$`After`, x=rep(2, NROW(df)), hjust=-0.1, size=3.5)
    p <- p + geom_text(label="Before", x=1, y=1.1*(max(df$`Before`, df$`After`)), hjust=-0.1, size=5, family="Times New Roman")  # title
    p <- p + geom_text(label="After", x=2, y=1.1*(max(df$`Before`, df$`After`)), hjust=-0.1, size=5, family="Times New Roman")  # title
    
    # Minify theme
    pf.graph <- p + theme(panel.background = element_blank(), 
                          panel.grid = element_blank(),
                          axis.ticks = element_blank(),
                          axis.text.x = element_blank(),
                          panel.border = element_blank(),
                          plot.margin = unit(c(1,2,1,2), "cm")) 
    return(pf.graph)
}

g1 <- graph_before_after(income_before_after(20, 10, "pass.fail"))
g2 <- graph_before_after(income_before_after(50, 10, "pass.fail"))
g3 <- graph_before_after(income_before_after(80, 10, "pass.fail"))
g4 <- graph_before_after(income_before_after(50, 1, "pass.fail"))
grid.arrange(g1, g2, g3, g4, ncol=2)


# income_before_after_gender ------------------------------------------------------

#dplyr::bind_rows(income_before_after(0.2, 1, "pass.fail"))

graph_before_after_2 <- function(df){
    colnames(df) <- c("group", "Before", "After")
    left_label <- paste0("$",format(round(df$`Before`), big.mark = ",", scientific = FALSE))
    right_label <- paste(df$group, paste0("$",format(round(df$`After`), big.mark = ",", scientific = FALSE)),sep=", ")
    df$class <- ifelse(df$group %in% c("Not Passed"), "lightgrey", "darkgrey")
    # Plot
    p <- ggplot(df) + geom_segment(aes(x=1, xend=2, y=`Before`, yend=`After`, col=class), size=.75, show.legend=F) + 
        geom_vline(xintercept=1, linetype="dashed", size=.1) + 
        geom_vline(xintercept=2, linetype="dashed", size=.1) +
        scale_color_manual(labels = c("Up", "Down", "Same"), 
                           values = c("lightgrey"="grey89", "darkgrey"="grey49")) +  # color of lines
        labs(x="", y="Mean Income in $", family="Times New Roman")  # Axis labels
          # X and Y axis limits
    
    # Add texts
    p <- p + geom_text(label=left_label, y=df$`Before`, x=rep(1, NROW(df)), hjust=1.1, size=3.5)
    p <- p + geom_text(label=right_label, y=df$`After`, x=rep(2, NROW(df)), hjust=-0.1, size=3.5)
    p <- p + geom_text(label="Before", x=1, y=1.1*(max(df$`Before`, df$`After`)), hjust=-0.1, size=5, family="Times New Roman")  # title
    p <- p + geom_text(label="After", x=2, y=1.1*(max(df$`Before`, df$`After`)), hjust=-0.1, size=5, family="Times New Roman")  # title
    
    # Minify theme
    pf.graph <- p + theme(panel.background = element_blank(), 
                          panel.grid = element_blank(),
                          axis.ticks = element_blank(),
                          axis.text.x = element_blank(),
                          panel.border = element_blank(),
                          plot.margin = unit(c(1,0,1,2), "cm")) 
    return(pf.graph)
}

gg1 <- graph_before_after_2(income_before_after(20, 10, "gender")) + xlim(0.5, 2.5) + ylim(0,70000)
gg2 <- graph_before_after_2(income_before_after(50, 10, "education")) + xlim(0.5, 2.5) + ylim(0,70000)
grid.arrange(gg1, gg2, ncol=1)




################# Mention dumble bar #########################
v1 <- motiv.ment(clean, 50, 10)[["mention"]]
data=data.frame(x=v1$wherementioned, value1=v1$before, value2=v1$after)
# Reorder data using average?
data = data %>% 
    rowwise() %>%
    mutate( mymean = mean(c(value1,value2))) %>% 
    arrange(mymean) %>% 
    mutate(x=factor(x, x)) %>% 
    dplyr::arrange(value1)
# plot
par(mar = c(2, 2, 2, 2))
# With a bit more style
men <- ggplot(data) +
    geom_segment( aes(x=x, xend=x, y=value1, yend=value2), color="black") +
    geom_point( aes(x=x, y=value1), color="#E69F00", size=3 ) +
    geom_point( aes(x=x, y=value2), color="#56B4E9", size=3 ) +
    coord_flip()+
    theme_light() +
    theme(
        legend.position = "top",
        panel.border = element_blank(),
        text = element_text(size=14),
        axis.title.y = element_text(colour="black",size=12, family = "Arial"),
        axis.title.x = element_text(colour="black",size=12, family = "Arial"))+
    xlab("Where they mentioned the specialization") +
    ylab("Income change")


grid.arrange(mot, men, ncol=2)





## plot of fitted values after the regression ----------------------------------
## -----------------------------------------------------------------------------

# first run the model in the main_analysis.R file
dfd <- broom::augment(model)
residuals <- ggplot(dfd, aes(x = .fitted, y = .resid)) + 
    geom_point(color="bisque3") + 
    geom_smooth() + #method = "lm"
    theme_light() +
    labs(x="Fitted values", y="Residuals") +
    theme(axis.text.y=element_text(size=12, color="grey12"),
          axis.text.x=element_text(size=12, color="grey12"),
          axis.title=element_text(size=12),
          panel.grid.major.y = element_line( size=.1, color="grey"))+
    scale_x_continuous(labels = dollar) +
    scale_y_continuous(labels = dollar)


ggsave("graphs/residuals.pdf", plot=residuals, device = NULL, path = NULL,
       scale = 1, width = 6, height = 4, units = c("in"),
       dpi = 300, limitsize = TRUE)

## package WeightIt ----------------------------------
## balance of propensity score
# https://cran.r-project.org/web/packages/cobalt/vignettes/cobalt_A0_basic_use.html
W.out <- weightit(pass.fail ~ expertise.in.ds +
                      were.employed +
                      #old.income.num +
                      #ds.frequency.old +
                      gender +
                      age +
                      white +
                      as.factor(education.level) +
                      major +
                      major.related.ds +
                      motive.career +
                      country +
                      only.jhu,
                  data = clean.progress.mutated, estimand = "ATE", method = "ps")

summary(W.out)
#bal.tab(W.out, m.threshold = .1, disp.v.ratio = TRUE)

# d.w <- svydesign(ids = ~1, weights = get.w(W.out),
#                  data = clean.progress.mutated)
# summary(svyglm(new.income.num ~ 
#                    pass.fail + 
#                    education.level +
#                    gender +
#                    age +
#                    major +
#                    country +
#                    white, design = d.w))

distributions <- bal.plot(W.out, var.name = "prop.score", which = "both") +
    theme_light() +
    xlab("Propensity score") +
    ylab("Density") +
    ggtitle("") +
    scale_fill_manual(name="Group",
                        breaks=c(0, 1),
                        labels=c("Passed below the threshold", "Passed above the threshold"),
                        values = c("#999999", "#E69F00")) +
    theme(legend.justification=c(1,1), 
          legend.position=c(1,1),
          legend.background = element_rect(fill=alpha('white', 0.4)))

ggsave("graphs/distributions.pdf", plot=distributions, device = NULL, path = NULL,
       scale = 1, width = 8, height = 4, units = c("in"),
       dpi = 300, limitsize = TRUE)


## covariate balance ------------------------------
# https://cran.r-project.org/web/packages/cobalt/vignettes/cobalt_A0_basic_use.html

covbalance <- love.plot(bal.tab(W.out), threshold = .1, abs = TRUE, which.treat = NULL)

ggsave("graphs/covbalance.pdf", plot=covbalance, device = NULL, path = NULL,
       scale = 1, width = 8, height = 6, units = c("in"),
       dpi = 300, limitsize = TRUE)



# Histogram of continuous and binary completion
hist <- ggplot() + 
    geom_density(aes(x=clean.progress.matching$passed, fill = "Binary Completion"), binwidth = 1, alpha = 0.4) + 
    geom_density(aes(x=clean.progress.matching$cont.passed, fill = "Non-binary Completion"), binwidth = 1, alpha = 0.4) +
    xlim(c(-10, 100)) +
    #scale_fill_manual(name="Courses",values=cols) +
    theme(legend.position = c(0.8, 0.8)) +
    xlab("Percentage of the overall program completed") +
    ylab("Density") +
    theme_light() +
    theme(legend.justification=c(1,1), 
          legend.position=c(1,1),
          legend.background = element_rect(fill=alpha('white', 0.4))) +
    scale_fill_manual("Passing variable",values = c("#999999","#E69F00"))

ggsave("graphs/hist.pdf", plot=hist, device = NULL, path = NULL,
       scale = 1, width = 6, height = 4, units = c("in"),
       dpi = 300, limitsize = TRUE)


# passed_courses_hist ----------------------------
## this shows the histogram for the share of courses passed among our survey
# respondents. Most people haven't passed ANY courses. 
ggplot(clean.progress, aes(x=passed)) +
    geom_histogram(aes(y=..count../sum(..count..)), fill="grey45") +
    xlab("Share of courses passed")+
    ylab("Count")+
    theme_light()

