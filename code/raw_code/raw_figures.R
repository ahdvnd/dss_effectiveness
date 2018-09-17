################################################################################
################# Number of jobs changed for pass/fail #########################

cleaned.data.course.progress.passed <- clean.progress %>% 
    filter(course_passing_state_id %in% c(1, 2))

cleaned.data.course.progress.failed <- clean.progress %>% 
    filter(course_passing_state_id==0)

proportion.1 <- prop.table(table(cleaned.data.course.progress.passed$num.jobs.changed))
proportion.2 <- prop.table(table(cleaned.data.course.progress.failed$num.jobs.changed))

combined.proportion <- rbind(proportion.1, proportion.2)
barplot(combined.proportion, beside=T, col=c("lightskyblue","coral"))
legend("topright", 
       legend = c("Passed", "Not Passed"), 
       fill = c("lightskyblue", "coral"))





################################################################################
################# Mentioned dumble bar #########################

data=data.frame(x=combined.mention$wherementioned, value1=combined.mention$before, value2=combined.mention$after)
# Reorder data using average?
data = data %>% rowwise() %>% mutate( mymean = mean(c(value1,value2))) %>% arrange(mymean) %>% mutate(x=factor(x, x)) %>% dplyr::arrange(value1)
# plot
ggplot(data) +
    geom_segment( aes(x=x, xend=x, y=value1, yend=value2), color="grey") +
    geom_point( aes(x=x, y=value1), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
    geom_point( aes(x=x, y=value2), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
    coord_flip() 
# With a bit more style
ggplot(data) +
    geom_segment( aes(x=x, xend=x, y=value1, yend=value2), color="grey") +
    geom_point( aes(x=x, y=value1), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
    geom_point( aes(x=x, y=value2), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
    coord_flip()+
    theme_light() +
    theme(
        legend.position = "top",
        panel.border = element_blank(),
    ) +
    xlab("Where mentioned taking the courses") +
    ylab("Income change from before to after taking the specialization")



################################################################################
################# Scatter plot of new versus old income ######################


plot.income.passed <- ggplot(subset(ctry_cutoff(50,1), pass.fail=="Passed"), aes(x=old.income.num, y=new.income.num)) + 
    geom_point() + 
    #geom_jitter(aes(col=pass.fail)) +
    geom_abline(intercept = 0, slope = 1, linetype = 2, col= "grey") +
    labs(title="All")

plot.income.notpassed <- ggplot(subset(ctry_cutoff(50,1), pass.fail=="Not Passed"), aes(x=old.income.num, y=new.income.num)) + 
    geom_point() + 
    #geom_jitter(aes(col=pass.fail)) +
    geom_abline(intercept = 0, slope = 1, linetype = 2, col= "grey") +
    labs(title="All")

grid.arrange(plot.income.passed, plot.income.notpassed, ncol=2)



# s1 <- cleaned.data %>%
#     dplyr::filter(major.related.ds=="Yes, very much related") %>% 
#     dplyr::filter(!is.na(major)) 
# 
# t1 <- table(cleaned.data$major)
# t2 <- table(s1$major)
# 

# t3 %>%  dplyr::mutate(V3=100*round(V2/V1,2))    



# library(tidyverse)
# 
# data.age <- cleaned.data %>% 
#     dplyr::filter(more=="Yes") %>% 
#     dplyr::filter(!is.na(age)) %>% 
#     dplyr::group_by(age) %>% 
#     dplyr::summarise(Total=n()) %>% 
#     dplyr::mutate(Share=round(Total/sum(Total),2)) %>% 
#     tibble::rowid_to_column("ID")
# # calculate the ANGLE of the labels
# number_of_bar=nrow(data.age)
# angle= 90 - 360 * (data.age$ID-0.5) /number_of_bar 
# # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
# # calculate the alignment of labels: right or left
# # If I am on the left part of the plot, my labels have currently an angle < -90
# data.age$hjust<-ifelse( angle < -90, 1, 0)
# 
# # flip angle BY to make them readable
# data.age$angle<-ifelse(angle < -90, angle+180, angle)
# # ----- ------------------------------------------- ---- #
# # Start the plot
# ggplot(data.age, aes(x=as.factor(ID), y=Total)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
#     
#     # This add the bars with a blue color
#     geom_bar(stat="identity", fill=alpha("skyblue", 0.7)) +
#     
#     # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
#     ylim(0,600) +
#     
#     # Custom the theme: no axis title and no cartesian grid
#     theme_minimal() +
#     theme(
#         axis.text = element_blank(),
#         axis.title = element_blank(),
#         panel.grid = element_blank(),
#         plot.margin = unit(rep(-2,4), "cm")      # Adjust the margin to make in sort labels are not truncated!
#     ) +
#     
#     # This makes the coordinate polar instead of cartesian.
#     coord_polar(start = 0) +
#     
#     # Add the labels, using the label_data dataframe that we have created before
#     geom_text(data=data.age, aes(x=ID, y=Total, label=age, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=4, angle= data.age$angle, inherit.aes = FALSE ) 
# 
# 
