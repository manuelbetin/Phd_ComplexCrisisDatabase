
Request=tf_barplot(mydata %>% filter(Review_number_new=="1 Request"),
                   vars_type = c("economic_shock","non_economic_shock","debt_outcomes"),
                   vars_nature = c("exogeneous","endogeneous"))

MidtermReview=tf_barplot(mydata %>% filter(Review_number_new=="2 midterm Review"),
                         vars_type = c("economic_shock","non_economic_shock","debt_outcomes"),
                         vars_nature = c("exogeneous","endogeneous"))

LastReview=tf_barplot(mydata %>% filter(Review_number_new=="3 Last Review"),
                      vars_type = c("economic_shock","non_economic_shock","debt_outcomes"),
                      vars_nature = c("exogeneous","endogeneous"))

Request$tf_fig_avg_prop+lims(y=c(0,0.5))+labs(title="Requests")
MidtermReview$tf_fig_avg_prop+lims(y=c(0,0.5))+labs(title="Midterm review")
LastReview$tf_fig_avg_prop+lims(y=c(0,0.5))+labs(title="Last review")

Request$tf_fig_avg+lims(y=c(0,0.05))+labs(title="Requests")
MidtermReview$tf_fig_avg+lims(y=c(0,0.05))+labs(title="Midterm review")
LastReview$tf_fig_avg+lims(y=c(0,0.05))+labs(title="Last review")

output[["timing_tf"]]=list(Requests=Request
                           , MidtermReview=MidtermReview
                           , LastReview=LastReview
                           )