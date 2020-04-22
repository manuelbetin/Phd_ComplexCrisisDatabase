
Request=tf_barplot(mydata %>% filter(review_number_new=="1 Request")#,
                   #vars_type = c("economic_shock","non_economic_shock","debt_outcomes"),
                   #vars_nature = c("exogeneous","endogeneous")
                   )

Midtermreview=tf_barplot(mydata %>% filter(review_number_new=="2 midterm Review")#,
                         #vars_type = c("economic_shock","non_economic_shock","debt_outcomes"),
                         #vars_nature = c("exogeneous","endogeneous")
                         )

Lastreview=tf_barplot(mydata %>% filter(review_number_new=="3 Last Review")#,
                      #vars_type = c("economic_shock","non_economic_shock","debt_outcomes"),
                      #vars_nature = c("exogeneous","endogeneous")
                      )

Request$tf_fig_avg_prop+lims(y=c(0,0.2))+labs(title="Requests")
Midtermreview$tf_fig_avg_prop+lims(y=c(0,0.2))+labs(title="Midterm review")
Lastreview$tf_fig_avg_prop+lims(y=c(0,0.2))+labs(title="Last review")

Request$tf_fig_avg+lims(y=c(0,0.02))+labs(title="Requests")
Midtermreview$tf_fig_avg+lims(y=c(0,0.02))+labs(title="Midterm review")
Lastreview$tf_fig_avg+lims(y=c(0,0.02))+labs(title="Last review")

output[["timing_tf"]]=list(Requests=Request
                           , Midtermreview=Midtermreview
                           , Lastreview=Lastreview
                           )


tfplot=tf_barplot(mydata
                   #vars_type = c("economic_shock","non_economic_shock","debt_outcomes"),
                   #vars_nature = c("exogeneous","endogeneous")
)


### tf-idf by type of crisis ------------------

mytf_plot=tf_barplot(mydata,vars_type=c("economic_shock","non_economic_shock","debt_outcomes"))

mytf_plot$tf_fig_avg+ggsave("2.graphs/tagged docs/tf-idf/tf_avg.png")

mytf_plot$tf_fig_avg_prop+ggsave("2.graphs/tagged docs/tf-idf/tf_avg_prop.png")
