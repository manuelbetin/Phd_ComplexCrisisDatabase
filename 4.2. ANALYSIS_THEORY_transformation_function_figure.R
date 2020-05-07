
#' @title figures transformation function
#' @description figures explaining the transformation
#' function of the expert judgment from data to words
#' @author Manuel Betin
#' @return figures of the comparison functions

path_data_directory="../Betin_Collodel/2. Text mining IMF_data"

x <- seq(-4, 6, 0.01)
fx <- (x <= -2) * 4 +
  (x > -2 & x <= 0) * 3 +
  (x > 0 & x <= 2) * 2 +
  (x > 2 & x<=4) * 1+
  (x >= 5) * 0


labels=c("Severe \n recession","Soft \n recession","Weak economic \n growth","Sustained economic\n growth","Fast growing \n economy")

ggplot()+
  geom_point(aes(x=x, y=fx),color="lightgrey")+
  annotate("text",x=seq(-3,5,2),y=4.4:0.4,label=labels,size=3,color=c("darkred","darkred","darkgrey","darkgreen","darkgreen"))+
  geom_vline(xintercept=c(-4,-2,0,2,4,6),
             linetype="dotted",
             color=c("black","black","red","black","black","black")) +
  labs(x="GDP growth rate (%)",y="T(g)")+
  theme_light() +
  ggsave(paste0(path_data_directory,"/output/Transformation function/Transformation_function_GDP.png"))


x <- seq(-0.4, 0.6, 0.01)
fx <- (x <= -0.2) * 4 +
  (x > -0.2 & x <= 0) * 3 +
  (x > 0 & x <= 0.2) * 2 +
  (x > 0.2 & x<=0.4) * 1+
  (x >= 0.5) * 0


labels=c("Currency \n crisis","Large currency\n depreciation","Currency \n appreciation","Large currency\n appreciation","Unsustainable\n currency appreciation")

ggplot()+
  geom_point(aes(x=x, y=fx),color="lightgrey")+
  annotate("text",x=seq(-0.3,0.5,0.2),y=4.4:0.4,label=labels,size=3,color=c("darkred","darkred","darkgrey","darkgreen","darkgreen"))+
  geom_vline(xintercept=c(-0.4,-0.2,0,0.2,0.4,0.6),
             linetype="dotted",
             color=c("black","black","red","black","black","black")) +
  labs(x="Exchange rate growth rate (%)",y="T(g)")+
  theme_light() +
  ggsave(paste0(path_data_directory,"/output/Transformation function/Transformation_function_currency.png"))
