
ggplot(stagm, aes(x =Year_Measurement, y = Height_Total))+
  geom_jitter(size=.5)+
  xlab("Year Measurement")+
  ylab("Height (ft)")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))










































