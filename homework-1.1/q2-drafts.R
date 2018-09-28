heart_fam_yes <- heart[heart$famhist == 'Present',]
heart_fam_no <- heart[heart$famhist == 'Absent',]


ggplot(data = heart, aes(y=heart$ldl, x=heart$famhist, col=heart$famhist))+ 
  geom_boxplot()

interaction.plot(heart$famhist, heart$chd, heart$ldl, fun=median,col=c("red","green"),pch=c(15,19), type="b", trace.label="chd")

barplot(table(heart$famhist))

hist(heart_fam_no$ldl)
hist(heart_fam_yes$ldl)

hist(heart$ldl)
hist(log(heart$ldl) ~heart$famhist)

library(ggplot2)
ggplot(heart, aes(x=(heart$ldl), fill=heart$famhist)) +
  geom_histogram()
