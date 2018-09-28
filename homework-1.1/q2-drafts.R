heart_fam_yes <- heart[heart$famhist == 'Present',]
heart_fam_no <- heart[heart$famhist == 'Absent',]

#distribution of the two category of people considered (Famhist = Absent or Present)
barplot(table(heart$famhist))

library(ggplot2)

# boxplot for ldl for the two different category (Famhist = Absent or Present)
ggplot(data = heart, aes(y=heart$ldl, x=heart$famhist, col=heart$famhist))+ 
  geom_boxplot()

#median for ldl level for the two category (Famhist = Absent or Present) and the two Class (chd = 1 or 0)
interaction.plot(heart$famhist, heart$chd, heart$ldl, fun=median,col=c("red","green"),pch=c(15,19), type="b", trace.label="chd")

# histogram for the two category, separated
ggplot(heart_fam_no, aes(x=(heart_fam_no$ldl))) +
  geom_histogram()
ggplot(heart_fam_yes, aes(x=(heart_fam_yes$ldl))) +
  geom_histogram()

# and together
ggplot(heart, aes(x=(heart$ldl), fill=heart$famhist)) +
  geom_histogram()
