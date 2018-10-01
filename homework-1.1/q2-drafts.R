heart_fam_yes <- heart[heart$famhist == 'Present',]
heart_fam_no <- heart[heart$famhist == 'Absent',]

#distribution of the two category of people considered (Famhist = Absent or Present)
barplot(table(heart$famhist))

library(ggplot2)

# boxplot for ldl for the two different category (Famhist = Absent or Present)
ggplot(data = heart, aes(y=heart$ldl, x=heart$famhist, col=heart$famhist))+ 
  geom_boxplot()+
  xlab('Family history')+
  ylab('ldl')+
  ggtitle('different distribution in ldl for the two family types')+
  labs(col="family history")

#median for ldl level for the two category (Famhist = Absent or Present) and the two Class (chd = 1 or 0)
interaction.plot(heart$famhist, heart$chd, heart$ldl, fun=median,col=c("red","green"),pch=c(15,19), type="b", trace.label="chd")

# histogram for the two category, separated
ggplot(heart_fam_no, aes(x=heart_fam_no$ldl, color = heart_fam_no$famhist)) +
  geom_histogram(fill="#F8766D") + 
  labs(color="familiy history")

ggplot(heart_fam_yes, aes(x=heart_fam_yes$ldl, color=heart_fam_yes$famhist )) +
  geom_histogram(fill='#00BFC4') +
  labs(color="familiy history")


#ggplot(heart, aes(x = heart$ldl, fill = heart$famhist)) +
#  facet_wrap(~heart$famhist) +
#  geom_histogram(binwidth = 1)

# and together
ggplot(heart, aes(x=(heart$ldl), fill=heart$famhist)) +
  geom_histogram()+
  labs(fill="familiy history")

# not stacked view
ggplot(heart, aes(x = heart$ldl, fill = famhist, group=famhist)) +
  geom_histogram(position=position_dodge()) +
  xlab("ldl") +
  ylab("Total Count") +
  labs(fill = "Family history")




# discretize the variable "ldl"
xs=quantile(heart$ldl,c(0,1/3,2/3,1))
xs[1]=xs[1]-.00005
heart$discr.ldl <- cut(heart$ldl, breaks = xs, labels = c('low','medium','high'))
summary(heart$discr.ldl)

#mosaic plot

table.heart=xtabs(~famhist+discr.ldl, data=heart)
prop.table(table.heart,1)
mosaicplot(table.heart,shade=TRUE, type="pearson",main="Correlation Famhist and ldl")

library("vcd")
mosaic(table.heart)
mosaic(table.heart, gp=shading_max, split_vertical=TRUE)
