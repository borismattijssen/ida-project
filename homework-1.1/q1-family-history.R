############# Dataset #############
# Describe dataset
dim(heart)
str(heart)

# Transform Coronary heart disease (chd) to a factor
heart$chd_fac = as.factor(heart$chd)

############# Visualisation #############

## Show relation between family history and chd
# create table
counts <- table(heart$chd, heart$famhist)
# transform to dataframe
table_df <- as.data.frame(prop.table(counts,2))
# add column with formatted text
table_df$Freq_lab = paste(round(table_df$Freq * 100, digits=2), "%")
# bar plot
ggplot(data=table_df, aes(x=Var2, y=Freq, fill=Var1)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  geom_text(aes(label=Freq_lab), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5) +
  xlab('Family history') +
  ylab('Frequency') + 
  guides(fill=guide_legend(title='chd')) + 
  ggtitle('Influence of Family History on Coronary Heart Disease')

tablex = xtabs(~famhist+chd, data=heart)
mosaic(tablex, gp=shading_max, split_vertical=TRUE)

# Split dataset in young and old (treshold = 40 years old)
heart_young <- heart[heart$age < 40,]
heart_old <- heart[heart$age >= 40,]

## Plot for young people (< 40)
# create table
counts_young <- table(heart_young$chd, heart_young$famhist)
# transform to dataframe
table_df_young <- as.data.frame(prop.table(counts_young,2))
# add column with formatted text
table_df_young$Freq_lab = paste(round(table_df_young$Freq * 100, digits=2), "%")
# bar plot
ggplot(data=table_df_young, aes(x=Var2, y=Freq, fill=Var1)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  geom_text(aes(label=Freq_lab), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5) +
  xlab('Family history') +
  ylab('Frequency') + 
  guides(fill=guide_legend(title='chd')) + 
  ggtitle('Influence of Family History on Coronary Heart Disease (age < 40)')


## Plot for old people (>= 40)
# create table
counts_old <- table(heart_old$chd, heart_old$famhist)
# transform to dataframe
table_df_old <- as.data.frame(prop.table(counts_old,2))
# add column with formatted text
table_df_old$Freq_lab = paste(round(table_df_old$Freq * 100, digits=2), "%")
# bar plot
ggplot(data=table_df_old, aes(x=Var2, y=Freq, fill=Var1)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  geom_text(aes(label=Freq_lab), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5) +
  xlab('Family history') +
  ylab('Frequency') + 
  guides(fill=guide_legend(title='chd')) + 
  ggtitle('Influence of Family History on Coronary Heart Disease (age >= 40)')
