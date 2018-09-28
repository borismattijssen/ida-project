############# Dataset #############
# Describe dataset
dim(heart)
str(heart)

# Transform Coronary heart disease (chd) to a factor
heart$chd_fac = as.factor(heart$chd)

############# Visualisation #############

# Show relation between family history and chd
boxplot(heart$chd ~heart$famhist, xlab='Family history', ylab='Coronary heart disease')
counts <- table(heart$chd, heart$famhist)
barplot(counts, legend=c('chd=0', 'chd=1'), xlab='Family history')
counts_prop <- prop.table(counts,2)
counts_prop

# Split dataset in young and old (treshold = 40 years old)
heart_young <- heart[heart$age < 40,]
heart_old <- heart[heart$age >= 40,]

# Plot for young people (< 40)
counts_young <- table(heart_young$chd, heart_young$famhist)
barplot(counts_young, legend=c('chd=0', 'chd=1'), xlab='Family history (age < 40)')
counts_young_prop <- prop.table(counts_young,2)
counts_young_prop

# Plot for old people (>= 40)
counts_old <- table(heart_old$chd, heart_old$famhist)
barplot(counts_old, legend=c('chd=0', 'chd=1'), xlab='Family history (age >= 40)')
counts_old_prop <- prop.table(counts_old,2)
counts_old_prop

############# Statistics #############