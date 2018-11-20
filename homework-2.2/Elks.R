source("code.R")
library("vcd")
library("vcdExtra")

Elks.table=data.frame(expand.grid(Action=c("Crossing", "Retreat"), Traffic=c("Low", "High"),
                                  Vehicle=c("Car", "Truck")), count=c(287,57,237,52,40,42,57,12))
Elks.table$Action = ordered(Elks.table$Action, levels = c("Crossing","Retreat"))
Elks.table$Vehicle = ordered(Elks.table$Vehicle, levels = c("Car","Truck"))
Elks.table$Traffic = ordered(Elks.table$Traffic, levels = c("Low","High"))

Elks.partial = xtabs(count~Action+Vehicle+Traffic, Elks.table)


## low traffic ##
Elks.low.Traffic = Elks.partial[,,1]
chisq.test(Elks.low.Traffic)
#il p-value e' piccolo, quindi rifiutiamo l'ipotesi dell'indipendenza
mosaic(t(Elks.low.Traffic), gp=shading_max, split_vertical=TRUE, labeling = labeling_values, main = "low traffic")
# confermato dal mosaic plot: i Retreat con i Truck sono molti di piu` di quello che ci si aspetta,
# anche i Crossing con Car abbondano, seppur in misura minone
# scarseggiano i Retrat con Car e i Crossing con Truck

## high traffic ##
Elks.high.Traffic = Elks.partial[,,2]
chisq.test(Elks.high.Traffic)
# p-value = 1, non possiamo rifiutare l'ipotesi di indipendenza
mosaic(t(Elks.high.Traffic), gp=shading_max, split_vertical=TRUE, labeling = labeling_values, main = "high traffic")
# mosaic sembrerebbe confermare l'indipendenza

fourfold(Elks.partial)


### odds ratio NO_LOG ###
#low traffic
or.low = oddsratio(Elks.low.Traffic, log=FALSE)
or.low
# or.low ~= 5.28 vuol dire che le possibilita  di attraversare quando passa una macchina 
# sono 5 volte di pi?? rispetto a quando passa un camion
confint(or.low)

#high traffic
or.high = oddsratio(Elks.high.Traffic, log=FALSE)
or.high
# or.high ~= 0.96 vuol dire che le possibilita  di attraversare quando passa una macchina 
# sono quasi quelle di quando passa un camion
confint(or.high)



### MARGINAL ###

Elks.marginal = xtabs(count~Action+Vehicle, Elks.table)
chisq.test(Elks.marginal)
# p-value piccolo, rifiutiamo ipotesi indipendenza

mosaic(t(Elks.marginal), gp=shading_max, split_vertical=TRUE, labeling = labeling_values, main = "marginal")
fourfold(Elks.marginal)



### HOMOGENUES ASSOCIATION ###

woolf_test(Elks.partial)
# p-value piccolo (~0.0001) ci dice che possiamo rifiutare l'ipotesi di omogeneita  di odds ratio tra le varie
# tabelle parziali, e quindi non possiamo prenderne una sola e generalizzare ma vanno discusse una per una.


### CONDITIONAL INDEPENDENCE 

mantelhaen.test(Elks.partial)
# altro p-value piccolo, le odd ratio non sono tutte 1