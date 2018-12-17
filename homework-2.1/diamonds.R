library(knitr)
library(gridExtra)
library(ggplot2)

diamonds <- read.table("/home/mehdi/HW-diamonds.txt", header=FALSE, col.names = c('caratage', 'color_purity', 'clarity', 'cert', 'price'))
kable(diamonds[1:5,])
summary(diamonds)

G1 <- ggplot(diamonds, aes(x=caratage, y=price)) + geom_point() + geom_smooth(method = "lm")
G2 <- ggplot(diamonds, aes(x=caratage, y=log(price))) + geom_point() + geom_smooth(method = "lm")
grid.arrange(G1,G2,ncol=2)

diamonds$color_purity = relevel(diamonds$color_purity, ref="I")
diamonds$clarity      = relevel(diamonds$clarity, ref="VS2")
diamonds$cert         = relevel(diamonds$cert, ref="HRD")

lm1 = lm(formula = log(price) ~ caratage + color_purity + clarity + cert, data=diamonds)
summary(lm1)

#New variable cat_carat created
diamonds$cat_carat<-cut(diamonds$caratage, c(0,0.5,1,1.1))
diamonds$cat_carat <- as.character(diamonds$cat_carat)
diamonds$cat_carat[diamonds$caratage <= 0.5] <- "small"
diamonds$cat_carat[caratage > 0.5 & caratage <= 1] <- "medium"
diamonds$cat_carat[caratage > 1] <- "large"
diamonds$cat_carat <- as.factor(diamonds$cat_carat)
diamonds$cat_carat = relevel(diamonds$cat_carat, ref = "small")

lm2 = lm(formula = log(price) ~ caratage + color_purity + clarity + cert + caratage:cat_carat, data = diamonds)
summary(lm2)
