hist(RestaurantTips$PctTip)
attach(RestaurantTips)


# how Values are distributed
ggplot(RestaurantTips, aes(x=PctTip)) +
  geom_histogram(fill='#00BFC4', color="black") +
  xlab("PctTip")
  
ggplot(RestaurantTips, aes(x=Bill)) +
  geom_histogram(fill='#00BFC4', color="black") +
  xlab("Bill")

ggplot(RestaurantTips, aes(x=Tip)) +
  geom_histogram(fill='#00BFC4', color="black") +
  xlab("Tip")

ggplot(RestaurantTips, aes(x=Bill, y=PctTip)) +
  geom_point(color = 'red')

cor(Bill, PctTip, method = "pearson")
cor.test(Bill, PctTip, method = 'pearson')


Rest_no_outlier = RestaurantTips[PctTip < 30,]
cor(Rest_no_outlier$Bill, Rest_no_outlier$PctTip, method = "pearson")

if(!require(coin)){install.packages("coin")}
library(coin)
independence_test(Bill ~ PctTip,
                  distribution = approximate(B = 10000),
                  data = RestaurantTips )

symmetry_test(Bill ~ PctTip | Individual
              data = RestaurantTips)


r.obt <- cor(Bill, PctTip, method = "pearson")
cat("The obtained correlation is ",r.obt,'\n')
nreps <- 10000
r.random <- numeric(nreps)

y_matrix <- matrix(PctTip, nrow=nreps, ncol=length(PctTip),byrow=TRUE)
# need to transpose the matrix after calling the 'apply' function
# wondering why it change the matrix shape
y_matrix_shuffled <- t(apply(y_matrix, MARGIN=1, FUN=function(x) permute(x))) 
corr_vector <- apply(y_matrix_shuffled, MARGIN=1, FUN = function(x) cor(x, Bill))
prob <- length(corr_vector[corr_vector >= r.obt])/nreps

cat("Probability randomized r >= r.obt [pvalue] ",prob)

hist(corr_vector, breaks = 50, main =  expression(paste("Distribution around ",rho, "= 0")), xlab = "r from randomized samples")
r.obt <- round(r.obt, digits = 2)
legend(.40, 200, r.obt, bty = "n")
arrows(.5,150,.53, 10)


install.packages('gtools')
library(gtools)
permute(PctTip)
