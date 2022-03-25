#Question 1.a

x1 = c()
n = 20000

##Calcul de la loi
x = runif(n, min = 0, max = 1)
## esperance de la série
esp = mean(x)
## variance de la série
sigmacarre = mean(x^2) - esp^2
##Def de Sn
Sn = cumsum(x)
N = seq(1, n, by = 1)
##Def de Zn
Zn = (sqrt(N) / sqrt(sigmacarre)) * ((Sn / N) - esp)

plot(Zn,main = "Convergence de Zn avec une loi Uniforme")

#Question 1.b

## f(x) correspond a une loi normale de paramètres N(µ,ø²)
x1 = c()

##esperance
esp = 1 / 2
##variance
sigmacarre = 1 / 12
n = 20000
tries = 100
#calcul loi uniforme
fsimu1 = function(echantillon) {
  for (i in 1:echantillon) {
    w = runif(1)
    x1[i] = qunif(w)
  }
  x1
}
# on a effectué la simulation de la v.a.r. de densité f(x) sur n
SumZn = c()
for (j in 1:n) {
  Sn = fsimu1(tries)
  tmpZn = (sqrt(tries) / sqrt(sigmacarre)) * ((sum(Sn) / tries) - esp)
  SumZn[j] = tmpZn
}

hist(SumZn,ylab = "Frequence", xlab = "Zn",main = "Zn sur 20000 test de taille N = 100 pour la loi Uniforme", breaks = 30,col = "red",probability = TRUE)

loiNormale <- seq(-5, 5, by = 0.01) # tracé de la loi normale
y <- dnorm(loiNormale)
lines(loiNormale, y, col = "blue")

# Question 2.a

x1 = c()
n = 20000

##Calcul de la loi
x = rexp(n)
##Esperance de la série
esp = mean(x)
##Variance
sigmacarre = (1 / (1/esp)^2)
Sn = cumsum(x)
N = seq(1, n, by = 1)
Zn = (sqrt(N) / sqrt(sigmacarre)) * ((Sn / N) - esp)


plot(Zn,main = "Convergence de Zn avec une loi Exponentielle")

# Question 2.b

x2 = c()

lambda = 0.5 
##Variance
sigmacarre = 1 / (lambda ^ 2)
##Esperance
esp = 1 / lambda

tries = 100
#calcul loi expo
fsimu2 = function(echantillon) {
  for (i in 1:echantillon) {
    w = runif(1)
    x2[i] = qexp(w,lambda)
  }
  x2
}
SumZn = c()
for (j in 1:n) {
  Sn = fsimu2(tries)
  tmpZn = (sqrt(tries) / sqrt(sigmacarre)) * ((sum(Sn) / tries) - esp)
  SumZn[j] = tmpZn
}

hist(SumZn,ylab = "Frequence", xlab = "Zn",main = "Simulation Zn sur 20000 tests de taille N = 100 pour la loi Exponentielle", breaks = 30,col = "red",probability = TRUE)

# tracé de la loi normale
loiNormale <- seq(-5, 5, by = 0.01)
y <- dnorm(loiNormale)
lines(loiNormale, y, col = "blue")



#Question 3.a

n = 20000
x = c()
for (i in 1:n) { # calcul de la loi
  w = runif(1)
  x[i] = sqrt(w)
}
esp = mean(x) # espérance de la série
sigmacarre = mean(x^2) - (esp^2) # variance de la série
Sn = cumsum(x)
N = seq(1, n, by = 1)
Zn = (sqrt(N) / sqrt(sigmacarre)) * ((Sn / N) - esp)

plot(Zn,main = "Convergence de Zn avec la loi de densité f(x) = 2x")

#Question 3.b

x3 = c()

tries = 100
sigmacarre = 1 / 18 # variance
esp = 2 / 3 # espérance
fsimu3 = function(echantillon) {
  # on a f(x) = 2x donc F(x) = x² et F⁻¹(x) = sqrt(x)
  for (i in 1:echantillon) {
    w = runif(1)
    x3[i] = sqrt(w)
  }
  x3
}
# on a donc effectué la simulation de la v.a.r. de densité f(x) sur n
SumZn = c()
for (j in 1:n) {
  Sn = fsimu3(tries)
  tmpZn = (sqrt(tries) / sqrt(sigmacarre)) * ((sum(Sn) / tries) - esp)
  SumZn[j] = tmpZn
}

hist(SumZn,ylab = "Frequence", xlab = "Zn",main = "Simulation Zn sur 20000 tests de taille N = 100 pour la loi f(x) = 2x", breaks = 30, col = "pink", probability = TRUE)

loiNormale <- seq(-5, 5, by = 0.01) # tracé de la fonction gaussienne standard
y <- dnorm(loiNormale)
lines(loiNormale, y, col = "red")

