#Exercice 2


#Question 1
# Loi de Bernoulli X~B(p)
bernoulliInversion <- function(p){
  u = runif(1) # loi uniforme entre 0 et 1
  # fonction inverse de la fonction de répartition de Bernoulli
  if(u<1-p){
    binom = 0
  } else {
    binom = 1
  }
  binom
}

p = 0.5
bernoulliInversion(p)

#Question 2
# Loi Géométrique X~G(lambda)
geometriqueInversion <- function(n, lambda){
  i = 0
  geom = array()
  while (i < n) {
    u = runif(1)
    # Fonction inverse de la fonction de répartition d'une loi géométrique 
    geom[i] = floor(log(1-u)/log(1-lambda))+1
    i = i+1
  }
  hist(geom, breaks=50, probability = TRUE, main="Loi géométrique par inversion", xlab="x", ylab="Valeurs prises par x", freq=F)
}

n = 5000
lambda = 0.2
geometriqueInversion(n, lambda)


# Question 3 
# Loi de Poisson X~P(lambda)
poissonInversion <- function(n, lambda){
  i = 0
  poiss = array()
  while (i < n) {
    # Fonction inverse de la fonction de répartition de la loi de Poisson
    p = 1
    x = 0
    while(p>=exp(-lambda)){
      u = runif(1)
      p = p*u
      x = x+1
    }
    poiss[i] = x
    i = i+1
  }
  poiss
}

n = 100
lambda = 0.5
poissonInversion(n, lambda)


#Question 4
# Loi Exponentielle X~E(lambda)

exponentielleInversion <- function(n, lambda){
  i = 0
  expo = array()
  while (i < n) {
    u = runif(1)
    # Fonction inverse de la fonction de répartition de la loi exponantielle
    expo[i] = -log(1-u)/lambda 
    i = i+1
  }
  hist(expo, breaks=50, probability = TRUE, main="Loi exponentielle par inversion", xlab="x", ylab="Valeurs prises par x", freq=F)
}

n = 5000
lambda = 0.5
exponentielleInversion(n, lambda)


