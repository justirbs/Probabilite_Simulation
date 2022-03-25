library(methods)
#Exercice 1

#Q1
bernoulli <- setRefClass("Loi de Bernoulli", fields = list(p = "numeric"), methods = list(
  esp = function() #Donne l'esperance
  {
    return(p)
  },
  var = function() #Donne la variance
  {
    return(p*(1-p))
    
  },
  simulation = function(n) #Génère une liste de taille n de 0 et 1 en fonction de la probabilité p donné
  {
    x=sample(c(0,1),n,replace=T,prob=c(1-p,p))
    return(x)
  },
  simu_proba = function(n) #Retrouve une approximation de P a partir d'un vecteur généré avec la fonction simulation
  {
    print(table(simulation(n))/n)
  }
))
ber = bernoulli(p=0.5)
ber$simulation(100)
#Q2
binomiale <- setRefClass("Loi binomiale", fields = list(n="numeric", p = "numeric"), methods = list(
  esp = function() #Donne l'esperance
  {
    return(n*p)
  },
  var = function() #Donne la variance
  {
    return(n*p*(1-p))
    
  },
  simulation = function(r) #Génère un vecteur de r entier entre 0 et n en fonction des probabilités
  {
    probabilites = c(0:n) #on fait une liste pour les probabilités de chaque valeur
    for(k in 0:n)
    {
      probabilites[k+1] = (factorial(n)/(factorial(k)*factorial(n-k)))*(p^k*((1-p)^(n-k))) #on calcule ces probabilités
    }
    x=sample(seq(from =0, to=n, by=1),r,replace=T,prob=probabilites)#on crée notre vecteur 
    return(x)
  }
))

binom = binomiale(n=10,p=0.45)
binom$simulation(100)

#Q3
geometrique <- setRefClass("Loi geometrique", fields = list(p = "numeric"), methods = list(
  esp = function() #Donne l'espérance
  {
    return(1/p)
  },
  var = function() #Donne la variance
  {
    return((1-p)/(p*p))
    
  },
  simulation = function(r) #on simule une loi géométrique avec une loi de bernoulli
  {
    j=1
    c=1
    result <- integer(r)
    bernoulli_approach <- bernoulli(p=p)
    bernoulli_list = bernoulli_approach$simulation(r)
    for(i in 0:r)
    {
      c=0
      while(bernoulli_list[j]==0&&j<r)#On compte le nombre d'échec avant un succès
      {
        c=c+1
        j=j+1
      }
      result[i] = c
      c=1
      j=1
      bernoulli_list = bernoulli_approach$simulation(r)
    }
    return(result)
  },
  simu_proba = function(r)
  {
    print(table(simulation(r))/r)
  }
))
geom = geometrique(p=0.3)
geom$simulation(100)

#Q4
uniformeSegment <- setRefClass("Loi uniforme sur [1,k]", fields = list(k = "numeric"), methods = list(
  esp = function() #Donne l'espérance
  {
    if(k<2)
    {
      return("Erreur: il faut k >= 2")
    }
    return((1+k)/2)
  },
  var = function() #Donne la variance
  {
    if(k<2)
    {
      return("Erreur: il faut k >= 2")
    }
    return((k*k)/12)
    
  },
  simulation = function(n)
  {
    if(k<2)
    {
      return("Erreur: il faut k >= 2")
    }
    x=round(runif(50,1,50))
    return(x)
  }
))
uniSegment = uniformeSegment(k=5)
uniSegment$simulation(1000)

#Q5
uniforme_11 <- setRefClass("Loi uniforme sur [-1,1]", methods = list(
  esp = function() #Donne l'espérance
  {
    return((-1+1)/2)
  },
  var = function() #Donne la variance
  {
    return(1/12)
    
  },
  graph = function(n) #on affiche sur un même graph la densité et l'histogramme de la loi
  {
    x = runif(n,-1,1)
    hist(x, main="Loi uniforme sur [-1;1]", breaks=20, xlab = "X", ylab = "valeurs", probability = TRUE, xlim = c(-1,1), ylim = c(0,1))
    abline(h = 1/2, col="blue")
  }
))
uni = uniforme_11()
uni$graph(100)
uni$graph(1000)
uni$graph(10000)
uni$graph(100000)
#plus n est grand et plus on se rapproche de la densité
