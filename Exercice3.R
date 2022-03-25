##Excercice 3 

##1 

n = 1000 #Nombre d'essaie 
esp = 1/2 #Espérance des Xk
Xk= runif(n,min=0,max=1)
Sn= cumsum(Xk) #Calcule de Sn
N = seq(1,n, by=1)
TgSn = Sn/N

plot(N,TgSn, col="red", main = "Suite des moyennes empiriques par rapport à n pour une loi uniforme")
abline(h=esp, col = "blue") #Tracer l'espérance sur le graphe 

# la suite des moyennes empiriques semble converger vers l'espérance des X_k

##2

n = 1000 #Nombre d'essaie 
lambda = 6 #Parametre pour la loi exponentielle
esp = 1/lambda #Espérance des Xk
Xk = rexp(n,lambda)
Sn = cumsum(Xk) #Calcule de Sn
N = seq(1,n, by=1)
TgSn=Sn/N

plot(N,TgSn, col="green", main = "Suite des moyennes empiriques par rapport à n pour une loi exponentielle")
abline(h=esp, col="blue") #Tracer l'espérance sur le graphe

# la suite des moyennes empiriques semble converger vers l'espérance des X_k


##3

n = 1000
f = function(x){ #fonction pour calculer l'espérance
  2*x*x
}
Xk = c()
i = 0

for (i in 1:n) {
  Xk[i] = sqrt(runif(1))
}

esp = integrate(f, lower = 0, upper = 1)
Sn = cumsum(Xk) #Calcule de Sn
N = seq(1,n, by=1)
TgSn = Sn/N
plot(N, TgSn, col="red", main= "Suite des moyennes empiriques par rapport à n pour une loi de densité")
abline(h = esp$value, col="blue") #Tracer de l'espérance sur le graphe

# la suite des moyennes empiriques semble converger vers l'espérance