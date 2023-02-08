# Radix Sort
Le package TriRadix a été réalisé comme projet pour le cours d’algorithmique du M2. L'objectif  est de coder l’algorithme de tri Radix Sort qui a une complexité de $O(d(n+k))$ en R et en Rcpp et comparer les performances des deux algorithmes en comparant aussi ces performances avec un autre algorithme de tri d'une complexité de $O(nlogn)$ qui est Heap Sort.
Ce fichier details les simulations effectuées dans le projet et les conclusions qui en ont été tirées.

# Package Installation
Pour commencer, il faut utiliser le package TriRadix et installer le package devtools et celui de M2algorithmique qu'on utilisera pour tester l'algorithme heap sort.

```{r}
devtools::install_github("vrunge/M2algorithmique")
library(M2algorithmique)
library(TriRadix)
```

On appelle les fonctions de Rcpp.

```{r}
radix_sort_Rcpp <- function(x) {
  library(Rcpp)
  sourceCpp("triradixcpp.cpp")
  return(radixsort(x))
}  
radix_sort_decimal_Rcpp <- function(x) {
  library(Rcpp)
  sourceCpp("triradixcpp.cpp")
  return(radix_sort_rcpp(x))
}
```


#Premier essai

On simule, comme un premier essai, v un vecteur de taille n contenant tous les entiers de 1 à n.

```{r}
n <- 10
v <- sample(0:10, n, replace = TRUE)
```

Le vecteur qu'on doit trier:
```{r}
v
```
[1] 2 8 0 8 3 7 6 9 7 2
Avec radix sort on obtient:

```{r}
radixSort(v)
```
 [1] 0 2 2 3 6 7 7 8 8 9
avec radix sort implementé en Rcpp:

```{r}
radix_sort_Rcpp(v)
```
[1] 0 2 2 3 6 7 7 8 8 9
avec heap sort:

```{r}
heap_sort(v)
```
[1] 0 2 2 3 6 7 7 8 8 9
et avec heap sort implemnté en Rcpp:

```{r}
heap_sort_Rcpp(v)
```
[1] 0 2 2 3 6 7 7 8 8 9

## Les 4 algorithmes à une taille fixé

L'objectif est de comparer les temps d’éxécution des $4$ algorithmes de tri. On fixera la taille $n$ et on comparera les perfomances de radix et heap sort.

# One simulation

On définie une fonction one.simu pour simplifier l'étude des simulations.

```{r}

one.simu <- function(n, type = "sample", func = "radixSort")
{
  if(type == "sample"){v <- sample(n)}else{v <- n:1}
  if(func == "radixSort"){t <- system.time(radixSort(v))[[1]]}
  if(func == "heap_sort"){t <- system.time(heap_sort(v))[[1]]}
  if(func == "radix_sort_Rcpp"){t <- system.time(radix_sort_Rcpp(v))[[1]]}
  if(func == "heap_sort_Rcpp"){t <- system.time(heap_sort_Rcpp(v))[[1]]}
  
  return(t)
}

```

On choisi $n = 10000$ et on évalue le temps d'éxécution de chaque algorithme avec one.simu.

```{r}
n <- 10**(4)
v <- sample(0:10, n, replace = TRUE)

one.simu(v, func = "radixSort")
one.simu(v, func = "heap_sort")
one.simu(v, func = "radix_sort_Rcpp")
one.simu(v, func = "heap_sort_Rcpp")

```
[1] 0.03
[1] 0.47
[1] 0.02
[1] 0

On peut noter de ces simulations que les algorithmes en Rcpp sont plus rapide que les algorithmes en R. On gagne un quantité de temps remarquable avec le heap sort en Rcpp qu'au lieu de celui en R.

## Simulations des nombres entiers

On compare maintenant les performances des algorithmes pour une suite d'éxécutions (nbSimus=10).

```{r}
# We compare the running time with repeated executions (nbSimus times)
n <- 10**(4)
nbSimus <- 10
time1 <- 0; time2 <- 0
time3 <- 0; time4 <- 0

for(i in 1:nbSimus){time1 <- time1 + one.simu(n, func = "radixSort")}
for(i in 1:nbSimus){time2 <- time2 + one.simu(n, func = "heap_sort")}
for(i in 1:nbSimus){time3 <- time3 + one.simu(n, func = "radix_sort_Rcpp")}
for(i in 1:nbSimus){time4 <- time4 + one.simu(n, func = "heap_sort_Rcpp")}

#temps d'éxvcution de chaque algorithme
time1
time2
time3
time4
```
[1] 0.71
[1] 4.33
[1] 0.07
[1] 0.02
On remarque que heap sort en R est le plus lent alors que l’algorithme le plus rapide est le radix sort en Rcpp suivi du heap sort. Par contre, lors du premier essai, ces deux algorithmes avaient à peu pres la même performance.

Calcul du gain en passant de R à Rcpp.
```{r}
#gain R -> Rcpp
time1/time3 #Radix Sort
time2/time4 #Heap Sort
```
[1] 10.14286
[1] 216.5
On remarque que le code est toujours plus rapide en Rcpp. Le radix sort en Rcpp est plus rapide qu’en R ainsi que le heap sort est beaucoup plus rapide (fois 141) en Rcpp.

Calcul du gain de Radix sort par rapport à heap sort
```{r}
#gain radix -> heap
time1/time2 #R
time3/time4 #Rcpp
```
[1] 0.1639723
[1] 3.5
```{r}
#max gain between radix R and heap sort Rcpp
time1/time4
```
[1] 35.5

On note un gain de temps remarquable entre le heap sort en R et le heap sort en Rcpp.
Dans tout les cas, le code en Rcpp est toujours plus rapide que le code en R et Le fait d’effectuer des nombreux simulations nous a permis de mieux comparer les algorithmes.


On vérifie maintenant que la taille de vecteur d'entrée affect le temps d'éxecution des algorithmes.
On va prendre la taille des vecteurs de $2$ jusqu'à $2^{12}$ et calcule le temps de tri $nbSimus=5$.

```{r}
n <- 1
nbSimus <- 5
time1 <- c(); time2 <- c()
time3 <- c(); time4 <- c()

for(j in 1:12){
  n <- n * 2
  t1 <- 0; t2 <- 0
  t3 <- 0; t4 <- 0
  for(i in 1:nbSimus){t1 <- t1 + one.simu(n, func = "radixSort")}
  time1 <- append(time1, t1)
  for(i in 1:nbSimus){t2 <- t2 + one.simu(n, func = "heap_sort")}
  time2 <- append(time2, t2)
  for(i in 1:nbSimus){t3 <- t3 + one.simu(n, func = "radix_sort_Rcpp")}
  time3 <- append(time3, t3)
  for(i in 1:nbSimus){t4 <- t4 + one.simu(n, func = "heap_sort_Rcpp")}
  time4 <- append(time4, t4)
}

#temps d'execution de chaque algo
time1
time2
time3
time4
```
[1] 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.06 0.06 0.10
[1] 0.00 0.00 0.00 0.00 0.00 0.00 0.02 0.00 0.02 0.04 0.10 0.57
[1] 0.06 0.02 0.07 0.00 0.02 0.05 0.03 0.04 0.05 0.06 0.02 0.00
[1] 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.04

```{r}
length <- c(2, 2**2, 2**3, 2**4, 2**5, 2**6, 2**7, 2**8, 2**9, 2**10, 2**1, 2**12)

plot(time1, length, type = 'b', xlab = "time in seconds", ylab = "data length")
plot(time2, length, type = 'b', xlab = "time in seconds", ylab = "data length")
plot(time3, length, type = 'b', xlab = "time in seconds", ylab = "data length")
plot(time4, length, type = 'b', xlab = "time in seconds", ylab = "data length")
```
```{r}
time1[12]/time3[12]
time2[12]/time4[12]
```
[1] 3.33
[1] 14.25
Radix sort est plus rapid en Rcpp que dans R. Ainsi que Heap Sort est plus rapid en Rcpp.

Maintenant on considere les nombres plus grands et on evalue les performances de nos algorithmes.

```{r}
one.simu <- function(n, func = "radixSort")
{
  v <- sample(10000:99999, n, replace = TRUE)
  if(func == "radixSort"){t <- system.time(radixSort(v))[[1]]}
  if(func == "heap_sort"){t <- system.time(heap_sort(v))[[1]]}
  if(func == "radix_sort_Rcpp"){t <- system.time(radix_sort_Rcpp(v))[[1]]}
  if(func == "heap_sort_Rcpp"){t <- system.time(heap_sort_Rcpp(v))[[1]]}
  return(t)
}
```

On evalue avec un vecteur de taille $10^{5}$ et nombre de simulations $5$.

```{r}
# We compare the running time with repeated executions (nbSimus times)
n <- 10**(5)
nbSimus <- 10
time1 <- 0; time2 <- 0
time3 <- 0; time4 <- 0

for(i in 1:nbSimus){time1 <- time1 + one.simu(n, func = "radixSort")}
for(i in 1:nbSimus){time2 <- time2 + one.simu(n, func = "heap_sort")}
for(i in 1:nbSimus){time3 <- time3 + one.simu(n, func = "radix_sort_Rcpp")}
for(i in 1:nbSimus){time4 <- time4 + one.simu(n, func = "heap_sort_Rcpp")}

time1
time2
time3
time4
```
[1] 10.14
[1] 229.84
[1] 0.15
[1] 0.41

On calcule le gain entre Radix et Heap Sort.
```{r}
#gain Heap -> Radix
time2/time1 #R
time4/time3 #Rcpp
```
[1] 22.66667
[1] 2.733333

On calcule le gain entre R et Rcpp
```{r}
#gain R -> Rcpp
time1/time3
time2/time4
```
[1] 67.6
[1] 560.5854

On remarque que Radix Sort dans Rcpp fonctionne le mieux, Heap Sort dans Rcpp le suit. Le pire algorithme est Heap Sort in R. En comparant, Radix Sort dans R est plus rapide que Heap Sort dans R, mais plus lent que Radix Sort dans Rcpp. Heap Sort dans Rcpp est plus lent que Radix Sort dans Rcpp, mais plus rapide que Heap Sort sur R.

On effectue mainteant le meme test avec des nombres à 5 chiffres et une longueur du tableau de $10$ à $10^{5}$.
```{r}
n <- 1
nbSimus <- 5
time1 <- c(); time2 <- c()
time3 <- c(); time4 <- c()

for(j in 1:5){
  n <- n * 10
  t1 <- 0; t2 <- 0
  t3 <- 0; t4 <- 0
  for(i in 1:nbSimus){t1 <- t1 + one.simu(n, func = "radixSort")}
  time1 <- append(time1, t1)
  for(i in 1:nbSimus){t2 <- t2 + one.simu(n, func = "heap_sort")}
  time2 <- append(time2, t2)
  for(i in 1:nbSimus){t3 <- t3 + one.simu(n, func = "radix_sort_Rcpp")}
  time3 <- append(time3, t3)
  for(i in 1:nbSimus){t4 <- t4 + one.simu(n, func = "heap_sort_Rcpp")}
  time4 <- append(time4, t4)
}

time1
time2
time3
time4

```
[1] 0.02 0.01 0.04 0.32 3.26
[1]  0.00  0.00  0.09  1.24 74.35
[1] 0.03 0.04 0.00 0.00 0.07
[1] 0.00 0.00 0.00 0.01 0.15

```{r}
length <- c(10, 10**2, 10**3, 10**4, 10**5)

plot(time1, length, type = 'b', xlab = "time in seconds", ylab = "data length")
plot(time2, length, type = 'b', xlab = "time in seconds", ylab = "data length")
plot(time3, length, type = 'b', xlab = "time in seconds", ylab = "data length")
plot(time4, length, type = 'b', xlab = "time in seconds", ylab = "data length")
```



# Simulation des nombres decimaux

Dans cette partie, on va simuler des nombres decimaux au lieu des nombres entiers. On commence par modifier la fonction one.sim pour qu’elle prend des valeurs decimales.

```{r}

one.simu <- function(n, type = "decimal", func = "radixSort", precision = 5)
{
  
  # Tire n nombres décimaux aléatoires compris entre 0 et n
  if(type == "decimal"){v <- round(runif(n, min=0, max = n), precision)}
  if(func == "heap_sort"){t <- system.time(heap_sort(v))[[1]]}
  if(func == "radix_sort_decimal"){t <- system.time(radix_sort_decimal(v))[[1]]}
  if(func == "heap_sort_Rcpp"){t <- system.time(heap_sort_Rcpp(v))[[1]]}
  if(func == "radix_sort_decimal_Rcpp"){t <- system.time(radix_sort_decimal_Rcpp(v))[[1]]}
 return(t)
}

# We evaluate the time with a given n over 2 algorithms. We choose
n <- 10000
one.simu(n, type= "decimal", func = "radix_sort_decimal")
one.simu(n, type= "integer",  func = "heap_sort")
one.simu(n, type= "decimal", func="radix_sort_decimal_Rcpp")
one.simu(n, type= "integer",  func = "heap_sort_Rcpp")
```
[1] 0.11
[1] 0.31
[1] 0
[1] 0.02

Le heap sort en R est toujours le plus lent tandis que le radix sort decimal est plus rapide. Les deux algorithmes en Rcpp ont la meme vitesse.

On compare maintenant les performances des algorithmes pour une suite d’execution de 10 fois.

```{r}

# Valeur de n (taille du vecteur à trier)
n <- 10000

# Nombre de fois où on répète l'algorithme sur un vecteur de taille n
nbSimus <- 10

# Temps d'exécutions
time1 <- 0; time2 <- 0
time3 <- 0; time4 <- 0

for(i in 1:nbSimus){time1 <- time1 + one.simu(n, func = "radix_sort_decimal")}
for(i in 1:nbSimus){time2 <- time2 + one.simu(n, func = "heap_sort")}
for(i in 1:nbSimus){time3 <- time3 + one.simu(n, func = "radix_sort_decimal_Rcpp")}
for(i in 1:nbSimus){time4 <- time4 + one.simu(n, func = "heap_sort_Rcpp")}

#  Temps d'exécutions des differents algo
time1
time2
time3
time4
```
[1] 0.76
[1] 4.41
[1] 0.05
[1] 0.04

Apres plusieurs simulations on a toujours les memes resultats pour les fonctions en R mais cette fois on peut voir que radix sort en Rcpp est plus rapide que le heap sort en rcpp.

```{r}
#gain R -> Rcpp
time1/time3 #RadixSort
time2/time4 #HeapSort
```
[1] 81
[1] 204

Radix est plus rapide en Rcpp que R, de meme pour le heap sort mais la diffrence de vitesse de heap sort est de 200 ce qui est remarquable.

```{r}
#gain  Heap -> Radix
time2/time1
time4/time3

```
[1] 5.802632
[1] 0.8

# Microbenchmark
On utilise dans cette partie les packages microbenchmark et ggplot2 afin de simuler plus profondement les algorithmes.
```{r}
library(microbenchmark)
library(ggplot2)
```

On compare Radix Sort et Heap Sort avec des vecteurs de taille $n=1000$.

```{r}
n <- 1000
res <- microbenchmark(one.simu(n, func = "radix_sort_Rcpp"), one.simu(n, func = "heap_sort_Rcpp"), times = 50)
autoplot(res)
```

```{r}
print(res)
```


On fait compare maintenant avec une taille plus grandes de $n = 100000$.
```{r}
n <- 10000
res <- microbenchmark(one.simu(n, func = "radix_sort_Rcpp"), one.simu(n, func = "heap_sort_Rcpp"), times = 50)

autoplot(res)
```

```{r}
print(res)
```
Le heap sort est généralement meilleur que le heap sort.

On compare maintenat Radix Sort decimal et Heap Sort avec des vecteurs de taille $n = 10000$.

```{r}
n <- 100000
res <- microbenchmark( one.simu(n = n, type = "decimal", func = "radix_sort_decimal_Rcpp"), one.simu(n = n, type = "decimal", func = "heap_sort_Rcpp"), times = 50)
autoplot(res)
```

```{r}
print(res)
```
On peut conclure que le radix sort pour les nombres decimaux est meilleur.
