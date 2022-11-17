data("Loblolly")
#install("UsingR") #omesso, fare solo la prima volta
str(Loblolly)
mean(Loblolly$height)
rnorm(100)
median(Loblolly$height)
#moda = valore variabile più ripetuta
table(Loblolly$age)
#age non ha moda perchè tutte le frequenze sono uguali
table(Loblolly$Seed)
#freq ass= num volte che si verfica un numero
#Frequenze relative=freq assoluta/la taglia del campione(o taglia senza N/A)
sum(is.na(Loblolly$age))
#is.na restituisce true or false
#in sum true=1 false=0
length(Loblolly$age)
str(dim(Loblolly)) #84 righe 3 colonne
dim(Loblolly)[2]
Loblolly[40, 3]
table(Loblolly$age) / length(Loblolly$age)
#freq percentuali = freq rel * 100
(table(Loblolly$age) / length(Loblolly$age)) * 100
#range= differenza tra val max e val min
range((Loblolly$height)) #min e max
rg <- range((Loblolly$height))
rg[2] - rg[1] #range effettivo
#quartile(divisione x 4)
#quantile = mediana di ordine 30 (30% più piccoli 70% più grandi)
#quartile entra nel box plot
quantile(Loblolly$height)
quantile(Loblolly$height, 0.9)
#Varianza campionaria chiamata S^2 = sommatoria di i=1 a x, ((x_i-mean)^2 / n-1)
#STATISTICHE = variabili del campione
#STIMATORI CORRETTI= stima buona se divisa per n-1 (boh)
var(Loblolly$height, na.rm = TRUE) #na.rm = remove na(valori nulli)
#ricordare scarto quadratico, questo numero sopra è al quadrato!
#deviazione standard = sqrt(varianza)
sd(Loblolly$height)
#Valori standardizzati o z-score = z_i = (x_i - mean)/devStandard
z <- (Loblolly$height - mean(Loblolly$height)) / sd(Loblolly$height)
mean(z)
sd(z)
#coefficiente di variazione CV= s/mean % (se x non è positiva si prende val ass)
#CV<1 distribuzione poco dispersa
#CV>1 distribuzione dispersa
sd(Loblolly$height) / mean(Loblolly$height) * 100
hist(Loblolly$height, freq = F)
abline(v = 32.25, col = "green", lwd = 3)
#___________________________NUOVA LEZIONE (14Nov22)____________________
#variabili quantitative univariate,
#variabili descrittive univariate
#variabili descrittive bivariate

#Rappresentazione grafici di dati quantificativi univariati:
#dotplto,Istogrammi,Boxplot
#Dotplot usato per campioni di taglia piccola
#Ex: x = (2,2,2,3,4,5,5,5,6,7,7,7,7,8) Ex:dati ordinati
#Rappresentiamo un punto per variabile sulla retta

#Istogramma
#è una rappresentazione empirica della densità campionaria della variabile che  # nolint
#quindi fornisce un'indicazione della densità della popolazione della popolazione da  # nolint
#cui si campiona
#Asse x= valori del range suddivisi in bins
#su ogni bin si costruisce una barra la cui altezza è proporzionale alla frequenza dei valori che  # nolint
#cadono nel bin
#L'area totale vale 1
#-Le barre devono essere adiacenti
#-L'istogramma è definito esclusivamente per variabili quantitative possibilmente continue # nolint
#Istogramma
library("UsingR")
data(math)
str(math)
hist(math, freq = FALSE, col = "red")
x <- rpois(100, 1)
#simula 100 valori estratti da una variabile di poisson di parametro lambda=1
hist(x, freq = FALSE, col = "green")
lines(density(math,color='red'))
#!!Stiamo facendo histogram di una variabile discreta!!
#Per avere una stima della densità della popolazione sotto forma di curva, usiamo plot(density(x)) # nolint
plot(density(x))
#rappresenta un grafico a parte rispetto all'istogramma per sovrapporre la linea della densità # nolint
#Regola empirica per la scelta del numero bin:
# n° bin = sqrt(n)
#hist(x,freq = F, breaks=n° proposto di bin) # nolint
x <- rnorm(2000)
boxplot(x)
View(x)
#Boxplot: se:
    #il baffo sinistro = max(min(x), Q_1 - 1.5 IQR) IQR="Scarto interquantile"= Q_3 - Q_1 # nolint
    #il baffo destro = min(max(x), Q_3 + 1.5 IQR)
    #outliers = valori minori del baffo sinistro o maggiori del baffo destro quando questi assumono i valori Q_1 - 1.5IQR e Q_3 + 1.5IQR # nolint
    #(i pallini nel boxplot indicano gli outliers, l'eventuale mancanza indica l'assenza di outliers) # nolint
    #boxplot(math) non possiede outliers; boxplot(x) con x = 2000 valori normali randomici ne possiede # nolint

#STATISTICA DESCRITTIVA PER VARIABILI QUANTITATIVE

#dati categorici(categoriali,qualitativi)

#Analisi:
        #Tabelle di frequenza:
            #assoluta
            #relitiva
            #percentuale
        #Grafici:
            #(Dotplot)
            #barplot
            #piechart
#Frequenza assolute: f_i n° di occorrenze di ogni categoria delle variabili nel campione # nolint
                    #Sommatoria da i=0 a k f_i = n  con k:n° di categorie
                    #table(x) # nolint
#Frequenze relative: (F_i)/n' dove n':n° osservazioni != NA
                    #table(x)/sum(table(x)) # nolint
#Frequenza(relative)percentuali: ((f_i)/n)*100%
                    #table(x)/sum(table(x))*100 # nolint
data(babies)
str(babies)
#Analisi statisctica descrittiva della race
table(babies$race)#Freq assoluta
table(babies$race) / sum(table(babies$race))#Freq Relativa
table(babies$race) / sum(table(babies$race)) * 100#Freq Relativa Percentuale
#Grafici:
        #Barplot: barre la cui altezza è proporzionale alla frequenza (le barre sono staccate) # nolint
                  #barplot(x) # nolint
        #Dotchar: simili ai barplot ma con dei punti, poco usati (solo per taglie piccole) # nolint
                  #dotchar(table(x)) # nolint
        #Piechart: [DEPRECATO] le altezza si visualizzano meglio delle aree
                  #pie(table(x)) # nolint 
        #Boxplot(Horizontal=TRUE) # nolint 

barplot(table(babies$race),col=rainbow(12)) # nolint 
barplot(table(babies$race) / sum(table(babies$race)),col=rainbow(10)) # nolint 
barplot(table(babies$race) / sum(table(babies$race))*100,col=rainbow(10)) # nolint 
dotchart(table(babies$race))
pie(table(babies$race), col = rainbow(13))

#STATISTICA DESCRITTIVA DI DATI BIVARIATI

#Avremo campioni del tipo (x,y) oppure ((x,t),(x,y)) ovvero: Coppie di valori [2 variabili insieme anche di tipi diversi] # nolint 
#In generale quando si hanno n>1 misure di variabili sugli stessi soggetti si parla di dati multivariati # nolint 
#Es: 1 variabile quantitativa / 1 variabile qualitativa ("/" divide il campione globale in sottogruppi corrispondenti ai livelli della variabile) # nolint 
#Range_y = {A,B,C} le x otterrano come valore uno di quelli appartenenti al range, si potranno quindi suddividere le x per sottogruppi # nolint 
#in base al valore che hanno ottenuto
                        #Domanda: I gruppi possono essere considerati come provenienti da un unica popolazione unica # nolint 
                        #         oppure danno origine a delle distribuzioni diverse? # nolint 
                        #-Indici
                        #-Grafici (box,plot,hist)     (sia indici che grafici per ogni gruppo) # nolint 
                #L'approcio migliore è di fare un'analisi descrittiva per ogni gruppo e poi paragonarli # nolint 
            #2 variabili quantitative
            #Domanda: Esiste o no una relazione tra (di solito lineari) tra le variabili # nolint 
                        #relazione lineare: y=ax+b (retta) relazione lineare deterministica # nolint 
            #Risposta: creiamo uno scatterplot(grafico a dispersione) per vedere se si può ipotizzare che esista un rel.lin. # nolint 
                      #dal fatto che i punti si dispongono approssimativamente intorno  a una retta. # nolint 
        #!!!!! Indice di correlazione lineare di Pearson !!!!!!
                                        #r= (1/(n-1)) * sum i=1 to n ((x_i-mediadix)/Sx) * ((y_i-mediadiy)/Sy)  [x e y standardizzate] # nolint 
                                        # z.score di x e z-score di y (si dimostra che sono incluse sempre in 1?) # nolint  
#_______________________________NUOVA LEZIONE__________________________________
#Statistica descrittiva per campioni bivarianti
    #1 variabile quanlitativa/1 variabile quantitativa
    #2 variabili quantitative
    #2 variabili qualitative
#Introduzione alla statistica inferenziale
#1 variabile qualitativa (1 variabile quantitativa)
            #Ragruppamenti = Gruppi: livelli o categorie della variabile qualitativa  # nolint
        #Si effettua un'analisi statistica descrittiva per ogni gruppo di oggetti che condividono una categoria di tale derivazione  # nolint

#Using babies
library("UsingR")
data("babies")
str(babies)
tapply(babies$wt, babies$race, mean, na.rm = NA, simplify = TRUE)
#Se abbiamo 2 variabili quantitative
    #relazioni lineari in senso statistico # nolint
#Indice di correlazione standard della popolazione: definito come "rho":media[(x-mediax)*(y-mediay)]/sqrt(var(x)*var(y)) # nolint
#Proprietà di Rho:
        #-|r| <= 1  -1 < r < 1
        #- r approx 0,nuvola omogenea, non c'è correlazione tra le z variabili
        #- r > 0: correlazione positiva, dipendenza diretta
    #si può immaginare una retta che fitti i punti, più i punti sono ficini alla retta, più è ficino a uno più la funzione è stringente # nolint
        #- r < 0: correlazione negativa,dipednenza inversa
    #Il valore assoluto di r dà un'indicazione della forza della relazione tra le z variabili # nolint
    #(0.2 < |r| < 0.5 : la relazione è depole, i punti sono molto dispersi intorno alla retta)  # nolint
    #(0.5 < |r| < 0.8 : la relazione è forte, i punti sono poco dispersi intorno alla retta, la relazione è ottima) # nolint
#Ci sono dei problemi per i valori molto distanti dalla media con differenze di segno opposto ad esse # nolint
#Indice di correlazione Spearman:
    #indici di monotonia, dà un'indicazione di quante osservazioni hanno la stessa occorrenza. # nolint
    #Pearson = cor(x,y) # nolint
    #Spearman = cor(x,y,method="spearman") # nolint

#Correlazione wt (pesp dei neonati) e wt1 (peso delle madri)
#x variabile INDIPENDENTE, y DIPENDENTE

plot(babies$wt1,babies$wt)
#togliamo le madri NA con peso dei figli
pesom <- babies$wt1[babies$wt1 != 999]
pesob <- babies$wt[babies$wt1 != 999]
plot(pesom, pesob)
cor(pesom, pesob) #relazione molto depole
cor(pesom, pesob, method = "spearman") #anche, duh

#2 variabili categoriali: Tabelle di contingenza
                #Distribuzione congiunta delle 2 variabili
            #!!Tabelle di contingenza!!(a doppia entrata)
                #X:n_1 livelli
                #Y:n_2 livelli
                #Colonna e riga al fondo indicano le frequenze:
                            #N.1 =prima colonna
                            #N.2 =seconda colonna
                            #N.1 =prima riga
                            #N.2 =seconda riga
                            #N
                #n_ij : (frequenze assolute che hanno livello di X pari ad i ed il livello di Y pari a j) # nolint
                #N_i oppure N_j : (frequenza marginali di riga e di colonna) (la somma di tutte le frequenza di quella riga o di quella colonna)  # nolint
                #N : totale, taglia del campione (somma di tutte le righe e di tutte le colonne)  # nolint
                #table(x,y)  # nolint
table(babies$race, babies$drace) #cor e table ignorano gli NA
plot(table(babies$race, babies$drace)) #nella diagonale ci saranno i valori maggiori, padre e figlio stessa razza # nolint
table(babies$race, babies$drace) #NON VENGONO TOLTI I 99(SCONOSCIUTI), TOGLIE SOLO GLI EFFETTIVI NA # nolint
#Mosaic plot:
    #Metodo di rappresentazione grafica per campioni di variati qualitative
    #Dati univariati: rettangoli la cui area è proporzionale alla frequenza della categoria # nolint
    #Dati bivarianti: rettangoli che compongono un unico rettangolo le cui aree sono proporzionali alle frequenze congiunte nella tabella di contingenza # nolint
mosaicplot(table(babies$race, babies$drace)) # i puntini valgono 0
brace <- babies$race[babies$race != 99]
drace <- babies$drace[babies$drace != 99]
table(brace, drace)
length(brace)#non va
length(drace)#non va
#Indice di Kendall per coppie di variabili quantitative o qualitative ordinali
            # Tau = (#coppie concordanti - #coppie discordanti) / (n(n-1))/2        n taglia del campione  # nolint
            # Misura l'associazione tra le due variabili di cui si considerano le coppie di valori # nolint
            #Coppie concordanti = hanno sia x che y entrambi > 0, entrambi x della coppia che si sta considerando y # nolint
            #Si sommano tutti i punti (coppie di x e y) per ottenere il valore di Tau # nolint
cor(pesom, pesob, method = "kendall")
#Esempio noto: Nota: l'esistenza di una associazione o relazione tra due variabili NON implica che una causi l'altra, NON implica # nolint
                    #che esista una relazione di causalità:
                    # X = anni di studio
                    # Y = misura di una certa patologia
                    # Y dipende da X   # nolint
                #Se misurata quantitativamente ha un'R grande, MA NON è UNA RELAZIONE CAUSALE, ci sono variabili intermezze omesse IMPORTANTI # nolint
                #Esempio: disponibilità economiche che influiscono su entrambi, ATTENZIONE A PRENDERE RELAZIONI FORTI COME RELAZIONE CAUSALITà # nolint
#_____________________________________________________________________Fin statistica descrittiva_______________________________________ # nolint 
#STATISTICA INFERENZIALE: Stima dei parametri
                        # Intervalli di confidenza per i parametri
                        # Test di ipotesi
    #Statistica parametrica, partiamo dal presupposto che le variabili statistiche, ovvero i dati che abbiamo a disposizione osnon stratti da distribuzioni # nolint
    #della popolazione appartenenti a familie note caratterizzate da parametri (Come la normale(mu, sigma quadro), Poisson(), exp(),Binom(n,p)) # nolint
    #Otteniamo dai dati infomrazioni su questi parametri tramite il procedimento di STIMA # nolint
#DEFINIZIONE: Stima: si dice STIMATORE detto anche statistica (con la s minuscola), una funzione calcolabile in base al campione casuale # nolint
                    #casuale(x_1,x_2,....,x_n) v.a i.i.d Esempio: "media campionaria" e "varianza campionaria corretta" sono stimatori # nolint
                    #campioni = insieme variabili i.i.d.
#___________________________________________________________NUOVA LEZIONE 5_______________________________________________________________  # noint
#Stimatori e stime
#Introduzione agli intervalli di confindenza

#Stimatori:
    #-funzioni delle variabili del campione:
        #S=S(X_1,X_2,.....,X_n) # nolint
    #X~N(mu,sigma^2) # nolint 
    #X~exp(A) # nolint
    #X~Binom(n,p) # nolint
    #Per i stimatori si usano lettere maiuscole, le STIME sono le loro realizzazioni # nolint
    #x_n = 1/n * sum con i da 1 ad n di x_i
    #s^2 = 1/(n-1) * sum di i da 1 ad n di (x_i - media di x_n)^2
    #Gli stimatori forniscono informazioni sui parametri della popolazione.
#Per sapere il livello della converegenza degli stimatori necessitiamo di diversi teoremi: # nolint
        #-legge di convergenza ()
        #-Teorema di limite centrale ()
        #-Legge dei grandi numeri
    #Legge dei grandi numeri:
        #dati x_1,...,x_n indipendenti identicamente distinte con media E(X_1)<inf: # nolint
            #Media di X_n = 1/n * sum di i da 1 a n di X_i con n -> inf E(X_1) 
    #E(media X_n) = E( 1/n * sum di i da 1 ad n di X_i)
                # = 1/n * E(sum di i da 1 ad n di X-i) = 1/n * sum di i da 1 ad n di E(X)_i # nolint
    #La media della media campionaria è la media della popolazione (PROPRIETà CHE VALE SEMPRE PER TUTTE LE VARIABILI) # nolint
    #La media campionaria è uno stimatore CORRETTO della media della popolazione. # nolint
#Varianze della media campionaria:
    #var(media X_n) = var(1/n * sum di i da 1 ad n di X_i) = 1/(n^2) * var(sum di i da 1 ad n di X_i) # nolint
                    #(se sono indipendenti, ovvero si perchè i.i.d.)
                    # = 1/(n^2) * sum di i da 1 ad n di var(X_i)
                    # = 1/(n^2) * sum di i da 1 ad n di sigma^2
                    # = 1/(n^2) * n * sigma^2
                    # = (sigma ^2) / (n^2)
#Se var(media X_n)=var(X_n)/n  --> var(media X_n) < var(X_1)  (media X_n = media campionaria ?) # nolint
#!!!!!______recap_____!!!!!
#-Popolazione (Nella statistica parametri la popolazione è MODELLIZZATA tramite una DISTRIBUZIONE[tramite una v.a] che tiene conto # nolint
    # che descrive la casualità o aleatorietà di ogni elemento[campione])
#-Parametri (mu, sigma^2,p,...) Sono numeri che forniscono un'interpretazione sintetica del comportamento della distribuzione della popolazione # nolint
    #all'interno di una famiglia di distribuzioni a cui corrisponde la popolazione (se usiamo exp() cercheremo il lambda attraverso una dist exp, non altre) # nolint
#-Statistica: è un sommario numerico del campione, funzione delle variabili che lo costituiscono (del campione) la cui misura o # nolint
    #realizzazione è il numero che fornisce la stima che interessa:
        #l'insieme dello spazio campionario conterrà i CAMPIONI sui quali vengono applicati la statistica campionaria ottenendo l'insieme delle stime: # nolint 
            #media di X_n, S^2 ...
            #Nell'insieme delle stime avremo :
                #media X_n (w'_1,w_2'....)=media di x'_n (cos' per valori w'',w''' ecc.) # nolint
#La leggede i grandi numeri ci dice COME converge la madia campionaria
#CVC - CONVERGENZA DELLA VARIANZA CAMPIONARIA:
    #Dato un campione x_1,x_2...x_n prese da un insieme i.i.d. EX_1 < inf e var(X_1) < inf: # nolint
    #La varianza campionaria corretta converge al crescere di n , alla varianza corretta # nolint (n = taglia del campione) + la taglia  è alta, la varianza diventa quasi identica alla popolazione, migliorando la statistica # nolint
        # 1/n-1 * sum di i da 1 ad n di (X_i - media X_n) ^ 2 tende a Var(X_1) con n->inf (tendente) # nolint 
    #La velocità di convergenza di (S^2) a (a^2) è inferiore alla convergenza di media (X_n) a (mu) # nolint
#TLC - TEOREMA DEL LIMITE CENTRALE:
    #Dato un campione x_1,x_2...x_n prese da un insieme i.i.d. EX_1 < inf e var(X_1) < inf: # nolint
        #La P(((media X_n - E(X_1))/sigma^2(X_1))/sqrt(n) <= x) --> P(Z< x) per n-> inf # nolint (Z stà per zentral)(Z = normale standard?o deviazione standard)
        #P(X>x) dove X è una v.a. e x è un numero
    #ovvero per n crescente, la variabile che si ottiene standardizzando la media campionaria, ha lo stesso comportamento di una normale standard (se valgono i vincoli indicati prima) # nolint
    #SINONIMATO:La standardizzazione della media campionaria (si sottrae la media e si di divide per la deviazione standard della media campionata) # nolint
                # Var( media X_n) = ...
                # si comporta al crescere di n sempre di più come una v.a. normale standard # nolint
    #Il TLC vale per qualunque v.a. (sia discreta che continua);
    #Dimostrato empiricamente che: la taglia n perchè l'approssimazione della media campionaria standard con la normale standard sia buona basta per essere abbastanza corretto almeno: 30 (what?) # nolint

#SU MOODLE SONO STATI AGGIUNTI DUE PROGRAMMI IN R PER VERIFICARE EMPIRICAMENTE QUESTE PROPRIETà, non è da sapere, ma male non fa # nolint
                        #LGN.r
#ignore
var <- 256
mu <- 100
n <- 100 #mettere a tanto alto se hai 80Gb di RAM
x <- rnorm(n, mean = mu, sd = sqrt(var))
hist(x)
#stop ignore
                        #Per teorema TLC il file è TLC.r



#Casi particolari: 
    #-Cosa succede se la variabile X_1 è già normale?: X_1 ~ N(mu,sigma) : media X_n ~ N(mu,sigma/(sqrt(n))) # nolint
    #-Se le x sono già normali: X_1 ~ N(mu,sigma) : (S^2 * (n-1))/sigma^2 ~ X^2 (n-1) [per qualunque n] # nolint
            #Distribuizione chi-quadro con n-1 (parametro) di libertà (i parametri sono i gradi di libertà) (serve per esempio per i test sulla varianza di una dist e per i test di indipendenza o di bontà del fit) # nolint

#INTERVALLI DI CONFIDENZA
    #Introduzione
    #I campioni casuali che noi estraiamo non sono "random" al 100%, ma sono estratti da valori estratti da una variabile, quindi soggette alle leggi delle probabilità # nolint
    #Dalle ipotesi sulla distribuzione della popolazione possiamo ottenere una certa PRECISIONE nell'usare le stime PUNTUALI per i parametri o per fare inferenza # nolint
        #media X_n ~ media --> x_n
        #S^2 --> s^2
        #Sono stime puntuali dei parametri
        #IC: valori intervallari non puntuali per la stima, intervallo numerico con CONFIDENZA (1-alpha){0.9 alpga=0.10 ; 0.95 alpha=0.05; 0.99 alpha=0.01} # nolint