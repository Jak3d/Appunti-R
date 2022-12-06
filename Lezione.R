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
#___________________________________________________________NUOVA LEZIONE 5_______________________________________________________________  # nolint
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
#_____________________________________NUOVA LEZIONE_________________
#Intervalli di confidenza:
    #stima di un parametro di una popolazione non in modo puntuale, ma tramite un intervallo di valori
    #.....a______________b....
    #[a,b] + 1-alpha (confidenza)(tra 0 e 1)
        #: 
        #1-alpha = 0.95,0.90,0.99
    #Diremo che l'intervallo di confidenza [ab] contiene il valore del parametro che ci interessa, ad es la media di una distribuzione
    #della popolazione(per esempio mu) con confidenza 95%, 90% o 99%.
    #LA CONFIDENZA NON è UNA PROBABILITà ma è collegata alla probabilità
    #La probailità in questo caso deriva dal fatto che per ottenere la formula dell'intervallo di confidenza (IC) e quindi la loro 
    #misura si parte da intervalli cosidettì aleatori(casuali) cioè che hanno come estremi delle variabili aleatorie o casuali
    #quindi sono delle statistiche funzioni del campione.
    #-Come ottenere un'intervallo di confidenza?
        #Passo 1: trovare una quantità cosidetta pivotale (Q)
            #è una variabile aleatoria funzione del campione e del parametro sconosciuto che però abbia una distribuzione note,
            #per cui quindi si possono calcolare le probabilità.
        #Esempio: 
            #Il parametro da stimare è mu, l'attesa di X_1,
            #la quantità pivotale sarà Q= (mediaCampinaria-mu)/(S/sqrt(n))
            #deriva dal TLC quando n>30 e/o la distribuzione della popolazione è normale.
            #Q ~ T_(n_1) [T di student con parametro n-1 i quali sono i gradi di libertà (degrees of freedom)]
        #La T di student ha attesa (E) sempre uguale a zero, sempre simmetrica rispetto allo zero e ha una forma a campana con
        #le code più alte della distribuzione (solo con un elevatissimo numero di gradi si comporta come una normale)
        #Guardare le tavole di t di student (AH! LMAO) (sono carine però, spiegano come funzionano)
                    #OPZIONALE
                    #T student scoperto da william gospet, alla guinnes, negli uffici per il controllo di qualità
                    #aveva scoperto che la Q non dava una normale, aveva le code più alte, da lì il nome in quanto non poteva usare il suo nome
                    #e voleva prendere in giro i suoi ex compagni
                    #FINE OPZIONALE
            #Recap:     
                #E[T]=0
                #code più pesanti rispetto a N(0,1)
                #Tende alla Z~N(0,1) per df--> inf (molto grande)
                #Simmetrica rispetto alla sua media
                #Ha un unico parametro, n° di gradi di libertà
        #Passo n° 2: 
            #Fissiamo il livello di confidenza 1-alpha (ad esempio 1-alpha = 0.95)
            #e troviamo i valori t_1 e t_2, realizziazione di T, tali che:
                #P(t_1 < Q < t_2) = 1-alpha = 0.95 [riguardare definizione di Q, mostra che è possibile scrivere la probabilità]
            #!!!!!!!!!!!!!!
            #Per ottenere i due estremi dell'intervallo aleatori,facciamo 2 passaggi: 
                #(1) t_1 < media(X)-mu / s*sqrt(n) = media(X)-mu > s/(sqrt(n))
                                                #  = mu < media(X_n)-S/sqrt(n)*t_1
                #(2) t_2 > media(X)-mu / s*sqrt(n) = media(X)-mu > s/(sqrt(n))
                                                # mu > media(X_n)-S/sqrt(n)*t_2
                #mettendoli insieme otteniamo: 
                    #PROBABILITà che( media(X_n)-S/sqrt(n)*t_2 < mu < media(X_n)-S/sqrt(n)*t_1) = 1-alpha
                    #ottenendo così l'intervallo aleatorio: 
                        #[media(X_n)-S/sqrt(n)*t_2 , mu < media(X_n)-S/sqrt(n)*t_1]
                #grafico della lezione 21/11/2022 alle 15:52
                    #nelle 2 code estreme avremo lo 5% di P (2,5 per lato)
                    #la parte al centro sarà uguale al 95%
                    #Area delle 2 code = 0.05 = alpha
                    #Il grafico non deve per forza essere simmetrico con le percentuali, la curca però rimane simmetrica
                    #(si può avere 3% a sx e 2% a dx)
                    #i loro quantili "sommati" faranno sempre 1 (quantile indica dove si passa da area alpha a area enorme)
                #L'intervallo di confidenza al 95% che ha per estremi q_0,025 e q_0,975 è il più stretto e quindi lo si preferisce:
                #Per la T di student vale che il q_0,025 è semplicemente -q_0,975 # nolint
        #Passo 3°
            #Dall'intervallo aleatorio otteniamo l'intervallo misurato in base al campione, e quindi l'intervallo numerico.
            #IC al_95% = [media(x) - t_2*(s/sqrt(n)), media(x) + t_2*(s/sqrt(n))]     [t_1 = - t_2]
                #è simmetrico rispetto alla media campionaria media(x): 
                    #estremo inferiore = media(x) - t_2*(s/sqrt(n))
                    #estremo superiore = media(x) + t_2*(s/sqrt(n))
                #anche chiamato ERRORE STANDARD SULLA MEDIA (la parte uguale, senza la x e senza il segno)
                #L'ampiezza dell'intervallo è uguale = 2*t_2*(s/sqrt(n))
            #Al crescere di n (taglia del campione)l'intervallo di confidenza diventa più stretto, quindi più preciso
            #Se la confidenza cresce, l'IC diventa più ampio, quindi meno preciso
            #Confidenza e precisione non sono mai migliorabili contemporaneamente per gli IC, occorre scegliere uno dei due (tocca allo statistico decidere in base alla situazione)
            #Non sempre la taglia è aumentabile.
    #Come calcolare l'IC
library("UsingR")
data(babies)
dstr(babies)
#Calcolo l'IC al 95% per age
t.test(babies$age) #non abbiamo imparato ancora come eseguire un test
#IC al 90%
t.test(babies$age, conf.level = 0.90)
#IC al 99%
t.test(babies$age, conf.level = 0.99)
# 95% 27.01103 27.73168 Più intermedio (si)
# 90% 27.06903 27.67368 Più preciso, ma meno confidenza
# 99% 26.89754 27.84518 Meno preciso, più confidenza

#Cosa vuol dire aver calcolato un IC?
    #Supponiamo di avere una popolazione con mu = 26.5 (valore del parametro vero sconosciuto)
    #*vedere grafico alle 16:30*
    #Tanti IC possibili, ripetuti esperimentinelle stesse condizioni
    #Ripetendo tante volte lo stesso esperimento nelle stesse condizioni, mediamente in alpha% dei casi il valore del parametro
    #non sarebbe contenuto nell'IC calcolato in base al dati
    #Nel 95% dei casi il parametro sarà contenuto nell'IC (perchè l'abbiamo creato così), di solito si fissa una percentuale, 5% in questo caso
    #che rappresenta il nostro margine di errore, fissato a priori
#SECONDO TIPO DI IC
#Intervalli di confidenza per una proporzione pigreco per una popolazione
    #p(pigreco) proporzione nella popolazione --> p capelleto = proporzione campionaria
    #X ~ Bernulli(p) (v.a. dicotomica)
    #abbiamo solo due valori per questa v.a.: 
        #P(X=s)=p; P(X=f)=1-p
        #Dove s è il successo e f è il fallimento
    #con (X_1,....,X_n) v.a. i.i.d X_1 ~ Bernulli(p)
    #La media(X_1) = sum di k di k * P(X_1 - k) = 1*p + 0(1-p)=p = media della popolazione
    #media(X_n) = (1/n) * sum di i da 1 ad n di X_i = p cappelletto = media(X_n) (o X con linea sopra)

    #Passaggi per la costruzione dell'intervallo di confidenza al 95% per p: 
    #1. Quantità pivotale (funzione delle variabili del campione e del parametro ignoto [p]); è una v.a. che deve avere una distribuzione NOTA
        # Q = [media(X_n) - p] / (dev.st. di media(X_n)/sqrt(n)) = (1/n * sum di i da 1 ad n di X_i - p)/ sqrt[p(1-p)/n]
        #Questa funzione Q del campione (tramite la media campionaria e del parametro p, media della popolazione) è di un tipo che abbiamo già visto
        # media(X_n) - E(X_1) / sqrt(var(X_1)/n) per n-->inf (TLC) N(0,1)  
        #Per n > 30 la distribuzione della nostra quantità pivotale Q è N(0,1)
        #RICORDARSI: stiamo parlando di proporzioni, per n<30 non ha senso calcolare la normalità della distribuzione della popolazione da cui
        #è estratto il campione (prima usavamo o n > 30 o distribuzione normalizzata), per BERNULLI serve lo stesso UNA TAGLIA GRANDE, altrimenti 
        #non sarebbe attendibile, occorre comunque un campione di taglia grande, ovvero n > 30, anche se 30 è ancora un po' piccolo, ideale = diverse centinaia
    #2. Trovare i quantili normali (non di student) della distribuzione di Q, Z_1 e Z_2 tali che: 
        #P(t_1 < (media(X)-p) / sqrt[p(1-p)/n] < t_2) = 1-alpha = 0.95
        #*Guardare grafico delle 16:50* (ma è praticamente uguale al grafico dell'IC con alpha = 0.05)
        # Z_1 = q_0,025 = q_(alpha/2)
        # Z_2 = q_0,975 = q_(1-alpha/2)
    #3. Trovare le formule per le 2 variabili a e b tali che: (contengono la media campionaria)
        #P(A<p<B) = 1-alpha=0.95    (Bisogna fare le due disuguaglianze)
        #Otteniamo il seguente intervallo:
            #IC_95%(p) = [p cappelletto -Z_2*(sqrt(p(1-p)/n)), p cappelletto + Z_2*(sqrt(p(1-p)/n))]
            #Z_2 = q_0,975 = 1,96 (valore fisso) (credo da tabelle?) (vedere per 95% su tabella)
            #IC_90%(p) = [p cappelletto -1,64*(sqrt(p cappelletto(1-p cappelletto)/n)), p cappelletto + 1,64*(sqrt(p cappelletto(1-p cappelletto)/n))]
            #IC_99%(p) = [p cappelletto -2,58*(sqrt(p cappelletto(1-p cappelletto)/n)), p cappelletto + 2,58*(sqrt(p cappelletto(1-p cappelletto)/n))]
            #P cappelletto? perché solo in 90% e 99%? (Ricontrollare da lezione 21/11/2022)
            #Tutta area = 99%
            #Quantile stretto, area sotto = 90%
            #Quantile normale, area sotto = 95%
            #q_0,975 = q_(1-0,05) = 1,96
            #q_0,95 = q_(1-0,10) = 1,64
            #q_0,995 = q_(1-0,01) = 2,58
            #Guardare grafico delle 17:00 sull'errore standard sulla proporzione fatto da E su p cappelletto

#Prossima lezione faremo i test
#___________________________________________________________________NUOVA LEZIONE____________________________________________________________________
#Test statistici di ipotesi e Test statistici per una media

#IC PER UNA POPOLAZIONE
#IC_95% (p) = [p cappelletto - 1.96(sqrt(p cappelletto(1-p cappelletto)/n),p cappelletto + 1.96(sqrt(p cappelletto(1-p cappelletto)/n))]
# dove 1.96 è il quantile 1-(alpha/2) nella ~ Normale dove o.95 + il livello di confidenza che abbiamo scelto
#prop.test() --> test che utilizza la statistica di X^2
#binom.test() --> test esatto
#Verrà detto all'esame quale utilizzare dei 2 !!!!!
#Quando non si è sicuri su una procedura di R, usare HELP (ahahah immagina non essere su R ma su VScode)

#calcolare l'intervallo di condifenza per la proprzione di favorevoli a una certa soluzione dato che un campione di 1000 intervistati
#580 si sono dichiarati favorevoli alla soluzione.
x<-580
n<-1000
prop.test(x,n)
# 0.5486573 0.6107204
prop.test(x,n,conf.level = 0.90)
# 0.5536427 0.6059172
prop.test(x,n,conf.level = 0.99)
# 0.5388960 0.6200364

#Test per una proporzione: binom.test
binom.test(x,n)
# 0.5487112 0.6108164
binom.test(x,n,conf.level = 0.90)
# 0.5536772 0.6059729
#Tra binom e proptest si hanno risultati DIVERSI

#Intervalli di confidenza PER 2 MEDIE
    #IC per la differenza tra 2 medie
        #IC per mu_2 - mu_1 , i due campioni
        #Possono essere di 2 tipi: 
            # 1) campioni indipendenti
                #es: tempo medio ad assemblare due componenti dello stesso tipo in 2 linee diverse o separate
            # 2) campioni dipendenti o appaiati
                #es: tempo medio di assemblaggio di componenti prima e dopo la manutenzione (casi diversi)
            #Non sarebbe corretto usare lo stesso tipo di procedure statistiche in entrambi i casi
            #Per calcolare intervallo di confidenza per mu_2 - mu_1 è necessario che entrambi i campioni abbiano taglia maggiore n>= 30
                # o siano estratti da distribuzioni normali
            #Nel caso 1) n_1 e n_2 possono anche essere diverse (non esageriamo però)
            #Nel caso 2) devono avere la stessa taglia
    #1) Parametro da stimare è la differenza tra media: mu_2-mu_1 che vuole dire l'attesa di X nella seconda popolazione
        #meno l'attesa della X nella prima popolazione
        #Ricordare quantità pivotale: 
            #Q =[ media(X_2)-meadia(X_1) - (mu_2-mu_1)] / SE(media(X_2)-media(X_1) [differenza tra i parametri nelle popolazioni a destra][medie campionarie a sinistra]
            #dove SE è lo standard error sulla differenza della medie campionarie
            #SE(X_2-X_1) se le varianze delle 2 popolazioni sigma_2^2 e sigma_1^2 sono uguali ha una forma 1.a)
            #Altrimenti se sono diverse, hanno una forma diversa 1.b)
        #Nel caso 1.a) Q ha una distribuzione del tipo T di Student con un numero di gradi di libertà di f pari a n_1+n_2-2
        #Nel caso 1.b) Q ha una distribuzione T di Student con un numero di gradi di libertà che può essere DECIMALE (non interi)

        #Se non si conosce la  varianza, è molto più furbo provare a usare il 1.b), altrimenti si guarda sia 1.a e 1.b 
        # e si controlla la differenza del valore, se è elevata o un minimo evidente, si fa il test sulle varianze

    #2) Parametro da stimare: mu_2-mu_1
        #In realtà il campione non sarà fatto da coppie di valori ma da differenze, uno per ogni campione, ma da differenza di valori
        #appaiati [(x_1,n),(x_2,n)] (coppie, a forma di coppie)(ma appaiate)
        #Dove diremo d_1 = x_2_1 - x_1_1, con d_n = x_2_n - x_1_n
        #Avremo un campione estratto dalla distribuzione della nuova variabile DIFFEREZA che è D=X_2 - X_1 (maiuscolo perchè sono variabili)
        #L'intervallo di confidenza verrà calcolato per il valore medio di D: mu_D = attesa[D]
            #Q ha una distribuzione T di Student con n-1 gradi, ovvero la taglia di entrembi i campioni -1 (qualcuno faccia una pull per spiegare il -1, grazie)
            #Questo IC non è un IC per la differenza tra medie, MA PER LA MEDIA DI DIFFERENZE
#t.test(1° campione, 2° campione) se caso 1), ovvero indipendenti
#t.test(1° campione, 2° campione,paired=TRUE) se caso 2) ovvero dipendenti

##IC per la differenza tra medie, caso non appaiato
library("UsingR")
data(babies)
str(babies)
table(babies$race)
#Vogliamo calcolare l'IC al 95% per la differenza nelle medie dei pesi alla nascita per bambini di race materna 5 e 7
wt5 <- babies$wt[babies$race == 5] #1° campione
wt7 <- babies$wt[babies$race == 7] #2° campione
length(wt5)
length(wt7)

#Le taglie sono diverse, MA COMPATIBILI e sopratutto MAGGIORI DI 30, non sappiamo se è da una dist Normale
t.test(wt5,wt7) #Sono evidentemente indipendente, quindi uso questa formula
#IC per la differenza tra i pesi medi alla nascita di bambini nati da madri di race 5 e race 7
t.test(wt5,wt7,conf.level=0.90)
t.test(wt5,wt7,conf.level=0.99)
#IC per la differenza tra l'età tra madre e padre

t.test(babies$age,babies$dage,paired=TRUE) #Età media delle madri è più piccola rispetto ai padri, il valore sarà negativo
#La media delle differenze è UN SINGOLO VALORE (se hai letto prima sai il perché)

#Intervallo di confidenza per la differenza tra due proporzioni
    #Popolazione 1 X_1                  Popolazione 2 X_2

#X_1 ~ Bernulli (p_1)                   X_2 ~ Bernulli(p_2)
#p cappelletto_1                        p cappelletto_2
#tramite p cap_1 - p cap_2 -----> p_1 - p_2   (parametro da stimare)[differenza tra 2 parametri]
    #Q =  [p cap_1 - p cap_2 - (p_1 - p_2)]/SE(p cap_1 - p cap_2)
    #Ha una distribuzione approssimabile con la normale all'incremento delle taglie n_1 e n_2
#Useremo prop.test(x_1,x_2,n_1,n_2)
#Esempio dal libro Verzani Esercizio 8-10 p. 282
# n_1 = 1000 persone intervistate
# x_1 = 560 (numero di successi)
# n_2 = 1200
# x_2 = 570
#Calcolare l'IC al 95% per la differenza tra le due proporzioni p_1-p_2

x1 <- 560
n1 <- 1000
x2 <- 570
n2 <- 1200

prop.test(c(x1,x2),c(n1,n2)) #c concatena, ricordare che usiamo delle coppie?
# 95 percent confidence interval:
# 0.04231207 0.12768793
# siccome gli estremi sono positivi, vuol dire che la prima proporzioni è superiore della seconda, nella prima intervista c'erano più casi positivi rispetto alla seconda
#Se l'intervallo di confidenza comprende lo zero, p_1 e p_2 non possiamo affermare che siano diverse
#Altrimenti, se i valori sono negativi, p_1 < p_2, se sono positivi, p_1 > p_2 e possiamo dire che è presente una realzione d'ordine
#Tutto ciò viene affermato con una confidenza del 95%, NON CON CERTEZZA (la statistica inferenziale non ci darà mai un risultato certo)

#Iniziamo a parlare di test
# TEST STATISTICI DI IPOTESI
    #Per controllare la variabilità e misurare l'incertezza viste nella stima statistica usiamo 2 metodi: 
        #Intervalli di confidenza
        #Test di ipotesi (sarà solo una grossa introduzione, ne esistono troppi per studiarli tutti ... in 3 lezioni)
    #I test di ipotesi sono delle procedure statistiche che permettono di validare o no un'ipotesi
    #1) Dobbiamo stabilire il tipo di variabile e quindi il parametro da sottoporre al test
    #2) Stabilire l'ipotesi NULLA h_0 e l'ipotesi alternativa h_1
    #3) Calcolare il valore della statistica di test e quindi il pValue
    #4) Prendere la decisione:
        #Rifiutare h_0
        #Non rifiutare h_0
# DEF: L'ipotesi è una proposizione (quantitativa o no) che riguarda la distribuzione della popolazione, per esempio nel caso
    #quantitativo il valore dei parametri
#DEF : Test parametrici : Le ioptesi riguardano i paramtri di una distribuzione, quindi sono di tipo quantitativo
    #Abbiamo le ipotesi: 
        #h_0: mu=10  ipotesi semplice h=hypoteis, 0 indica 0 differenza, è un'uguaglianza pura e si chiama ipotesi nulla
        #h_1 o H_A : mu != h_0 dove 1/A indica non 0 o alternativo e si chiama ipotesi alternativa
        #h_1 unito a h_0 = insieme dei paramtri
    #h_0: mu = 10
    #h_1: mu!= 10 oppure mu>10 oppure mu=12, ne esistono davvero tante cose possibili di alternative
        # ma se h_1 = mu > 10, per fare l'insieme totale, h_0 sarà mu <= 10, DEVONO FORMARE L'INTERO INSIEME
    #Se l'ipotesi è : 
        #h_0: mu=10
        #h_1: mu!=10 -> two-sided test (a 2 code)
        #h_1: mu>10 -> one sided test right-tail test (a coda destra)
        #h_1: mu<10 -> one sided test left-tail  test (a coda sinistra)
#Il compito del pValue, sarà di rifiutare o accettare le ipotesi
#DEF: Statistica di test: v.a. funzione del campione[dle valore ipotizzato per il parametro] la cui distribuzione è NOTA

#I dati campionari e quindi la realizzazione della statistica di test che viene ottenuta da essi ci consentono di stabilire tramite il calcolo del pValue
    #(che è una probabilità valutata per la statistica di test[che capiremo attraverso la media dei test ?]) se l'ipotesi h_0 va RIFIUTATA o NO e quindi prendere la decisione
#L'esito di un test sarà di questo tipo:
    #"I dati forniscono sufficiente evidenza statistica (oppure NON) per rifiutare h_0 a favore di h_1" !!!!!!! NON è SCRITTO A CASO  !!!!!
    #DEVE essere RIFIUTARE o NO, anche nel TESTO, tutto si basa sul rifiutare o no.



#Un test statistico conduce a rifiutare oppure al non rifiutare l'ipotesi nulla h_0, NON ad accettare, sarebbe ERRORE

#________________________________NUOVA LEZIONE_________________________________________________ 

#Test di ipotesi per una media e per una proporzione 
    #1. ipotsi nulla:
        #H_0: mu = mu_0
        #H_1: mu != mu_0
        #Questo è un caso di test a due code o bilaterale (ci interessa sempre di più l'ipotesi alternativa a quella nulla)

        #H_0: mu <= mu_0
        #H_1: mu > mu_0
        #test ad una coda o unilaterale (coda a destra)

        #H_0: mu >= mu_0
        #H_1: mu < mu_0
        #test ad una coda o unilaterale (coda a sinistra)

    #Test per una media
    #Test per una proporzione
#Idea (valida per tutti i test statistici di ipotesi): 
    #se h_0 è vera, si cerca una v.a. la cui distribuzione sia nota e che coinvolga una statistica (quindi è una funzione delle variabili aleatorie che conpongono il campione).
    #Se in base ai dati si ottiene un risultato attendibile o ragionevole secondo h_0, l'ipotesi nulla non viene rifiutata e viceversa. 
    #Come si decide se il risultato è attendibile? Ce lo dice i P-Value (che è una probabilità)

    #Test per una media: 
        #h_0 : mu = 10   #ipotesi nulla mai con solo > o < siccome dobbiamo calcolare la probabilità, abbiamo bisogno di dare valori specifici alla distribuzione, ottenuto da h_0
        #h_2 : mu > 10 

        #P(1/n * sum di i da 1 ad n di X_i   >  media x = 1/n * sum di i da 1 ad n di x_i) = p (p-value)  cioè la probabilità di ottenere almeno un valore estremo quanto quello ottenuto dai dati
        #ci vuole un mu, quello di h_0
        #Usiamo ed otteniamo la statistica di test con una distribuzione che corrisponde all'ipotesi h_0
                #statistica di test = T_(n-1) ~ (media x_n - mu_0) / S*sqrt(n)  --> T di student con n-1 gradi di libertà
            #Il piccolo del p-value è 0,05
            #media x valore ottenuto dai dati campionari -> probabilità abbastanza grande (> di 0,05): non viene rifiutata
            #media x valore ottenuto dai dati campionari -> probabilità molto piccola rispetto ad h_1 (< di 0,05): viene rifiutata
                 #siamo portati a ritenere che il valore si avveri molto più frequentemente sotto h_1 che non sotto h_0

                #!!!Attenzione le distribuzioni implicate non sono quelle delle popolazioni da cui estraiamo i campioni, ma sono quelle delle statistiche (o delle statistiche di test) come quelle che abbiamo scritto sopra
            #H_0 : mu = 10; 
            #H_1 : mu < 10; (test a coda sinistra, ricordarsi che si basa su H_1)
            #(ci sono grafici interessanti sulle slides)
            # a coda sinistra --> si calcola che la distribuzione sia minore o ugule a quella osservata

            #H_0 : mu = 10;
            #H_1 : mu > 10; (a coda destra)
            #P(media X >= media x)
            

            #H_0 : mu = mu_0
            #H_1 : mu != mu_0 (a due code)
            #si guarda il modulo di entrambi i lati, in quanto osserviamo 2 grafici (< di mu_0 e > di mu_0)
            #--> P(|media X| >=media x)  p-value test a due code

            #Quale statistica di test utilizziamo?
                 #P ((media X - mu_0)/S*swrt(n) > (media x - mu_0)/S * sqrt(n)) = p = P(T_(n-1) > y)
                 # ~T la stessa utilizza per gli IC
                 #Quali sono i valori del p-value che discriminano il rifiuto di h_0 dal non rifiuto?
                    #p = 0,01
                    #p = 0,05 --> se p > 0,05 non si rifiuta, se p < 0,05 si rifiuta
                    #p = 0,10
                #Si può sempre eseguire un T-test? (ovvero con la T di student per ipotesi su una media)
                        #NO, perché la taglia deve essere almeno 30, oppure se n < 30 la distribuzione da cui è stato estratto il campione deve essere normale
                # n >= 30 --> si può eseguire il tet indipendentemente alla distribuzione della popolazione
                # n < 30, occorre verificare che la distribuzione da cui è estratto il campione sia normale
                #Se nessuna delle due è verificata, si possono usare dei test cosidetti parametrici [test di Wilcoxon] che sottopongono a test le mediane (non le vediamo)(mediana è circa le media)
#Comando per i test: t.test(variabile, mu = mu_0, c'è o non c'è la coda [alternative = Less (sinistra), Greater (coda destra)]) #la coda si mette solo se è ad una coda, il default è a due code

library("UsingR")
data("alaska.pipeline")
str(alaska.pipeline)
#Verificare che la media di field defect sia: 
    #1. uguale a 20
    #2. minore di 30
    #3. maggiore di 22
 #Test 1 è bilaterale 
    #H_0 : mu = 20
    #H_1 : mu != 20

t.test(alaska.pipeline$field.defect, mu = 20)
#p.value < 0.05 , quindi si rifiuta:
    #C'è sufficiente evidenza statistica per rifiutare l'ipotesi nulla a favore di quella alternativa

#2. Test unilaterale coda a destra
    #H_0 : mu <= 30
    #H_1 : mu > 30
t.test(alaska.pipeline$field.defect, mu = 30, alternative = "greater")
#p.value < 0,05, quindi si fifiuta:
    #C'è sufficiente evidenza statistica per rifiutare l'ipotesi nulla a favore di quella alternativa

#3. Test unilaterale coda a sinistra
    #H_0 : mu >= 22
    #H_1 : mu < 22
t.test(alaska.pipeline$field.defect, mu = 22, alternative = "less")
#p.value > 0.05 (p.value = 1 [o molto vicino, 0,9999]):
    #I dati non ci forniscono sufficiente evidenza per rifiutare l'ipotesi nulla

#Test sulle PROPORZIONI
    #p cappelletto = n° successi / n° totale di prove = proporzione campionaria, che stima la proporzione effettiva della popolazione
    #A quale domanda dobbiamo rispondere?
        #a partire da una proporzione campionaria, possiamo affermare ad un certo livello di significatività che sia uguale, maggiore o minore di un valore ipotizzato?
    
    #H_0 : p = p_0
    #H_1 : p != p_0 test bilaterale a due code

    #H_0 : p >= p_0
    #H_1 : p < p_0 test unilaterale a coda a sinista

    #H_0 : p <= p_0
    #H_1 : p > p_0 test unilaterale a coda a destra

    #Il campione casuale (estrtatto da una popolazione di bernulli) è il numero di successi x in n prove
        #X ~ Binomiale (n,p) dove p è la prob di successo e n è il numero di prove   [la binom è la somma dei bernulli {credo}]

       #Questa è una STATISTICA #P cappelletto = sum di i da 1 ad n di X_i  --> per n -> inf si avvicina ad una normale
                #X_i ~ Bernulli(p)
        #occorre individuare una statistica di test che contenga il paramtro stimato h_0 che ci permetta di caloclare probabilità del tipo
                #P(P cappelletto > p cappelletto) = p.value  (p cappelletto sotto ipotesi h_0 o nulla)
    #Statistica di test: 
        #(P cappelletto - p_0)/ sqrt([p_0(1-p_0)]/n) ~ sotto H_0, n grande, N(p_0 , sqrt[p_0(1-p_0)/n])

    #prop.test(x,n,p,alternative="") dove x = n° successi, n = n° prove, alternative = testa di coda ?? rivalutare la presenza di p (guardare riga 792)


#______________________________________NUOVA LEZIONE____________________________________________
#Esercizi del verzani
library("UsingR")
data("stud.recs")

 #test per una media
 #h_0 mu_sta.m = 500
 #h_1 mu sta.m != 500
 length(stud.recs$sat.m) # > 30 allora sufficientemente grande, non dobbiamo verificare la normalità
 t.test(stud.recs$sat.m, mu=500)  # nolint
 #p-value = 0.01099 --> non si accetta

 #Es: 9_11
data("babies")
heights <- babies$dht[babies$dht != 99]
#Test a una coda(destra)
#h_0 : mu_dht <= 68
#h_1 : mu_dht > 68
t.test(heights, mu = 68, alternative = "greater") #< 0.05 rifiutiamo h_0 in favore di h_1

#test sulle proporzioni !!

#Presi sempre dal verzani
#Es 9.3 pg 303

#40 su 50 non hanno ricaduta dopo malattia, senza droga percentuale di ricorrenza attesa = 75%
# n = 50 pazienti
# x = 40 numPazienti
# p_0 = 75%
# H_0 : p  <= 0.75
# H_1 : p > 0.75
#si pone come ipotesi alternativa quella che si vorrebbe confermare sperando di avere sufficiente evidenza nei statistica dai dati per negare l'ipotessi opposta nulla

n <- 50

x <- 40
p_0 <- 0.75
prop.test(x, n,p_0, alternative = "greater") #NON abbiamo abbastanza prova statistica per rifiutare l'ipotesi nulla in favore dell'ipotesi alternativa
#In alternativa
binom.test(x, n, p_0, alternative = "greater")
#Non specificare il p implementa automaticamente P = 0.5 !!!!!! (sia per binom che per prop)

#Es 9.7 pg 303
# p_0 = 10%
# 25000 = num
# 2700 = macchine con problemi
p_0 <- 0.10
n <- 25000
x <- 2700
#h_0 : p_0 <= 0.10
#h_0 : p_0 > 0.10 
#Poniamo come ipotesi alternativa quella che vorremmo verificare sperando di rigiutare h_0
prop.test(x, n, p_0, alternative= "greater") #C'è evidente prova statistica per rifiutare l'ipotesi nulla a favore di quella alternativa
binom.test(x, n, p_0, alternative= "greater")
#Sottoponiamo a test l'ipotesi che la percentuale con difetti entro il periodo di garanzia sia inferiore a 0.125
#h_0 : p >= 0.125
#h_1 : p < 0.125
p_0 <- 0.125
prop.test(x, n, p_0, alternative= "less") #C'è abbastanza prova statistica per rifiutare l'ipotesi nulla a favore di quella alternativa

#Puntualizzazione sui test per una proporzione
#Puntualizzazione sui test per due proporzioni

#Parte da un esercizio: 
    #Tasso di povertà begli stati uniti nel 2010:15%
    #Nel 2011 si considera si considera un campione di 150000 persone su cui viene stimato il nuovo tasso di povertà : 15,13%
    #Si può affermare ad un opportuno livello di significatività che il tasso di povertà sia aumentato nel 2011 rispetto al 2010?
     #abbiamo la proporzione di successi, non il numero, dobbiamo solo calcolarcelo
    #n=150000 
    #p_0 = 0.15
    #P_c = 0.1513 proporzione campionaria
    #x = p_c * n

n <- 150000
p_0 = 0.15
p_c = 0.1513
x <- p_c * n
#h_0 : p <= 0.15
#h_1 : p > 0.15
prop.test(x,n,p_0,alternative = "greater") #Non c'è abbastanza prova campionaria per rifiutare l'ipotesi nulla in favore dell'ipotesi alternativa

#Test per due proporzioni
#Con i dati di prima, (avessi anche la taglia del campione del 2010), le proporzioni di povertà negli usa negli anni 2010 e 2011 sono state le stesse?
#Oppure, le previsioni di voto in due momenti diversi
#Funzionamento di un certo tipo di macchinari prima e dopo un intervento di manutenzione
#etc..
#Torniamo ai dati sul tasso di povertà negli USA nel 2010 e 2011
#p_c_1 : tasso povertà stimato nel 2010
#p_c_2 : tasso povertà stimato nel 2011  (la c vuol dire campionaria,cappelletto)
#Domanda : si può affermare che il tasso di povertà negli USA sia aumentato (o invariato) tra il 2010 ed il 2011?

#Domanda 1
#h_0 : p_1 = p_2 (p_1 - p_2 = 0)
#h_1 : p_1 > p_2  #test a coda destra

#Domanda 2
#h_0 : p_1 = p_2 (p_1 - p_2 = 0)
#h_1 : p_1 !=p_2 #test a due code o bilaterale

#X_1 ~ Binom (n_1, p_1) #n_1 = numero di prove, taglia del campione
#X_2 ~ Binom (n_2, p_2) 
#n_1 può essere diverso da n_2
#Statistica di test = stima della proporzione e le sottraiamo l'una dall'altra
# Z = [p_c_1 - p_c_2 -(p_1 - p_2)]/SE(p_1 -p_2)   #differenza ipotizzata tra parentesi, di solito = 0
#Dove SE è l'errore standard sulla differenza tra popolazione
#SE(p_1-p_2)_h0 = sqrt(p_1(1-p_1)/n_1 + p_2(1-p_2)/n_2)
#p_1 e p_2 sono le proporzioni della popolazione, cioè incognite, ma sotto h_0 sono uguali siccome per h_0 (p_1 = p_2)
#che diventa sqrt(p(1-p)*(1/n_1 + 1/n_2)) 
#ma p è ancora incognita: 
    # Stimiamo p come : p_c = (x_1 + x_2 )/ n_1 + n_2
    # = somma dei successi / somma delle taglie dei campioni
# (p_1_c + p_2_c) / n_1 + n_2
#Z = (p_1_c - p_2_c)/ sqrt(p(1-p)*(1/n_1 + 1/n_2)) ~ N(0,1) per taglie abbastanza grandi (proporzioni non gli bastano normali, hanno bisogno di qualche centianaia di campioni)
#Come lo eseguiamo in R?
#prop.test(x,n,p_0,alternative="") dove x = c(x_1,x_2) e n=c(n_1,n_2) dove c è concatenate siccome sono requisiti dei vettori (bisognerebbe mettere p_1 - p_2, ma spesso è 0 e quindi non si mette)

n_1 <- 160000
p_1_c <- 0.1513
n_2 <- 150000
p_2_c <- 0.15
#sottoporre a test l'ipotesi che nel 2012 il tasso di povertà sia diminuito
#h_0 : p1 <= p2
#h_1 : p1 > p2

x_1 <- p_1_c * n_1
x_2 <- p_2_c * n_2
prop.test(c(x_1,x_2),c(n_1,n_2),alternative = "greater") #QUESTO è MOLTO IMPORTANTE

#__________________________NUOVA LEZIONE______________________________
#Es. 9.26 pg. 320
#1250 n_1
#98,9 %

#1100 n_2
#96,9

n_1 <- 1250
p_1_c <- 0.989
n_2 <- 1100
p_2_c <- 0.969
#h_0 : p_1 = p_2
#h_1 : p_1 != p_2 , test a due code

x_1 <- p_1_c *n_1
x_2 <- p_2_c * n_2

prop.test(c(x_1,x_2),c(n_1,n_2))  #p-value = 0.001042
#C'è abbastanza prova statistica per rifiutare l'ipotesi nulla a favore di quella alternativa
#Possiamo sostenere ad un opportuno livello di significatività che la proporzione sia diminuita?

#h_0 : p_1 <= p_2
#h_1 : p_1 > p_2 , test a coda destra
prop.test(c(x_1,x_2),c(n_1,n_2),alternative = "greater")#C'è suffuciente evidenza statistica per rifiutare l'ipotesi nulla a favore di quella alterantiva

#Test sulla differenza tra medie: 

    #Es: confronto da durata medi di 2 dispositivi simili ma prodotti da aziende diverse
    #Es: la pressione di una persona prima e dopo una cura
    #Es: livello di inquinamento medio in due città
    #etc...
        #H_0 : mu_1 = mu_2 (mu_1 - mu_2 = 0)
        #H_0 : mu_1 != mu_2 test bilaterale a due code

        #H_0 : mu_1 <= mu_2
        #H_0 : mu_1 > mu_2 test a coda destra

        #H_0 : mu_1 >= mu_2
        #H_0 : mu_1 < mu_2 test a coda sinistra

    #Statistica di test
         #Condizioni per l'utilizzo del test (t-test): 
            #Se le due taglie n_1 e n_2 dei campioni, non necessariamente uguali, n>= 30, allora si può usare il test
            #indipendentemente dalle distribuzioni
                #Se n_1 o n_2 < 30, è necessario che le due popolazioni siano distribuite normalmente (o tutte o nessune)
            #Se entrambe le condizioni non sono rispettate, allora si usano i test parametrici (che noi non vediamo, lì si confrontano le mediane)
            #(test di tipo wilcoxon)
            #T = [media X_1 - media X_2 - (mu_1 - mu_2 [spesso 0])] / SE(X_1 - X_2) [errore standard sulla differenza delle media campionarie]
        #SE viene calcolato in due 2: 
            #1) le varianze delle popolazioni sono uguali (sigma_1 ^2 = sigma_2 ^2)[var.test{non necessario}]
            #SP = sqrt([(n_1 - 1)*S_1 + (n_2-1)*S_2]/(n_1 - n_2) )
            #T = (media X_1 - media X_2) / SP sqrt(1/n_1  + 1/n_2)
    
            #2) le varainze delle popolazioni diverse (sigma_1 ^2 != sigma_2 ^2)
            #SE = sqrt([S_1^2]/n_1 + [S_2^2]/n_2)
            #T = (media X_1 - media X_2)/sqrt([S_1^2]/n_1 + [S_2^2]/n_2) ~ T (df) [metodo di Welch]

            #3) Caso di default
            #t.test(campione_1,campione_2,alternative=" ")
     #Se il dataset è strutturato in modo da avere un unica variabile quantitativa e una variabile qualitativa dicotomica che divide in 2 sottocampioni
     #la precedente, si può usare anche il comando: 
        #t.test(var.quantutativa ~ var.qualitativa,alternative=" ")    (~ vuol dire: "diviso in base a ")

#Es: tempi di rottura di un macchinario prodotto in due stabilimenti diversi
#tempi  stabilimento
#2.35   1   
#4.28   2
#3.18   2
#etc    1
#h_0 : mu_tempo_1 = mu_tempo_2
#h_1 : mu_tempo_1 > mu_tempo_2

#t.test(tempo ~ stabilimento, alternative = "greater")
#(prima: stabilimento <- as.factor(stabilimento))  per evitare che la consideri qualitativa incece che quantitativa (la rende una factor)

#Esercizi dal Verzani
#9.32 pag 332
library("UsingR")
data("normtemp")
#H_0 : mu_temp_1 = mu_temp_2
#H_1 : mu_temp_1 != mu_temp_2 test a due code

length(normtemp$temperature[normtemp$gender==1])
length(normtemp$temperature[normtemp$gender==2])
#Hanno la stessa lunghezza e sono sufficientemente grandi per l'uso del t.test
t.test(normtemp$temperature~normtemp$gender) #p-value = 0.02394
#L'IC per la differenza tra le media, cioè per mu_1 - mu_2 non contiene lo 0, perciò la differenza è significativa
#se avesse contenuto lo 0, questo sarebbe un'indicazione a favore del fatto che la differenza tra le medie non sia significativa


#9.31
data("babies")
length(babies$age)
length(babies$dage)
#sono uguali e abbastanza grandi
#H_0 : mu_age_m >= mu_age_d
#H_1 : mu_age_m < mu_age_d
t.test(babies$age,babies$dage, alternative="less") # si rifiuta H_0 in favore di H_1
#Per verifica se un campione è normale, si può facilmente usare un istogramma

#9.35

library("MASS")
data("shoes")
#Importante! : test per due medie per campioni appaiati
    #(X_1_i , x_2_i) per i da 1 ad n
    #x_1_1,x_2_1
    #x_1_n,n_2_n
    #In questo caso si considerano le differenze: 
        #D: x_1 - x_2 (di ogni coppia)(differenza)
        #d_n = x_1_n - x_2_n
        #mu_i = media della variabile differenza 
        #media D = 1/n * sum di i da 1 ad n di (x_1_i - x_2_i)

        #mu != 0, mu < 0, mu > 0
        #NOTARE CHE è DIVERSO
    #Non si effettua il test sulla differenza tra 2 medie, ma sulla media delle differenze, !!! usare t.test non è UN ERRORE !!! quando i campioni sono appaiati (non indipendenti)

    #t.test(campione_2,campione_2,paired="T",alternative=" ")
    #Dove entrambi i campioni hanno la stessa taglia, abbastanza grande(>= 30) o se <30 campioni estratti da distribuzioni normali

#
str(shoes)
hist(shoes$A) #meh, abbastanza normale
hist(shoes$B) #meh, anche questo abbastanza normale
#parametri ergonomici sono spesso estratti da distribuzioni normali, quindi possiamo usare il t.test
#H_0 : mu_A = mu_B
#H_! : mu_A != mu_B
t.test(shoes$A,shoes$B,paired="TRUE")
#t = -3.3489, df = 9 (gradi di libertà), p-value = 0.008539
#p-value = 0.008539 < 0.05 rifiutiamo ipotesi nulla

#Si può affermare che ad un opportuno livello di significatività che la misura di A sia minore della misura B in media?
#H_0: mu_A >= mu_B
#H_1: mu_A < mu_B coda a sinistra
t.test(shoes$A,shoes$B,paired="TRUE",alternative="less")
#t = -3.3489, df = 9, p-value = 0.004269
#p-value = 0.004269 < 0.05 : 
        #C'è abbastanza evidenza statistica per sostenere che la media di A sia minore della media di B [rifiutiamo h_0 a favore di h_1]

#FARE ESERCIZI DALLE PAGINE SU MOODLE ASSEGNATE


#____________~~~~~~~~~~~~____________Buono studio e buona fortuna!___________~~~~~~~~~~~~____________                                                # nolint