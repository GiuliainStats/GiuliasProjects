################################################################################
#1° APPELLO - METODI ESPLORATIVI PER BIG DATA
#Studentessa: Lumia Giulia

################################################################################
#OBIETTIVO GENERALE:
#Adattare una procedura che, sulla base delle caratteristiche possedute da un 
#telefono cellulare, sia in grado di prevederne la fascia di prezzo.
#In questo modo Bob, considerando le caratteristiche dei telefoni prodotti dalla
#sua azienda, sarà in grado di applicare il giusto prezzo ai propri prodotti, 
#tenendo in considerazione i prezzi applicati dalla concorrenza.
################################################################################

#PACCHETTI NECESSARI ALL'ANALISI
library(readr)        #importazione
library(dplyr)        #operazioni su dati
library(ggplot2)      #rappresentazioni grafiche
library(gridExtra)    #griglie di ggplot
library(ggcorrplot)   #matrice di correlazione
library(purrr)        #funzioni vettorizzate
library(rpart)        #alberi di classificazione
library(rpart.plot)   #rappresentazione grafo
library(randomForest) #random forest
library(caret)        #confusion matrix
library(viridis)      #palette di colori
library(tidyr)        #funzioni varie

options(scipen=999) 
#disattivo la notazione scientifica per le rappresentazioni grafiche che verranno



################################################################################
#0. IMPORTAZIONE, PREPARAZIONE E CONTESTUALIZZAZIONE DEI DATI
################################################################################

#0.1 Importazione e descrizione del dataset.
(dati_originari<-read_csv("train.csv"))
str(dati_originari)

#Il dataset importato contiene dati riferiti alle caratteristiche strutturali e 
#funzionali di 2000 telefoni cellurare, compresa la fascia del prezzo di vendita. 
#Le 21 caratteristiche rilevate sono:
# - battery_power,ossia la capacità della batteria, misurata in mAh;
# - blue,         variabile dicotomica, indicatrice della presenza o meno del bluetooth;
# - clock_speed,  velocità del microprocessore;
# - dual_sim,     variabile dicotomica, indicatrice della presenza o meno della doppia sim;
# - fc,           megapixel della fotocamera frontale;
# - four_g,       variabile dicotomica, indicatrice della presenza o meno del 4G;
# - int_memory,   capacità della memoria interna, misurata in Gigabyte;
# - m_dep,        profondità dello schermo, espressa in cm;
# - mobile_wt,    peso del telefono cellulare;
# - n_cores,      numero di processori core;
# - pc,           megapixel della fotocamera posteriore;
# - px_height,    risoluzione verticale in pixel;
# - px_width,     risoluzione orizzontale in pixel;
# - ram,          Megabyte di RAM presenti nel telefono cellulare;
# - sc_h,         altezza del telefono cellulare, espressa in cm;
# - sc_w,         larghezza del telefono cellulare, espressa in cm;
# - talk_time,    tempo di chiamata massimo con batteria carica;
# - three_g,      variabile dicotomica, indicatrice della presenza o meno del 3G;
# - touch_screen, variabile dicotomica, indicatrice della presenza o meno del touch screen;
# - wifi,         variabile dicotomica, indicatrice della presenza o meno del wifi;
# - price_range,  codifica della classe di prezzo di appartenenza. 


################################################################################
#0.2 Controllo sulla natura delle variabili ed eventuale trasformazione.
#Alcune variabili sono state importate come numeriche, ma in realtà rappresentano 
#delle codifiche di variabili qualitative (es. presenza o assenza del bluetooth) o
#quantitative suddivise in classi (es. fascia di prezzo). In questo caso è opportuno 
#trasformare tali variabili in fattori, in modo tale che non vengano trattate come numeriche.
#Prima di effettuare la trasformazione è conveniente controllare il numero di 
#modalità con cui le variabili si manifestano nel dataset, perché potrebbero 
#essere presenti degli errori.

#Vettore delle variabili da trasformare in fattori.
fattori<- c("blue","dual_sim","four_g","three_g","touch_screen","wifi","price_range")

dati_originari %>%
  select_if(colnames(dati_originari) %in% fattori) %>%
  map(table)

(dati<- dati_originari %>%
    map_if(colnames(dati_originari) %in% fattori, as.factor) %>%
    #l'output di map_if è una lista per cui ritrasformo in tibble
    tibble::as_tibble())
str(dati)

#Le modalità presenti sono esclusivamente quelle della codifica per cui è
#possibile procedere con la trasformazione in fattori.
#Com'è possibile notare sia dall'output di str sia dall'indicatore posto sotto il
#nome della variabile, la trasformazione in fattori è avvenuta con successo.



################################################################################
#1. ANALISI ESPLORATIVA
################################################################################
#OBIETTIVI:
# - rilevare l'eventuale presenza di dati mancanti;
# - rilevare l'eventuale presenza di dati errati (es. variabili di conteggio con 
#   valori negativi, tassi superiori all'unità...);
# - rilevare l'eventuale presenza di outlier, ossia di valori che si discostano 
#   particolarmente da quelli assunti in genere dal resto delle unità; 
# - evidenziare il potere discriminante delle variabili esplicative rispetto alle
#   modalità della variabile di risposta.


################################################################################
#0.1 Controllo sui valori assunti dalle variabili.
summary(dati)

#OSSERVAZIONI:
# -> Nessuna delle variabili in esame presenta dati mancanti o fuori del proprio campo di esistenza;
# -> Alcune variabili si manifestano nel dataset in maniera inusuale, motivo per
#    il quale sarebbe necessario approfondire ulteriormente la fonte dei dati
#    (es. spesso dei cellulari pari a 0.1 cm, dimensioni particolarmente elevate...).


################################################################################
#0.2 Controllo sull'eventuale presenza di outlier tramite boxplot.

x11()
par(mfrow=c(3,5))
for (i in 1:ncol(dati)){
  if (!(colnames(dati[i]) %in% fattori)){
    boxplot(dati[i], xlab=names(dati[i]), col="darkcyan", horizontal = T)
  }
}  
par(mfrow=c(1,1))
#Le uniche variabili che presentano degli outlier sono:
# - fc, ossia il numero di megapixel della fotocamera frontale;
# - px_heigth, ossia il numero di pixel in altezza nello schermo.


################################################################################
#0.3 Distribuzioni condizionate Xi | Y

#L'output del ciclo è costituito da due liste contenenti le due tipologie di grafici che permetteranno
#di valutare il POTERE DISCRIMINANTE delle variabili esplicative rispetto a quella di risposta:
distr_fattori<-list() #contiene i grafici a barre condizionati delle 6 variabili dicotomiche;
distr_quantit<-list() #contiene i boxplot condizionati delle 14 variabili quantitative.

for(i in 1:(ncol(dati)-1)){
  #condizione in base al tipo di variabile:
  
  if (colnames(dati[i]) %in% fattori){
    # - qualitativa, grafico a barre condizionato;
    distr_fattori[[length(distr_fattori)+1]] <-(
                ggplot(data=dati) + 
                geom_bar(mapping = aes_string(x = "price_range", 
                                              fill = colnames(dati[i])), 
                         position = "dodge", show.legend = FALSE) +
                labs(title = paste(" Frequenza assoluta", colnames(dati[i]), 
                               "| price_range"),
                     x=NULL, y=NULL) +
                scale_fill_manual(values=c("#66CD00", "darkcyan")))}
  
  else { 
    # - quantitativa, boxplot condizionato.
    distr_quantit[[length(distr_quantit)+1]]<- (
                ggplot(data=dati) + 
                geom_boxplot(mapping = aes_string(y = "price_range", 
                                                  x = colnames(dati[i]), 
                                                  fill="price_range"),
                             show.legend = FALSE)) +
                labs(x = paste(colnames(dati[i]), 
                     "| price_range"), y = NULL) 
  }
}

dev.off()
x11()
grid.arrange(grobs=distr_fattori, nrow = 3, ncol = 2, top="DISTRIBUZIONI CONDIZIONATE DELLE VARIABILI ESPLICATIVE QUALITATIVE RISPETTO ALLA FASCIA DI PREZZO",
             bottom="turchese: caratteristica presente
verde: caratteristica assente")
grid.arrange(grobs=distr_quantit, nrow = 5, ncol = 3, top="DISTRIBUZIONI CONDIZIONATE DELLE VARIABILI ESPLICATIVE QUANTITATIVE RISPETTO ALLA FASCIA DI PREZZO")

# - VARIABILI QUALITATIVE DICOTOMICHE: non presentano alcun potere discriminante;
# - VARIABILI QUANTITATIVE: la maggior parte delle esplicative di tipo quantitativo 
#   ha un potere discriminante nullo o minimo rispetto alle modalità della variabile 
#   di risposta, ad eccezione delle variabili Ram, px_width, px_height e battery_power.
#   In un futuro adattamento di una procedura di classificazione, si suppone che tali 
#   variabili verranno utilizzate e che la più rilevante sarà RAM.


################################################################################
#2. ANALISI DI CLASSIFICAZIONE: ALBERO DI CLASSIFICAZIONE SINGOLO
################################################################################
#L'obiettivo da raggiungere richiede l'applicazione delle tecniche di analisi di 
#classificazione: l'ALBERO DI CLASSIFICAZIONE o regressione è una di esse.


################################################################################
#0.1 Suddivisione del dataset in training e validation/test set.
#Le osservazioni non risultano essere state ordinate nel dataset rispetto alla
#variabile di risposta, pertanto l'estrazione potrà avvenire casualmente 
#considerando l'insieme totale delle osservazioni presenti:
# - TRAINING SET   -> 75% delle osservazioni;
# - VALIDATION SET -> 25% delle osservazioni.

set.seed(1234)
id_train <- sample(1:nrow(dati), nrow(dati)*0.75)

training <- dati[id_train,]
test     <- dati[-id_train,]

training %>%
  count(price_range)

test %>%
  count(price_range)
#Training e test set sono bilanciati rispetto alla variabile di risposta.


################################################################################
#0.2 Albero di classificazione ottimo
#Si costruisce un albero di dimensioni elevate e poi si procede all'indietro,
#tramite potatura, rimuovendo i rami meno utili. La scelta dell'albero ottimo, 
#tra quelli appartenenti alla sequenza individuata, viene effettuata secondo la 
#regola UNA VOLTA LO STANDARD ERROR: si considera la stima minore del tasso di 
#errata classificazione per cross validation, si somma il suo standard error e si 
#seleziona l'albero di dimensione minore e tasso stimato inferiore alla soglia individuata.


#Albero da potare
set.seed(1234)
tree_0<-rpart(price_range ~ ., 
      data = training, 
      method = "class", 
      parms=list(split="Gini"),  #funzione d'impurità (default)
      cp=0.0001)
printcp(tree_0) #contiene 40 foglie


#Funzione che adatta la regola UNA VOLTA LO STANDARD ERROR ad un albero di 
#dimensioni massime inserito come argomento.
optimum_tree<- function(albero_max) {
  
  #SOGLIA: MIN XERROR + SE
  soglia <- albero_max$cptable %>%  #estraggo la cptable
    as_tibble() %>%                 #trasformo in tibble
    select(4:5) %>%                 #seleziono xerror, xstd
    slice_min(xerror, with_ties = F) %>%  #seleziono il minimo di xerror
    mutate(soglia=xerror+xstd) %>%        #aggiungo lo standard error
    pull(soglia)                          #estraggo il valore soglia

  #SELEZIONE DELL'ALBERO OTTIMO
  ottimo <- albero_max$cptable %>%  #estraggo la cptable
    as_tibble() %>%                 #trasformo in tibble
    filter(xerror<=soglia) %>%      #alberi ottimi candidati
    slice_min(nsplit)               #tra i possibili alberi ottimi con xerror sotto 
                                    #la soglia seleziono il più semplice e piccolo.
  
  #POTATURA 
  tree_opt <- prune(albero_max,      #poto in corrispondenza del
                    cp = ottimo$CP)  #CP dell'albero ottimo
                    
  #OGGETTI CHE LA FUNZIONE RESTITUIRA' IN OUTPUT
  output<-list("SOGLIA 1 VOLTA LO STANDARD ERROR" = soglia,
               "VALORI ALBERO IDEALE" = ottimo,
               "ALBERO POTATO" = tree_opt,
               "COMMENTO PROCEDURA" = (noquote(paste("La soglia massima di xerror individuata tramite la regola di UNA VOLTA LO SE è pari a ", 
                                                   round(soglia, 4), ".",
                                                   "L'albero più semplice tra quelli con xerror inferiore alla soglia ha ",
                                                   (ottimo$nsplit)+1, "foglie.",
                                                   "La potatura viene effettuata in corrispondenza del cp dell'albero ideale."))))
  return(output)
}

#PLOTCP
#grafico che permette di visualizzare come varia la stima per cross-validation 
#del tasso di errata classificazione al variare della grandezza e del peso
#dato alla complessità dell'albero stessso.
plotcp(tree_0, col=6, lty=5) #albero massimo

#PRUNING
tree_opt <- optimum_tree(albero_max = tree_0) #funzione personalizzata

tree_opt$`COMMENTO PROCEDURA`      #commento sulla procedura di potatura
tree_opt$`VALORI ALBERO IDEALE`    #caratteristiche dell'albero potato

tree_1<- tree_opt$`ALBERO POTATO`
printcp(tree_1)  #contiene 23 foglie

#GRAFO
prp(tree_1, type=2, fallen.leaves = T, cex=0.5, 
    box.palette=list("gold", "#7CCD7C","#00868B", "#009ACD"),
    extra=100, round=0.6)



################################################################################
#0.3 Bontà dell'albero
#Utilizzando il test set (insieme di osservazioni di cui si conosce la classe della
#variabile di risposta di appartenenza, ma non utilizzate in fase di creazione dell'albero)
#valuto la capacità dell'albero creato di classificare un'osservazione in maniera corretta.

#classificazione del test set
test_tree_1<- predict(tree_1,             
                      newdata = test, 
                      type = "class") 

#confusion matrix sulle previsioni del test set
confm_tree_1<-confusionMatrix(data = test_tree_1, 
                                     reference = test$price_range, 
                                     positive = "1")
confm_tree_1$table
confm_tree_1$byClass
 
confm_tree_1$overall[1]                          #accuracy
(err_tree_1<- 1-unname(confm_tree_1$overall[1])) #misclassification rate



################################################################################
#3. ANALISI DI CLASSIFICAZIONE: RANDOM FOREST 
################################################################################
#Il random forest è una procedura di analisi di classificazione nella quale 
#l'esito della classificazione viene ottenuto combinando i risultati provenienti 
#da più alberi, costruiti su gruppi bootstrap del dataset di partenza e campionando, 
#ad ogni split, un certo numero di variabili esplicative da utilizzare.
#L'ottimizzazione deve essere effettuata sia con riferimento al numero di 
#alberi da creare con campioni bootstrap (ntree) sia con riferimento al
#numero di variabili campionate ad ogni split di ogni albero (mtry). 


################################################################################
#0.1 Implementazione della procedura di random forest

# - TRAINING SET
x=training[-21]         #variabili esplicative
y=training$price_range  #variabile di risposta (fattore)

# - TEST SET
xtest=test[-21]         #variabili esplicative
ytest=test$price_range  #variabile di risposta (fattore)


#Implementazione della procedura
#Vi è casualità dovuta al bootstrap e alla scelta di un sottoinsieme di variabili,
#pertanto è necessario fissare il seed per ottenere valori costanti.

foresta_completa<- tibble(num_alberi=integer(), 
                          num_variabili=integer(), 
                          OOB_err.rate=numeric(),
                          TEST_err.rate=numeric())
foreste_varie<-list()

#tempo di esecuzione < 1 min ca.
for (i in 1:20){ set.seed(123)
  
  #Lista contenente le foreste con numero variabile di mtry
  foreste_varie[[i]]<- randomForest(x=x, 
                          y=y, 
                          xtest=xtest,
                          ytest=ytest,
                          mtry = i, 
                          ntree=800, 
                          importance = TRUE) 
  
  #Tibble contenente il tasso di errata classificazione stimato sulle osservazioni
  #OOB e su quelle appartenenti al TEST SET per ciascuna combinazione di ntree ed mtry.
  foresta_completa<- foresta_completa %>%
                      bind_rows(tibble(num_alberi=1:foreste_varie[[i]]$ntree,         #numero di alberi foresta
                                       num_variabili=rep(i,                           #numero di variabili foresta
                                                         foreste_varie[[i]]$ntree), 
                                       OOB_err.rate=foreste_varie[[i]]$err.rate[,1],         #tasso di errata classificazione OOB foresta
                                       TEST_err.rate=foreste_varie[[i]]$test$err.rate[,1]))  #tasso di errata classificazione TEST foresta
}

summary(foreste_varie)
View(foresta_completa)
#La i-esima riga del tibble contiene informazioni riguardo l'errore di classificazione
#calcolato sulle osservazioni OOB e su quelle contenute nel TEST SET, con riferimento
#ad una foresta avente numero di alberi pari a 'num_alberi' costruiti estraendo ad
#ogni split un numero di variabili esplicative pari a 'num_variabili'.


################################################################################
#0.2 Ottimizzazione della procedura di random forest
#Una volta ottenute le 16.000 possibili foreste adattabili fissata la dimensione
#massima della foresta (numero di alberi x numero di variabili), è necessario
#ottimizzare la procedura sia dal punto di vista del numero di alberi contenuti
#nella foresta sia dal punto di vista del numero di variabili estratte.


#ISPEZIONE GRAFICA COMPLESSIVA - TEST SET
foresta_completa %>%
  ggplot(aes(x =  num_alberi, y = TEST_err.rate, 
             group=num_variabili, colour=as.factor(num_variabili))) +
  geom_line() +
  scale_color_viridis(discrete=T) +
  labs(title = "PERFORMANCE DELLE RANDOM FOREST",
       y = "tasso di errata classificazione sulle osservazioni del TEST SET",
       x = "numero di alberi nella foresta",
       color = "n. di variabili") +
  geom_text(data= (foresta_completa %>% 
              filter(num_alberi==800) %>%
              slice_max(TEST_err.rate, n=3)),
            aes(label=num_variabili), hjust=0.1, show.legend = F)
#Il grafico appena rappresentato mostra l'andamento del tasso di errata classificazione
#calcolato sulle osservazioni contenute nel test set al variare del numero di alberi
#presenti nella foresta e del numero di variabili esplicative utilizzate.
#Avendo rappresentato alberi con 20 numeri diversi di variabili esplicative il
#grafico non risulta molto leggibile.
#Nonostante ciò è tuttavia possibile notare che a spezzate più scure, aventi un 
#numero inferiore di variabili (1,2 e 3 come da etichetta) è associato un tasso 
#di errata classificazione più alto rispetto alle spezzate di colore più chiaro).


#ISPEZIONE GRAFICA E CONFRONTO PRESTAZIONI:

# 1. RANDOM FOREST CON ACCURATEZZA MAGGIORE;
foresta_completa %>%
  filter(num_alberi==800) %>%
  slice_min(TEST_err.rate, n=3) %>%
  arrange(num_variabili)
#num_variabili: 13, 14, 15, 16.
#FORESTE CONTENENTI 800 alberi con errore minimo.
#num_variabili   OOB_err.rate    TEST_err.rate
#13              0.105           0.096
#14              0.108           0.1 
#15              0.107           0.1
#16              0.107           0.1

# 2. BAGGING;
#num_variabili: 20 (numero complessivo di esplicative disponibili)

# 3. RANDOM FOREST CON NUMERO DI VARIABILI DI DEFAULT
#num_variabili: sqrt(numero di variabili esplicative disponibili)
sqrt(ncol(dati[-21]))


foresta_completa %>%  
  filter(num_variabili %in% c(5, 13:16, 20) ) %>%
  ggplot(aes(x =  num_alberi, y = TEST_err.rate, 
             group=num_variabili, colour=as.factor(num_variabili))) +
  geom_line() +
  labs(title = "PERFORMANCE DELLE RANDOM FOREST",
       y = "tasso di errata classificazione sulle osservazioni del TEST SET",
       x = "numero di alberi nella foresta",
       color = "n. di variabili") +
  #evidenzio la foresta ottimizzata
  geom_point(aes(x=544, 
                 y=(foresta_completa %>%
                   filter(num_alberi==544,
                          num_variabili==13) %>%
                   select(TEST_err.rate) %>%
                   pull())), colour="#CD3333", pch=18, size=2) +
  geom_text(aes(544, 0.09), label = "Foresta ottima", 
            colour = "#CD3333")
#Rappresentazione grafica delle random forest con:
# - m = l'insieme complessivo delle variabili esplicative;
# - m = radice quadrata del numero di variabili;
# - m = valori di m il cui tasso di errata classificazione, in corrispondenza 
#     di una dimensione pari a 800 alberi, risulta minimo.
#E' possibile notare che le foreste i cui alberi sono stati costruiti utilizzando 
#ad ogni split una variabile esplicativa tratta da un sottoinsieme di 13 permettono di 
#effettuare una classificazione più accurata, a partire da un certo numero di alberi in poi. 

#NUMERO DI VARIABILI OTTIMO: 13
#NUMERO DI ALBERI OTTIMO:    544

set.seed(123)
opt_forest<- randomForest(x=x, y=y, xtest=xtest, ytest=ytest, importance = TRUE,
                          mtry = 13, ntree=544)   #parametri ottimizzati



################################################################################
#4. CONFRONTO PRESTAZIONI: RANDOM FOREST vs ALBERO SINGOLO
################################################################################
#Confronto tra le bontà di classificazione delle osservazioni dello stesso test set,
#utilizzando due procedure differenti: random forest e albero singolo.
#L'obiettivo è quello di capire se la random forest ha permesso di migliorare le 
#prestazioni del singolo albero. 


################################################################################
#0.1 Tasso di errata classificazione sul test set

foresta_completa %>%  
  filter(num_variabili == 13) %>%  #seleziono soltanto le foreste con m ottimo
  ggplot(aes(x = num_alberi)) +
  geom_line(aes(y = TEST_err.rate), colour="#CD5B45") +
  geom_segment(aes(x=0, y=err_tree_1, xend=800, yend=err_tree_1),
               linetype="dashed", colour="#008B00", linewidth=0.7)+
  labs(title = "PRESTAZIONI A CONFRONTO: RANDOM FOREST vs ALBERO SINGOLO",
       y = "tasso di errata classificazione sulle osservazioni del TEST SET",
       x = "numero di alberi nella foresta") +
  #evidenzio la foresta ottimizzata
  geom_point(aes(x=544, 
                 y=(foresta_completa %>%
                      filter(num_alberi==544,
                             num_variabili==13) %>%
                      select(TEST_err.rate) %>%
                      pull())), colour="#CD3333", pch=18, size=3) +
  geom_text(aes(544, 0.09), label = "Foresta ottima", 
            colour = "#CD3333") +
  #aggiungo delle etichette per facilitare la lettura
  geom_text(aes(750, 0.10), label = "Random forest m*=13", colour = "#CD5B45") +
  geom_text(aes(750, 0.17), label = "Albero singolo", colour = "#008B00")
#La random forest ottima migliora le performance del singolo albero di classificazione.


################################################################################
#0.2 Confusion matrix

#Albero singolo ottimo
confm_tree_1$table

#Random forest ottimo
confm_forest<- confusionMatrix(data = unname(opt_forest$test$predicted), 
                               reference = test$price_range)
confm_forest$table

#Confronto
grid.arrange(grobs=list((#1° grafico: ALBERO SINGOLO OTTIMO
                           ggplot(as.data.frame(confm_tree_1$table), 
                           aes(y=Prediction, x=Reference, fill= Freq)) +
                           geom_tile() + 
                           geom_text(aes(label=Freq)) + 
                           ggtitle("CONFUSION MATRIX ALBERO OTTIMO")+
                           scale_fill_gradient(low="white", high="#008B00") + 
                           theme(legend.position = "none")),
                        (#2° grafico: RANDOM FOREST OTTIMA
                           ggplot(as.data.frame(confm_forest$table), 
                           aes(y=Prediction, x=Reference, fill= Freq)) +
                           geom_tile() + 
                           geom_text(aes(label=Freq)) + 
                           ggtitle("CONFUSION MATRIX RANDOM FOREST OTTIMA")+
                           scale_fill_gradient(low="white", high="#008B00") + 
                           theme(legend.position = "right"))),
            nrow = 1, ncol = 2)
#Con la random forest è più frequente che una osservazione di classe i sia 
#correttamente classificata come appartenente alla classe i.


################################################################################
#0.3 Statistiche sulla bontà della procedura adottata

statistiche<- as_tibble(confm_tree_1$byClass) %>%
              mutate("Class"=as.factor(c(0:3)),
                     "Model"="Albero singolo",
                     "Accuracy" = unname(confm_tree_1$overall[1])) %>%
              bind_rows(
                as_tibble(confm_forest$byClass) %>%
                mutate("Class"=as.factor(c(0:3)),
                "Model"="Random forest",
                "Accuracy" = unname(confm_forest$overall[1]))) %>%
              select(1:4, 12:14) %>%
              pivot_longer(cols=c(Accuracy, Sensitivity,Specificity, 
                                  `Pos Pred Value`,`Neg Pred Value`),
                           names_to="Statistic") 
  

statistiche %>%
  filter(Statistic != "Accuracy") %>%
  ggplot(aes(x=Statistic, y=value, group=Model, fill=Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "PRESTAZIONI A CONFRONTO: RANDOM FOREST vs ALBERO SINGOLO",
       y = "Valore delle statistiche",
       x = NULL) +
  geom_text(aes(label = round(value,3), group=value),
            position = position_dodge(1),vjust = 1.5)+
  facet_wrap(~Class, scales="free") +
  scale_fill_manual(values = c("#00CDCD", "#00868B"), 
                    guide = guide_legend(reverse = TRUE))
  
statistiche %>%
  filter(Statistic == "Accuracy") %>%
  ggplot(aes(x=Statistic, y=value, group=Model, fill=Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "PRESTAZIONI A CONFRONTO: RANDOM FOREST vs ALBERO SINGOLO",
       y = "Valore dell'accuracy",
       x = NULL) +
  geom_text(aes(label = round(value,3), group=value),
            position = position_dodge(1),vjust = 1.5) +
  scale_fill_manual(values = c("#00CDCD", "#00868B"), 
                    guide = guide_legend(reverse = TRUE))



######################################################################################
#5. IMPORTANZA DELLE VARIABILI: RANDOM FOREST e ALBERO SINGOLO vs ANALISI ESPLORATIVA
######################################################################################
#In questa fase l'obiettivo è quello di vedere se le variabili più importanti,
#più discriminanti, ossia quelle utilizzate dalle procedure di classificazione
#implementate, siano in linea con quelle ipotizzate attraverso la visualizzazione grafica.


#VARIABILI PIU' IMPORTANTI ANALISI ESPLORATIVA:
# -> RAM , battery_power, px_width, px_height


#VARIABILI PIU' IMPORTANTI ALBERO SINGOLO:
# -> RAM, px_width, px_height, battery_power, int_memory, mobile_wt, 
#    sc_w, m_dep, clock_speed, talk_time

barplot(tree_1$variable.importance[1:10],  
        main="IMPORTANZA DELLE VARIABILI ESPLICATIVE NELL'ALBERO POTATO", 
        ylim=c(0, 700), col=hcl.colors(10, palette = "viridis"), cex.names=0.95)


#VARIABILI PIU' IMPORTANTI RANDOM FOREST:
# -> RAM, battery_power, px_width, px_height, mobile_wt

grid.arrange(grobs=list((#1° misura d'importanza
                          as.data.frame(opt_forest$importance) %>%
                          mutate(variabile=(rownames(opt_forest$importance))) %>%
                          slice_max(MeanDecreaseGini, n=10) %>%
                          ggplot(aes(x=reorder(variabile, -MeanDecreaseGini), 
                                     y=MeanDecreaseGini, 
                                     fill=reorder(variabile, -MeanDecreaseGini))) +
                          geom_bar(stat = "identity", show.legend = F) +
                          scale_fill_viridis_d() + 
                          labs(title = "IMPORTANZA DELLE VARIABILI ESPLICATIVE NELLA RANDOM FOREST",
                               x=NULL, y=NULL) +
                          geom_text(aes(x = 9.5, y = 650, label = "MeanDecreaseGini"))),
                        (#2° misura d'importanza
                         as.data.frame(opt_forest$importance) %>%
                         mutate(variabile=(rownames(opt_forest$importance))) %>%
                         slice_max(MeanDecreaseAccuracy, n=10) %>%
                         ggplot(aes(x=reorder(variabile, -MeanDecreaseAccuracy), 
                                    y=MeanDecreaseAccuracy, 
                                    fill=reorder(variabile, -MeanDecreaseAccuracy))) +
                         geom_bar(stat = "identity", show.legend = F) +
                         scale_fill_viridis_d() + 
                         labs(title = NULL,
                              x=NULL, y=NULL)) +
                         geom_text(aes(x = 9.5, y = 0.45, label = "MeanDecreaseAccuracy"))),
             nrow = 2, ncol = 1)
             
#In conclusione è possibile affermare che i risultati dell'analisi esplorativa 
#sono in linea con quelli dell'analisi di classificazione effettuata.

