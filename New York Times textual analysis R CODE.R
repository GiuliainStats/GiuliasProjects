#PROVA IN ITINERE METODI ESPLORATIVI PER BIG DATA
#gruppo: Giulia Lumia, Giuliano De Blasi, Marco D'Antoni.

#PACCHETTI NECESSARI ALL'ANALISI
library(readr)        #importazione
library(dplyr)        #operazioni su dati
library(tidytext)     #tokenizzazione, tf-idf
library(RColorBrewer) #colorazione 
library(wordcloud2)   #rappresentazione wordcloud
library(ggplot2)      #rappresentazioni grafiche
library(tidyr)        #funzione separate
library(ggraph)       #network di
library(igraph)       #bigrammi
library(quanteda.textstats)  #leggibilità
library(topicmodels)         #topic model LDA
library(widyr)               #operazioni pairwise

options(scipen=999) 
#disattivo la notazione scientifica per le rappresentazioni grafiche che verranno

#0. IMPORTAZIONE E PREPARAZIONE DEI DATI

(dati <- read_csv2(file = "NewYorkTimes_df.csv") %>%
  filter(mese == 5, giorno %in% c(1:7)) %>%
  group_by(giorno) %>%
  mutate(doc = row_number()) %>%
  select(doc, everything()) %>%
  ungroup())

#Il dataset ridotto contiene 845 articoli pubblicati sul New York Times tra l'1
#e il 7 Maggio 2016. Per ciascun articolo sono indicate le variabili:
# - 'doc', identificativo dell'articolo, condizionatamente al giorno;
# - 'documento', identificativo dell'articolo;
# - 'anno', anno di pubblicazione dell'articolo;
# - 'mese', mese di pubblicazione dell'articolo;
# - 'giorno', giorno di pubblicazione dell'articolo;
# - 'text', testo dell'articolo.

summary(dati) 
#non sono presenti NA.

################################################################################

# 1. ANALISI ESPLORATIVA

################################################################################

# 1.1 ANALISI DEI MONOGRAMMI

#### Parole più frequentemente utilizzate in tutti gli articoli pubblicati nell'arco
#### temporale considerato, non tenendo conto del giorno di pubblicazione.

#Frequenze di apparizione dei monogrammi negli articoli pubblicati nei 7 giorni considerati.
(count_mono1 <- dati %>%    
   unnest_tokens(word, text) %>%  
   anti_join(stop_words) %>%      
   count(word, sort = TRUE))      

#Rappresentazione grafica wordcloud statico.
count_mono1 %>%
  filter(n >= 300) %>%
  wordcloud2(size = 0.4, 
             shape = "diamond", 
             rotateRatio = 10, 
             minSize = 1.4) 
  
#(puntando la parola con il cursore apparirà la sua frequenza assoluta)

#Dall'analisi del wordcloud è possibile notare la presenza di ulteriori stop-words
#da rimuovere, ossia di parole inutili ai fini dell'analisi e assenti nell'elenco
#di stop-words precedentemente rimosse (presenti nel package 'tidytext').
#E' necessario pertanto rimuoverle e ripetere l'analisi.

stop_words2 <- tibble(word = c("ms", "it’s", "including", "called", "10",
                               "don’t", "1", "i’m", "that’s", "added", "told", "found"))

#Rimozione delle stop-words individuate tramite wordcloud.
count_mono2 <- count_mono1 %>%    
  anti_join(stop_words2)        

#Rappresentazione grafica wordcloud statico.
count_mono2 %>%
  filter(n >= 300) %>%
  wordcloud2(size = 0.4, 
             shape = "diamond",
             rotateRatio = 10, 
             minSize = 1.4) 


#### Parole più frequentemente utilizzate in ciascun giorno di pubblicazione.

#Creo un oggetto che non contiene stop-words, da utilizzare anche successivamente.
(dati2 <- dati %>%    
    unnest_tokens(word, text) %>%   
    anti_join(stop_words) %>%        
    anti_join(stop_words2))          

#Estraggo le 5 parole più utilizzate in ciascuno dei 7 giorni considerati
#e le rappresento graficamente tramite un word barplot.
dati2 %>%    
  group_by(giorno) %>%            
  count(word, sort = TRUE) %>%     
  top_n(5) %>%                     
  mutate(word = reorder(word, n)) %>%
  #rappresentazione grafica
  ggplot(aes(word, n, fill = giorno)) +
  geom_col(show.legend = FALSE)+
  facet_wrap(~giorno, scales = "free_y", nrow = 2 ) +
  labs(y = "frequenza parole", x = NULL) +
  coord_flip()

# Dall'analisi del word barplot è possibile notare che "people" e "time",
# tra le parole più diffuse considerando l'insieme degli articoli,
# sono presenti con una frequenza molto elevata negli articoli 
# di ciascun giorno.
# A partire dal terzo giorno in esame tra le parole più rilevanti si trova
# anche il giorno di pubblicazione (monday, tuesday, wednesday, thursay...)


#### Calcolo di alcune quantità riferite alle parole:
#### - TF, Term Frequency;
#### - IDF, Inverse Document Frecuency;
#### - TF-IDF, il prodotto tra le precedenti quantità.

#Per ciascun giorno individuo le 5 parole con valori più elevati di TF-IDF:
#parole con maggiore potere discriminante tra gli articoli pubblicati in giorni diversi.
dati2 %>%
  count(giorno, word, sort = TRUE) %>% 
  bind_tf_idf(word, giorno, n) %>%     
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(giorno) %>%         
  top_n(5) %>%                 
  ungroup () %>%
  print(n = 35) %>%
  ggplot(aes(word, tf_idf, fill = giorno)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~giorno, nrow = 2, scales = "free") +
  coord_flip()

################################################################################

# 1.2 ANALISI DEI BIGRAMMI
#Analizzare i bigrammi (coppie di parole consecutive) piuttosto che i 
#monogrammi permette di inserire i token in un contesto e di ottenere 
#risultati più facilmente interpretabili.

#Per l'analisi dei bigrammi considero soltanto quei bigrammi in cui nessuna
#delle parole che lo compongono è una stop-word.
(bigrammi_token <- dati %>%
   unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%  
   separate(bigram, c("word1", "word2"), sep = " ") %>%      
   filter(!word1 %in% stop_words$word,            
          !word1 %in% stop_words2$word,          
          !word2 %in% stop_words$word,           
          !word2 %in% stop_words2$word))

#### Bigrammi più frequentemente utilizzati in tutti gli articoli pubblicati nell'arco
#### temporale considerato, non tenendo conto del giorno di pubblicazione.

freccia <- grid::arrow(type = "closed", length = unit(.15, "inches"))

#Creo l'oggetto freccia da aggiungere successivamente nello strato
#riguardante gli edges al fine di mostrare la direzione della relazione.

#Visualizzazione dei bigrammi più frequenti.
bigrammi_token %>%
  count(word1, word2, sort = TRUE) %>% 
  print(n = 10) %>%
  #Rappresentazione dei bigrammi più frequenti tramite text network.
  filter(n > 40) %>%            
  graph_from_data_frame() %>%   
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,       
                 arrow = freccia, end_cap = circle(.07, 'inches')) +   
  geom_node_point(color = "seagreen3", size = 6) +              
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +      
  theme_void()

#Il text network mostra la relazione asimmetrica tra le parole
#appartenenti ai bigrammi più frequenti negli articoli.
#L'edge, ossia il collegamento tra i nodi, assume una 
#trasparenza proporzionale alla rarità del bigramma 
#(a frecce più scure corrispondono bigrammi più frequenti).


#### Bigrammi più frequentemente utilizzati in ciascun giorno di pubblicazione.

#Estraggo i 5 bigrammi più utilizzati in ciascuno dei 7 giorni considerati
#e li rappresento graficamente tramite un word barplot.
bigrammi_token %>%    
  unite(bigram, word1, word2, sep = " ") %>%     
  group_by(giorno) %>%               
  count(bigram, sort = TRUE) %>%     
  top_n(5) %>%                       
  mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>%
  #mutate(bigram = reorder(bigram, n)) %>%
  #rappresentazione grafica
  ggplot(aes(bigram, n, fill=giorno)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~giorno, scales = "free_y", nrow = 2 ) +
  labs(y = "frequenza parole", x = NULL) +
  coord_flip()


#### Calcolo di alcune quantità riferite ai bigrammi:
#### - TF, Term Frequency;
#### - IDF, Inverse Document Frecuency;
#### - TF-IDF, il prodotto tra le precedenti quantità.

#Per ciascun giorno individuo i 5 bigrammi con valori più elevati di TF-IDF:
#coppie di parole consecutive con maggiore potere discriminante tra  
#gli articoli pubblicati in giorni diversi.

bigrammi_token %>%    
  unite(bigram, word1, word2, sep = " ") %>%    
  count(giorno, bigram, sort = TRUE) %>% 
  bind_tf_idf(bigram, giorno, n) %>%     
  arrange(desc(tf_idf)) %>%
  mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>%
  group_by(giorno) %>%         
  top_n(5) %>%              
  print(n = 40) %>%
  # !!! alcuni bigrammi hanno lo stesso valore di tf-idf  
  ggplot(aes(bigram, tf_idf, fill = giorno)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~giorno, nrow = 2, scales = "free") +
  coord_flip()

#Tra i bigrammi con valore di tf-idf più elevato nel giorno 6 compare '05 06':
#potrebbe sembrare una stop-word, ma in realtà è possibile si riferisca ad una 
#stagione sportiva (motivo per cui non è stata rimossa).

################################################################################

# 1.3 DISTRIBUZIONE DELLA LUNGHEZZA DEGLI ARTICOLI
#Per ciascuno degli 845 articoli pubblicati nei 7 giorni considerati calcolo 
#la lunghezza, data dal numero di parole in essi contenute, e ne visualizzo
#sia la distribuzione complessiva sia la distribuzione condizionata rispetto 
#al giorno di pubblicazione.

dati %>%  
  unnest_tokens(word, text) %>%   
  group_by(documento)       %>%
  count(documento, sort = T)  %>%  
  ungroup() %>%
  ggplot(aes(n)) +
  geom_histogram(binwidth = 100, fill="#00CD66", col=I("#008B45")) +
  labs(x = "lunghezza articoli", y = "frequenze") 

#ASSE X: lunghezza dell'articolo ottenuta come numero di parole in esso contenute.
#ASSE Y: numero di articoli con lunghezza indicata dall'asse X.

#E' possibile notare che la distribuzione delle lunghezze degli articoli è 
#asimmetrica positiva: la quasi totalità degli articoli ha una lunghezza di 
#al più 2000 parole.


dati %>%
  unnest_tokens(word, text) %>%
  group_by(giorno, doc) %>%
  count(word) %>%
  summarise(totparole = sum(n)) %>%
  ggplot(aes(doc, totparole)) +
  geom_line(color = "#00AFBB", size = 0.5) +
  labs(x = "articolo", y = "Lunghezza articolo") +
  facet_wrap(~giorno, ncol = 3, scales = "free_x")

#ASSE X: numero progressivo attributo all'articolo condizionatamente al giorno.
#ASSE Y: lunghezza dell'articolo ottenuta come numero di parole in esso contenute.
#CONDIZIONAMENTO: giorno di pubblicazione.

#Tramite questa rappresentazione grafica è possibile notare la presenza di
#articoli particolarmente lunghi (picchi nella distribuzione).

#!!! Dovendone considerare lunghezza e successivamente leggibilità, in questa
#    fase dell'analisi si opera sugli articoli contenenti le stop-words. 

################################################################################

# 1.4 INDICE FLESCH
#A ciascuno degli 845 articoli viene attribuito un valore di leggibilità, 
#calcolato tramite l'indice di Flesch.

#L'indice di leggibilità di Flesch è un indice che permette di quantificare
#la leggibilità di un testo, ossia la facilità con cui il lettore medio
#può riuscire a comprenderne il contenuto.
#La formula tiene conto del numero di frasi e parole utilizzate e
#permette di ottenere una quantità che varia tra 0 e 100, 
#suddivisibile in 10 classi di punteggio:
#a punteggi maggiori corrisponde una maggiore leggibilità.

Flesch_index <- textstat_readability(as.character(dati$text),
                                     remove_hyphens = T,
                                     measure = "Flesch")

dati %>%
  mutate(readability = Flesch_index$Flesch) %>%
  group_by(giorno) %>%
  summarise(Flesch = mean(readability)) %>%
  ggplot(aes(giorno, Flesch)) +
  ylim(47.5,60) +
  geom_line(color = "#00B2EE") +
  geom_point(color = "#00688B", size = 2.5) +
  labs(x = "giorno di pubblicazione", y = "media giornaliera indice di Flesh") 

#ASSE X: giorno di pubblicazione.
#ASSE Y: media giornaliera dei valori di leggibilità degli articoli.

#I primi due giorni presentano in media degli articoli di più facile comprensione.



################################################################################

# 2. SENTIMENT ANALYSIS: vocabolari NRC e AFINN

################################################################################

# 2.1 ANALISI DEL SENTIMENT TRAMITE VOCABOLARIO NRC

#NRC è un vocabolario contenente insiemi di parole riferiti a 10 sentiment.
#Tali insiemi di parole non sono però disgiunti poiché una stessa parola 
#può appartenere a più sentiment anche contrastanti (e ad esempio apparire sia
#come 'trust' sia come 'fear').
#Si è deciso di operare su due macrocategorie di sentiment: 
# - positivi (joy, positive, trust);
# - negativi (anger, disgust, fear, negative, sadness).

#Da queste categorie sono state escluse le parole ambigue, ossia quelle 
#precedentemente attribuite a sentiment sia positivi sia negativi, nonché
#i sentiment 'anticipation' e 'surprise' in quanto non univocamente riconducibili  
#a nessuna delle due categorie (si potrebbero interpretare in entrambi i modi).

get_sentiments("nrc") %>%
  count(sentiment) 
length(unique(get_sentiments("nrc")$word))  
#Il numero di parole diverse incluse in NRC è diverso dal numero di parole totali
#in esso contenute: i sentiment hanno parole in comune.


#Creazione del nuovo vocabolario con le macrocategorie POSITIVE e NEGATIVE
(nrc_def <- get_sentiments("nrc") %>%
  filter(sentiment %in% c("sadness", "anger", "negative", "joy",
                          "positive", "fear", "disgust", "trust")) %>%
  group_by(word, sentiment) %>%
  count(word, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(positivi = sum(trust, positive ,joy), 
         negativi = sum(anger, disgust, fear, negative, sadness))%>%
  filter(negativi == 0 | positivi == 0) %>% 
  mutate(sentiment = if_else(positivi > 0,"positive", "negative")) %>%
  select(word, sentiment))


#Andamento del sentiment degli articoli nei 7 giorni considerati:
#per ciascun articolo il sentiment si ottiene come differenza tra il numero di
#parole positive e il numero di parole negative utilizzate.
dati2 %>%
  inner_join(nrc_def) %>%
  group_by(giorno, doc) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentimento = positive - negative) %>%
  ungroup() %>%
  #rappresentazione grafica
  ggplot(aes(doc, sentimento, fill = giorno)) +
  geom_col(show.legend = FALSE) +
  geom_abline(intercept = 0, slope = 0)+
  facet_wrap(~giorno, scales = "free_x", nrow = 2) +
  labs(y = "Sentiment",
       x = "articolo") 

#ASSE X: numero progressivo attributo all'articolo condizionatamente al giorno.
#ASSE Y: sentiment dell'articolo ottenuto come: #parole+ - #parole- .
#CONDIZIONAMENTO: giorno di pubblicazione.


#Parole che contribuiscono maggiormente al sentiment, sia positivo sia negativo,
#di ciascun giorno di pubblicazione: 
#per ciascun giorno di pubblicazione si considerano le 3 parole positive e 
#negative maggiormente utilizzate negli articoli pubblicati.

dati2 %>%
  inner_join(nrc_def) %>%
  group_by(giorno, sentiment) %>%
  count(word) %>%
  arrange(giorno, desc(n)) %>%
  top_n(3) %>%
  mutate(word = reorder(word, n)) %>%
  ungroup() %>%
  #rappresentazione grafica
  ggplot(aes(word, n, fill = sentiment))+
  geom_col(show.legend = F ) +
  labs(y = "Contribution to the daily sentiment", 
       x = NULL) +
  facet_wrap(~giorno, scales = "free_y", nrow = 2) +
  coord_flip()
 
#ASSE X: frequenza di utilizzo delle parole (intesa come contributo al sentiment del giorno).
#ASSE Y: 3 parole positive e negative maggiormente utilizzate negli articoli di quel giorno.
#CONDIZIONAMENTO: giorno di pubblicazione;
#ROSSO: parole negative;
#AZZURRO: parole positive.

#Non è stato possibile ordinare in base al sentiment, neanche specificando
#i livelli del fattore con:
#factor(sentiment, levels = c("positive", "negative")


################################################################################

# 2.2 ANALISI DEL SENTIMENT TRAMITE VOCABOLARIO AFINN

#AFINN è un vocabolario contenente un insieme di quasi 2500 parole differenti 
#alle quali è stato assegnato un valore da -5 a +5 (0 escluso):
# - i valori positivi indicano l'intensità del sentiment positivo espresso dalla parola;
# - i valori negativi indicano l'intensità del sentiment negativo espresso dalla parola.

#Andamento del sentiment degli articoli nei 7 giorni considerati:
#per ciascun articolo il sentiment si ottiene come somma delle intensità delle
#parole in esso contenute; valori positivi di tale somma, ad esempio,
#indicheranno un sentiment positivo per l'articolo di riferimento.

(Afinn <- get_sentiments("afinn"))

dati2 %>%
  inner_join(Afinn) %>%
  group_by(giorno, doc) %>%
  summarise(sentiment = sum(value)) %>%
  #rappresentazione grafica
  ggplot(aes(doc, sentiment, fill = giorno)) +
  geom_col(show.legend = F) +
  geom_abline(intercept = 0, slope = 0)+
  facet_wrap(~giorno, scales = "free_x", nrow = 2)+
  labs(y = "Sentiment",
       x = "articolo")

#ASSE X: numero progressivo attributo all'articolo condizionatamente al giorno.
#ASSE Y: sentiment dell'articolo ottenuto come somma delle intensità delle parole.
#CONDIZIONAMENTO: giorno di pubblicazione.

#Rispetto al sentiment ottenuto utilizzando il vocabolario NRC, tramite AFINN è
#possibile notare che in realtà gli articoli hanno un sentiment molto meno 
#positivo (si noti la presenza di numerosi valori negativi).


#Parole che contribuiscono maggiormente al sentiment, sia positivo sia negativo,
#di ciascun giorno di pubblicazione: 
#per ciascun giorno di pubblicazione si considerano le 6 parole positive o 
#negative maggiormente utilizzate negli articoli pubblicati, pesate per la
#intensità del sentiment che esprimono: maggiore sarà l'intensità del sentiment
#e il numero di volte in cui una parola è stata utilizzata in un articolo, 
#maggiore sarà il suo contributo al sentiment generale dell'articolo. 

dati2 %>%
  inner_join(Afinn) %>%
  group_by(giorno) %>%
  count(word,value) %>%
  mutate(contrib = abs(value*n)) %>%
  #è necessario considerare il valore assoluto perché l'intensità potrebbe essere negativa
  arrange(giorno, desc(abs(contrib))) %>%
  top_n(6) %>%
  mutate(word = reorder(word, contrib)) %>%
  ggplot(aes(word, contrib, fill = n*value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(y = "Contribution to the daily sentiment", 
       x = NULL) +
  facet_wrap(~giorno, scales = "free", nrow = 2)+
  coord_flip()

#ASSE X: frequenza di utilizzo delle parole ponderata per l'intensità del sentiment, espressa in valore assoluto.
#ASSE Y: 6 parole positive o negative con il contributo maggiore negli articoli di quel giorno.
#CONDIZIONAMENTO: giorno di pubblicazione.

#Osservando le parole più rilevanti è possibile intuire anche l'argomento principale 
#(o gli argomenti) degli articoli pubblicati in ciascun giorno di pubblicazione. 
#Per ottenere informazioni più precise a riguardo è però necessario adattare un topic model.



################################################################################

# 3. TOPIC MODEL: latent Dirichlet allocation

################################################################################

#### L'obiettivo è adattare diversi modelli LDA, con numero di topic variabile
#### da 3 a 8 e scegliere il numero di topic ottimale sulla base della 
#### correlazione interna media tra le parole che, con maggiore probabilità, 
#### caratterizzano un certo topic: 
#### maggiore è la correlazione interna tra parole di uno stesso topic maggiore è
#### la plausibilità con cui coppie di esse si presenteranno in uno stesso articolo.


#### 1. Creazione della document term matrix.
(articoli_dtm <- dati %>%    
   unnest_tokens(word, text) %>%  
   anti_join(stop_words) %>%      
   anti_join(stop_words2) %>% 
   count(documento, word, sort = TRUE) %>%    
   cast_dtm(documento, word, n))              

dim(articoli_dtm)
#La DTM ha dimensioni 845*36536 (num. articoli * num. parole differenti)


#### 2. Modello LDA ed estrazione di 20 parole per topic.
#Creo una funzione che richiede in input la DTM e il numero di topic e
#restituisce in output  una lista contenente, in ciascun elemento, le 20
#parole con maggiore probabilità di appartenere ai k topic.

venti_parole <- function(dtm, n_topic) {
  #dtm: document term matrix
  #n_topic: numero di topic in base a cui adattare il modello LDA
  
  #adatto il topic model LDA 
  parole<-LDA(dtm, k = n_topic, control = list(seed = 1234)) %>%
    #estraggo le probabilità beta
    tidy(matrix = "beta") %>%
    #raggruppo per topic 
    group_by(topic) %>%
    #seleziono per ciascun gruppo (topic) le 20 parole con beta maggiore
    slice_max(beta, n = 20) %>%
    ungroup() %>%
    #colonne: topic, term, beta (rimuovo beta)
    select (-beta)
  
  #creo una lista vuota in cui riporre l'output del ciclo for
  parole_filtrate<-vector(mode='list', length=n_topic)
  #ciclo esteso a ciascuno dei k topic
  for (i in 1:n_topic){
    parole_filtrate[i]<- parole %>%
      filter(topic == i) %>%
      select(term) %>%
      as.vector()
  }
  #mando a video l'output 
  return(parole_filtrate)
} 


#### 3. Correlazione interna tra parole appartenenti ad uno stesso topic.
#Creo una funzione che, dato il dataset iniziale, calcola la correlazione
#interna a ciascuno dei topic individuati dal modello.

#La pairwise correlation esprime quanto è più facile osservare, o meno,
#le coppie di parole appartenenti ad uno stesso topic nei vari articoli.
# -> data una coppia di parole i e j la correlazione a coppie si calcola 
#    considerando il numero di articoli in cui sono contemporaneamente assenti
#    o presenti sia i sia j e il numero di articoli in cui è presente soltanto 
#    una delle due (i o j).

corr_interna <- function(tibble_word, topic_words) {
  #tibble_word: oggetto avente le colonne 'documento' e 'word' (documento e parole in esso contenute)
  #topic_words: lista di lunghezza k in cui ciascun elemento è un vettore di parole riferito a ciascuno dei k  topic.
  
  #creo una vettore di zeri in cui inserire l'output del ciclo
  medie_topic<-rep(0, length(topic_words))
  #per ciascuno dei k topic contenuti nell'argomento in input:
  for(i in 1:length(topic_words)){
    #considero l'oggetto contenente i dati iniziali
    medie_topic[i]<-tibble_word %>%
      #filtro le parole contenute in ciascuno dei topic
      filter(word %in% topic_words[[i]]) %>%
      #calcolo la correlazione tra le coppie di parole
      pairwise_cor(word, documento) %>%
      select(correlation) %>%
      #calcolo la media per ciascun topic
      colMeans(na.rm = T)
  }
  #mando a video l'output 
  return(medie_topic)
}


#### 4. Considerando le 6 diverse suddivisioni in k topic (3:8), estraggo le
####    20 parole più 'caratteristiche' di ciascun topic e ne calcolo la 
####    correlazione interna (un valore per ciascun topic).
####    Calcolo poi il valore medio di correlazione interna corrispondente 
####    ad una certa suddivisione in k topic.

#NUMERO DI TOPIC = 3
(topic_3<-venti_parole(articoli_dtm, 3)) 
(corr_3<-corr_interna(dati2, topic_3))
mean(corr_interna(dati2, topic_3))    #0.1071791

#NUMERO DI TOPIC = 4
(topic_4<-venti_parole(articoli_dtm, 4)) 
(corr_4<-corr_interna(dati2, topic_4))
mean(corr_interna(dati2, topic_4))    #0.1119556

#NUMERO DI TOPIC = 5
(topic_5<-venti_parole(articoli_dtm, 5)) 
(corr_5<-corr_interna(dati2, topic_5))
mean(corr_interna(dati2, topic_5))    #0.1008093

#NUMERO DI TOPIC = 6
(topic_6<-venti_parole(articoli_dtm, 6)) 
(corr_6<-corr_interna(dati2, topic_6))
mean(corr_interna(dati2, topic_6))    #0.0912329

#NUMERO DI TOPIC = 7
(topic_7<-venti_parole(articoli_dtm, 7)) 
(corr_7<-corr_interna(dati2, topic_7))
mean(corr_interna(dati2, topic_7))    #0.1097703

#NUMERO DI TOPIC = 8
(topic_8<-venti_parole(articoli_dtm, 8)) 
(corr_8<-corr_interna(dati2, topic_8))
mean(corr_interna(dati2, topic_8))    #0.1047389


#### 5. Scelta del modello ottimale
#### Il topic model ottimale, scelto sulla base della correlazione interna 
#### media, è quello che permette di distinguere 4 topic.

topic_4 #parole appartenenti a ciascun topic (top20 con beta maggiore)
corr_4  #correlazione interna a ciascuno dei 4 topic


#### 6. Rappresentazione grafica del modello ottimale (k=4)
LDA(articoli_dtm, k = 4, control = list(seed = 1234)) %>%
  tidy(matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 20) %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  labs(y = NULL) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

#Osservando le parole che con maggiore probabilità sono state generate da
#ciascuno dei 4 topic individuati si presume che, negli articoli pubblicati
#dal New York times nella prima settimana di maggio siano stati principalmente
#affrontati i seguenti argomenti:
# - POLITICA/ELEZIONI (Trump, Clinton, government, president, party, campaign...);
# - VITA QUOTIDIANA (art, city, house, design, home, music, museum, family...)
# - SPORT (game, season, team, games, league, play, players, yankees, won...)
# - ECONOMIA (company, business, companies, market, executive, chief, quarter...).
