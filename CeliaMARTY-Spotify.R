#CELIA MARTY M1 IA

#Most Streamed Spotify Songs 2023 : https://www.kaggle.com/datasets/nelgiriyewithana/top-spotify-songs-2023


#ETAPE 1 :  Formulez clairement les questions spécifiques auxquelles vous souhaitez répondre.
#La problématique générale est la suivante : Quel type de musique faut-il produire pour avoir du succès ?
#Pour cela on va répondre à différentes questions : 
#- Est-ce que le type de notes musicales utilisé dans une chanson influence sa popularité ? 
#- Est-ce que le mode utilisé dans une chanson influence sa popularité ?
#- Est-ce que les chansons ayant une danceability élevée rencontrent le plus de succès ?
#- Est-ce que les chansons qui ont un taux d’instruments élevé rencontrent le plus de succès ?

#ETAPE 2 : Rédigez une courte introduction présentant votre analyse et pourquoi elle est pertinente.
#Dans cette analyse de données tirées des chansons les plus streamées sur Spotify en 2023, on va s'intéresser aux différents facteurs qui contribuent au succès d'une chanson, mesuré ici grâce au nombre de streams.
#Nous allons utiliser les facteurs musicaux tels que le type notes utilisées, le type de mode, le taux de dansabilité, et enfin le taux d'intruments dans une chanson. 
#Cela va nous aider à comprendre si ces facteurs influent sur la popularité d'une chanson. 
#On pourra donc à la fin, en déduire quel type de chanson il faut produire pour être beaucoup streamé sur Spotify et avoir du succès.
#Ces informations peuvent ensuite permettre à des producteurs, des artistes...d'optimiser leurs chances d'avoir des chansons à succès.

#ETAPE 3 : Utilisez des outils statistiques pour comprendre la structure de vos données.
#Importation des données spotify dans R 
spotify <- read.csv("/Users/celiamarty/Desktop/R studio/spotify-2023.csv", header= TRUE, sep =",")

#Affichage des premières lignes de la table
head (spotify)

#Affichage du type des données
str(spotify)

#Résumé des données
summary(spotify)

#Packages
library(ggplot2)
library(tidyverse)
library(readr)
library(readxl)
library(stringr)
library(lubridate)
library(xts)

#ETAPE 4 : Créez des visualisations pour explorer les tendances et les relations dans vos données.
#LES NOTES DE MUSIQUES LES PLUS UTILISÉES
ggplot(spotify, aes(x = key, fill = key)) +
  geom_bar() +
  labs(title = "Répartition du type de notes",
       x = "Notes", 
       y = "Nombre de chansons",
       fill= "Type de notes") +
  theme_classic() +
  scale_fill_manual(values = c("blue","pink","black","orange","purple","brown","sky blue","yellow","grey","red","green","Navy Blue"),
                    name ="Type de notes",
                    guide = guide_legend(reverse = TRUE))+
  ylim(0,150)
#on réalise un diagramme en barre avec pour chaque notes une couleur, pour pouvoir voir quelles notes sont le plus utilisées et inversement.

#TYPE DE MODE ET LE PLUS UTILISÉ
ggplot(spotify, aes(x= mode, fill = mode)) +
  geom_bar() +
  labs(title = "Répartition du mode ",
       x = "Type de Mode", 
       y = "Nombre de chansons",
       fill= "Type de mode") +
  theme_classic() +
  scale_fill_manual(values = c("blue","pink"),
                    name ="Type de mode",
                    guide = guide_legend(reverse = TRUE))+
  ylim(0,600)
#on réalise un diagramme en barre avec deux barres : une pour le mode minor et une pour le mode major, on rappelle que le mode majeur est le mode clair, lumineux, souvent joyeux.
#le mode minor lui est dans les musiques tristes, mélancoliques, sentimentales par exemple.Cela nous permet de voir quel mode domine le plus

#RÉPARTITION DU TAUX DE DANCEABILITY
ggplot(spotify, aes(x = danceability_.)) +
  geom_histogram(binwidth = 10, fill="orange", color="black", alpha=0.7) +
  labs(title = "Répartition de la Danceability",
  x = "Danceability",
  y = "Nombre de chansons") + 
theme_light()
#on réalise un histogramme pour voir le % danceability dans les chansons
#la danceability est l'adaptabilité à la dance

#RÉPARTITION DU TAUX D'INSTRUMENTS
ggplot(spotify, aes(x = instrumentalness_.)) +
  geom_histogram(binwidth = 50, fill="pink", color="blue", alpha=0.7) +
  labs(title = "Répartition de l'instrumentalness",
       x = "Insrumentalness",
       y = "Nb de chansons") + 
  theme_light()

#on réalise un histogramme pour voir le % d'instruments dans les chansons

#ETAPE 5 : Mettez en œuvre toute transformation nécessaire pour préparer vos données pour l'analyse. Cela peut inclure la création de nouvelles variables, la transformation de données, etc.
#création d'une nouvelle colonne qui est uniquement remplie d'entier et de valeurs NA lorsqu'il n'y a rien ou pas de nombres entier.
spotify$streams_entier <- as.integer(spotify$streams)

#supression des NA
spotify <- na.omit(spotify)

#nouvelle colonne qui regroupe par catégories la variable streams_entier ??????
seuils <- c(2000, 20000, 200000,2000000, 20000000,20000000000, Inf)  # Par exemple, trois catégories : Faible, Moyen, Élevé
labels <- c("Très Faible", "Faible", "Moyen Bas", "Moyen Haut", "Plutôt Élevé", "Très élevé")
spotify$streams_group <- cut(spotify$streams_entier, breaks = seuils, labels = labels)


#ETAPE 6 : Sélectionnez les tests statistiques ou modèles appropriés pour répondre à vos questions. Implémentez ces modèles et évaluez leurs performances.
#Type de notes en fonction du succès : boxplot 
boxplot(streams_entier ~ key, data = spotify,
        main = "Type de Notes en fonction du nombre de streams", 
        xlab = "Type de Notes",
        ylab = "Nombre de Streams")
#distribution du nombre de stream selon le type de notes grace à un diagramme en boîte

#Type de mode en fonction du succès : boxplot 
boxplot(streams_entier ~ mode, data = spotify,
        main = "Type de Mode en fonction du nombre de streams", 
        xlab = "Type de Mode",
        ylab = "Nombre de Streams")
#distribution du nombre de stream selon le type de mode grace à un diagramme en boîte

#Taux de dansabilité en fonction du succès : nuage de point + modèle de regression
plot(streams_entier ~ danceability_., data = spotify,
     xlab = "Taux de Dansabilité",
     ylab = "Nombre de streams",
     main = "Relation entre le taux de dansabilité et le nombre de streams")
#Relation entre le taux de dansabilité et le nombre de streams avec un nuage de point 

#modele de regression lineaire simple
modele1 <- lm(streams_entier ~ danceability_., data = spotify) 
summary(modele1) 

#droite de reg
plot(streams_entier ~ danceability_., data = spotify, 
     xlab = "Taux de Dansabilité",
     ylab = "Nombre de Streams",
     main = "Ajustement du modèle de régression linéaire simple") 
abline(modele1, col="blue") 

coeff1 <- coefficients(modele1)
eq1 <- paste0("danceability = ", round(coeff1[2],2), "x + ", round(coeff1[1],2)) 
cat("Équation du modèle:", eq1, "\n")

confint(modele1) 
#la droite d'équation lineaire est la droite bleue dans le nuage de points qui représente le mieux la distribution entre le taux de dansabilité et le nombre de streams.
#le R^2 nous indique si la droite explique une grande partie de la variation du nombre de streams ou pas, ici il est de 0,01099, très proche de 0 donc on dit que le modèle est mal ajusté et qu'il n'explique presque aucunes variations.

#Taux d'instrumental en fonction du succès + modèle de régression
plot(streams_entier ~ instrumentalness_., data = spotify,
     xlab = "Taux d'Instruments",
     ylab = "Nombre de streams",
     main = "Relation entre le taux d'instruments et le nombre de streams")
#Relation entre le taux de d'instruments et le nombre de streams avec un nuage de point 

#modele de regression lineaire simple
modele2 <- lm(streams_entier ~ instrumentalness_., data = spotify) 
summary(modele2) 

#droite de reg
plot(streams_entier ~ instrumentalness_., data = spotify, 
     xlab = "Taux d'instruments",
     ylab = "Nombre de Streams",
     main = "Ajustement du modèle de régression linéaire simple") 
abline(modele2, col="green") 

coeff2 <- coefficients(modele2)
eq2 <- paste0("instru = ", round(coeff2[2],2), "y + ", round(coeff2[1],2)) 
cat("Équation du modèle:", eq2, "\n")

confint(modele2)
#la droite d'équation lineaire verte dans le nuage de points représente le mieux la distribution entre le taux de d'instruments et le nombre de streams.
#le R^2 ici il est de 0.001177, très proche de 0 donc on dit que le modèle est mal ajusté et qu'il n'explique presque aucunes variations.


#ETAPE 7 : Sur la base de vos analyses, tirez des conclusions et répondez aux questions que vous avez formulées au début.
#1
#Grâce au diagramme en barre de l’étape 4, on peut remarquer que la note C# est la plus utilisée, elle apparait dans environ dans 120 chansons. 
#Derrière cette note il y a la barre qui correspond à aucune note de musique. Dans environ 90 chansons il n’y a pas de données donc on peut en déduire que soit il n’y a aucune note de musique utilisée soit ce sont des sons créés à partir de mix de plusieurs bruits/sons qui sont utilisés. 
#On retrouve ensuite les notes G, G# et F dans environ respectivement 90 à 80 chansons. 
#La note D# apparait le moins, dans environ 30 chansons. 
#Avec le diagramme en boîte réalisé dans l’étape 6 qui relie le nombre de streams au type de notes utilisées, on remarque que c’est la note de musique E qui se démarque des autres, cela montre que ce sont les musiques où la note de musique E apparait qui sont streamées le plus. 
#Derrière cette boite il y celle de la note C#, on peut rallier cela au fait qu’elle est beaucoup utilisée et donc que forcément il y a plus de chances pour qu’elle corresponde à des chansons beaucoup streamées. 
#A l’inverse les musiques les moins streamées sont celles qui contiennent la note G. 
#On peut remarquer également que la boite qui correspond à aucune note de musique est au même niveau que la note F. Il ne faut donc pas négliger les chansons qui n’ont pas de notes de musiques car elles sont aussi beaucoup streamées. 
#Pour conclure sur cette question, les chansons qui possèdent la note de musique F ou qui n’ont pas de notes de musiques possèdent le plus de streams. 

#2
#Grâce au diagramme en barre de l’étape 4, on peut remarquer que le mode majeur domine le mode mineur (environ 520 contre 400). 
#On remarque que c’est de nouveau le mode major qui domine avec la boite à moustache (diagramme en boîte de l'étape 6 : Relation entre le nombre de streams  et le type de notes) qui tire le plus vers le haut et qui a les valeurs aberrantes les plus hautes. 
#On peut donc conclure que ce sont les chansons de style major qui sont le plus streamées. 
#Pour rallier avec la question d’avant, F est une gamme majeure, on peut alors dire que c'est logique qu'elle ait du succès. 
#Avec un peu de recul, de manière générale on aime écouter des musiques joyeuses pour se changer les idées, pour le moral, pour faire la fête, danser... donc c’est normal que ce mode domine.  
#Le mode mineur est quant à lui assez visible et cela montre que certaines chansons beaucoup streamées ont un style plus mélancolique.  
#Pour conclure oui ce sont les chansons au style majeur qui sont le plus stremées mais ce n’est pas forcément vrai pour toutes. 

#3
#Grâce à l’histogramme de l’étape 4, on peut voir que la plupart des chansons ont un taux de danceability d’environ 60 à 80%.  
#Au nuage de point réalisé à l’étape 6 qui nous permet de voir la relation entre le nombre de streams et le taux de danceability, on peut voir qu’il y a beaucoup de chansons qui ont un taux de danceability entre 70 et 90% comme remarqué dans l’histogramme d’avant.  
#Mais on voit que les plus streamées ont un taux de danceability entre 50 et 70% 
#Donc on en déduire que ce sont les chansons qui ont un taux de danceability entre 50 et 70% qui sont le plus streamées.  
#On a plus tendance à écouter les chansons qui sont adaptées pour danser que les autres. On peut aussi rallier cette remarque avec le style de mode. Les chansons joyeuses vont être souvent adaptées pour danser et donc il y aura forcément plus de chances de succès que les autres.  

#4
#Grâce à l’histogramme on voit qu’il y a énormément de chansons qui ont un taux très faible d’instruments (quasi toutes à 0%) 
#Avec le nuage de point réalisé après qui relie le taux d’instruments et le nombre de streams, il y a peu de chansons avec un taux d’instruments supérieur à 50% qui ont beaucoup de streams. On peut en conclure que pour avoir du succès il ne faut pas forcément un taux d’instruments élevé.  

#Conclusion générale 
#Pour répondre à notre problématique de départ : quel type de musique faut-il produire pour avoir du succès ? 
#On peut dire qu’il faut produire des musiques de style majeur donc plutôt joyeuses.  
#Qu’il faut utiliser la note de musique F ou pas de notes de musiques à proprement parler.  
#Il faut également faire en sorte que la chanson soit dance c'est à dire avec un taux de danceability assez élevé (entre 70 et 90%).  
#Et enfin posséder un taux d'instruments faible (en dessous de 50%, ou même très proche de 0) 

#ETAPE 8 : Identifiez les limitations de votre analyse et suggérez des étapes ou des analyses futures.
#Il manque des données à certains endroits donc il y a des chansons qui ne sont pas prises en compte. 
#Il faut aussi remarquer que la problématique est basée sur les chiffres de 2023, on ne sait pas les tendances précédentes. Pour pouvoir prédire les tendances du futur on pourrait renouveler cette analyse sur les années précédentes pour voir si les tendances évoluent et de quelle manière en fonction des années. 
#On peut aussi se dire que ces informations peuvent limiter la créativité des artistes, certaines informations peuvent les empêcher de créer une musique par peur que ça ne fonctionne pas bien.  
