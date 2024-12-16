# Rapport du TP 3 - Système expert d'ordre 0+

## Page de Garde

**Binôme :**
- Julien HELLEC
- May LAJNEF

**Date de rendu :** 20/12/2024

**Date de démonstration :** 17/12/2024

**Cours :** IA01 - Intelligence Artificielle

**Université de Technologie de Compiègne**

## Sommaire

1. [Introduction](#introduction)
2. [Sujet d'expertise](#sujet-dexpertise)
3. [Base de connaissances](#base-de-connaissances)
     1. [Base de faits](#base-de-faits)
     2. [Base de règles](#base-de-règles)
4. [Arbre de déduction associé](#arbre-de-déduction-associé)
5. [Implémentation du système expert](#implémentation-du-système-expert)
     1. [Principe général](#principe-général)
     2. [Moteur d'inférences](#moteur-dinférences)
6. [Conclusion](#conclusion)

## Introduction
Le but de ce TP est de réaliser un système expert à l'ordre 0+. Pour cela, on s'appuiera sur une base de faits composée de variables prenant des valeurs booléennes, numériques ou symboliques, pour ensuite concevoir une base de règles et un moteur d'inférence.

## Sujet d'expertise

## Base de connaissances

### Base de faits

### Base de règles

## Arbre de déduction associé

## Implémentation du système expert

### Principe général

Notre système expert de recommandation de cadeaux de Noël repose sur 3 composants :

- Base de connaissances
- Interface utilisateur
- Moteur d'inférences

Concrètement, comment ça marche ?

 - Création des bases générales (à filtrer ultérieurement)
   
On remplit la [base de règles](#base-de-règles) ```*RULES-SE*```, la base de cadeaux ```*CADEAUX-SE*``` et la base de questions  ```*QUESTIONS-SE*``` du système en faisant appel aux fonctions respectives ```add-rule()```, ```add-gift()``` et ```add-question()```.

 - Interaction avec l'utilisateur
   
On pose une question à l'utilisateur. On enregistre sa réponse qui représente la valeur prise par un fait en particulier. Notons qu'à chaque itération, on sélectionne une question parmi la base de questions appropriées dite ```*QUESTIONS*``` qui est construite à partir du filtrage de la base générale de questions dite ```*QUESTIONS-SE*``` selon l'historique des réponses aux questions précédentes. Sachant qu'on associe chaque question à un fait, la remise à jour de la base de questions appropriées ```*QUESTIONS*``` suppose une remise à jour de la [base de faits](#base-de-faits) ```*FACTS*```, en parallèle.

***Filtrage des questions : quelles sont les questions appropriées ?***

 - Traitement en interne
   
En somme, ici, on fait tourner le moteur d'inférences, ce qui est expliqué [ci-dessous dans la section "Moteur d'inférences"](#moteur-dinférences).

 - Renvoi des cadeaux recommandés
   
On retourne à l'utilisateur tous les cadeaux adaptés stockés dans ```*CADEAUX*```. Si cette base est vide, on retourne un message pour signaler l'échec de la recherche.

### Moteur d'inférences
Afin de recommander des cadeaux de Noël, il est plus pertinent de s'appuyer sur des mécanismes de raisonnement déductifs, en inférant sur les valeurs prises par les faits. Ainsi, pour s'inscrire dans cette logique propositionnelle de recommandation de cadeaux de Noël, on utilisera un algorithme de recherche en profondeur en chaînage avant. En effet, on exploitera la base de règles en étant guidé par les valeurs prises par les faits. Plus précisément, on sélectionnera toutes les règles applicables afin de les exécuter. Pour ce faire, on parcourt les règles de la base de règles générale dite ```*RULES-SE*```, et à chaque itération, on répond à la question suivante : 
Est-ce que la règle courante est applicable ?

→ Si "Oui" (c'est-à-dire : les valeurs réelles prises par les faits sont telles qu'elles remplissent la condition de la prémisse) alors : ajouter la conclusion (cadeau proposé) dans la base de cadeaux adaptés dite ```*CADEAUX*``` et désactiver la règle ainsi exécutée (rendre inactif le cadeau utilisé).

→ Si "Non", la règle courante n'est pas exécutée. 

On sort de cette boucle au moment où il n'y a plus aucune règle applicable (condition d'arrêt). On dit que le moteur fonctionne par saturation.

Parlons maintenant plus en détail du procédé de sélection de règles applicables.

***Filtrage des règles : quelles sont les règles applicables ?***

On implémente des fonctions de service pour déterminer si une règle est applicable ou non.

     - Qu'est-ce qu'une règle applicable ?


## Conclusion
