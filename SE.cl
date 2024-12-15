;; ########### CAS D'USAGE DU SYSTEME EXPERT ###########
;; CHAINAGE-AVANT exemple 1:  
 ;; (chainage-avant)
 ;; > 4
 ;; > intellectuelle 
 ;; > reflexive
 ;; > cartes
 ;; > Nous avons trouvons une activit� qui vous convient : le Bridge !


;; CHAINAGE-AVANT exemple 2:  
 ;; (chainage-avant)
 ;; > 5
 ;; > physique
 ;; > terrain
 ;; > 3
 ;; > Nous avons trouvons une activit� qui vous convient : le Baseball !

;; CHAINAGE-ARRIERE:
 ;; (chainage-arriere)
 ;; > "Cyclisme"
 ;; > Physique
 ;; > EXTERIEUR
 ;; > 1
 ;; L'activite Cyclsime convient � l'utilisateur.


;; Code du SE � compiler:
(progn
  (format t "~%Opening expert system...")
  
  ;; Initialisation a NIL des diff�rentes variables globales au SE
  (setq *RULES-SE* NIL)
  (setq *QUESTIONS-SE* NIL)
  (setq *ACTIVITIES-SE* NIL)
  (setq *RULES* NIL)
  (setq *QUESTIONS* NIL)
  (setq *ACTIVITIES* NIL)
  (setq *FACTS* NIL)
  (setq *HISTORIC* NIL)
  
  
  ;; ########### FONCTIONS EN RAPPORT AVEC LES REGLES ########### 
  
  ;; Fonction d'ajout d'une r�gle 
  (defun add-rule (conditions conclusion)
    (let ((id (gentemp "R")))
      (set id (list (list 'conditions conditions) (list 'conclusion conclusion)))
      (pushnew id *RULES-SE*)
      )
    )
  
  ;; D�finition de fonctions d'acc�s
  (defun get-conditions-rule (rule)
    (cadr (assoc 'CONDITIONS rule))
    )

  (defun get-conclusion-rule (rule)
    (cadr (assoc 'CONCLUSION rule))
    )

  ;; Fonction de d�sactivation d'une r�gle
  (defun desactive-rule (rule)
    (setf *RULES* (delete-if #'(lambda (item) (eq (symbol-value item) rule)) *RULES*))
    )
    
  ;; Fonction d'ex�cution d'une r�gle
  (defun execute-rule (rule)
    (let ((conclusion (get-conclusion-rule rule)))
      ;; (pushnew (symbol rule) *HISTORIC*)
      (pushnew conclusion *FACTS*)
      )
    )
  
  
  ;; Fonction de v�rification si une r�gle peut-�tre enclench�e, ne peut plus �tre enclench�e, ou doit attendre un prochain �tat
  (defun check-rule (rule)
      (let ((to-delete 0)(execute 1))
        (dolist (condition (get-conditions-rule rule)) ;; Pour chaque condition ET de la regle
          (if (is-fact-defined  (car condition)) ;; On regarde si la variable est definie
              (progn 
                (setq execute (* (check-condition condition) execute)) ;; On check la du test
                (if (= (check-condition condition) 0) (setq to-delete 1))
                )
              (setq execute 0)    
              )
          )
        (if (= execute 1) (progn (execute-rule rule)(desactive-rule rule))) ;; On execute eventuellement la regle si toutes les conditions sont valides
        (if (= to-delete 1)(desactive-rule rule)) ;; On supprime la regle si au moins un test est 0, ou si tous les tests sont valides
        execute
        )
    )
  
  ;; Fonction pour v�rifier toutes les r�gles avec la base de fait
  (defun check-rules ()
    (let ((executed NIL))
    (dolist (rule *rules* T) ;; Pour chaque regle
      (if (= (check-rule (symbol-value rule)) 1) (setq executed T)) ;; On verifie la regle
      )
      executed
      )
    )
  
  ;; Fonction pour v�rifier si un fait est d�fini (pour par la suite prendre sa valeur)
  (defun is-fact-defined (fact)
    (if (member fact *FACTS*) T NIL)
    )
  
  ;; Fonction qui effectue le test condition (variable operateur valeur) -> retourne le test
  (defun check-condition (condition)
    (if (funcall (cadr condition) (symbol-value (car condition)) (caddr condition)) 1 0)
    )
   
  
  
  ;; ########### FONCTIONS EN RAPPORT AVEC LES QUESTIONS ########### 
  
  ;; Fonction d'ajout de fonction 
   (defun add-question (question response)
    (let ((id (gentemp "Q")))
      (set id (list (list 'question question) (list 'response response)))
      (pushnew id *QUESTIONS-SE*)
      )
    )
  
  ;; D�finition de fonctions d'acc�s
  (defun get-question (question)
    (cadr (assoc 'QUESTION question))
    )
   (defun get-response-var (question)
    (cadr (assoc 'RESPONSE question))
    )
  
  ;; Fonction qui pose une question
  (defun ask-question (question)
    (format t "~%~S~%> " (get-question question)) ;; Pose la question
    (clear-input)
    (set (get-response-var question) (read)) ;; Lis la reponse
    (pushnew (get-response-var question) *facts*) ;; Enregistre la reponse
    (setf *QUESTIONS* (delete-if #'(lambda (item) (eq (symbol-value item) question)) *QUESTIONS*)) ;; Delete la question posee
    )
  
  
  
   ;; ########### FONCTIONS EN RAPPORT AVEC LES ACTIVITES ########### 
  
  ;; Fonction d'ajout d'une activite
  (defun add-activity (conditions activity description)
    (let ((id (gentemp "A")))
      (set id (list (list 'conditions conditions) (list 'activity activity)(list 'description description)))
      (pushnew id *ACTIVITIES-SE*)
      )
    )
  
  ;; Fonction d'acces
  (defun get-conditions-activity (activity)
    (cadr (assoc 'conditions activity))
    )
  (defun get-activity-activity (activity)
    (cadr (assoc 'activity activity))
    )
  (defun get-description-activity (activity)
    (cadr (assoc 'description activity))
    )
  
  ;; Fonction pour trouver retrouver une activit� par identifiant demand�
  (defun ask-activity ()
    (let ((act NIL)(returnID NIL))
      (format t "~%Indiquez le nom de l'activit� dont vous voulez v�rifier si elle vous convient~%> ") ;; Pose la question pour savoir l'activit�
      (clear-input)
     (setq act (read))
     (dolist (activity *ACTIVITIES*) ;; Cherche l'activit� dans la base d'activit�s
       (if (equal (get-activity-activity (symbol-value activity)) act) (setq returnID activity))
       )
     returnID
     )
   )
  
  ;; Fonction de d�sactivation d'une activite
  (defun desactive-activity (activity)
    (setf *ACTIVITIES* (delete-if #'(lambda (item) (eq (symbol-value item) activity)) *ACTIVITIES*))
    )
  
  ;; Fonction pour v�rifier si un fait est toujours atteignable A -> A existant en conclusion d'une r�gle encore active
  (defun is-fact-possible (fact)
    (let ((possible NIL))
      (dolist (rule *rules*) ;; Pour chaque regle
        (if (equal (get-conclusion-rule (symbol-value rule)) fact) (setq possible T)) ;; On regarde si la conclusion correspond au fait recherche
        )
      possible
      )
    )
  
  ;; Fonction pour v�rifier si une condition est toujours atteignable '(A B C) -> A OU B OU C
  (defun is-condition-possible (condition)
    (let ((possible NIL))
      (dolist (fact condition) ;; Pour chaque fait, on regarde s'il est encore atteignable (il en faudra au moins 1)
        (if (is-fact-possible fact) (setq possible T))
        )
      possible
      )
    )
  
  ;; Fonction pour v�rifier si un fait est approuv�e
  (defun is-fact-approved (fact)
    (if (member fact *facts*) T NIL) ;; Fait boolean valide correspond a sa presence dans la base de faits
    )
  
  ;; Fonction pour v�rifier si une condition est approuv�e
  (defun is-condition-approved (condition)
    (let ((approved NIL))
      (dolist (fact condition) ;; Pour chaque condition OU, on verifie si c'est valide
        (if (is-fact-approved fact) (setq approved T))
        )
      approved
      )
    )
  
  
  ;; Fonction pour v�rifier si activit� est possible
  (defun check-activity (activity)
    (let ((possible T)(to-delete NIL))
      (dolist (condition (get-conditions-activity activity)) ;; Pour chaque conditions ET des activites
        (if (not (is-condition-approved condition)) ;; On verifie si la condition est valide
            (progn 
              (setq possible NIL)
              (if (not (is-condition-possible condition)) ;; On verifie si elle est encore possible (pour etre validee), si elle ne peut pas etre validee, on la supprime
                  (setq to-delete T)
                )
              )
          )
        )
      (if to-delete (desactive-activity activity)) 
      possible
      )
    )
  
  (defun check-activities ()
    (let ((act NIL))
      (dolist (activity *activities*) ;; Pour chaque activite
        (if (check-activity (symbol-value activity)) (setq act activity)) ;; On la "met a jour" (supprime si elle ne pourra jamais etre atteinte)
        )
      (if (= (length *activities*) 1) (setq act (car *activities*))) ;; S'il ne reste qu'une activite, on la renvoit
      act
    )
    )
  
  ;; Fonction permettant de trouver la prochaine question � poser
  (defun ask-better-question ()
    (let ((questions NIL)(param NIL)(best-score 0)(question-to-ask NIL))
      (dolist (activity *activities*) ;; On parcours toutes les activites
        (dolist (condition (get-conditions-activity (symbol-value activity))) ;; Pour chaque condition ET des activites 
          (dolist (fact condition) ;; Pour chaque condition OU des activites
            (setq questions (increment-var-to-fact fact questions)) ;; On incremente le compteur question permettant de clarifier le fait de la condition
            )
          )
        )
      (dolist (q questions) ;; On cherche la question avec le meilleur score
        (if (>= (cadr q) best-score)
            (progn
              (setq best-score (cadr q))
              (setq question-to-ask (car q))
              )
        )
        )
      (if question-to-ask (progn (ask-question (symbol-value question-to-ask)) T) NIL) ;; On pose la question si elle existe et on retourne T, NIL sinon
      )
    )
  
  ;; Fonction qui incr�mente les variables questions n�cessaires pour activer une r�gle menant au but
  (defun increment-var-to-fact (fact variables)
    (let ((variables-r variables)) 
    (dolist (rule *rules*) ;; Pour toutes les regles
      (if (eq (get-conclusion-rule (symbol-value rule)) fact) ;; Si la conclusion de la regle mene au fait que l'on cherche
          (progn 
            (dolist (condition (get-conditions-rule (symbol-value rule))) ;; Alors, pour toutes les conditions de cette regle, on incremente la question relative a la condition
              (setq variables-r (increment-question-priority variables-r (car condition)))
              )
            )
        )
      )
      variables-r
      )
    )
  
  ;; Fonction qui cherche la question a incr�menter
  (defun increment-question-priority (variables param)
    (let ((variables-r variables)(tmp NIL))
      (dolist (question *questions*) ;; Pour toutes les questions
        (if (eq (get-response-var (symbol-value question)) param) ;; Si la variable de stockage de la reponse et identique a celle du parametre de la condition
            (if (assoc question variables-r)
                (progn    ;; On incremente le compteur associe a la question s'il existe
                  (setf tmp (cadr (assoc question variables-r)))
                  (setf (cadr (assoc question variables-r)) (+ tmp 1))
                 )
              (pushnew (list question 1) variables-r) ;; Ou on l'initialise a 1
              )
          )
        )
      variables-r
      )
    )
  
  
  (format t "~%Lancez le (chainage-avant) ou (chainage-arriere) pour essayer le SE ~%")
  
  ;; ########### BOUCLE PRINCIPALE CHAINAGE AVANT ########### 
  (defun chainage-avant ()
    (setf *RULES* (copy-list *RULES-SE*))
    (setf *FACTS* NIL)
    (setf *QUESTIONS* (copy-list *QUESTIONS-SE*))
    (setf *ACTIVITIES*  (copy-list *ACTIVITIES-SE*))
    (let (
          (end NIL)
          (activity NIL)
          )
      (loop
        (if (not (ask-better-question)) (setq end T) ;; S'il n'y a plus de questions a poser, on met end a vrai
          (progn 
            (loop ;; Tant que des r�gles sont activ�es, on regarde si on peut activer de nouvelles r�gles
             (when (not (check-rules)) (return T))
              )
            (setq activity (check-activities)) ;; On regarde si une activit� match avec les faits
            (if activity (setq end T)) ;; Si oui, on met fin a vrai
            )
          )   
         (when end (return activity)) ;; On quitte quitte la boucle quand fin est vrai, sinon on repete les etapes
        )
       (if activity  ;; Affichage de l'activite si elle est existante
           (progn (format t "~%~%###################################~%~%Nous avons trouv� une activit� qui pourrait vous convenir !")
             (format t "~%Il s'agit de l'activit� ~S" (get-activity-activity (symbol-value activity)))
             (format t "~%~S" (get-description-activity (symbol-value activity)))
             )
         (format t "~%~%###################################~%~%Nous n'avons malheureusement pas trouv� d'activit� pour vous...~%(les activites les plus proches de vos envies se situent dans la liste *ACTIVITIES*)")
         )
      )
    (format t "~%~%~%Lancez � nouveau (chainage-avant) ou (chainage-arriere) pour re-essayer le SE ~%")
    )
  
  ;; ########### BOUCLE PRINCIPALE CHAINAGE ARRIERE ########### 
  ;; Indiquez le nom exact de l'activit� : exemple> "Badminton"
  (defun chainage-arriere ()
    (setf *RULES* (copy-list *RULES-SE*))
    (setf *FACTS* NIL)
    (setf *QUESTIONS* (copy-list *QUESTIONS-SE*))
    (setf *ACTIVITIES*  (copy-list *ACTIVITIES-SE*))
    (let (
          (end NIL)
          (activityA NIL)
          )
      (setq *ACTIVITIES* (list (ask-activity)))
      (loop
        (if (not (ask-better-question)) (setq end T) ;; S'il n'y a plus de questions a poser, on met end a vrai
          (progn 
            (loop ;; Tant que des r�gles sont activ�es, on regarde si on peut activer de nouvelles r�gles
             (when (not (check-rules)) (return T))
              )
            (setq activity (check-activities)) ;; On regarde si une activit� match avec les faits
            )
          )   
         (when end (return activity)) ;; On quitte quitte la boucle quand fin est vrai, sinon on repete les etapes
        )
       (if activity  ;; Affichage de l'activite si elle est existante
           (format t "~%~%###################################~%~%L'activit� que vous avez indiqu� semble vous convenir !")
         (format t "~%~%###################################~%~%L'activit� ne vous convient malheureusement pas...")
         )
      )
    (format t "~%~%~%Lancez � nouveau (chainage-avant) ou (chainage-arriere) pour re-essayer le SE ~%")
    )

  
  ;; Ajout des r�gles 
  (format t "~%Generating rules...")
  (add-rule '((type_activite eq intellectuelle)) 'intellectuelle)
  (add-rule '((type_activite eq physique)) 'physique)
  (add-rule '((type_intellectuelle eq creative)) 'creative)
  (add-rule '((type_intellectuelle eq culturelle)) 'culturelle)
  (add-rule '((type_intellectuelle eq reflexive)) 'reflexive)
  (add-rule '((type_intellectuelle eq detente)) 'detente)
  (add-rule '((art eq visuel)) 'visuel)
  (add-rule '((art eq musical)) 'musical)
  (add-rule '((art eq litteraire)) 'litteraire)
  (add-rule '((reflexion eq plateau)) 'plateau)
  (add-rule '((reflexion eq cartes)) 'cartes)
  (add-rule '((reflexion eq informatique)) 'informatique)
  (add-rule '((personnes = 1)) 'seul)
  (add-rule '((personnes = 2)) 'duo)
  (add-rule '((personnes = 3)) 'trio)
  (add-rule '((personnes = 4)) 'quatuor)
  (add-rule '((personnes > 4)(personnes < 9)) 'petit_groupe)
  (add-rule '((personnes > 8)) 'grand_groupe)
  (add-rule '((type_info eq software)) 'software)
  (add-rule '((type_info eq hardware)) 'hardware)
  (add-rule '((type_art_visuel eq numerique)) 'numerique)
  (add-rule '((type_art_visuel eq manuel)) 'manuel)
  (add-rule '((heure <= 2)) 'duree_courte)
  (add-rule '((heure > 2)(heure <= 4)) 'duree_moyenne)
  (add-rule '((heure > 4)) 'duree_longue)
  (add-rule '((lieu_culture eq domicile)) 'chez_sois)
  (add-rule '((lieu_culture eq exterieur)) 'exterieur)
  (add-rule '((budget = 0)) 'gratuit)
  (add-rule '((budget > 0)(budget <= 10)) 'petit_budget)
  (add-rule '((budget > 10)(budget <= 100)) 'moyen_budget)
  (add-rule '((budget > 100)) 'grand_budget)
  (add-rule '((proches_activite eq famille)) 'famille)
  (add-rule '((proches_activite eq amis)) 'amis)
  (add-rule '((type_detente eq societe)) 'jeux_societe)
  (add-rule '((type_detente eq video)) 'jeux_video)
  (add-rule '((type_physique eq terrain)) 'pTerrain)
  (add-rule '((type_physique eq interieur)) 'pIntCours)
  (add-rule '((type_physique eq speciale)) 'pSpeciale)
  (add-rule '((type_physique eq exterieur)) 'pExterieur)
  (add-rule '((membre_physique eq pied)) 'se_joue_au_pied)
  (add-rule '((membre_physique eq main)) 'se_joue_a_la_main)
  (add-rule '((type_pIntCours eq cours)) 'pCours)
  (add-rule '((type_pIntCours eq interieur)) 'pInterieur)
  (add-rule '((type_pInterieure eq filet)) 'filet)
  (add-rule '((type_pInterieure eq but)) 'but)
  (add-rule '((type_pInterieure eq panier)) 'panier)
  (add-rule '((type_pSpeciale eq hivernale)) 'pHivernale)
  (add-rule '((type_pSpeciale eq aquatique)) 'pAquatique)
  (add-rule '((proche_station eq oui)) 'proche_station_ski)
  (add-rule '((proche_station eq non)) 'loin_station_ski)
  
  ;; Ajout de questions
  (format t "~%Generating questions...")
  (add-question "Quel type d'activit� chechez-vous ? (intellectuelle ou physique)" 'type_activite)
  (add-question "Quel type d'activit� intellectuelle chechez-vous ? (creative, culturelle, reflexive ou detente)" 'type_intellectuelle)
  (add-question "Quel art aimez-vous ? (visuel, musical ou litteraire)" 'art)
  (add-question "Quel type de reflexion aimez-vous ? (jeu de PLATEAU, jeu de CARTES, INFORMATIQUE)" 'reflexion)
  (add-question "Vous cherchez une activit� pour combien de personnes ?" 'personnes)
  (add-question "Quel aspect vous attire le plus en informatique ? (software, hardware)" 'type_info)
  (add-question "Pr�f�rez-vous le NUMERIQUE ou le MANUEL ?" 'type_art_visuel)
  (add-question "Indiques le nombre d'heures que vous pouvez consacrer � l'activit� (par session)" 'heure)
  (add-question "Pr�f�rez vous une activit� culturelle chez vous (DOMICILE) ou � l'ext�rieur (EXTERIEUR) ?" 'lieu_culture)	
  (add-question "Quel est votre budget par session en euros ?" 'budget)
  (add-question "Avec qui voulez-vous faire cette activit� ? (FAMILLE, AMIS)" 'proches_activite)
  (add-question "Etes vous plutot jeux de SOCIETE ou jeux VIDEO ?" 'type_detente)
  (add-question "Quel type d'activit� physique chechez-vous ? (sur TERRAIN, en INTERIEUR (et cours), SPECIALE (aquatique et hivernale) ou en EXTERIEUR)" 'type_physique)
  (add-question "Pr�f�rez-vous jouer au PIED ou a la MAIN ?" 'membre_physique)
  (add-question "Cherchez-vous une activit� dans une COURS ou en INTERIEUR ?" 'type_pIntCours)
  (add-question "Quelle particularit� pr�f�rez-vous parmis FILET, BUT, PANIER ?" 'type_pInterieure)
  (add-question "Cherchez-vous une activit� dans AQUATIQUE ou HIVERNALE ?" 'type_pSpeciale)
  (add-question "Etes-vous proche d'une station de ski ? (OUI ou NON)" 'proche_station)
  
  ;; Ajout d'activit�s
  (format t "~%Generating activities...")
  (add-activity '((intellectuelle)(creative)(visuel)(numerique)) "Photographie" "Evadez vous, capturez et observez mieux l'environnement qui vous entoure !")
  (add-activity '((intellectuelle)(creative)(visuel)(manuel)) "Peinture" "Evadez vous, capturez et observez mieux l'environnement qui vous entoure !")
  (add-activity '((intellectuelle)(creative)(musical)) "Composition musicale" "Faites parler votre cr�ativit� dans le domaine musicale !")
  (add-activity '((intellectuelle)(creative)(litteraire)) "Ecriture" "Prennez un papier et un crayon et exprimez-vous librement !")
  (add-activity '((intellectuelle)(reflexive)(plateau)(duree_moyenne duree_longue)) "Echecs" "R�flechissez aux meilleures strat�gies pour battre votre adversaire ! Les math�matiques vous aiderons.")
  (add-activity '((intellectuelle)(reflexive)(plateau)(duree_courte)) "Dames" "Jeu classique, d�tendez-vous tout en r�flechissant aux strat�gies pour prendre les pions de l'adversaire !")
  (add-activity '((intellectuelle)(reflexive)(cartes)(seul)) "Le solitaire" "Le jeu classique, id�al pour passer le temps.")
  (add-activity '((intellectuelle)(reflexive)(cartes)(duo)) "Manille d�couverte" "Peu connu mais interessant, une variante du jeu a 4 ou 6 joueurs.")
  (add-activity '((intellectuelle)(reflexive)(cartes)(trio)) "Rummy" "Id�al pour passer le temps a trois")
  (add-activity '((intellectuelle)(reflexive)(cartes)(quatuor)) "Bridge" "R�flechissez bien avant de prononcer vos contrats, ayez une bonne coh�sion avec votre pole inverse.")
  (add-activity '((intellectuelle)(reflexive)(cartes)(petit_groupe grand_groupe)) "Pr�sident" "Un jeu qui s'apprend rapidement et dont on ne se lasse jamais.")
  (add-activity '((intellectuelle)(reflexive)(informatique)(software)) "Programmation" "R�flechissez aux meilleurs algos, optimisez vos programmes, d�couvrez le monde de l'informatique grace aux nombreuses ressources.")
  (add-activity '((intellectuelle)(reflexive)(informatique)(hardware)) "Robotique" "Quoi de mieux que de voir ses cr�ations se mouvoir ! Fabriquez votre Frankeinstein de m�tal !")
  (add-activity '((intellectuelle)(culturelle)(chez_sois)(visuel)) "Documentaire" "Youtube et d'autres plateformes sont riches en documentaires ! De nombreux sujets disponibles et des documentaires de 5minutes � plus de 2heurex ! D�couvrez le monde qui vous entoure !")
  (add-activity '((intellectuelle)(culturelle)(chez_sois)(musical)) "Musique" "Ecoutez les messages transmis dans les paroles, voyagez par ces styles musicaux si vari�s !")
  (add-activity '((intellectuelle)(culturelle)(chez_sois)(litteraire)) "Lecture" "Plongez dans un livre et voyagez avec l'auteur et vous m�me !")
  (add-activity '((intellectuelle)(culturelle)(exterieur)(gratuit)) "Musee" "D�couvrez les meilleurs pi�ces artistiques, des cultures, des styles, des repr�sentations uniques!")
  (add-activity '((intellectuelle)(culturelle)(exterieur)(petit_budget)) "Cinema" "Regardez de v�ritables chefs-d'oeuvre sur grand �cran avec un son de qualit�, plongez dans les histoires !")
  (add-activity '((intellectuelle)(culturelle)(exterieur)(moyen_budget grand_budget)) "Theatre" "Emerveillez-vous devant de magnifiques repr�sentations !")
  (add-activity '((intellectuelle)(detente)(amis)(jeux_societe)) "Jeux de soci�t� humouristique" "Passez de bons moments avec vos amis avec des jeux du style Limite-Limite")
  (add-activity '((intellectuelle)(detente)(famille)(jeux_societe)) "Jeux de soci�t� famillaux" "Passez de bons moments avec votre famille avec des jeux du style Monopoly")
  (add-activity '((intellectuelle)(detente)(amis)(jeux_video)) "Jeux video sur console de salon (PS, Xbox)" "Passez des bons moments avec vos amis sur des jeux de console comme The Last of Us, Sackboy, Forza !")
  (add-activity '((intellectuelle)(detente)(famille)(jeux_video)) "Jeux video sur console famillale" "Passez des bons moments avec votre famille sur des jeux comme Mariokart !")
  (add-activity '((physique)(pTerrain)(duree_courte)(se_joue_au_pied)(petit_groupe grand_groupe)) "Football" "D�foulez-vous avec une bonne partie de football !")
  (add-activity '((physique)(pTerrain)(duree_courte)(se_joue_a_la_main)(petit_groupe grand_groupe)) "Rugby" "D�foulez-vous avec une bonne partie de rugby !")
  (add-activity '((physique)(pTerrain)(duree_moyenne)(petit_groupe grand_groupe)) "Baseball" "D�foulez-vous avec une bonne partie de baseball !")
  (add-activity '((physique)(pTerrain)(duree_longue)(petit_groupe grand_groupe)) "Cricket" "D�foulez-vous avec une bonne partie de cricket !")
  (add-activity '((physique)(pIntCours)(pInterieur)(seul trio)) "Squash" "D�foulez-vous avec une bonne partie de squash !")
  (add-activity '((physique)(pIntCours)(pInterieur)(duo quatuor)) "Badminton" "D�foulez-vous avec une bonne partie de badminton !")
  (add-activity '((physique)(pIntCours)(pInterieur)(petit_groupe grand_groupe)(filet)) "Volleyball" "D�foulez-vous avec une bonne partie de volleyball !")
  (add-activity '((physique)(pIntCours)(pInterieur)(petit_groupe grand_groupe)(but)) "Handball" "D�foulez-vous avec une bonne partie de handball !")
  (add-activity '((physique)(pIntCours)(pInterieur)(petit_groupe grand_groupe)(panier)) "Basketball" "D�foulez-vous avec une bonne partie de basketball !")
  (add-activity '((physique)(pIntCours)(pCours)) "Tennis" "D�foulez-vous avec une bonne partie de tennis !")
  (add-activity '((physique)(pSpeciale)(pAquatique)(seul)) "Natation" "Faites-vous plaisir en nageant en piscine, mer, rivi�re, lac !")
  (add-activity '((physique)(pSpeciale)(pAquatique)(duo trio quatuor)) "Plong�e / apn�e" "D�couvrez-la plong�e et l'apn�e � plusieurs !")
  (add-activity '((physique)(pSpeciale)(pAquatique)(petit_groupe grand_groupe)) "Waterpolo" "D�foulez-vous avec une bonne partie de waterpolo !")
  (add-activity '((physique)(pSpeciale)(pHivernale)(proche_station_ski)) "Ski" "Descendez les pistes � toute vitesse et profitez de la vue !")
  (add-activity '((physique)(pSpeciale)(pHivernale)(loin_station_ski)) "Patinnage" "D�vouvrez la sensation unique de glisse !")
  (add-activity '((physique)(pExterieur)(seul)) "Cyclisme" "D�vorez le bitume lors de sorties plus ou moins longues ! Choisissez la route ou le tout terrain ! Ce n'est pas le v�lo qui fait mais l'homme, alors en selle !")
  (add-activity '((physique)(pExterieur)(duo)) "Equitation" "Faites des randonn�es a cheval, prennez soin de vos animaux, entrainez vous a la carriere !")
  (add-activity '((physique)(pExterieur)(trio)) "Course a pied" "D�foulez vous et d�vorez les kilom�tres avec vos proches !")
  (add-activity '((physique)(pExterieur)(quatuor)) "Petanque" "En �quipe ou chacun pour sois, rapprochez-vous le plus du cochonnet et d�gommez les adversaires !")
  (add-activity '((physique)(pExterieur)(petit_groupe grand_groupe)) "Airsoft" "Trouvez votre style de jeu, courrez, campez, 'r�animez' vos alli�s ! Mais n'oubliez pas qu'il s'agit d'une simulation et d'un jeu.")
  T
  (chainage-avant)
  )

