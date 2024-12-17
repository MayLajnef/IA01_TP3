(progn
   (format t "~%Ouverture du système expert de recommandations de cadeaux...")
  ;; Initialisation des variables globales du SE
  
  (defvar *RULES-SE* NIL)
  (defvar *QUESTIONS-SE* NIL)
  (defvar *CADEAUX-SE* NIL)
  (defvar *RULES* NIL)
  (defvar *QUESTIONS* NIL)
  (defvar *CADEAUX* NIL)
  (defvar *FACTS* NIL)
  (defvar *CADEAUX-PRECEDENT* NIL)


  ;; Fonction d'ajout de cadeaux
  (defun addGift (conditions &rest gifts)
    (let ((id (gentemp "C")))
      (set id (list 
              (list 'conditions conditions) 
              (list 'gifts gifts)))
      (pushnew id *CADEAUX-SE*)
    )
  )
  
  ;; REGLES
  (defun addRule (conditions conclusion)
    (let ((id (gentemp "R")))
      (set id (list (list 'conditions conditions) (list 'conclusion conclusion)))
      (pushnew id *RULES-SE*)
    )
  )

  ;; Fonction d'accès de rules
  (defun getConditionRule (rule)
    (cadr (assoc 'conditions rule))
  )

  (defun getConclusionRule (rule)
    (cadr (assoc 'conclusion rule))
  )

  ;; Fonction d'accès des cadeaux
  (defun getConditionGift (gift)
    (cadr (assoc 'conditions gift))
  )

  (defun getGift (gift)
    (caadr (assoc 'gifts gift))
  )
  (defun getDescriptionGift (gift)
    (cadr (assoc 'description gift))
  )

  ;; QUESTIONS

  ;; Fonction d'ajout de question spécifique au système de cadeaux
  (defun addQuestion (question response)
    (let ((id (gentemp "Q")))
      (set id (list (list 'question question) (list 'response response)))
      (pushnew id *QUESTIONS-SE*)
    )
  )

    (defun desactiveRule (rule)
    ;; Fonction qui supprime de *RULES* une règle passé en paramètre
      (setf *RULES* (delete-if #'(lambda (item) (eq (symbol-value item) rule)) *RULES*))
    )
    
    (defun executeRule (rule)
    ;; Fonction qui ajoute une conclusion à notre ensemble de fait
      (let ((conclusion (getConclusionRule rule)))
        (pushnew conclusion *FACTS*)
      )
    )

    (defun checkRule (rule)
      ;; Fonction test une règle en fonction des réponses de l'utilisateur
      (let ((to-delete 0)(execute 1))
        (dolist (condition (getConditionRule rule))
          (if (isFactDefined  (car condition)) ;; On regarde si la variable est definie
            (progn 
              (setq execute (* (checkCondition condition) execute))
              (if (= (checkCondition condition) 0) (setq to-delete 1))
            )
            (setq execute 0)    
          )
        )
        ;; Execute la règle (ajoute sa conclusion à *FACTS* et supprime la règle de *RULES*) si besoin
        (if (= execute 1) (progn (executeRule rule)(desactiveRule rule)))

        ;; Suppression de la règle si ces conditions ne sont pas remplis
        (if (= to-delete 1)(desactiveRule rule))
        execute
      )
    )

    (defun isFactDefined (fact)
      (if (member fact *FACTS*) T NIL)
    )

    (defun checkRules ()
    ;; Fonction qui vérifie chaque règle
      (let ((executed NIL))
        (dolist (rule *rules* T)
          (if (= (checkRule (symbol-value rule)) 1) (setq executed T))
        )
      executed
      )
    )

    (defun checkCondition (condition) 
      ;; Renvoi 1 si la condition est validé, 0 sinon
      (if (funcall (cadr condition) (symbol-value (car condition)) (caddr condition)) 1 0)
    )

    (defun askQuestion (question)
        (format t "~%~S~%> " (getQuestion question)) ;; Pose la question
        (clear-input)
        (set (getReponseVar question) (read)) ;; Lis la reponse
        (pushnew (getReponseVar question) *FACTS*) ;; Enregistre la reponse
        (setf *QUESTIONS* (delete-if #'(lambda (item) (eq (symbol-value item) question)) *QUESTIONS*)) ;; Delete la question posee
    )

    (defun getQuestion (question)
        (cadr (assoc 'QUESTION question))
    )
   (defun getReponseVar (question)
        (cadr (assoc 'RESPONSE question))
    )

  (defun desactiveGift (gift)
  ;; Fonction qui supprime un cadeaux de la base de recherche
    (setf *CADEAUX* (delete-if #'(lambda (item) (eq (symbol-value item) gift)) *CADEAUX*))
  )

  (defun isFactPossible (fact)
  ;; renvoie True si un fait est possible avec les conclusions de règles actuelles, sinon NIL
    (let ((possible NIL))
      (dolist (rule *rules*)
        (if (equal (getconclusionrule (symbol-value rule)) fact) (setq possible T))
      )
      possible
    )
  )

  (defun isConditionPossible (condition)
  ;; renvoie True si la condition est encore atteignable, NIL sinon
    (let ((possible NIL))
      (dolist (fact condition)
        (if (isFactPossible fact) (setq possible T))
      )
      possible
    )
  )
  
  (defun isFactApproved (fact)
  ;; Fonction qui retourne True si le fait est dans *FACTS*, sinon NIL
    (if (member fact *FACTS*) T NIL)
  )
  
  (defun isConditionApproved (condition)
  ;; Fonction qui retourne True si la condition est validée, sinon NIL (peut-être doublon avec la précédente)
  ;; Je n'ai pas encore testé en détail si dans nos appels on avait des listes de contitions ou pas
    (let ((approved NIL))
      (dolist (fact condition)
        (if (isFactApproved fact) (setq approved T))
      )
      approved
    )
  )
  
  (defun checkGift (gift)
  ;; Fonction qui test les conditions d'un cadeau
  ;; si l'une d'entre elle n'est pas validée et n'est plus atteignable, on le supprimer des cadeaux potentiels
    (let ((approved T)(to-delete NIL))
      (dolist (condition (getConditionGift gift))
        (if (not (isConditionApproved condition))
          (progn
            (setq approved NIL)
            (if (not (isConditionPossible condition)) ;; potentiel problème dans la fonction isconditionpossible
              (setq to-delete T)
            )
          )
        )
      )
      (if to-delete (desactiveGift gift))
      approved
    )
  )
  
  (defun checkGifts ()
  ;; Fonction qui test tous les cadeaux et qui renvoit le cadeau s'il n'en reste qu'un
    (let ((cadeau NIL))
      (dolist (gift *CADEAUX*)
        (if (checkGift (symbol-value gift)) (setq cadeau gift))
        )
      (if (= (length *CADEAUX*) 1) (setq cadeau (car *CADEAUX*)))
      cadeau
    )
  )
  
  ;; Gestion des questions pour poser la meilleure à chaque fois

  (defun askBestQuestion ()
  ;; Fonction principale gérant l'incrémentation du score des questions
  ;; Principe simple pour chaque cadeaux restants on regarde tous les faites qui restent à clarifier
  ;; Et on incrémente la question qui y correspond de 1

    (let ((questions NIL)(param NIL)(best-score 0)(question-to-ask NIL))
      (dolist (gift *CADEAUX*)
        (dolist (condition (getConditionGift (symbol-value gift)))
          (dolist (varCondition condition)
            (setq questions (varConditionToRules varCondition questions))
          )
        )
      )

      ;; Recherche la question avec le meilleur score
      (dolist (q questions)
        (if (>= (cadr q) best-score)
          (progn
            (setq best-score (cadr q))
            (setq question-to-ask (car q))
          )
        )
      )
      (if question-to-ask (progn (askQuestion (symbol-value question-to-ask)) T) NIL) ;; On pose la question si elle existe et on retourne T, NIL sinon
    )
  )
  
  (defun varConditionToRules (fact scoreQuestions)
  ;; Dans cette fonction on se charge de trouver depuis la variable condition du cadeau
  ;; De trouver la règle qui va nous permettre de clarifier 

    (let ((tmpScoreQuestions scoreQuestions)) 
    (dolist (rule *rules*) ;; Pour toutes les regles
      (if (eq (getconclusionrule (symbol-value rule)) fact) ;; Si la conclusion de la regle mene au fait que l'on cherche
          (progn 
            (dolist (condition (getConditionRule (symbol-value rule))) ;; Alors, pour toutes les conditions de cette regle, on incremente la question relative a la condition
              (setq tmpScoreQuestions (upVoteQuestion tmpScoreQuestions (car condition)))
            )
            )
        )
      )
      tmpScoreQuestions
      )
  )
  
  (defun upVoteQuestion (scoreQuestions condition)
  ;; Fonction qui incrémente la priorité des questions ou l'initialise à 1
  ;; Pour se faire on prend en paramètre le score actuel des questions ainsi qu'une condition d'une règle validant un fait de cadeau restant
  ;; On parcourt la liste de question pour trouver la questions correspondante et on l'incrémente

    (let ((tmpScoreQuestions scoreQuestions))
      (dolist (question *questions*)
        (if (eq (getReponseVar (symbol-value question)) condition)
          (if (assoc question tmpScoreQuestions)
              ;; Si la question est déjà dans les scores on l'incrémente
              (setf (cadr (assoc question tmpScoreQuestions)) (+ 1 (cadr (assoc question tmpScoreQuestions))))
              ;; Sinon on lui créé un score
              (pushnew (list question 1) tmpScoreQuestions)
          )
        )
      )
      tmpScoreQuestions    
    )
  )
  
  ;; ########### BOUCLE PRINCIPALE CHAINAGE AVANT ########### 
  (defun chainageAvant ()
    (setf *RULES* (copy-list *RULES-SE*))
    (setf *FACTS* NIL)
    (setf *QUESTIONS* (copy-list *QUESTIONS-SE*))
    (setf *CADEAUX*  (copy-list *CADEAUX-SE*))
    (let (
          (end NIL)
          (gift NIL)
          )
      (loop
        (if *CADEAUX*
          (setf *CADEAUX-PRECEDENT* (copy-list *CADEAUX*))
        )

        (if (not (askBestQuestion)) (setq end T)
          (progn 
            (loop
              (when (not (checkRules)) (return T))
            )
            (setq gift (checkGifts))
            (if gift (setq end T))
            )
          )
         (when end (return gift))
        )
        (if gift
          (progn (format t "~%---------------------------------~%Un cadeau semble vous correspondre !")
            (format t "~%Il s'agit du cadeau :~S" (getGift (symbol-value gift)))
            (format t "~%~S" (getdescriptiongift (symbol-value gift)))
          )
          
          (progn 
            (format t "~%---------------------------------~%Aucun cadeau n'a été trouvé :/~%")
            (format t "Voici une suggestion de cadeau qui pourrait vous intérésser~%")
            (dolist (suggestionCadeau *CADEAUX-PRECEDENT*)
              (format t "~%~S" (getGift (symbol-value suggestioncadeau)))
            )
          )
        )
      )
    )

  ;; Ajout des cadeaux
  (format t "~%Ajout des cadeaux...")
  ;; PETIT BUDGET

  (addGift '((petitBudget)(bebe)(utilitaire)) "Berceaux, Tétine ou Habits" "")
  (addGift '((petitBudget)(bebe)(mignon)) "Peluche ou jouet" "")
  (addGift '((petitBudget)(enfant)(utilitaire)(sport))"Habits ou matériel lié au sport" "")
  (addGift '((petitBudget)(enfant)(utilitaire)(art))"Matériel de dessin ou de peinture" "")
  (addGift '((petitBudget)(enfant)(utilitaire)(musique))"Ecouteur" "")
  (addGift '((petitBudget)(enfant)(utilitaire)(lecture))"Marque Page" "")
  (addGift '((petitBudget)(enfant)(utilitaire)(technologie))"Kano" "Apprenez à coder à l'aide de jeu ludique, le tout dans un petit ordinateur à monter")
  (addGift '((petitBudget)(enfant)(utilitaire)(autres))"Habits" "")
  (addGift '((petitBudget)(enfant)(sentimental))"Album photo" "")
  (addGift '((petitBudget)(enfant)(experience)(sport))"Place pour un match de son équipe favorite" "")
  (addGift '((petitBudget)(enfant)(experience)(musique))"Place de concert" "")
  (addGift '((petitBudget)(enfant)(experience)(jeux))"Session d'escape Game" "")
  (addGift '((petitBudget)(enfant)(experience)(art))"Entrée pour un musé ou une exposition" "")
  (addGift '((petitBudget)(enfant)(experience)(dessinAnime))"Place de cinéma (dessin animé)" "")
  (addGift '((petitBudget)(enfant)(divertissement)(jeux)(video))"Une clef d'activation pour un jeux" "")
  (addGift '((petitBudget)(enfant)(divertissement)(jeux)(societe))"Monopoly" "")
  (addGift '((petitBudget)(enfant)(divertissement)(jeux)(carte))"Uno" "")
  (addGift '((petitBudget)(enfant)(divertissement)(technologie))"Lego Technique" "")
  (addGift '((petitBudget)(enfant)(divertissement)(bricolage))"Lego" "")
  (addGift '((petitBudget)(enfant)(divertissement)(dessinAnime))"DVD de dessin animé" "")
  (addGift '((petitBudget)(enfant)(divertissement)(lecture))"Livre" "")
  (addGift '((petitBudget)(enfant)(divertissement)(musique))"CD de son artiste préféré" "")
  (addGift '((petitBudget)(adolescent)(utilitaire)(sport))"Habits ou matériel lié au sport" "")
  (addGift '((petitBudget)(adolescent)(utilitaire)(technologie))"Périphérique, carte éléctronique programmable" "")
  (addGift '((petitBudget)(adolescent)(utilitaire)(art))"Matériel de dessin ou de peinture" "")
  (addGift '((petitBudget)(adolescent)(utilitaire)(musique))"Ecouteur" "")
  (addGift '((petitBudget)(adolescent)(utilitaire)(lecture))"Marque page" "")
  (addGift '((petitBudget)(adolescent)(utilitaire)(autres))"Habit" "")
  (addGift '((petitBudget)(adolescent)(experience)(technologie))"Entrée dans un musée technologique" "")
  (addGift '((petitBudget)(adolescent)(experience)(sport))"Place pour un match de son équipe favorite" "")
  (addGift '((petitBudget)(adolescent)(experience)(musique))"Place pour un concert" "")
  (addGift '((petitBudget)(adolescent)(experience)(art))"Place pour une pièce de théâtre ou une exposition" "")
  (addGift '((petitBudget)(adolescent)(experience)(lecture))"Dédicace" "")
  (addGift '((petitBudget)(adolescent)(experience)(cinema))"Place pour aller voir un film" "")
  (addGift '((petitBudget)(adolescent)(experience)(jeux))"Escape Game" "")
  (addGift '((petitBudget)(adolescent)(sentimental)(proche ami))"Album photo ou Fleur" "")
  (addGift '((petitBudget)(adolescent)(sentimental)(amoureux))"Des roses ou un bracelet" "")
  (addGift '((petitBudget)(adolescent)(divertissement)(musique))"CD de son artiste" "")
  (addGift '((petitBudget)(adolescent)(divertissement)(lecture))"Livre ou BD" "")
  (addGift '((petitBudget)(adolescent)(divertissement)(jeux)(video))"Une clef d'activation pour un jeux vidéo" "")
  (addGift '((petitBudget)(adolescent)(divertissement)(jeux)(societe))"Splendor" "")
  (addGift '((petitBudget)(adolescent)(divertissement)(jeux)(carte))"Skyjo" "")
  (addGift '((petitBudget)(adulte)(utilitaire)(cuisine))"Air Fryer ou tout autre ustensile de cuisine" "")
  (addGift '((petitBudget)(adulte)(utilitaire)(technologie))"Support téléphone ou tablette" "")
  (addGift '((petitBudget)(adulte)(utilitaire)(bricolage))"Caisse à outil miniature" "")
  (addGift '((petitBudget)(adulte)(experience)(cinema))"Place(s) pour un film en plein air" "")
  (addGift '((petitBudget)(adulte)(experience)(art))"Place(s) pour un vernissage" "")
  (addGift '((petitBudget)(adulte)(experience)(sport))"Carte cadeau pour un cour de yoga" "")
  (addGift '((petitBudget)(adulte)(experience)(cuisine))"Place pour un cours de cuisine" "")
  (addGift '((petitBudget)(adulte)(experience)(lecture))"Place pour 'la grande librairie', ou une dédicace" "")
  (addGift '((petitBudget)(adulte)(sentimental)(amoureux))"Cadre d'un souvenir commun" "")
  (addGift '((petitBudget)(adulte)(sentimental)(proche ami))"Carte personnalisé avec un mot" "")
  (addGift '((petitBudget)(adulte)(divertissement)(alcool))"Bière de dégustation" "")
  (addGift '((petitBudget)(adulte)(divertissement)(jeux)(societe))"Casse-tête" "")
  (addGift '((petitBudget)(adulte)(divertissement)(dessinAnime))"Coffret DVD d'un dessin animé culte" "")
  (addGift '((petitBudget)(personneAgee)(utilitaire)(cuisine))"Tablier personnalisé par toute la famille" "")
  (addGift '((petitBudget)(personneAgee)(utilitaire)(lecture))"Loupe pour lire ou une liseuse" "")
  (addGift '((petitBudget)(personneAgee)(utilitaire)(voyage))"Guide de voyage" "")
  (addGift '((petitBudget)(personneAgee)(experience)(art))"Place pour un atelier de peinture" "")
  (addGift '((petitBudget)(personneAgee)(experience)(cinema))"Carte d'abonnement pour des sorties cinema" "")
  (addGift '((petitBudget)(personneAgee)(experience)(musique))"Place pour un concert d'un orchestre" "")
  (addGift '((petitBudget)(personneAgee)(experience)(sport))"Bon pour une séance de yoga" "")
  (addGift '((petitBudget)(personneAgee)(sentimental)(proche))"Famileo (marque déposée) ou un album photo familial" "")
  (addGift '((petitBudget)(personneAgee)(sentimental)(amoureux ami))"cadre numérique pour afficher plusieurs souvenir" "")
  (addGift '((petitBudget)(personneAgee)(divertissement)(alcool))"Vin en fonction des goûts" "")
  (addGift '((petitBudget)(personneAgee)(divertissement)(art))"Kit de peinture" "")

  ;; BUDGET MOYEN
  ;; Bébé - Expérience
  (addGift '((moyenBudget)(bebe)(experience)(musique)) "Éveil musical avec instruments adaptés" "")
  (addGift '((moyenBudget)(bebe)(experience)(art)) "Atelier de peinture sensorielle" "")
  ;; Bébé - Sentimental
  (addGift '((moyenBudget)(bebe)(mignon)) "Veilleuse personnalisée pour bébé" "")
  ;; Bébé - Utilitaire
  (addGift '((moyenBudget)(bebe)(utilitaire)) "Siège auto groupe 0+/1 (jusqu’à 4 ans)" "")
  ;; Enfant - Divertissement
  (addGift '((moyenBudget)(enfant)(divertissement)(jeux)) "Console de jeux portable" "Jeux-vidéos" "")
  ;; Enfant - Utilitaire
  (addGift '((moyenBudget)(enfant)(utilitaire)(technologie)) "Kit de robotique junior" "Tablette éducative" "")
  (addGift '((moyenBudget)(enfant)(utilitaire)(lecture)) "Collection de romans" "")
  ;; Adolescent - Utilitaire
  (addGift '((moyenBudget)(adolescent)(utilitaire)(sport)) "Sac de sport multifonction" "")
  (addGift '((moyenBudget)(adolescent)(utilitaire)(habillage)) "Sneaker tendance" "")
  (addGift '((moyenBudget)(adolescent)(utilitaire)(technologie)) "Pack accessoires multimédia" "")
  (addGift '((moyenBudget)(adolescent)(utilitaire)(musique)) "Casque audio" "Enceinte bluetooth" "")
  (addGift '((moyenBudget)(adolescent)(utilitaire)(lecture)) "Collection de romans" "")
  (addGift '((moyenBudget)(adolescent)(utilitaire)(jeux)) "Kit de création de jeux vidéo" "Manettes d'une console de jeux vidéos" "Console de jeux portable" "")
  ;; Adolescent - Divertissement
  (addGift '((moyenBudget)(adolescent)(divertissement)(cinema)) "Abonnement streaming 6 mois" "")
  (addGift '((moyenBudget)(adolescent)(divertissement)(jeux)) "Jeux-vidéos" "")
  ;; Adolescent - Expérience
  (addGift '((moyenBudget)(adolescent)(experience)(sport)) "Cours d'initiation à un sport" "")
  ;; Adulte - Expérience
  (addGift '((moyenBudget)(adulte)(experience)(voyage)(europe)) "Billet de train direction Bruxelle" "")
  (addGift '((moyenBudget)(adulte)(experience)(voyage)(europe)) "Réservation dans un hôtel pour un week-end à Rome" "")
  ;; Adulte - Utilitaire
  (addGift '((moyenBudget)(adulte)(utilitaire)(cuisine)) "Robot de cuisine multifonction" "")
  (addGift '((moyenBudget)(adulte)(utilitaire)(lecture)) "Collection de romans" "")
  (addGift '((moyenBudget)(adulte)(utilitaire)(habillage)) "Écharpe en laine de qualité" "")
  ;; Adulte - Sentimental
  (addGift '((moyenBudget)(adulte)(sentimental)(cadeauDurable)) "Bijou gravé" "")
  (addGift '((moyenBudget)(adulte)(sentimental)(voyage)) "Album photo de voyage personnalisé" "")
  (addGift '((moyenBudget)(adulte)(sentimental)(lecture)) "Bibliothèque numérique personnelle" "")
  ;; Personne âgée - Utilitaire
  (addGift '((moyenBudget)(personneAgee)(utilitaire)) "Coussin ergonomique" "")
  ;; Personne Âgée - Divertissement
  (addGift '((moyenBudget)(personneAgee)(divertissement)(jeux)) "Jeux de société" "")
  (addGift '((moyenBudget)(personneAgee)(divertissement)(technologie)) "Tablette simplifiée pour seniors" "")

  ;; GROS BUDGET
  ;; Bébé - Utilitaire
  (addGift '((grosBudget)(bebe)(utilitaire)) "Poussette de haute qualité" "")
  (addGift '((grosBudget)(bebe)(utilitaire)(jeux)) "Espace d'apprentissage interactif" "")
  ;; Enfant - Divertissement
  (addGift '((grosBudget)(enfant)(divertissement)(jeux)) "Console de salon dernière génération" "")
  ;; Enfant - Sentimental
  (addGift '((grosBudget)(enfant)(sentimental)) "Chambre décorée sur mesure" "")
  (addGift '((grosBudget)(enfant)(experience)(art)) "Cours particulier d'art sur plusieurs mois" "")
  ;; Adolescent - Technologie
  (addGift '((grosBudget)(adolescent)(utilitaire)(technologie)) "Smartphone récent" "")
  ;; Adolescent - Expérience
  (addGift '((grosBudget)(adolescent)(experience)(voyage)(asie)) "Séjour découverte en Inde" "")
  (addGift '((grosBudget)(adolescent)(experience)(sport)) "Stage sportif intensif international" "")
  (addGift '((grosBudget)(adolescent)(experience)(technologie)) "Bootcamp de programmation" "")
  ;; Adulte - Expérience
  (addGift '((grosBudget)(adulte)(experience)(voyage)(asie)) "Séjour au Japon tout compris" "")
  ;; Adulte - Utilitaire
  (addGift '((grosBudget)(adulte)(utilitaire)(bricolage)) "Coffret complet d'outillage électrique" "")
  (addGift '((grosBudget)(adulte)(utilitaire)(bricolage technologie)) "Atelier complet avec équipement high-tech" "")
  (addGift '((grosBudget)(adulte)(utilitaire)(bricolage art)) "Espace créatif professionnel" "")
  (addGift '((grosBudget)(adulte)(utilitaire)(autres)(habillage)) "Manteau d'hiver premium" "")
  (addGift '((grosBudget)(adulte)(utilitaire)(autres)) "Test" "")
  ;; Adulte - Sentimental
  (addGift '((grosBudget)(adulte)(sentimental)(cadeauDurable)) "Montre personnalisée haut de gamme" "")
  ;; Personne âgée - Sentimental
  (addGift '((grosBudget)(personneAgee)(sentimental)) "Livre photo personnalisé premium" "")
  ;; Personne âgée - Utilitaire
  (addGift '((grosBudget)(personneAgee)(utilitaire)) "Fauteuil électrique relaxant" "")
  (addGift '((grosBudget)(personneAgee)(utilitaire)(technologie)) "Solution domotique complète" "")
  (addGift '((grosBudget)(personneAgee)(utilitaire)(technologie sante)) "Système de télémédecine personnalisé" "")

  ;; Base de règle
  (format t "~%Générations de règle ...")
  ;; Budget
  (addRule '((budget < 50)) 'petitBudget)
  (addRule '((budget >= 50)(budget <= 300)) 'moyenBudget)
  (addRule '((budget > 300)) 'grosBudget)
  ;; Age
  (addRule '((age < 3)) 'bebe)
  (addRule '((age >= 3)(age < 12)) 'enfant)
  (addRule '((age >= 12)(age < 20)) 'adolescent)
  (addRule '((age >= 20)(age < 60)) 'adulte)
  (addRule '((age >= 60)) 'personneAgee)
  ;; Type
  (addRule '((typeCadeau eq divertissement)) 'divertissement)
  (addRule '((typeCadeau eq utilitaire)) 'utilitaire)
  (addRule '((typeCadeau eq mignon)) 'mignon)
  (addRule '((typeCadeau eq sentimental)) 'sentimental)
  (addRule '((typeCadeau eq experience)) 'experience)
  ;; Centre d'intérêt
  (addRule '((centreInteret eq technologie)) 'technologie)
  (addRule '((centreInteret eq sport)) 'sport)
  (addRule '((centreInteret eq art)) 'art)
  (addRule '((centreInteret eq cinema)) 'cinema)
  (addRule '((centreInteret eq autres)) 'autres)
  (addRule '((centreInteret eq voyage)) 'voyage)
  (addRule '((centreInteret eq cuisine)) 'cuisine)
  (addRule '((centreInteret eq bricolage)) 'bricolage)
  (addRule '((centreInteret eq dessinAnime)) 'dessinAnime)
  (addRule '((centreInteret eq alcool)) 'alcool)
  (addRule '((centreInteret eq jeux)) 'jeux)
  (addRule '((centreInteret eq sante)) 'sante)
  (addRule '((centreInteret eq musique)) 'musique)
  (addRule '((centreInteret eq lecture)) 'lecture)

  ;; Durée de vie
  (addRule '((duree eq cadeauDurable)) 'cadeauDurable)
  ;; AJOUTER AUX CADEAUX L'ATTRIBUE VIECOURTE ??
  ;; Type de jeu
  (addRule '((typeJeux eq societe)) 'societe)
  (addRule '((typeJeux eq video)) 'video)
  (addRule '((typeJeux eq carte)) 'carte)
  ;; Type de relation
  (addRule '((typeRelation eq proche)) 'proche)
  (addRule '((typeRelation eq amoureux)) 'amoureux)
  (addRule '((typeRelation eq ami)) 'ami)
  ;; Zone de voyage
  (addRule '((locVoyage eq europe)) 'europe)
  (addRule '((locVoyage eq asie)) 'asie)
  (addRule '((habillage eq oui)) 'habillage)

  ;; Ajout de questions
  (format t "~%Génération de questions...")
  (addQuestion "Quel est votre budget ? " 'budget)
  (addQuestion "Quel est l'âge de la personne à qui vous offrez le cadeau ? " 'age)
  (addQuestion "Quel relation entretenez-vous avec cette personne (ami, proche ou amoureux) ? " 'typeRelation)
  (addQuestion "Souhaitez-vous offrir des habits, bijoux ou chaussures ? (OUI ou NON)" 'habillage) 
  (addQuestion "Quel type de cadeau aimeriez-vous offrir ? (utilitaire, mignon, expérience, sentimental ou divertissement)" 'typeCadeau)
  (addQuestion "Quels est le principal centre d'intérêt de la personne ? (sport, art, musique, technologie, lecture, cinéma, alcoolisme, dessins-animés, jeux, bricolage, voyage, autres)" 'centreInteret)
  (addQuestion "Aimeriez-vous que votre cadeau dure dans le temps ? (OUI ou NON)" 'duree)
  (addQuestion "Quelle région du monde intéresse la personne ? (Europe, Asie)" 'locVoyage)
  (addQuestion "Quel type de jeu souhaitez-vous offrir ? (société, carte, vidéo)" 'typeJeux)

  (defun get-user-input (prompt options)
    ;; Affiche une question et retourne la réponse choisie par l'utilisateur.
    (format t "~a ~%" prompt)
    (dolist (option options)
      (format t "~a " option)
    )
    (terpri)
    (read)
  )

(chainageAvant)

)
