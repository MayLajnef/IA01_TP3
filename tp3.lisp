(progn
   (format t "~%Ouverture du système expert de recommandations de cadeaux...")
  ;; Initialisation des variables globales du SE
  (setq *RULES-SE* NIL)
  (setq *QUESTIONS-SE* NIL)
  (setq *CADEAUX-SE* NIL)
  (setq *RULES* NIL)
  (setq *QUESTIONS* NIL)
  (setq *CADEAUX* NIL)
  (setq *FACTS* NIL)

  ;; Fonction d'ajout de cadeaux
  (defun add-gift (conditions &rest gifts)
    (let ((id (gentemp "C"))) ; Génère un identifiant temporaire unique
      (set id (list 
              (list 'conditions conditions) 
              (list 'gifts gifts))) ; Associe conditions et cadeaux
      (pushnew id *CADEAUX-SE*))) ; Ajoute l'ID à la liste *CADEAUX-SE*
  
  ;; REGLES
  (defun add-rule (conditions conclusion)
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
    (cadr (assoc 'gifts gift))
  )

    ;; QUESTIONS

  ;; Fonction d'ajout de question spécifique au système de cadeaux
  (defun add-question (question response)
    (let ((id (gentemp "Q")))
      (set id (list (list 'question question) (list 'response response)))
      (pushnew id *QUESTIONS-SE*)
      )
    )
  
  ;; Définition de fonctions d'accès
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
    (pushnew (get-response-var question) *FACTS*) ;; Enregistre la reponse
    (setf *QUESTIONS* (delete-if #'(lambda (item) (eq (symbol-value item) question)) *QUESTIONS*)) ;; Supprime la question posee
    )
    
  
  ;; Fonction de désactivation d'un cadeau
  (defun desactive-gift (gift)
    (setf *CADEAUX* (delete-if #'(lambda (item) (eq (symbol-value item) gift)) *CADEAUX*))
    )

    (defun is-fact-possible (fact)
    (let ((possible NIL))
      (dolist (rule *RULES*)
        (if (equal (getConclusionRule (symbol-value rule)) fact) (setq possible T))
        )
      possible
      )
    )
  
  (defun is-condition-possible (condition)
    (let ((possible NIL))
      (dolist (fact condition)
        (if (is-fact-possible fact) (setq possible T))
        )
      possible
      )
    )
  
  (defun is-fact-approved (fact)
    (if (member fact *FACTS*) T NIL)
    )
  
  (defun is-condition-approved (condition)
    (let ((approved NIL))
      (dolist (fact condition)
        (if (is-fact-approved fact) (setq approved T))
        )
      approved
      )
    )
  
  (defun check-gift (gift)
    (let ((possible T)(to-delete NIL))
      (dolist (condition (getConditionGift gift))
        (if (not (is-condition-approved condition))
            (progn 
              (setq possible NIL)
              (if (not (is-condition-possible condition))
                  (setq to-delete T)
                )
              )
          )
        )
      (if to-delete (desactive-gift gift)) 
      possible
      )
    )
  
  (defun check-gifts ()
    (let ((gift NIL))
      (dolist (g *CADEAUX*)
        (if (check-gift (symbol-value g)) (setq gift g))
        )
      (if (= (length *CADEAUX*) 1) (setq gift (car *CADEAUX*)))
      gift
    )
    )
  
  (defun ask-better-question ()
    (let ((questions NIL)(param NIL)(best-score 0)(question-to-ask NIL))
      (dolist (gift *CADEAUX*)
        (dolist (condition (getConditionsGift (symbol-value gift)))
          (dolist (fact condition)
            (setq questions (increment-var-to-fact fact questions))
            )
          )
        )
      (dolist (q questions)
        (if (>= (cadr q) best-score)
            (progn
              (setq best-score (cadr q))
              (setq question-to-ask (car q))
              )
        )
        )
      (if question-to-ask (progn (ask-question (symbol-value question-to-ask)) T) NIL)
      )
    )
  
  (defun increment-var-to-fact (fact variables)
    (let ((variables-r variables)) 
    (dolist (rule *RULES*)
      (if (eq ( getConditionRule (symbol-value rule)) fact)
          (progn 
            (dolist (condition ( getConditionRule (symbol-value rule)))
              (setq variables-r (increment-question-priority variables-r (car condition)))
              )
            )
        )
      )
      variables-r
      )
    )
  
  (defun increment-question-priority (variables param)
    (let ((variables-r variables)(tmp NIL))
      (dolist (question *QUESTIONS*)
        (if (eq (get-response-var (symbol-value question)) param)
            (if (assoc question variables-r)
                (progn
                  (setf tmp (cadr (assoc question variables-r)))
                  (setf (cadr (assoc question variables-r)) (+ tmp 1))
                 )
              (pushnew (list question 1) variables-r)
              )
          )
        )
      variables-r
      )
    )
  
  ;; Chaînage avant pour recommander des cadeaux
  (defun chainage-avant ()
    (setf *RULES* (copy-list *RULES-SE*))
    (setf *FACTS* NIL)
    (setf *QUESTIONS* (copy-list *QUESTIONS-SE*))
    (setf *CADEAUX*  (copy-list *CADEAUX-SE*))
    (let (
          (end NIL)
          (gift NIL)
          )
      (loop
        (if (not (ask-better-question)) (setq end T)
          (progn 
            (loop
             (when (not (check-rules)) (return T))
              )
            (setq gift (check-gifts))
            (if gift (setq end T))
            )
          )   
         (when end (return gift))
        )
       (if gift
           (progn 
             (format t "~%~%###################################~%~%Nous avons trouvé un cadeau qui pourrait lui plaire !")
             (format t "~%Il s'agit de : ~S" (getGift (symbol-value gift)))
             )
         (format t "~%~%###################################~%~%Nous n'avons malheureusement pas trouvé de cadeau idéal pour cette personne...")
         )
      )
    (format t "~%~%~%Relancez (chainage-avant) pour une nouvelle recommandation ~%")
    )

  ;; Ajout des cadeaux
  (format t "~%Ajout des cadeaux...")
  ;; PETIT BUDGET

  (add-gift '((petitBudget)(bebe)(utilitaire)) "Berceaux, Tétine ou Habits" "")
  (add-gift '((petitBudget)(bebe)(mignon)) "Peluche ou jouet" "")
  (add-gift '((petitBudget)(enfant)(utilitaire)(sport))"Habits ou matériel lié au sport" "")
  (add-gift '((petitBudget)(enfant)(utilitaire)(art))"Matériel de dessin ou de peinture" "")
  (add-gift '((petitBudget)(enfant)(utilitaire)(musique))"Ecouteur" "")
  (add-gift '((petitBudget)(enfant)(utilitaire)(lecture))"Marque Page" "")
  (add-gift '((petitBudget)(enfant)(utilitaire)(technologie))"Kano" "")
  (add-gift '((petitBudget)(enfant)(utilitaire)(autre))"Habits" "")
  (add-gift '((petitBudget)(enfant)(sentimental))"Album photo" "")
  (add-gift '((petitBudget)(enfant)(experience)(sport))"Place pour un match de son équipe favorite" "")
  (add-gift '((petitBudget)(enfant)(experience)(msuique))"Place de concert" "")
  (add-gift '((petitBudget)(enfant)(experience)(jeux))"Session d'escape Game" "")
  (add-gift '((petitBudget)(enfant)(experience)(art))"Entrée pour un musé ou une exposition" "")
  (add-gift '((petitBudget)(enfant)(experience)(dessinAnime))"Place de cinéma (dessin animé)" "")
  (add-gift '((petitBudget)(enfant)(divertissement)(jeux)(video))"Une clef d'activation pour un jeux" "")
  (add-gift '((petitBudget)(enfant)(divertissement)(jeux)(societe))"Monopoly" "")
  (add-gift '((petitBudget)(enfant)(divertissement)(jeux)(carte))"Uno" "")
  (add-gift '((petitBudget)(enfant)(divertissement)(technologie))"Lego Technique" "")
  (add-gift '((petitBudget)(enfant)(divertissement)(bricolage))"Lego" "")
  (add-gift '((petitBudget)(enfant)(divertissement)(dessinAnime))"DVD de dessin animé" "")
  (add-gift '((petitBudget)(enfant)(divertissement)(lecture))"Livre" "")
  (add-gift '((petitBudget)(enfant)(divertissement)(musique))"CD de son artiste préféré" "")
  (add-gift '((petitBudget)(adolescent)(utilitaire)(sport))"Habits ou matériel lié au sport" "")
  (add-gift '((petitBudget)(adolescent)(utilitaire)(technologie))"Périphérique, carte éléctronique programmable" "")
  (add-gift '((petitBudget)(adolescent)(utilitaire)(art))"Matériel de dessin ou de peinture" "")
  (add-gift '((petitBudget)(adolescent)(utilitaire)(musique))"Ecouteur" "")
  (add-gift '((petitBudget)(adolescent)(utilitaire)(lecture))"Marque page" "")
  (add-gift '((petitBudget)(adolescent)(utilitaire)(autre))"Habit" "")
  (add-gift '((petitBudget)(adolescent)(experience)(technologie))"Entrée dans un musée technologique" "")
  (add-gift '((petitBudget)(adolescent)(experience)(sport))"Place pour un match de son équipe favorite" "")
  (add-gift '((petitBudget)(adolescent)(experience)(musique))"Place pour un concert" "")
  (add-gift '((petitBudget)(adolescent)(experience)(art))"Place pour une pièce de théâtre ou une exposition" "")
  (add-gift '((petitBudget)(adolescent)(experience)(lecture))"Dédicace" "")
  (add-gift '((petitBudget)(adolescent)(experience)(cinema))"Place pour aller voir un film" "")
  (add-gift '((petitBudget)(adolescent)(experience)(jeux))"Escape Game" "")
  (add-gift '((petitBudget)(adolescent)(sentimental)(proche ami))"Album photo ou Fleur" "")
  (add-gift '((petitBudget)(adolescent)(sentimental)(amoureux))"Des roses ou un bracelet" "")
  (add-gift '((petitBudget)(adolescent)(divertissement)(musique))"CD de son artiste" "")
  (add-gift '((petitBudget)(adolescent)(divertissement)(lecture))"Livre ou BD" "")
  (add-gift '((petitBudget)(adolescent)(divertissement)(jeux)(video))"Une clef d'activation pour un jeux vidéo" "")
  (add-gift '((petitBudget)(adolescent)(divertissement)(jeux)(societe))"Splendor" "")
  (add-gift '((petitBudget)(adolescent)(divertissement)(jeux)(carte))"Skyjo" "")
  (add-gift '((petitBudget)(adulte)(utilitaire)(cuisine))"Air Fryer ou tout autre ustensile de cuisine" "")
  (add-gift '((petitBudget)(adulte)(utilitaire)(technologie))"Support téléphone ou tablette" "")
  (add-gift '((petitBudget)(adulte)(utilitaire)(bricolage))"Caisse à outil miniature" "")
  (add-gift '((petitBudget)(adulte)(experience)(cinema))"Place(s) pour un film en plein air" "")
  (add-gift '((petitBudget)(adulte)(experience)(art))"Place(s) pour un vernissage" "")
  (add-gift '((petitBudget)(adulte)(experience)(sport))"Carte cadeau pour un cour de yoga" "")
  (add-gift '((petitBudget)(adulte)(experience)(cuisine))"Place pour un cours de cuisine" "")
  (add-gift '((petitBudget)(adulte)(experience)(lecture))"Place pour 'la grande librairie', ou une dédicace" "")
  (add-gift '((petitBudget)(adulte)(sentimental)(amoureux))"Cadre d'un souvenir commun" "")
  (add-gift '((petitBudget)(adulte)(sentimental)(proche ami))"Carte personnalisé avec un mot" "")
  (add-gift '((petitBudget)(adulte)(divertissement)(alcool))"Bière de dégustation" "")
  (add-gift '((petitBudget)(adulte)(divertissement)(jeux)(societe))"Casse-tête" "")
  (add-gift '((petitBudget)(adulte)(divertissement)(dessinAnime))"Coffret DVD d'un dessin animé culte" "")
  (add-gift '((petitBudget)(personneAgee)(utilitaire)(cuisine))"Tablier personnalisé par toute la famille" "")
  (add-gift '((petitBudget)(personneAgee)(utilitaire)(lecture))"Loupe pour lire ou une liseuse" "")
  (add-gift '((petitBudget)(personneAgee)(utilitaire)(voyage))"Guide de voyage" "")
  (add-gift '((petitBudget)(personneAgee)(experience)(art))"Place pour un atelier de peinture" "")
  (add-gift '((petitBudget)(personneAgee)(experience)(cinema))"Carte d'abonnement pour des sorties cinema" "")
  (add-gift '((petitBudget)(personneAgee)(experience)(musique))"Place pour un concert d'un orchestre" "")
  (add-gift '((petitBudget)(personneAgee)(experience)(sport))"Bon pour une séance de yoga" "")
  (add-gift '((petitBudget)(personneAgee)(sentimental)(proche))"Famileo (marque déposée) ou un album photo familial" "")
  (add-gift '((petitBudget)(personneAgee)(sentimental)(amoureux ami))"cadre numérique pour afficher plusieurs souvenir" "")
  (add-gift '((petitBudget)(personneAgee)(divertissement)(alcool))"Vin en fonction des goûts" "")
  (add-gift '((petitBudget)(personneAgee)(divertissement)(art))"Kit de peinture" "")

  ;; BUDGET MOYEN
  ;; Bébé - Expérience
  (add-gift '((moyenBudget)(bebe)(experience)(musique)) "Éveil musical avec instruments adaptés" "")
  (add-gift '((moyenBudget)(bebe)(experience)(art)) "Atelier de peinture sensorielle" "")
  ;; Bébé - Sentimental
  (add-gift '((moyenBudget)(bebe)(mignon)) "Veilleuse personnalisée pour bébé" "")
  ;; Bébé - Utilitaire
  (add-gift '((moyenBudget)(bebe)(utilitaire)) "Siège auto groupe 0+/1 (jusqu’à 4 ans)" "")
  ;; Enfant - Divertissement
  (add-gift '((moyenBudget)(enfant)(divertissement)(jeux)) "Console de jeux portable" "Jeux-vidéos" "")
  ;; Enfant - Utilitaire
  (add-gift '((moyenBudget)(enfant)(utilitaire)(technologie)) "Kit de robotique junior" "Tablette éducative" "")
  (add-gift '((moyenBudget)(enfant)(utilitaire)(lecture)) "Collection de romans" "")
  ;; Adolescent - Utilitaire
  (add-gift '((moyenBudget)(adolescent)(utilitaire)(sport)) "Sac de sport multifonction" "")
  (add-gift '((moyenBudget)(adolescent)(utilitaire)(habillage)) "Sneaker tendance" "")
  (add-gift '((moyenBudget)(adolescent)(utilitaire)(technologie)) "Pack accessoires multimédia" "")
  (add-gift '((moyenBudget)(adolescent)(utilitaire)(musique)) "Casque audio" "Enceinte bluetooth" "")
  (add-gift '((moyenBudget)(adolescent)(utilitaire)(lecture)) "Collection de romans" "")
  (add-gift '((moyenBudget)(adolescent)(utilitaire)(jeux)) "Kit de création de jeux vidéo" "Manettes d'une console de jeux vidéos" "Console de jeux portable" "")
  ;; Adolescent - Divertissement
  (add-gift '((moyenBudget)(adolescent)(divertissement)(cinema)) "Abonnement streaming 6 mois" "")
  (add-gift '((moyenBudget)(adolescent)(divertissement)(jeux)) "Jeux-vidéos" "")
  ;; Adolescent - Expérience
  (add-gift '((moyenBudget)(adolescent)(experience)(sport)) "Cours d'initiation à un sport" "")
  ;; Adulte - Expérience
  (add-gift '((moyenBudget)(adulte)(experience)(voyage)(europe)) "Billet de train direction Bruxelle" "")
  (add-gift '((moyenBudget)(adulte)(experience)(voyage)(europe)) "Réservation dans un hôtel pour un week-end à Rome" "")
  ;; Adulte - Utilitaire
  (add-gift '((moyenBudget)(adulte)(utilitaire)(cuisine)) "Robot de cuisine multifonction" "")
  (add-gift '((moyenBudget)(adulte)(utilitaire)(lecture)) "Collection de romans" "")
  (add-gift '((moyenBudget)(adulte)(utilitaire)(habillage)) "Écharpe en laine de qualité" "")
  ;; Adulte - Sentimental
  (add-gift '((moyenBudget)(adulte)(sentimental)(cadeauDurable)) "Bijou gravé" "")
  (add-gift '((moyenBudget)(adulte)(sentimental)(voyage)) "Album photo de voyage personnalisé" "")
  (add-gift '((moyenBudget)(adulte)(sentimental)(lecture)) "Bibliothèque numérique personnelle" "")
  ;; Personne âgée - Utilitaire
  (add-gift '((moyenBudget)(personneAgee)(utilitaire)) "Coussin ergonomique" "")
  ;; Personne Âgée - Divertissement
  (add-gift '((moyenBudget)(personneAgee)(divertissement)(jeux)) "Jeux de société" "")
  (add-gift '((moyenBudget)(personneAgee)(divertissement)(technologie)) "Tablette simplifiée pour seniors" "")

  ;; GROS BUDGET
  ;; Bébé - Utilitaire
  (add-gift '((grosBudget)(bebe)(utilitaire)) "Poussette de haute qualité" "")
  (add-gift '((grosBudget)(bebe)(utilitaire)(jeux)) "Espace d'apprentissage interactif" "")
  ;; Enfant - Divertissement
  (add-gift '((grosBudget)(enfant)(divertissement)(jeux)) "Console de salon dernière génération" "")
  ;; Enfant - Sentimental
  (add-gift '((grosBudget)(enfant)(sentimental)) "Chambre décorée sur mesure" "")
  (add-gift '((grosBudget)(enfant)(experience)(art)) "Cours particulier d'art sur plusieurs mois" "")
  ;; Adolescent - Technologie
  (add-gift '((grosBudget)(adolescent)(utilitaire)(technologie)) "Smartphone récent" "")
  ;; Adolescent - Expérience
  (add-gift '((grosBudget)(adolescent)(experience)(voyage)(asie)) "Séjour découverte en Inde" "")
  (add-gift '((grosBudget)(adolescent)(experience)(sport)) "Stage sportif intensif international" "")
  (add-gift '((grosBudget)(adolescent)(experience)(technologie)) "Bootcamp de programmation" "")
  ;; Adulte - Expérience
  (add-gift '((grosBudget)(adulte)(experience)(voyage)(asie)) "Séjour au Japon tout compris" "")
  ;; Adulte - Utilitaire
  (add-gift '((grosBudget)(adulte)(utilitaire)(bricolage)) "Coffret complet d'outillage électrique" "")
  (add-gift '((grosBudget)(adulte)(utilitaire)(bricolage technologie)) "Atelier complet avec équipement high-tech" "")
  (add-gift '((grosBudget)(adulte)(utilitaire)(bricolage art)) "Espace créatif professionnel" "")
  (add-gift '((grosBudget)(adulte)(utilitaire)(habillage)) "Manteau d'hiver premium" "")
  ;; Adulte - Sentimental
  (add-gift '((grosBudget)(adulte)(sentimental)(cadeauDurable)) "Montre personnalisée haut de gamme" "")
  ;; Personne âgée - Sentimental
  (add-gift '((grosBudget)(personneAgee)(sentimental)) "Livre photo personnalisé premium" "")
  ;; Personne âgée - Utilitaire
  (add-gift '((grosBudget)(personneAgee)(utilitaire)) "Fauteuil électrique relaxant" "")
  (add-gift '((grosBudget)(personneAgee)(utilitaire)(technologie)) "Solution domotique complète" "")
  (add-gift '((grosBudget)(personneAgee)(utilitaire)(technologie sante)) "Système de télémédecine personnalisé" "")

  ;; Base de règle
  (format t "~%Générations de règle ...")
  ;; Budget
  (add-rule '((budget < 50)) 'petitBudget)
  (add-rule '((budget >= 50)(budget <= 300)) 'moyenBudget)
  (add-rule '((budget > 300)) 'grosBudget)
  ;; Age
  (add-rule '((age < 3)) 'bebe)
  (add-rule '((age >= 3)(age < 12)) 'enfant)
  (add-rule '((age >= 12)(age < 20)) 'adolescent)
  (add-rule '((age >= 20)(age < 60)) 'adulte)
  (add-rule '((age >= 60)) 'personneAgee)
  ;; Type
  (add-rule '((typeCadeau eq divertissement)) 'divertissement)
  (add-rule '((typeCadeau eq utilitaire)) 'utilitaire)
  (add-rule '((typeCadeau eq mignon)) 'mignon)
  (add-rule '((typeCadeau eq sentimental)) 'sentimental)
  (add-rule '((typeCadeau eq experience)) 'experience)
  ;; Centre d'intérêt
  (add-rule '((centreInteret eq technologie)) 'technologie)
  (add-rule '((centreInteret eq sport)) 'sport)
  (add-rule '((centreInteret eq art)) 'art)
  (add-rule '((centreInteret eq cinema)) 'cinema)
  (add-rule '((centreInteret eq autre)) 'autre)
  (add-rule '((centreInteret eq voyage)) 'voyage)
  (add-rule '((centreInteret eq cuisine)) 'cuisine)
  (add-rule '((centreInteret eq bricolage)) 'bricolage)
  (add-rule '((centreInteret eq dessinAnime)) 'dessinAnime)
  (add-rule '((centreInteret eq alcool)) 'alcool)
  (add-rule '((centreInteret eq jeux)) 'jeux)
  (add-rule '((centreInteret eq sante)) 'sante)
  ;; Durée de vie
  (add-rule '((duree eq cadeauDurable)) 'cadeauDurable)
  ;; AJOUTER AUX CADEAUX L'ATTRIBUE VIECOURTE ??
  ;; Type de jeu
  (add-rule '((typeJeux ep societe)) 'jeuxSociete)
  (add-rule '((typeJeux ep video)) 'jeuxVideo)
  (add-rule '((typeJeux ep video)) 'jeuxCarte)
  ;; Type de relation
  (add-rule '((typeRelation eq proche)) 'proche)
  (add-rule '((typeRelation eq amoureux)) 'amoureux)
  (add-rule '((typeRelation eq ami)) 'ami)
  ;; Zone de voyage
  (add-rule '((locVoayge eq europe)) 'europe)
  (add-rule '((locVoayge eq asie)) 'asie)

  ;; Ajout de questions
  (format t "~%Génération de questions...")
  (add-question "Quel est votre budget ? " 'budget)
  (add-question "Quel est l'âge de la personne à qui vous offrez le cadeau ? " 'age)
  (add-question "Quel relation entretenez-vous avec cette personne (ami, proche ou amoureux) ? " 'relation)
  (add-question "Souhaitez-vous offrir des habits, bijoux ou chaussures ? (OUI ou NON)" 'habillage) 
  (add-question "Quel type de cadeau aimeriez-vous offrir ? (utilitaire, mignon, expérience, sentimental ou divertissement)" 'typeCadeau)
  (add-question "Quels est le principal centre d'intérêt de la personne ? (sport, art, musique, technologie, lecture, cinéma, alcoolisme, dessins-animés, jeux, bricolage, voyage, autres)" 'centre_interet)
  (add-question "Aimeriez-vous que votre cadeau dure dans le temps ? (OUI ou NON)" 'durable) ;; Question à supprimer si typeCadeau = expérience
  (add-question "Quelle région du monde intéresse la personne ? (Europe, Asie)" 'region) 

  (defun get-user-input (prompt options)
    ;; Affiche une question et retourne la réponse choisie par l'utilisateur.
    (format t "~a ~%" prompt)
    (dolist (option options)
      (format t "~a " option))
    (terpri)
    (read))
    
  (defun recommend (item)
    "Affiche une recommandation à l'utilisateur."
    (format t "Recommandation : ~a~%" item))
)
