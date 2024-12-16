
(progn
;;; Variables globales
(defvar *FACTS* '((budget nil) (age nil) (habillement nil) (typeCadeau nil) (relation nil) (centreInteret nil) (typeJeux nil) (locVoyage nil) (durabilité nil)))  ;; Base de faits initiale
(defvar *CADEAUX-SE* '())  ;; Base de cadeaux du catalogue complet
(defvar *CADEAUX* '())  ;; Cadeaux proposés
(defvar *QUESTIONS-SE* '())  ;; Base des questions qui peuvent être posées
(defvar *QUESTIONS* '())  ;; Base des bonnes questions à poser
(defvar *RULES-SE* '())  ;; Base des règles générale
(defvar *RULES* '())  ;; Base des règles pour l'inférence

;; Fonction d'ajout de cadeaux
(defun addGift (conditions &rest gifts)
(let ((id (gentemp "C"))) ; Génère un identifiant temporaire unique
    (set id (list 
            (list 'conditions conditions) 
            (list 'gifts gifts))) ; Associe conditions et cadeaux
    (pushnew id *CADEAUX-SE*))) ; Ajoute l'ID à la liste *CADEAUX-SE*

  
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
    (cadr (assoc 'gifts gift))
  )

  ;; QUESTIONS

  ;; Fonction d'ajout de question spécifique au système de cadeaux
  (defun addQuestion (question response)
    (let ((id (gentemp "Q")))
      (set id (list (list 'question question) (list 'response response)))
      (pushnew id *QUESTIONS-SE*)
      )
    )
;;; Fonction pour traiter le budget
(defun process-budget (response)
  "Catégorise le budget en petit, moyen ou gros."
  (cond
   ((and (> response 0) (<= response 50)) 'petitBudget)
   ((and (> response 50) (<= response 300)) 'moyenBudget)
   (t 'grosBudget)))

;;; Fonction pour traiter l'âge
(defun process-age (response)
  "Catégorise l'âge en bébé, enfant, adolescent, adulte ou personne âgée."
  (cond
   ((and (>= response 0) (<= response 2)) 'bebe)
   ((and (>= response 3) (<= response 12)) 'enfant)
   ((and (>= response 13) (<= response 19)) 'adolescent)
   ((and (>= response 20) (<= response 59)) 'adulte)
   ((>= response 60) 'personneAgee)))

(defun update-fact (fact value)
  (setf *facts* (mapcar (lambda (f) (if (equal (car f) fact) (cons fact value) f)) *facts*)))


;;; Fonction pour poser une question à l'utilisateur
(defun ask-question (question)
  "Pose une question et met à jour la base de faits en fonction de la réponse."
  (let* ((texte (first question))
         (fact (second question)))
    (format t "~a " texte)
    (clear-input)
    (let ((response (read)) (value nil))
      (cond
       ((equal fact 'budget)
        (setq value (process-budget response)))
       ((equal fact 'age)
        (setq value (process-age response)))
        t (setq value response))
        (update-fact fact value))))

;;; Réadaption du ask-better-question avec la mise en oeuvre du "Qui est-ce ?"
;; Algorithme de ask-better-question
;; Initialiser faits-inconnus à la liste des faits qui valent NIL dans *FACTS*
;; Initialiser fait-inconnu-le-plus-fréquent à nil
;; Initialiser liste-frequences à nil
;; Pour chaque fait-inconnu dans faits-inconnus 
    (Calculer sa fréquence d'apparition dans *RULES-SE*)
    ;; Initialiser un compteur à 0;
    ;; Pour chaque règle dans *RULES-SE*
        ;; Si fait-inconnu est un fait apparaissant dans la prémisse de la règle
            ;; Incrémenter le compteur
    ;; Ajouter (cons fait-inconnu compteur) à liste-frequences
    ;; Déterminer le maximum des fréquences de liste-frequences et affecter fait-inconnu-le-plus-fréquent au fait-inconnu associé
    ;; Initialiser appropriate-question à la question associée à fait-inconnu-le-plus-fréquent
    ;; (ask-question appropriate-question)
;; 
    
;;; ...
)
