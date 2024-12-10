
(setq *CADEAUX-SE* NIL)

;; Fonction d'ajout de cadeaux
(defun add-gift (conditions &rest gifts)
  (let ((id (gentemp "C")))
    (set id (list 
             (list 'conditions conditions) 
             (list 'gifts gifts)))
    (pushnew id *CADEAUX-SE*)
    )
  )


(format t "~%Ajout des cadeaux...")
;; Ajout des cadeaux
;; PETIT BUDGET

;; Bébé - Utilitaire
(add-gift '((petitBudget)(bebe)(utilitaire)(habillage)) "Bavoir personnalisé" "")
(add-gift '((petitBudget)(bebe)(utilitaire)(cuisine)) "Set de cuillères ergonomiques" "")

;; Enfant - Utilitaire
(add-gift '((petitBudget)(enfant)(utilitaire)(technologie)) "Étui de protection pour tablette" "")
(add-gift '((petitBudget)(enfant)(utilitaire)(sport)) "Gourde ludique avec design enfantin" "")
(add-gift '((petitBudget)(enfant)(utilitaire)(lecture)) "Marque-page personnalisé" "")
add-gift '((petitBudget)(enfant)(utilitaire)(art)) "Kit de dessin pour débutant" "")

;; Enfant - Expérience
(add-gift '((petitBudget)(enfant)(experience)(musique)) "Place de concert locale" "")
(add-gift '((petitBudget)(enfant)(experience)(jeux)) "Session d'escape game" "")
(add-gift '((petitBudget)(enfant)(experience)(art)) "Entrée pour une exposition" "")

;; Enfant - Sentimental
(add-gift '((petitBudget)(enfant)(sentimental)(art)) "Kit de création de bracelet d'amitié" "")

;; Adolescent - Utilitaire
(add-gift '((petitBudget)(adolescent)(utilitaire)(habillage)) "Bonnet personnalisé" "")
(add-gift '((petitBudget)(adolescent)(utilitaire)(technologie)) "Coque de téléphone" "")
(add-gift '((petitBudget)(adolescent)(utilitaire)(musique)) "Casque audio entrée de gamme" "")

;; Adolescent - Sentimental
(add-gift '((petitBudget)(adolescent)(sentimental)(lecture)) "Marque-page personnalisé" "")

;; Adolescent - Expérience
(add-gift '((petitBudget)(adolescent)(experience)(musique)) "Place de concert locale" "")
(add-gift '((petitBudget)(adolescent)(experience)(jeux)) "Session d'escape game" "")
(add-gift '((petitBudget)(adolescent)(experience)(art)) "Entrée pour une exposition" "")

;; Adulte - Expérience
(add-gift '((petitBudget)(adulte)(experience)(musique)) "Place de concert locale" "")
(add-gift '((petitBudget)(adolescent)(experience)(jeux)) "Session d'escape game" "")
(add-gift '((petitBudget)(adolescent)(experience)(art)) "Entrée pour une exposition" "")

;; Adulte - Utilitaire
(add-gift '((petitBudget)(adulte)(utilitaire)(cuisine)) "Ustensile de cuisine pratique" "")
(add-gift '((petitBudget)(adulte)(utilitaire)(technologie)) "Support de téléphone multifonction" "")
(add-gift '((petitBudget)(adulte)(utilitaire)(voyage)) "Guide touristique régional" "")
(add-gift '((petitBudget)(adulte)(experience)(voyage)(Amérique)) "Carte touristique de New York" "")

;; Adulte - Sentimental
(add-gift '((petitBudget)(adulte)(sentimental)) "Cadre photo personnalisé" "")

;; Adulte - Divertissement
(add-gift '((petitBudget)(adulte)(divertissement)(alcoolisme)) "Coffret de dégustation de bières locales" "")

;; Personne âgée - Sentimental
(add-gift '((petitBudget)(personneAgee)(sentimental)) "Cadre photo personnalisé" "")

;; Personne âgée - Divertissement
(add-gift '((petitBudget)(personneAgee)(divertissement)(lecture)) "Abonnement à un magazine" "")

;; BUDGET MOYEN

;; Bébé - Expérience
(add-gift '((moyenBudget)(bebe)(experience)(musique)) "Éveil musical avec instruments adaptés" "")
(add-gift '((moyenBudget)(bebe)(experience)(art)) "Atelier de peinture sensorielle" "")

;; Bébé - Sentimental
(add-gift '((moyenBudget)(bebe)(sentimental)) "Veilleuse personnalisée pour bébé" "")

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
(add-gift '((moyenBudget)(adulte)(experience)(voyage)(Europe)) "Billet de train direction Bruxelle" "")
(add-gift '((moyenBudget)(adulte)(experience)(voyage)(Europe)) "Réservation dans un hôtel pour un week-end à Rome" "")

;; Adulte - Utilitaire
(add-gift '((moyenBudget)(adulte)(utilitaire)(cuisine)) "Robot de cuisine multifonction" "")
(add-gift '((moyenBudget)(adulte)(utilitaire)(lecture)) "Collection de romans" "")
(add-gift '((moyenBudget)(adulte)(utilitaire)(habillage)) "Écharpe en laine de qualité" "")

;; Adulte - Sentimental
(add-gift '((moyenBudget)(adulte)(sentimental)(cadeaux-perdurables)) "Bijou gravé" "")
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
(add-gift '((grosBudget)(enfant)(sentimental)(art)) "Cours particulier d'art sur plusieurs mois" "")

;; Adolescent - Technologie
(add-gift '((grosBudget)(adolescent)(technologie)) "Smartphone récent" "")

;; Adolescent - Expérience
(add-gift '((grosBudget)(adolescent)(experience)(voyage)(Asie)) "Séjour découverte en Inde" "")
(add-gift '((grosBudget)(adolescent)(experience)(sport)) "Stage sportif intensif international" "")
(add-gift '((grosBudget)(adolescent)(experience)(technologie)) "Bootcamp de programmation" "")

;; Adulte - Expérience
(add-gift '((grosBudget)(adulte)(experience)(voyage)(Asie)) "Séjour au Japon tout compris" "")

;; Adulte - Utilitaire
(add-gift '((grosBudget)(adulte)(utilitaire)(bricolage)) "Coffret complet d'outillage électrique" "")
(add-gift '((grosBudget)(adulte)(utilitaire)(bricolage)(technologie)) "Atelier complet avec équipement high-tech" "")
(add-gift '((grosBudget)(adulte)(utilitaire)(bricolage)(art)) "Espace créatif professionnel" "")
(add-gift '((grosBudget)(adulte)(utilitaire)(habillage)) "Manteau d'hiver premium" "")

;; Adulte - Sentimental
(add-gift '((grosBudget)(adulte)(sentimental)(cadeaux-perdurables)) "Montre personnalisée haut de gamme" "")

;; Personne âgée - Sentimental
(add-gift '((grosBudget)(personneAgee)(sentimental)) "Livre photo personnalisé premium" "")

;; Personne âgée - Utilitaire
(add-gift '((grosBudget)(personneAgee)(utilitaire)) "Fauteuil électrique relaxant" "")
(add-gift '((grosBudget)(personneAgee)(utilitaire)(technologie)) "Solution domotique complète" "")
(add-gift '((grosBudget)(personneAgee)(utilitaire)(technologie)(sante)) "Système de télémédecine personnalisé" "")


(defun get-user-input (prompt options)
  "Affiche une question et retourne la réponse choisie par l'utilisateur."
  (format t "~a ~%" prompt)
  (dolist (option options)
    (format t "~a " option))
  (terpri)
  (read))

  
(defun recommend (item)
  "Affiche une recommandation à l'utilisateur."
  (format t "Recommandation : ~a~%" item))



