(progn
  (setq *CADEAUX-SE* NIL)
  (setq *RULES-SE* NIL)

  ;; Fonction d'ajout de cadeaux
  (defun add-gift (conditions &rest gifts)
    (let ((id (gentemp "C"))) ; Génère un identifiant temporaire unique
      (set id (list 
              (list 'conditions conditions) 
              (list 'gifts gifts))) ; Associe conditions et cadeaux
      (pushnew id *CADEAUX-SE*))) ; Ajoute l'ID à la liste *CADEAUX-SE*

  (format t "~%Ajout des cadeaux...")

  (defun add-rule (conditions conclusion)
  (let ((id (gentemp "R")))
    (set id (list (list 'conditions conditions) (list 'conclusion conclusion)))
    (pushnew id *RULES-SE*)
    )
  )

  ;; Ajout des cadeaux
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
  (add-gift '((petitBudget)(adolescent)(sentimental)(proche))"Album photo ou Fleur" "")
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
  (add-gift '((petitBudget)(adulte)(sentimental)(proche))"Carte personnalisé avec un mot" "")
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
  (add-gift '((petitBudget)(personneAgee)(sentimental)(amoureux))"cadre numérique pour afficher plusieurs souvenir" "")
  (add-gift '((petitBudget)(personneAgee)(divertissement)(alcool))"Vin en fonction des goûts" "")
  (add-gift '((petitBudget)(personneAgee)(divertissement)(art))"Kit de peinture" "")

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

  ;; Base de règle

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