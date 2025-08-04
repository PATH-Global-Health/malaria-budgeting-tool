tab0UI <- function(id) {
  #-NAMESPACING the module----------------------------
  ns <- NS(id)

  #-MAIN PAGE CONTENT---------------------------------
  fluidPage(

    # Yellow message banner at the top (alert-style)
    div(
      style = "background-color: #fff3cd; border-left: 6px solid #ffa500; padding: 15px; margin-bottom: 20px;",
      strong(head_bold),
      main_text
    ),

    # Main title and introductory text
    titlePanel("Outil de comparaison des budgets de lutte contre le paludisme DEMO"),
    h6("Cet outil aide les programmes nationaux de lutte contre le paludisme à générer, examiner et comparer les budgets d’intervention contre le paludisme dans différents scénarios opérationnels."),
    h6("Suivez les instructions ci-dessous pour naviguer dans l'outil. Chaque section explique comment saisir des données, afficher des résultats et générer des rapports."),
    br(),

    # Instructions header
    h3("Instructions"),

    #-ACCORDIAN PANEL INSTRUCTIONS-------------------
    accordion(
      id = ns("instructions"),
      open = FALSE,
      #-METHODS------------------
      accordion_panel(
        title = "Méthodes",
        open = FALSE,
        tagList(
          p(
            "Cette section fournit une référence détaillée sur les sources de données, la logique de calcul et les hypothèses utilisées tout au long du processus de génération du budget. Il est destiné à soutenir l'interprétation, la reproductibilité et la citation des résultats produits par l'outil."
          )
        )
      ),
      #-DATA UPLOAD--------------
      accordion_panel(
        title = "Entrées utilisateur",
        open = FALSE,
        tagList(
          p("Cette section permet à l'utilisateur de spécifier des plans opérationnels de lutte contre le paludisme pour la budgétisation d'années et de lieux spécifiques, en détaillant les interventions qui doivent être mises en œuvre."),
          tags$b("1. Définir la combinaison d'intervention:"),
          p("Sélectionnez Années de planification pour définir l'étendue de votre scénario."),
          p("Cliquez sur « Télécharger le modèle de scénario vide »."),
          p("Remplissez le modèle Excel: "),
          tags$ul(
            tags$li("Chaque feuille correspond à une année qui a été spécifiée dans l'outil. "),
            tags$li("Chaque ligne représente la plus petite unité spatiale utilisée pour la planification de l'intervention (niveau de la zone de santé) avec les données adm0, amd1 et adm2 préspécifiées pour le pays d'intérêt (RDC)."),
            tags$li("Les colonnes « code_ » détaillent un type spécifique d'intervention antipaludique qui peut être dispensée, comme suit : 1 = Oui en cours de livraison OU 0/Blanc = Non non livré."),
            tags$li("Les colonnes « type_ » comportent des listes déroulantes permettant de sélectionner le type d'intervention spécifique délivré ")
          ),
          div(
            style = "text-align: center; margin-top: 20px;",
            tags$a(
              href = "scenario-template-image.png",
              target = "_blank",
              tags$img(
                src = "scenario-template-image.png",
                style = "max-width: 100%; height: auto; border: 1px solid #ccc; cursor: zoom-in;",
                alt = "Exemple de modèle de scénario"
              )
            ),
            tags$div(
              style = "font-style: italic; font-size: 90%; margin-top: 5px;",
              "Cliquez sur l'image pour l'agrandir"
            )
          ),
          p("Une fois qu'un plan a été spécifié en indiquant les interventions à cibler, où chaque année l'utilisateur peut sauvegarder une copie locale de ce fichier."),
          p("Revenez à l'application Web et téléchargez le fichier Excel complété à l'aide du bouton Télécharger."),
          p("Donnez au scénario un nom abrégé : par exemple Plan 1 BAU et une description : par exemple « Interventions simples - campagnes de masse, distribution de routine et CPP minimal » – assurez-vous qu'il s'agit de descriptions informatives, car elles seront utiles lors de la comparaison des plans."),
          p("Appuyez sur le bouton « Soumettre le scénario » et la feuille de calcul sera téléchargée dans l'outil. Les plans téléchargés apparaîtront dans un tableau récapitulatif avec les détails associés."),
          tags$b("2. Définition des coûts unitaires:"),
          p("Cliquez sur « Télécharger le modèle de coût vide »."),
          p("Assurez-vous que les en-têtes de colonne des colonnes A : J restent inchangés et que des colonnes supplémentaires peuvent être ajoutées selon les besoins de l'utilisateur."),
          p("Des feuilles supplémentaires pour le suivi des calculs de coûts unitaires, par exemple, peuvent également être ajoutées librement."),
          p("Les cellules ne contiennent pas de formules prédéfinies et c'est à l'utilisateur de saisir ou de calculer les données comme il l'entend."),
          p("Le modèle est prérempli avec certaines interventions, types d'intervention, classes de coûts et unités courants – ces lignes peuvent être modifiées et/ou supprimées selon les besoins de l'utilisateur, mais assurez-vous que pour chaque intervention réalisée dans le plan opérationnel, il y a des coûts unitaires pour l'intervention spécifique et le type d'intervention."),
          p("S'il le souhaite, l'utilisateur peut également ajouter des coûts unitaires spécifiques à l'emplacement. Pour ce faire, ajoutez des colonnes supplémentaires pour les spécifications 'adm1' et adm2' et assurez-vous que les noms sont cohérents entre celle-ci et la feuille de calcul du mix d'intervention, l'outil s'occupera du reste!"),
          p("Remplissez le modèle Excel : "),
          tags$ul(
            tags$li("« code_intervention » : Sélectionnez dans la liste déroulante l'intervention à laquelle les données de coût se rapportent."),
            tags$li("« type_intervention » : Sélectionnez dans la liste déroulante le type d'intervention spécifique auquel le coût se rapporte. Ces valeurs se rapportent à la colonne « type_ » du modèle précédent. Si l'utilisateur souhaite inclure des coûts fixes pour une intervention, cela est également spécifié dans cette colonne, par exemple Coûts fixes pour l'entreposage annuel de moustiquaires au cours d'une campagne."),
            tags$li("« cout_classe » : Sélectionnez la classe de coûts (Approvisionnement, Distribution, Opérationnel, Support, Autre) Si vous sélectionnez « autre », indiquez dans la colonne « cout_classe_autre » de quoi il s'agit."),
            tags$li("« description » : fournissez une brève description des composants du coût unitaire dans la colonne de description."),
            tags$li("« unite » : Sélectionnez dans la liste déroulante l'unité spécifique pour le coût, par exemple par filet, par enfant, par dose, par an , etc."),
            tags$li("« cout_monnaie_locale » : Valeur monétaire du coût unitaire spécifique en CDF"),
            tags$li("« taux_de_change » : taux de change à convertir de CDF en USD pour renseigner les valeurs de coût unitaire dans la  colonne « cout_usd » "),
            tags$li("« count_annee_pour_analyse » : cette valeur est l'année du plan opérationnel pour lequel le coût unitaire doit être utilisé pour calculer le budget. Si cette colonne est laissée vide, le même coût unitaire sera appliqué pour chaque année de livraison dans le plan spécifié."),
            tags$li("Pour faciliter la conversion des estimations de coût unitaire générées à partir de données historiques en valeurs monétaires actuelles, il existe les colonnes supplémentaires suivantes pour faciliter cette tâche : « cout_unitaire_d'origine » l' estimation du coût unitaire d'origine, « cout_unitaire_original_annee » l'année des données utilisées pour estimer le coût unitaire et enfin « facteur_d'inflation_initial_à_l'année_d'analyse » le facteur d'inflation à appliquer pour avoir des coûts unitaires en valeurs attendues actuelles et futures"),
            tags$li("Les colonnes « Notes » et « Source » peuvent être utilisées pour stocker des notes et des détails spécifiques sur la source de données utilisée pour générer les coûts unitaires. ")
          ),
          div(
            style = "text-align: center; margin-top: 20px;",
            tags$a(
              href = "unit-template-image.png",
              target = "_blank",
              tags$img(
                src = "unit-template-image.png",
                style = "max-width: 100%; height: auto; border: 1px solid #ccc; cursor: zoom-in;",
                alt = "Exemple de modèle de coûts unitaires"
              )
            ),
            tags$div(
              style = "font-style: italic; font-size: 90%; margin-top: 5px;",
              "Cliquez sur l'image pour l'agrandir"
            )
          ),
          p("Une fois que les données de coût unitaire ont été spécifiées, l'utilisateur peut enregistrer une copie locale de ce fichier."),
          p("Revenez à l'application Web et téléchargez le fichier Excel complété à l'aide du formulaire et donnez à la feuille de coûts un nom : par exemple « Coût 1 », etc. et une description : par exemple « Basé sur les données de coûts historiques » – assurez-vous qu'il s'agit de descriptions informatives car elles seront utiles lors de la génération et de la comparaison des plans."),
          p("Appuyez sur le bouton « Soumettre la feuille de coûts » et la feuille de calcul sera téléchargée dans l'outil."),
          p("Les données de coût téléchargées apparaîtront dans un tableau récapitulatif."),
          tags$b("3. Données précédentes téléchargées :"),
          p("Une fois qu'une feuille de calcul a été téléchargée dans l'outil, l'utilisateur est capable de télécharger un modèle basé sur un scénario spécifique téléchargé - cela peut faciliter le remplissage rapide d'un nouveau scénario sans avoir à répliquer chaque élément, mais assurez-vous de saisir un nouveau nom et une nouvelle description de scénario lors du rechargement.")
        )
      ),
      #-SCENARIO VERIFICATION--------
      accordion_panel(
        title = "Vérifier le scénario",
        open = FALSE,
        tagList(
          p("Une fois qu'un plan a été téléchargé, l'utilisateur peut passer à l'onglet « Vérifier le scénario ». Cette section permet aux utilisateurs de valider leurs plans d'intervention téléchargés et de signaler toute incohérence ou erreur logique."),
          tags$b("Étapes d'utilisation :"),
          tags$ul(
            tags$li("Sélectionnez un forfait précédemment téléchargé dans le menu déroulant."),
            tags$li("Sélectionnez l'année qui vous intéresse dans la liste déroulante."),
            tags$li("Les données rempliront une table en conséquence."),
            tags$li("Cliquez sur l'onglet d'une intervention pour afficher les informations de ciblage"),
            tags$li("Affichez les statistiques de couverture des titres affichées sous forme de cases récapitulatives colorées"),
            tags$ul(
              tags$li("🟩 Couverture complète – nombre de provinces où toutes les zones de santé bénéficient de l'intervention spécifique."),
              tags$li("🟧 Couverture partielle – nombre de provinces avec seulement certaines zones de santé ciblées, mais pas toutes"),
              tags$li("🟥 Aucune couverture – nombre de provinces sans zones de santé ciblées"),
            ),
            tags$li("•	Le tableau récapitule à l'échelle provinciale pour l'année sélectionnée, le nombre de zones de santé dans cette province, le nombre de bénéficiaires de l'intervention sélectionnée, le nombre de personnes ne recevant pas l'intervention et le pourcentage de couverture de cette intervention à l'échelle de la province.")
          ),
          p("Si l'utilisateur détecte des erreurs dans la spécification de la combinaison d'interventions, il peut revenir à l'onglet de téléchargement des données, supprimer la feuille de calcul incorrecte de l'application, corriger la version enregistrée localement et la télécharger à nouveau dans l'application.")
        )
      ),
      #-BUDGET GEN-----------------
      accordion_panel(
        title = "Générer des budgets",
        open = FALSE,
        tagList(
          p("Pour combiner ensuite les entrées de coût opérationnel et unitaire afin de générer un budget, l'utilisateur se déplace dans l'onglet Générer un budget."),
          p("Avant de générer des budgets, vous trouverez des informations détaillées sur la méthodologie sous-jacente dans l'  onglet Méthodes - veuillez consulter ces données avant de générer des budgets - si la méthodologie actuelle doit être mise à jour, veuillez contacter l'équipe d'assistance technique de PATH pour corriger cela."),
          p("En bref, l'outil quantifiera les produits nécessaires à chaque intervention en fonction de la population cible d'une zone de santé. Une fois les besoins en matières premières quantifiés, ils peuvent être multipliés par le coût unitaire correspondant pour obtenir le coût global final de l'intervention."),
          p("Pour configurer l'outil de calcul des budgets, spécifiez d'abord les combinaisons de sources de données dans la matrice de spécification :"),
          tags$ul(
            tags$li("Sélectionnez une spécification de plan dans le menu déroulant."),
            tags$li("Sélectionnez une spécification de coût dans le menu déroulant."),
            tags$li("Décidez si les hypothèses de base utilisées pour les calculs budgétaires sont acceptables ou si vous souhaitez ajuster l'une ou l'autre des hypothèses."),
            tags$li("En cas d'ajustement, sélectionnez l'hypothèse spécifique dans la liste déroulante et spécifiez une nouvelle valeur spécifique et cliquez sur « ajouter l'ajustement » pour finaliser le changement. Si vous souhaitez supprimer cette modification, il vous suffit de cliquer sur le petit « x » à côté de la valeur dans la colonne."),
            tags$li("Plusieurs budgets peuvent être générés à la fois en ajoutant des lignes supplémentaires à la matrice de sélection."),
            tags$li("Une fois que les sélections sont prêtes, cliquez sur le bouton « générer un budget » (s'il existe des lignes vides ou des lignes partiellement terminées, elles enverront un message d'erreur à l'utilisateur et seront mises en surbrillance afin que l'utilisateur puisse compléter ou supprimer ces lignes selon les besoins).")
          ),
          p("L'outil fournira des messages d'état utiles sur le processus de génération et les données budgétaires générées apparaîtront dans le tableau à droite de la page."),
          p("L'utilisateur a la possibilité de supprimer tous les budgets générés de ce tableau même s'ils ne l'intéressent plus."),
          p("Une fois généré, le nouveau budget restera disponible dans l'outil jusqu'à ce qu'il soit supprimé, ce qui signifie qu'il n'est pas nécessaire de régénérer les budgets à chaque fois qu'un utilisateur accède à l'outil.")
        )
      ),
      #-BUDGET VISUALISATION--------
      accordion_panel(
        title = "Visualisation du Budget",
        open = FALSE,
        tagList(
          p("Cette section permet aux utilisateurs d'afficher les résultats détaillés d'un plan budgétisé sélectionné. Il comprend des cartes, des tableaux et des résumés visuels montrant la distribution spatiale, la répartition des coûts et le profil budgétaire global du plan d'intervention sélectionné."),
          p("Sélectionnez les entrées en haut de la page (budget d'intérêt, échelle spatiale, année et devise)."),
          p("Lors de la consultation du budget pour les 3 années du CO-OP, une option de saisie supplémentaire est disponible pour l'utilisateur - enveloppe budgétaire disponible pour 3 ans - entrez cette valeur dans la devise correspondante et l'outil déterminera si le budget actuel dépasse (case rouge) ou s'il rentre dans (case verte) l'enveloppe disponible."),
          p("Faites défiler les résultats restants pour mieux comprendre les différentes ventilations des coûts d'intervention et les facteurs de coût.")
        )
      ),
      #-BUDGET COMPARISON----------
      accordion_panel(
        title = "Comparaisons budgétaires",
        open = FALSE,
        tagList(
          p("Cette section permet aux utilisateurs de comparer les budgets côte à côte. Les comparaisons sont effectuées à l'échelle nationale uniquement et aident les utilisateurs à comprendre les différences dans la combinaison d'interventions, les exigences budgétaires et l'évolution des coûts entre les différents plans."),
          p("Sélectionnez le budget principal, c'est à cela que seront comparés les budgets restants."),
          p("Sélectionnez un ou plusieurs budgets de comparaison."),
          p("Sélectionnez l'année qui vous intéresse et la devise.")
        )
      ),
      #-REPORT GENERATION-----------
      accordion_panel(
        title = "Génération de rapports",
        open = FALSE,
        tagList(
          p("Téléchargez un rapport complet résumant les contributions, les hypothèses et la méthodologie. Il permet également d'accéder à tous les chiffres et données brutes utilisés pour la génération du budget afin de favoriser une transparence et une reproductibilité totales.")
        )
      )
    ),

    #-PDF DOWNLOAD INSTRUCTIONS-------------------------------------------------
    downloadButton(ns("download_inst"), "Télécharger le manuel d'instructions PDF", class = "btn-primary"),
  )
}
