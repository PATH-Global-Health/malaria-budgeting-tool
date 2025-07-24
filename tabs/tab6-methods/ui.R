tab6UI <- function(id) {
  ns <- NS(id)
  fluidPage(
    div(
      style = "background-color: #fff3cd; border-left: 6px solid #ffa500; padding: 15px; margin-bottom: 20px;",
      strong(head_bold),
      main_text
    ),
    withMathJax(),
    navset_card_tab(
      id = ns("method_tabs"),
      title = "Méthodologie",

      # --- 1. Données requises ---
      nav_panel(
        "Données requises",
        p("La version actuelle de l'outil budgétaire est une version de démonstration destinée à des fins d'illustration. Les jeux de données d'espace réservé suivants ont été utilisés  :"),
        tags$ul(
          tags$li("Données démographiques : valeurs de population simulées et instables par unité géographique et par année, conçues pour illustrer la structure de l'outil."),
          tags$li("Données de coût : prix unitaires simulés pour les produits et services clés, y compris les produits, la livraison et les coûts fixes. Les coûts sont indiqués en CDF et en USD."),
          tags$li("Plans d'intervention : Exemples de scénarios qui précisent les interventions mises en œuvre dans chaque lieu et chaque année, sélectionnées au hasard et non basées sur un PNS actuel.")
        )
      ),

      # --- 2. Quantification ---
      nav_panel(
        "Quantification & coûts",
        p("À moins qu'il ne s'agisse d'un coût national fixe, toutes les quantités et tous les coûts d'intervention sont calculés au niveau de la zone de santé (adm2)."),
        p("Pour chaque intervention, l'outil calcule les quantités requises à l'aide d'hypothèses sélectionnées et de données démographiques, puis les multiplie par les coûts unitaires à partir de l'ensemble de données de coûts. Les sections suivantes décrivent comment chaque intervention est quantifiée et chiffrée.")
      ),

      # --- 3. Campagne MII ---
      nav_panel(
        "Campagne MII",
        p("Pour estimer le nombre de moustiquaires imprégnées d'insecticide (MII) nécessaires à la diffusion de la campagne dans les zones de santé ciblées :"),
        tags$ul(
          tags$li("La population totale de la zone de santé est multipliée par la couverture de l'intervention cible (100 %) pour estimer la population cible de la campagne, puis divisée par 1,8 (1 moustiquaire est distribuée pour 1,8 personne) pour obtenir le total des moustiquaires nécessaires à l'achat."),
          tags$li("Une stock tampon de 10 % est appliquée pour tenir compte du gaspillage et des imprévus."),
          tags$li("Le nombre de balles est calculé en divisant le nombre de MII par 50 (en supposant 50 MII par balle).")
        ),
        p("Formule :"),
        p("$$\\text{MII nécessaires} = \\left(\\frac{\\text{Pop cible} \\times \\text{couverture}}{1.8}\\right) \\times \\text{stock tampon}$$"),
        h5("Exemples de calculs de coûts :"),
        tags$ul(
          tags$li("Coût d'approvisionnement = Nombre de MII × coût unitaire par MII."),
          tags$li("Frais de livraison = Nombre de MII ou de balles × coût unitaire de livraison par MII ou balle."),
          tags$li("Coûts de la campagne opérationnelle = Nombre de MII × coût de diffusion opérationnelle par MII."),
          tags$li("Coûts fixes")
        ),
        p("Tous les éléments de coût sont résumés pour produire le coût total de l'intervention."),
        h5("Hypothèses de base :"),
        tags$ul(
          tags$li("Personnes par MII: 1.8"),
          tags$li("Population cible : population totale"),
          tags$li("Couverture : 100 % de la population cible"),
          tags$li("MII par balle : 50"),
          tags$li("Stock tampon : 10 %")
        )
      ),

      # --- 4. Routine MII ---
      nav_panel(
        "Routine MII",
        p("Estimer le nombre de moustiquaires nécessaires pour les canaux de distribution habituels, souvent par le biais des services ANC et EPI :"),
        tags$ul(
          tags$li("La population totale de femmes de moins de 5 ans et de femmes enceintes d'une zone de santé est multipliée par la couverture de distribution régulière prévue et une stock tampon de 10 % pour obtenir la totalité des moustiquaires nécessaires à l'achat.")
        ),
        p("Formule :"),
        p("$$\\text{MII nécessaires} = \\text{Population (moins de 5 ans et femmes enceintes)} \\times \\text{Couverture} \\times \\text{Stock tampon}$$"),
        h5("Exemples de calculs de coûts :"),
        tags$ul(
          tags$li("Coût d'approvisionnement = Nombre de MII × coût unitaire par MII."),
          tags$li("Coût de livraison = Nombre de MII x coût unitaire de livraison par MII."),
          tags$li("Coûts opérationnels = Nombre de MII × coût de livraison opérationnel par MII.")
        ),
        p("Tous les éléments de coût sont résumés pour produire le coût total de l'intervention."),
        h5("Hypothèses de base :"),
        tags$ul(
          tags$li("Population cible : enfants de moins de 5 ans et femmes enceintes"),
          tags$li("Couverture : 30 % de la population cible"),
          tags$li("Stock tampon : 10 %")
        )
      ),

      # --- 5. TPIp ---
      nav_panel(
        "TPIp",
        p("Pour estimer la quantité de SP (plaquettes alvéolées de 3 pilules) à procurer pour la TPI, nous prenons :"),
        p("La population cible de femmes enceintes dans une zone de santé suppose une couverture cible de 80 % de présence aux soins prénatals et 3 points de contact pour l'accouchement du TPI par femme et un stock tampon 10 %."),
        p("Formule :"),
        p("$$\\text{Quantité SP} = \\text{Femmes enceintes} \\times \\text{Présence ANC} \\times 3 \\times \\text{Stock tampon}$$"),
        h5("Exemples de calculs de coûts :"),
        tags$ul(
          tags$li("Coût d'approvisionnement = quantité de SP × coût unitaire d'approvisionnement par SP."),
          tags$li("Frais de livraison = quantité de SP × coût unitaire de livraison par SP.")
        ),
        p("Tous les éléments de coût sont résumés pour produire le coût total de l'intervention."),
        h5("Hypothèses de base :"),
        tags$ul(
          tags$li("Population cible : femmes enceintes"),
          tags$li("Couverture : 80 % de participation à l'ANC"),
          tags$li("Points de contact : 3 par femme enceinte"),
          tags$li("Stock tampon : 10 %")
        )
      ),

      # --- 6. CPS ---
      nav_panel(
        "CPS",
        p("Pour estimer le nombre de sachets co-bliqués SP+AQ nécessaires pour la chimioprévention du paludisme saisonnier (SMC), nous supposons que chaque sachet contient un traitement complet pour un seul cycle (1 comprimé de SP et 3 comprimés d'AQ). La SMC est dispensée sur 4 cycles mensuels et cible deux groupes d'âge : les enfants âgés de 3 à <12 mois et les enfants âgés de >12 à 59 mois."),
        p("Nous estimons d'abord la population cible en appliquant des proportions fixes au nombre total d'enfants de moins de 5 ans. La couverture de la population cible est supposée être de 100 %, sauf indication contraire, et est appliquée avant le calcul de la stock tampon. Une stock tampon de 10 % est ensuite incluse pour tenir compte du redosage, du gaspillage et du traitement des enfants provenant de l'extérieur de la zone de chalandise."),
        p("Formule :"),
        p("$$\\text{Paquets SMC nécessaires} = \\text{Population cible}_{\\text{groupe d'âge}} \\times \\text{Couverture} \\times \\text{Cycles} \\times \\text{Stock tampon}$$"),
        h5("Exemples de calculs de coûts :"),
        tags$ul(
          tags$li("Coût d'approvisionnement = quantité SP+AQ  × coût unitaire d'approvisionnement par SP+AQ."),
          tags$li("Frais de livraison = quantité SP+AQ  × coût unitaire de livraison par SP+AQ."),
          tags$li("Coût opérationnel = population cible × coût unitaire opérationnel par enfant")
        ),
        p("Tous les éléments de coût sont résumés pour produire le coût total de l'intervention."),
        h5("Hypothèses de base :"),
        tags$ul(
          tags$li("Population cible : enfants 3 mois - 5 ans"),
          tags$li("Cycles : 4 cycles mensuels"),
          tags$li("Couverture : 100 %"),
          tags$li("Répartition selon l'âge : 18 % 3-11 mois, 77 % 12-59 mois"),
          tags$li("Stock tampon : 10 %")
        )
      ),

      # --- 7. PMC ---
      nav_panel(
        "CPP",
        p("Pour estimer la quantité de sulfadoxine-pyriméthamine (SP) nécessaire à la chimioprévention du paludisme pérenne (CMP), nous supposons que l'administration est intégrée dans les visites de routine du Programme élargi de vaccination (PEV). Chaque enfant admissible reçoit le SP à quatre points de contact de vaccination systématique par an, avec une posologie adaptée à l'âge :"),
        tags$ul(
          tags$li("Les enfants âgés de 0 à 1 an reçoivent 1 comprimé de SP par contact"),
          tags$li("Les enfants âgés  de 1 à 2 ans reçoivent 2 comprimés de SP par contact")
        ),
        p("Pour tenir compte du sous-dosage dû au faible poids, qui touche environ 25 % des enfants de chaque groupe d'âge, un facteur d'échelle de 0,75 est appliqué aux deux groupes d'âge. Ce facteur reflète la réduction moyenne du nombre de comprimés nécessaires en raison de l'ajustement posologique (p. ex., demi-comprimés pour les nourrissons présentant une insuffisance pondérale)."),
        p("Un taux de couverture de 85 % est supposé et un stock tampon 10 % est ensuite inclus pour couvrir le gaspillage, le redosage et les ruptures de stock."),
        p("Formule :"),
        p("$$\\text{Doses SP nécessaires} = [(\\text{pop}_{0-1} \\times 1) + (\\text{pop}_{1-2} \\times 2)] \\times \\text{Couverture} \\times \\text{Facteur nutritionnel} \\times \\text{Points de contact} \\times \\text{Stock tampon}$$"),
        h5("Exemples de calculs de coûts :"),
        tags$ul(
          tags$li("Coût d'approvisionnement = quantité de SP × coût unitaire d'approvisionnement par SP."),
          tags$li("Frais de livraison = quantité de SP × coût unitaire de livraison par SP."),
          tags$li("Coût opérationnel = population cible × coût unitaire opérationnel par enfant/SP")
        ),
        p("Tous les éléments de coût sont résumés pour produire le coût total de l'intervention."),
        h5("Hypothèses de base :"),
        tags$ul(
          tags$li("Population cible : enfants de 0 à 2 ans"),
          tags$li("Couverture : 85 %"),
          tags$li("Points de contact : 4 par an"),
          tags$li("Facteur d’échelle nutritionnel : 75 %"),
          tags$li("Stock tampon : 10 %")
        )
      ),

      # --- 8. Vaccin antipaludique ---
      nav_panel(
        "Vaccin",
        p("Pour estimer le nombre de doses de vaccin antipaludique nécessaires, nous supposons que chaque enfant admissible recevra un calendrier à 4 doses. En supposant que les trois premières doses sont administrées mensuellement et commencent vers l'âge de 5 mois et que la 4e dose est administrée ~12 à 15 mois après la 3e dose. Le vaccin est administré par le biais de contacts de vaccination systématiques, avec une couverture attendue de 84 % parmi la population cible."),
        p("Un stock tampon de 10 % est inclus pour tenir compte des pertes pendant le transport, le stockage et l'administration."),
        p("Formule :"),
        p("$$\\text{Doses vaccin nécessaires} = \\text{Population cible} \\times \\text{Couverture} \\times \\text{Doses par enfant} \\times \\text{Stock tampon}$$"),
        h5("Exemples de calculs de coûts :"),
        tags$ul(
          tags$li("Coût d'approvisionnement = quantité de dose  × coût unitaire d'achat par dose"),
          tags$li("Coût de livraison = quantité de dose  × coût unitaire de livraison par dose"),
          tags$li("Coût opérationnel = population cible × coût unitaire opérationnel par enfant")
        ),
        p("Tous les éléments de coût sont résumés pour produire le coût total de l'intervention."),
        h5("Hypothèses de base :"),
        tags$ul(
          tags$li("Population cible : enfants de 5 à 36 mois"),
          tags$li("Couverture : 84 %"),
          tags$li("Doses : 4 doses par enfant"),
          tags$li("Stock tampon : 10 %")
        )
      ),

      # --- 9. Coûts fixes ---
      nav_panel(
        "Coûts fixes",
        p("En plus de quantifier les produits spécifiques à l'intervention et les coûts de livraison, il est important de tenir compte  des coûts fixes, c'est-à-dire des dépenses qui ne varient pas directement en fonction du nombre d'unités livrées ou de la taille de la population cible."),
        p("Les coûts fixes comprennent généralement :"),
        tags$ul(
          tags$li("Formation et supervision (p. ex., formation de recyclage pour les agents de santé, indemnités journalières des superviseurs)"),
          tags$li("Mobilisation sociale et campagnes de communication"),
          tags$li("Expansion ou maintien de la chaîne du froid (pour les vaccins)"),
          tags$li("Activités de suivi et d'évaluation"),
          tags$li("Ateliers de planification logistique et de microplanification"),
          tags$li("Coordination et administration au niveau national"),
        ),
        p("Ces coûts sont encourus quelle que soit l'ampleur de la mise en œuvre au sein d'une unité administrative donnée (par exemple, une zone de santé, un district ou une province)."),
        p("Les coûts fixes peuvent être intégrés dans le processus de budgétisation de l'une des deux façons suivantes :"),
        tags$ol(
          tags$li("Sous forme de montant forfaitaire par unité administrative :"),
          tags$ul(
            tags$li("Par exemple, attribuez une valeur de coût fixe par province ou zone de santé où l'intervention est mise en œuvre."),
            tags$li("Ceci est utile pour les coûts tels que la formation ou les réunions de planification, qui sont menées une fois par unité, quelle que soit la taille de la population.")
          ),
          p("$$\\text{Total Coûts Fixes} = \\text{Nombre d'unités bénéficiant d'une intervention} \\times \\text{Coût fixe par unité}$$"),
          tags$li("2.	Dans le cadre d'une structure de coûts unitaires."),
          tags$ul(
            tags$li("Les coûts fixes peuvent également être intégrés dans le coût unitaire de la prestation de services lors des exercices de liste de lignes et d'établissement des coûts.")
          ),
        )
      ),

      # --- 10. Méthodes manquantes ---
      nav_panel(
        "Méthodes manquantes",
        p("Certains éléments ne sont pas encore entièrement mis en œuvre et nécessitent une consultation programmatique  :"),
        tags$ul(
          tags$li("Pulvérisation intradomiciliaire à effet rémanent (PID) : Méthodologie de quantification et d'établissement des coûts à définir avec la participation du programme national."),
          tags$li("Prise en charge public : Méthode de quantification et d'établissement des coûts à définir avec l'apport du programme national.")
        ),
        p("En outre, des éléments tels que la surveillance entomologique, la surveillance, le suivi et l'évaluation, entre autres, ne sont pas encore mis en œuvre, mais peuvent être intégrés à la suite de discussions sur la méthodologie appropriée")
      ),

      # --- 11. Fonctionnalités supplémentaires ---
      nav_panel(
        "Fonctionnalités supplémentaires",
        p("L'outil prend en charge des fonctionnalités supplémentaires, qui peuvent être activées si vous le souhaitez :"),
        tags$ul(
          tags$li("Utilisation des coûts unitaires infranationaux (p. ex., coûts unitaires propres à une province ou à une zone de santé)."),
          tags$li("Intégration des hypothèses de couverture par unité infranationale"),
          tags$li("Possibilité d'attribuer des interventions ou des lieux à différentes sources de financement.")
        )
      ),

      # --- 12. Sortie finale ---
      nav_panel(
        "Sortie finale",
        p("Les produits du budget comprennent :"),
        tags$ul(
          tags$li("Quantités par intervention, unité et géographie"),
          tags$li("Frais d’approvisionnement et de livraison en CDF et en USD"),
          tags$li("Coûts totaux par élément budgétaire"),
          tags$li("Résumé des hypothèses (par défaut ou ajusté)")
        )
      )
    )
  )
}
