# GLOG partie épidémiologie

Ce code est la propriété de BRINDEL Léonard, COUBARD Chrislain, GERBER Zoé, MANGANE Fatoumata et NICOLLIN Elena.

Il a été produit pour le Projet de Génie Logiciel 2022, dans el cadre du Master 2 de Bio-informatique de Bordeaux. Nous avons travaillé sous l'encadrement de notre enseignante Marie BEURTON-AIMAR.


## Préambule

Cette application web nécessite une connexion internet pour la bonne exécution des applications Shiny (celles-ci étant hébergées sur shinyapps.io).

Pour exécuter les applications Shiny localement, les fichiers app_stats.R et app_maps.R permettent d'avoir le code utilisé. Il est en revanche nécessaire d'installer les différentes librairies utilisées.


## Utilisation

La page principale est home.html. Elle offre une vue d'ensemble du contenu de l'application web, et peut rediriger vers stats.html ou maps.html afin d'isoler l'une ou l'autre application Shiny.

Le script home.css est appliqué aux trois pages HTML.


## Données fournies

Les fichiers dans le dossier data peuvent servir d'exemple pour tester les différentes options de l'application. Le fichier stats1.csv et stats2.csv sert à la partie statistique, et les fichiers maps1.csv et maps2.csv à la partie cartographie.
