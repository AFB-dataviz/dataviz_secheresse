# Dataviz Sécheresse

Ce dépôt permet de créer les graphiques de la dataviz construite à partir des données du suivi usuel de l'observatoire national des étiages ([Onde](http://onde.eaufrance.fr/)) de 2012 à 2019 et publiée [ici](https://professionnels.ofb.fr/fr/node/1084).

Les sujets couverts sont l'évolution des observations des différentes modalités d'écoulement; l'évolution inter-annuelle de la distribution départementale des sites en assec au moins une fois dans l'année; l'évolution temporelle (mensuelle et annuelle) des sites en situation d'assec; la récurrence mensuelle des assecs (nombre de mois consécutifs avec un assec entre mai et septembre); et la récurrence annuelle des assecs (nombre d'années avec un assec entre 2012 et 2019).

Le traitement des données et la création des graphiques ont été réalisés avec le logiciel [R](https://www.r-project.org/). Le déroulement des tâches est géré grâce au package [drake](https://github.com/ropensci/drake), la préparation des données est réalisée avec les packages du [tidyverse](https://github.com/tidyverse/tidyverse), la création des graphiques interactifs a été réalisée avec [plotly](https://github.com/ropensci/plotly).

Contributeurs:

- Delphine Node
- Cédric Mondy (OFB)
