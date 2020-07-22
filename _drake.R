
if (!"PlanAnalyses" %in% ls())
  source("02_PlanAnalyses.R")

drake::drake_config(
  # Nom de l'objet contenant le plan d'analyse
  PlanAnalyses,
  # affiche une barre de progression pour suivre combien de cibles du plan ont
  # été réalisées
  verbose = 2, 
  # type de parallelisation utilisé
  parallelism = "future",
  # nombre maximum de jobs utilisés en parralèle
  jobs = 3,
)
