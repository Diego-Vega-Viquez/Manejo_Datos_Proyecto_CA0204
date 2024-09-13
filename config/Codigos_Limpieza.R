
# Este documento corresponde al archivo .R que contiene los códigos para la 
# limpieza de la base de datos


# Librerías:

install.packages("ProjectTemplate")
install.packages("devtools")
install.packages("tidyverse")
install.packages("cowplot")
install.packages("openintro")

library("digest")
library("tibble")
library(ProjectTemplate)
library(tidyverse)
library(cowplot)
  # Estos son necesarios para cargar openintro
  library(airports)
  library(cherryblossom)
  library(usdata)
library(openintro)
library(haven) # Para cargar el .sav


# Cargamos la base de datos 

datos <- read_sav("data/GPES-ELAB-GEBD-ENAHO-2023_BdBasePublica.sav")

