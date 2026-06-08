#### Le changement de la fonction d'utilit? n?cessite le changement des variables apr?s ~#######

### Import de la librairie de choix modal. Il faut installer la librairie avant son import
library(mlogit)

### Lecture des données
choix_modal<-read.table("Modele_desagrege-chaine-retour domicile travail VPTC.csv",sep=";",dec=",",header=T)
head (choix_modal, 5)
CM<- mlogit.data(choix_modal, varying = c(1:2), choice = "Premier_mode", shape = "wide")

### Définition des fonctions d'utilité

# Utilité 1
model1<- mlogit(Premier_mode ~ 0|tps_gen_TC_D89+tps_gen_VP_D89+factor(sexe, levels=c(2, 1))+Densite89+niveau_etudes+age+I(age*age)+factor(permis)+abonnementTC, reflevel='VP', CM)
summary(model1)

# Utilité 2
model2<- mlogit(Premier_mode ~ 0| I(tps_gen_TC_D89-tps_gen_VP_D89)+motorisation+Densite89,reflevel='VP',CM)
summary(model2)


# Utilité 3
model3<- mlogit(Premier_mode ~ 0|tps_gen_TC_D89+tps_gen_VP_D89+motorisation+Densite89+I(log(age)) ,reflevel='VP',CM)
summary(model3)

# Utilité 4
model4<- mlogit(Premier_mode ~0|tps_gen_TC_D89+tps_gen_VP_D89+motorisation+Densite89+age18.25+age26.35+age36.45+age46.59,reflevel='VP',CM )
summary(model4)

# Utilité 5
model5<- mlogit(Premier_mode ~0|I(tps_gen_TC_D89-tps_gen_VP_D89)+motorisation+Densite89+lnage18,reflevel='VP',CM )
summary(model5)

# Utilité 5 bis
model5<- mlogit(Premier_mode ~tpsgenD89|permis,reflevel='VP',CM )
summary(model5)

# Utilité 6
model6<- mlogit(Premier_mode ~0|tps_gen_TC_D89+tps_gen_VP_D89+motorisation+Densite89+I(log(age)),reflevel='VP',CM )
summary(model6)

