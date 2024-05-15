rm(list=ls())

nb_mailles=36
population_min=2500


library(tidyverse)
library(FactoMineR)
library(spdep)
library(sf)
library(readxl)
library(leaflet)
library(htmlwidgets)
load("Données/Donnees_Logement_2017.RData")
importsAirDNA <- read_excel("Données/stat_airdna_aout2020_mod.xlsx", range = "A4:I364")
Import_carteCommunesCorses=st_read("Données/SHP_COMMUNES/COMMUNE.shp")
carteCommunesCorses=Import_carteCommunesCorses %>% select(INSEE_COM,geometry)
colnames(carteCommunesCorses)[1]="CODGEO"
carteCommunesCorses=carteCommunesCorses %>% arrange(CODGEO)
carteCommunesCorses <- st_transform(carteCommunesCorses, crs = 4326)
airDNA=importsAirDNA %>% select(2,4)
colnames(airDNA)=c("CODGEO","Loc.actives")
Donnees_Logement_2017=Donnees_Logement_2017 %>% inner_join(airDNA, by="CODGEO")
Indicateurs_parCommune= Donnees_Logement_2017 %>% 
  mutate(indic.transac = round(100 * nb_mut / LOG, 1),
         indic.menag = round(POP/RP, 1),
         indic.rs = round(100 * RSECOCC / LOG, 1),
         indic.lv = round(100 * LOGVAC / LOG, 1),
         indic.jeun = round(jeun / anc, 1),
         indic.social = round(100 * social / tot_lgmt, 1),
         indic.prop = round(100 * proprietaire / tot_lgmt, 1),
         indic.duroc = duroc,
         indic.prix = round(100 * moyenne_prix/rev_ucm, 1),
         indic.airbnb=round(100*Loc.actives/LOG, 1)) %>% 
  select(CODGEO,LIBGEO,POP,LOG,contains("indic"))
Indicateurs_parCommune=Indicateurs_parCommune %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
winsorisation= function (x, prob.min = 0.01, prob.max = .99){
  a <- quantile(x, probs = c(prob.min, prob.max))
  x[x > a[2]] <- a[2]
  x[x < a[1]] <- a[1]
  return(x)
}
listeIndicateurs=Indicateurs_parCommune %>% select(contains("indic")) %>% names()
Indicateurs_parCommune[listeIndicateurs]=apply(Indicateurs_parCommune[listeIndicateurs],MARGIN = 2, FUN = winsorisation)
Indicateurs_parCommune_valeurs=Indicateurs_parCommune[listeIndicateurs] 
carteCommunesCorses_spdf=carteCommunesCorses %>% left_join(Indicateurs_parCommune,by="CODGEO")%>% select(CODGEO,LIBGEO,POP) %>%  as("Spatial")
table_voisinage=poly2nb(carteCommunesCorses_spdf,queen=F)
ACP_parCommunes <- Indicateurs_parCommune_valeurs %>%  PCA(graph = F)
nb_dim <- which(ACP_parCommunes$eig[, 3] > 80)[1] 
ACP_parCommunes <- Indicateurs_parCommune_valeurs %>%  PCA(graph = F,ncp = nb_dim)
donnees_pondérations=ACP_parCommunes$ind$coord
pondérations=nbcosts(table_voisinage, data = donnees_pondérations)
table_voisinage_pondérée=nb2listw(table_voisinage, glist = pondérations, style = "C")
arbre_poids_minimal <- mstree(table_voisinage_pondérée)

resultat_skater=skater(edges = arbre_poids_minimal[,1:2],data=donnees_pondérations,ncuts=nb_mailles-1,crit=population_min,vec.crit=Donnees_Logement_2017$POP)

carteMailles=carteCommunesCorses %>% mutate(maille=resultat_skater$groups) %>%  group_by(maille) %>%  summarise(geometry = st_union(geometry))

Maillage=data.frame(CODGEO=Indicateurs_parCommune$CODGEO,maille=resultat_skater$groups)

Indicateurs_parMaille=Maillage %>% 
  left_join(Donnees_Logement_2017, by="CODGEO") %>% 
  group_by(maille) %>% 
  summarise_if(is.numeric, funs(sum(., na.rm = TRUE))) %>% 
  mutate(indic.transac = round(100 * nb_mut / LOG, 1),
         indic.menag = round(POP/RP, 1),
         indic.rs = round(100 * RSECOCC / LOG, 1),
         indic.lv = round(100 * LOGVAC / LOG, 1),
         indic.jeun = round(jeun / anc, 1),
         indic.social = round(100 * social / tot_lgmt, 1),
         indic.prop = round(100 * proprietaire / tot_lgmt, 1),
         indic.duroc = duroc,
         indic.prix = round(100 * moyenne_prix/rev_ucm, 1),
         indic.airbnb=round(100*Loc.actives/LOG, 1)) %>% 
  select(maille,POP,LOG,contains("indic")) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))


Indicateurs_parMaille_valeurs=Indicateurs_parMaille %>% select(contains("indic"))
Indicateurs_parMaille_valeurs=Indicateurs_parMaille_valeurs %>% apply(MARGIN = 2, FUN = winsorisation)
acp_parMailles=PCA(Indicateurs_parMaille_valeurs,ncp=5,graph=F)
hc4 <- HCPC(acp_parMailles, nb.clust = 4, consol = T, graph = F)
cl4 <- data.frame(maille = Indicateurs_parMaille$maille, cl_maille4 = hc4$data.clust[,"clust"])
carteMaillesSecteurs <- left_join(carteMailles, cl4, by = "maille")

carteCommunesCorses_merged <- left_join(carteCommunesCorses, Donnees_Logement_2017, by = "CODGEO")
carteCommunesCorses_sf <- st_as_sf(carteCommunesCorses_merged)



# Création d'une palette de couleurs pour les secteurs
palSecteurs = colorFactor(palette = c("seagreen3", "orange2","blueviolet", "deeppink"), domain = carteMaillesSecteurs$cl_maille4)



# Création d'une carte des communes avec un trait gris fin
carteCommunes = leaflet(carteCommunesCorses_sf) %>%
  addPolygons(fillColor = "white",
              fillOpacity = 0.8,
              color = "gray",
              weight = 1)

# Ajout des mailles avec un trait noir plus épais et colorées selon le secteur
carte=carteCommunes %>%
  addPolygons(data = carteMaillesSecteurs,
              fillColor = ~palSecteurs(cl_maille4),
              fillOpacity = 0.8,
              color = "black",
              weight = 2)
carte

