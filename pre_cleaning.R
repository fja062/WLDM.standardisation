# load packages
library("tidyverse")
library("readxl")

### France

# espece, : le nom de l’espèce concernée
# annee :  l’année de début de saison cynégétique (exemple 2020 pour la sasion 2020/2021) je n’ai donc pas la saison 2021(/2022) qui vient seulement de commencer
# "Département" : le numéro du département
# "attribution HE" : les attributions hors parcs et enclos (pas toujours disponible)
# "realisations HE" : les réalisations hors parcs et enclos
# "MA HE » : les destructions lors des mesures adminisatrtives hors parcs et enclos
# "somme MA+real" : somme …


species_dictionary <- tribble(
 ~espece, ~scientificName,
"chevreuil", "Capreolus capreolus",
"sanglier" , "Sus scrofa",
"cerf élaphe", "Cervus elaphus",
"chamois" , "Rupicapra rupicapra",
"mouflon" , "Ovis aries",
"isard" , "Rupicapra pyrenaica",
"daim" , "Dama dama",
"cerf sika" , "Cervus nippon"
)


hunting_FR <- read_delim(file = "~/enetwild/data/tableaux_de_chasse.csv", delim = ";")

hunting_FR <- hunting_FR %>%
  mutate(annee_end = annee + 1) %>%
  left_join(species_dictionary) %>%
  filter(annee %in% 2014:2021) %>%
  select(-espece)

write_csv(hunting_FR, file = "~/enetwild/data/Ungulates_Hunting_OFB_2014_2020.csv")




dat %>% filter(scientificName == "Sus scrofa", annee == 2019) %>%
  summarise(sum(`somme MA+real`))

print(paste0("Total 2017 : ", sum(tab_FR$`Realisations hors enclos parcs`[which(tab_FR$`Annee debut campagne` == 2017)])))
print(paste0("Total 2018 : ", sum(tab_FR$`Realisations hors enclos parcs`[which(tab_FR$`Annee debut campagne` == 2018)])))
print(paste0("Total 2019 : ", sum(tab_FR$`Realisations hors enclos parcs`[which(tab_FR$`Annee debut campagne` == 2019)])))


dat %>%
  ggplot(aes(x = annee, y = `somme MA+real`, colour = scientificName)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(.~scientificName, scales = "free_y") +
  theme_bw()



### Belgium
dat_be <- read_csv2(file = "enetwild/dat.csv")
dat_be %>% summarise(sum(aantal))




### Switzerland

sanglier_ch <- read_csv(file = "enetwild/Sanglier, 2011-2020.csv")
sanglier_ch %>%
  group_by(Ann?e) %>%
  mutate(total_killed = Verrat + Laie + Marcassin)
