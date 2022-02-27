require(metafor)

data = read.csv("data/initial-dataset.csv",h=T,sep=';')

data = (data[data$category=="winner",])

library(ape)
library(rotl)

spp = tnrs_match_names(data$species, context_name = "Animals")
my_tree = tol_induced_subtree(spp$ott_id)

my_tree.ult<-compute.brlen(my_tree, method = "Grafen")

my_tree.ult$tip.label = c("Lasiorhychus barbicornis","Narnia femorata",
                          "Teleogryllus commodus","Melanotes ornata","Hemideina crassidens",
                          "Gammarus pulex","Austruca annulipes","Austruca mjoebergi",
                          "Orconectes virilis", "Aegla longirostri", "Neogonodactylus bredini",
                          "Cambridgea foliata")

cov.matrix<-vcv(my_tree.ult,corr=T)

model1 = rma.mv(yi = zf, V = v.zf, mods = ~slope_weapon, data = data,
                random = list(~1|species), R = list( species = cov.matrix))
summary(model1)
plot( zf ~ slope_weapon, data = data)

