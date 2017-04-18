### LOAD CLEA R WORKSPACE/DATA FILE FIRST (HERE AS 'clea_20160523')

require(dplyr)

install.packages(c("ggmap", "rgdal", "rgeos", "maptools", "grid", "gridExtra"))

library(ggmap)
library(rgdal)
library(rgeos)
library(maptools)
library(grid)
library(gridExtra)

### BELGIUM

BELGIUM<-subset(clea_20160523, ctr==56 & yr==1995)
### Top 4 Parties in 1995: CPP (CV) - yellow/orange, FLD (VLD) - blue, SPW - red, SPF - red
### Party numbers: CPP - 5, FLD - 30, SPW - 19, SPF - 26
BELGIUM<-subset(BELGIUM, pty==5 | pty == 30 | pty == 19 | pty == 26)

BELtop<-BELGIUM %>%
		mutate(CST_YEAR = paste(ctr,yr,cst, sep=".")) %>%
		select(CST_YEAR, pty, pty_n, pvs1) %>%
		data.frame()

belgium_raw <- readOGR(dsn = path.expand("./Belgium1995"), layer = "GRED_Belgium") %>% 
			spTransform(CRS("+proj=longlat +datum=WGS84"))
belgium_fortified <- fortify(belgium_raw, region="CST_1995")

maxPVS<-max(BELtop$pvs1)

belgium_new <- left_join(belgium_fortified, BELtop[BELtop$pty==5,], by = c("id" = "CST_YEAR")) %>% mutate(pty_n=as.factor(pty_n))
a <- ggplot() +
	geom_polygon(data = belgium_new, aes(x=long, y=lat, group=group, fill = pvs1), color = "black", size = 0.25) +
	scale_fill_gradient(low = "white", high = "orange", limits =c(0,maxPVS), na.value = "gray95") +
	coord_map() +
	theme_nothing(legend=T) +
	labs(title = "Belgium 1995 - Christelijke Volkspartij")
ggsave(filename="BEL-CPP.png", plot = a, scale = 2)

belgium_new <- left_join(belgium_fortified, BELtop[BELtop$pty==30,], by = c("id" = "CST_YEAR")) %>% mutate(pty_n=as.factor(pty_n))
b <- ggplot() +
	geom_polygon(data = belgium_new, aes(x=long, y=lat, group=group, fill = pvs1), color = "black", size = 0.25) +
	scale_fill_gradient(low = "white", high = "royalblue", limits =c(0,maxPVS), na.value = "gray95") +
	coord_map() +
	theme_nothing(legend=T) +
	labs(title = "Belgium 1995 - Vlaamse Liberalen en Democraten")
ggsave(filename="BEL-FLD.png", plot = b, scale = 2)

belgium_new <- left_join(belgium_fortified, BELtop[BELtop$pty==19,], by = c("id" = "CST_YEAR")) %>% mutate(pty_n=as.factor(pty_n))
c <- ggplot() +
	geom_polygon(data = belgium_new, aes(x=long, y=lat, group=group, fill = pvs1), color = "black", size = 0.25) +
	scale_fill_gradient(low = "white", high = "red", limits =c(0,maxPVS), na.value = "gray95") +
	coord_map() +
	theme_nothing(legend=T) +
	labs(title = "Belgium 1995 - Parti Socialiste")
ggsave(filename="BEL-SPW.png", plot = c, scale = 2)

belgium_new <- left_join(belgium_fortified, BELtop[BELtop$pty==26,], by = c("id" = "CST_YEAR")) %>% mutate(pty_n=as.factor(pty_n))
d <- ggplot() +
	geom_polygon(data = belgium_new, aes(x=long, y=lat, group=group, fill = pvs1), color = "black", size = 0.25) +
	scale_fill_gradient(low = "white", high = "red", limits =c(0,maxPVS), na.value = "gray95") +
	coord_map() +
	theme_nothing(legend=T) +
	labs(title = "Belgium 1995 - Socialistische Partij")
ggsave(filename="BEL-SPF.png", plot = d, scale = 2)

a <- a + labs(title="Christelijke Volkspartij")
b <- b + labs(title="Vlaamse Liberalen en Democraten")
c <- c + labs(title="Parti Socialiste")
d <- d + labs(title="Socialistische Partij")

all<-grid.arrange(a,b,c,d, ncol=2, nrow=2, top=textGrob("Belgium 1995 - PSNWsw = 0.526",gp=gpar(fontsize=20)))
ggsave(filename="BEL-1995.png", plot = all, scale = 2)

a <- a + scale_fill_gradient(low = "white", high = "black", limits =c(0,maxPVS), na.value = "white")
b <- b + scale_fill_gradient(low = "white", high = "black", limits =c(0,maxPVS), na.value = "white")
c <- c + scale_fill_gradient(low = "white", high = "black", limits =c(0,maxPVS), na.value = "white")
d <- d + scale_fill_gradient(low = "white", high = "black", limits =c(0,maxPVS), na.value = "white")

all<-grid.arrange(a,b,c,d, ncol=2, nrow=2, top=textGrob("Belgium 1995 - PSNWsw = 0.526",gp=gpar(fontsize=20)))
ggsave(filename="BEL-1995-GS.png", plot = all, scale = 2)


### ITALY

ITALY<-subset(clea_20160523, ctr==380 & yr==1996)
### Top 4 Parties in 1996: DPL (PDS) - red, FI - light blue, NA (AN) - dark blue, LN - green
### Party numbers: DPL - 29, FI - 9, NA - 1, LN - 15
ITALY<-subset(ITALY, pty == 29 | pty == 9 | pty == 1 | pty == 15)

ITAtop<-ITALY %>%
		mutate(CST_YEAR = paste(ctr,yr,cst, sep=".")) %>%
		select(CST_YEAR, pty, pty_n, pvs1) %>%
		data.frame()

maxPVS<-max(ITAtop$pvs1)

italy_raw <- readOGR(dsn = path.expand("./Italy1996"), layer = "GRED_Italy_PRtier") %>% 
			spTransform(CRS("+proj=longlat +datum=WGS84"))
italy_fortified <- fortify(italy_raw, region="CST_1996")

italy_new <- left_join(italy_fortified, ITAtop[ITAtop$pty==29,], by = c("id" = "CST_YEAR")) %>% mutate(pty_n=as.factor(pty_n))
a <- ggplot() +
	geom_polygon(data = italy_new, aes(x=long, y=lat, group=group, fill = pvs1), color = "black", size = 0.25) +
	scale_fill_gradient(low = "white", high = "red", limits =c(0,maxPVS), na.value = "gray95") +
	coord_map() +
	theme_nothing(legend=T) +
	labs(title = "Italy 1996 - Partito Democratico Della Sinistra")
ggsave(filename="ITA-DPL.png", plot = a, scale = 2)

italy_new <- left_join(italy_fortified, ITAtop[ITAtop$pty==9,], by = c("id" = "CST_YEAR")) %>% mutate(pty_n=as.factor(pty_n))
b <- ggplot() +
	geom_polygon(data = italy_new, aes(x=long, y=lat, group=group, fill = pvs1), color = "black", size = 0.25) +
	scale_fill_gradient(low = "white", high = "royalblue", limits =c(0,maxPVS), na.value = "gray95") +
	coord_map() +
	theme_nothing(legend=T) +
	labs(title = "Italy 1996 - Forza Italia")
ggsave(filename="ITA-FI.png", plot = b, scale = 2)

italy_new <- left_join(italy_fortified, ITAtop[ITAtop$pty==1,], by = c("id" = "CST_YEAR")) %>% mutate(pty_n=as.factor(pty_n))
c <- ggplot() +
	geom_polygon(data = italy_new, aes(x=long, y=lat, group=group, fill = pvs1), color = "black", size = 0.25) +
	scale_fill_gradient(low = "white", high = "navyblue", limits =c(0,maxPVS), na.value = "gray95") +
	coord_map() +
	theme_nothing(legend=T) +
	labs(title = "Italy 1996 - Alleanza Nazionale")
ggsave(filename="ITA-AN.png", plot = c, scale = 2)

italy_new <- left_join(italy_fortified, ITAtop[ITAtop$pty==15,], by = c("id" = "CST_YEAR")) %>% mutate(pty_n=as.factor(pty_n))
d <- ggplot() +
	geom_polygon(data = italy_new, aes(x=long, y=lat, group=group, fill = pvs1), color = "black", size = 0.25) +
	scale_fill_gradient(low = "white", high = "forestgreen", limits =c(0,maxPVS), na.value = "gray95") +
	coord_map() +
	theme_nothing(legend=T) +
	labs(title = "Italy 1996 - Lega Nord")
ggsave(filename="ITA-LN.png", plot = d, scale = 2)

a <- a + labs(title = "Partito Democratico Della Sinistra")
b <- b + labs(title = "Forza Italia")
c <- c + labs(title = "Alleanza Nazionale")
d <- d + labs(title = "Lega Nord")

all<-grid.arrange(a,b,c,d, ncol=2, nrow=2, top=textGrob("Italy 1996 - PSNWsw = 0.611",gp=gpar(fontsize=20)))
ggsave(filename="ITA-1996.png", plot = all, scale = 2)

a <- a + scale_fill_gradient(low = "white", high = "black", limits =c(0,maxPVS), na.value = "white")
b <- b + scale_fill_gradient(low = "white", high = "black", limits =c(0,maxPVS), na.value = "white")
c <- c + scale_fill_gradient(low = "white", high = "black", limits =c(0,maxPVS), na.value = "white")
d <- d + scale_fill_gradient(low = "white", high = "black", limits =c(0,maxPVS), na.value = "white")

all<-grid.arrange(a,b,c,d, ncol=2, nrow=2, top=textGrob("Italy 1996 - PSNWsw = 0.611",gp=gpar(fontsize=20)))
ggsave(filename="ITA-1996-GS.png", plot = all, scale = 2)

### SPAIN

SPAIN<-subset(clea_20160523, ctr==724 & yr==2008)
### Top 3 Parties in 2008: SSWP (PSOE) - red, PP - light blue, C&U (CiU) - orange/dark blue
### Party numbers: PSOE - 23, PP - 20, CiU - 8
SPAIN<-subset(SPAIN, pty == 23 | pty == 20 | pty == 8)

SPAtop<-SPAIN %>%
		mutate(CST_YEAR = paste(ctr,yr,cst, sep=".")) %>%
		select(CST_YEAR, pty, pty_n, pvs1) %>%
		data.frame()

maxPVS<-max(SPAtop$pvs1)

spain_raw <- readOGR(dsn = path.expand("./Spain2008"), layer = "GRED_Spain") %>% 
			spTransform(CRS("+proj=longlat +datum=WGS84"))
spain_fortified <- fortify(spain_raw, region="CST_2008")

spain_new <- left_join(spain_fortified, SPAtop[SPAtop$pty==23,], by = c("id" = "CST_YEAR")) %>% mutate(pty_n=as.factor(pty_n))
a <- ggplot() +
	geom_polygon(data = spain_new, aes(x=long, y=lat, group=group, fill = pvs1), color = "black", size = 0.25) +
	scale_fill_gradient(low = "white", high = "red", limits =c(0,maxPVS), na.value = "gray95") +
	coord_map() +
	theme_nothing(legend=T) +
	labs(title = "Spain 2009 - Partido Socialista Obrero Espanol")
ggsave(filename="SPA-PSOE.png", plot = a, scale = 2)

spain_new <- left_join(spain_fortified, SPAtop[SPAtop$pty==20,], by = c("id" = "CST_YEAR")) %>% mutate(pty_n=as.factor(pty_n))
b <- ggplot() +
	geom_polygon(data = spain_new, aes(x=long, y=lat, group=group, fill = pvs1), color = "black", size = 0.25) +
	scale_fill_gradient(low = "white", high = "royalblue", limits =c(0,maxPVS), na.value = "gray95") +
	coord_map() +
	theme_nothing(legend=T) +
	labs(title = "Spain 2009 - Partido Popular")
ggsave(filename="SPA-PP.png", plot = b, scale = 2)

spain_new <- left_join(spain_fortified, SPAtop[SPAtop$pty==8,], by = c("id" = "CST_YEAR")) %>% mutate(pty_n=as.factor(pty_n))
c <- ggplot() +
	geom_polygon(data = spain_new, aes(x=long, y=lat, group=group, fill = pvs1), color = "black", size = 0.25) +
	scale_fill_gradient(low = "white", high = "orange", limits =c(0,maxPVS), na.value = "gray95") +
	coord_map() +
	theme_nothing(legend=T) +
	labs(title = "Spain 2009 - Convergencia i Unio ")
ggsave(filename="SPA-CiU.png", plot = c, scale = 2)

a <- a + labs(title = "Partido Socialista Obrero Espanol")
b <- b + labs(title = "Partido Popular")
c <- c + labs(title = "Convergencia i Unio")

all<-grid.arrange(a,b,c, ncol=2, nrow=2, top=textGrob("Spain 2009 - PSNSsw = 0.623",gp=gpar(fontsize=20)))
ggsave(filename="SPA-2009.png", plot = all, scale = 2)

a <- a + scale_fill_gradient(low = "white", high = "black", limits =c(0,maxPVS), na.value = "white")
b <- b + scale_fill_gradient(low = "white", high = "black", limits =c(0,maxPVS), na.value = "white")
c <- c + scale_fill_gradient(low = "white", high = "black", limits =c(0,maxPVS), na.value = "white")

all<-grid.arrange(a,b,c, ncol=2, nrow=2, top=textGrob("Spain 2009 - PSNSsw = 0.623",gp=gpar(fontsize=20)))
ggsave(filename="SPA-2009-GS.png", plot = all, scale = 2)

### ALBANIA

ALBANIA<-subset(clea_20160523, ctr==8 & yr==2009)
### Top 3 Parties in 2009: DP (PDeS) - blue, SP (PSeS) - purple, SMI (LSpI) - red
### Party numbers: PDeS - 23, PSeS - 52, LSpI - 8
ALBANIA<-subset(ALBANIA, pty == 23 | pty == 52 | pty == 8)

ALBtop<-ALBANIA %>%
		mutate(CST_YEAR = paste(ctr,yr,cst, sep=".")) %>%
		select(CST_YEAR, pty, pty_n, pvs1) %>%
		data.frame()
maxPVS<-max(ALBtop$pvs1)

albania_raw <- readOGR(dsn = path.expand("./Albania2009"), layer = "GRED_Albania") %>% 
			spTransform(CRS("+proj=longlat +datum=WGS84"))
albania_fortified <- fortify(albania_raw, region="CST_2009")

albania_new <- left_join(albania_fortified, ALBtop[ALBtop$pty==23,], by = c("id" = "CST_YEAR")) %>% mutate(pty_n=as.factor(pty_n))
p <- ggplot() +
	geom_polygon(data = albania_new, aes(x=long, y=lat, group=group, fill = pvs1), color = "black", size = 0.25) +
	scale_fill_gradient(low = "white", high = "blue", limits =c(0,maxPVS), na.value = "gray95") +
	coord_map() +
	theme_nothing(legend=T) +
	labs(title = "Albania 2009 - Partia Demokratike e Shqipërisë")
ggsave(filename="ALB-PDeS.png", plot = p, scale = 2)

albania_new <- left_join(albania_fortified, ALBtop[ALBtop$pty==52,], by = c("id" = "CST_YEAR")) %>% mutate(pty_n=as.factor(pty_n))
p <- ggplot() +
	geom_polygon(data = albania_new, aes(x=long, y=lat, group=group, fill = pvs1), color = "black", size = 0.25) +
	scale_fill_gradient(low = "white", high = "purple", limits =c(0,maxPVS), na.value = "gray95") +
	coord_map() +
	theme_nothing(legend=T) +
	labs(title = "Albania 2009 - Partia Socialiste e Shqipërisë")
ggsave(filename="ALB-PSeS.png", plot = p, scale = 2)

albania_new <- left_join(albania_fortified, ALBtop[ALBtop$pty==8,], by = c("id" = "CST_YEAR")) %>% mutate(pty_n=as.factor(pty_n))
p <- ggplot() +
	geom_polygon(data = albania_new, aes(x=long, y=lat, group=group, fill = pvs1), color = "black", size = 0.25) +
	scale_fill_gradient(low = "white", high = "red", limits =c(0,maxPVS), na.value = "gray95") +
	coord_map() +
	theme_nothing(legend=T) +
	labs(title = "Albania 2009 - Lëvizja Socialiste për Integrim")
ggsave(filename="ALB-LSpI.png", plot = p, scale = 2)

### UNITED STATES

US<-subset(clea_20160523, ctr==840 & yr==2006)
### Top 2 Parties in 2006: Democratic - blue, Republican - red
### Party numbers: Democratic - 180, Republican - 583
US<-subset(US, pty == 180 | pty == 583)

USAtop<-US %>%
		mutate(CST_YEAR = paste(ctr,yr,cst, sep=".")) %>%
		select(CST_YEAR, pty, pty_n, pvs1) %>%
		data.frame()

maxPVS<-max(USAtop$pvs1)

us_raw <- readOGR(dsn = path.expand("./USA2006"), layer = "GRED_USA") %>% 
			spTransform(CRS("+proj=longlat +datum=WGS84"))
us_fortified <- fortify(us_raw, region="CST_2006")

us_new <- left_join(us_fortified, USAtop[USAtop$pty==180,], by = c("id" = "CST_YEAR")) %>% mutate(pty_n=as.factor(pty_n))
p <- ggplot() +
	geom_polygon(data = us_new, aes(x=long, y=lat, group=group, fill = pvs1), color = "black", size = 0.25) +
	scale_fill_gradient(low = "white", high = "royalblue", limits =c(0,maxPVS), na.value = "gray95") +
	coord_map(xlim=c(-125, -65), ylim=c(25, 50)) +
	theme_nothing(legend=T) +
	labs(title = "United States 2006 - Democratic Party")
ggsave(filename="USA-DEM.png", plot = p, scale = 2)

us_new <- left_join(us_fortified, USAtop[USAtop$pty==583,], by = c("id" = "CST_YEAR")) %>% mutate(pty_n=as.factor(pty_n))
p <- ggplot() +
	geom_polygon(data = us_new, aes(x=long, y=lat, group=group, fill = pvs1), color = "black", size = 0.25) +
	scale_fill_gradient(low = "white", high = "red", limits =c(0,maxPVS), na.value = "gray95") +
	coord_map(xlim=c(-125, -65), ylim=c(25, 50)) +
	theme_nothing(legend=T) +
	labs(title = "United States 2006 - Republican Party")
ggsave(filename="USA-REP.png", plot = p, scale = 2)

### GERMANY

GERMANY<-subset(clea_20160523, ctr==276 & yr==2009)
### Top 5 Parties in 2009: CDU - black, CSU - black(?), SPD - red, Linke - purple, FDP - yellow
### Party numbers: CDU - 7, CSU - 8, SPD - 51, Linke - 73, FDP - 31
GERMANY<-subset(GERMANY, pty == 7 | pty == 8 | pty == 51 | pty == 73 | pty == 31)

GERtop<-GERMANY %>%
		mutate(CST_YEAR = paste(ctr,yr,cst, sep=".")) %>%
		select(CST_YEAR, pty, pty_n, pvs1) %>%
		data.frame()

maxPVS<-max(GERtop$pvs1)

germany_raw <- readOGR(dsn = path.expand("./Germany2009"), layer = "GRED_Germany_PRtier") %>% 
			spTransform(CRS("+proj=longlat +datum=WGS84"))
germany_fortified <- fortify(germany_raw, region="CST_2009")

germany_new <- left_join(germany_fortified, GERtop[GERtop$pty==7,], by = c("id" = "CST_YEAR")) %>% mutate(pty_n=as.factor(pty_n))
p <- ggplot() +
	geom_polygon(data = germany_new, aes(x=long, y=lat, group=group, fill = pvs1), color = "black", size = 0.25) +
	scale_fill_gradient(low = "white", high = "blue", limits =c(0,maxPVS), na.value = "gray95") +
	coord_map() +
	theme_nothing(legend=T) +
	labs(title = "Germany 2009 - Christlich Demokratische Union")
ggsave(filename="GER-CDU.png", plot = p, scale = 2)

germany_new <- left_join(germany_fortified, GERtop[GERtop$pty==8,], by = c("id" = "CST_YEAR")) %>% mutate(pty_n=as.factor(pty_n))
p <- ggplot() +
	geom_polygon(data = germany_new, aes(x=long, y=lat, group=group, fill = pvs1), color = "black", size = 0.25) +
	scale_fill_gradient(low = "white", high = "blue", limits =c(0,maxPVS), na.value = "gray95") +
	coord_map() +
	theme_nothing(legend=T) +
	labs(title = "Germany 2009 - Christlich-Soziale Union in Bayern")
ggsave(filename="GER-CSU.png", plot = p, scale = 2)

germany_new <- left_join(germany_fortified, GERtop[GERtop$pty==51,], by = c("id" = "CST_YEAR")) %>% mutate(pty_n=as.factor(pty_n))
p <- ggplot() +
	geom_polygon(data = germany_new, aes(x=long, y=lat, group=group, fill = pvs1), color = "black", size = 0.25) +
	scale_fill_gradient(low = "white", high = "red", limits =c(0,maxPVS), na.value = "gray95") +
	coord_map() +
	theme_nothing(legend=T) +
	labs(title = "Germany 2009 - Sozialdemokratische Partei Deutschlands")
ggsave(filename="GER-SPD.png", plot = p, scale = 2)

germany_new <- left_join(germany_fortified, GERtop[GERtop$pty==73,], by = c("id" = "CST_YEAR")) %>% mutate(pty_n=as.factor(pty_n))
p <- ggplot() +
	geom_polygon(data = germany_new, aes(x=long, y=lat, group=group, fill = pvs1), color = "black", size = 0.25) +
	scale_fill_gradient(low = "white", high = "purple", limits =c(0,maxPVS), na.value = "gray95") +
	coord_map() +
	theme_nothing(legend=T) +
	labs(title = "Germany 2009 - Die Linke")
ggsave(filename="GER-Linke.png", plot = p, scale = 2)

germany_new <- left_join(germany_fortified, GERtop[GERtop$pty==31,], by = c("id" = "CST_YEAR")) %>% mutate(pty_n=as.factor(pty_n))
p <- ggplot() +
	geom_polygon(data = germany_new, aes(x=long, y=lat, group=group, fill = pvs1), color = "black", size = 0.25) +
	scale_fill_gradient(low = "white", high = "yellow", limits =c(0,maxPVS), na.value = "gray95") +
	coord_map() +
	theme_nothing(legend=T) +
	labs(title = "Germany 2009 - Freie Demokratische Partei")
ggsave(filename="GER-FDP.png", plot = p, scale = 2)

### MEXICO

MEXICO<-subset(clea_20160523, ctr==484 & yr==2012)
### Top 3 Parties in 2008: SSWP (PSOE) - red, PP - light blue, C&U (CiU) - orange/dark blue
### Party numbers: PSOE - 23, PP - 20, CiU - 8
MEXICO<-subset(MEXICO, pty == 9 | pty == 3 | pty == 21)

MEXtop<-MEXICO %>%
		mutate(CST_YEAR = paste(ctr,yr,cst, sep=".")) %>%
		select(CST_YEAR, pty, pty_n, pvs1) %>%
		data.frame()

maxPVS<-max(MEXtop$pvs1)

mexico_raw <- readOGR(dsn = path.expand("./Mexico2012"), layer = "GRED_Mexico_PRtier") %>% 
			spTransform(CRS("+proj=longlat +datum=WGS84"))
mexico_fortified <- fortify(mexico_raw, region="CST_2012")

mexico_new <- left_join(mexico_fortified, MEXtop[MEXtop$pty==9,], by = c("id" = "CST_YEAR")) %>% mutate(pty_n=as.factor(pty_n))
a <- ggplot() +
	geom_polygon(data = mexico_new, aes(x=long, y=lat, group=group, fill = pvs1), color = "black", size = 0.25) +
	scale_fill_gradient(low = "white", high = "yellow", limits =c(0,maxPVS), na.value = "gray95") +
	coord_map() +
	theme_nothing(legend=T) +
	labs(title = "Mexico 2012 - Partido de la Revolucion Democratica")
ggsave(filename="MEX-PRD.png", plot = a, scale = 2)

mexico_new <- left_join(mexico_fortified, MEXtop[MEXtop$pty==3,], by = c("id" = "CST_YEAR")) %>% mutate(pty_n=as.factor(pty_n))
b <- ggplot() +
	geom_polygon(data = mexico_new, aes(x=long, y=lat, group=group, fill = pvs1), color = "black", size = 0.25) +
	scale_fill_gradient(low = "white", high = "royalblue", limits =c(0,maxPVS), na.value = "gray95") +
	coord_map() +
	theme_nothing(legend=T) +
	labs(title = "Mexico 2012 - Partido Accion Nacional")
ggsave(filename="MEX-PAN.png", plot = b, scale = 2)

mexico_new <- left_join(mexico_fortified, MEXtop[MEXtop$pty==21,], by = c("id" = "CST_YEAR")) %>% mutate(pty_n=as.factor(pty_n))
c <- ggplot() +
	geom_polygon(data = mexico_new, aes(x=long, y=lat, group=group, fill = pvs1), color = "black", size = 0.25) +
	scale_fill_gradient(low = "white", high = "green", limits =c(0,maxPVS), na.value = "gray95") +
	coord_map() +
	theme_nothing(legend=T) +
	labs(title = "Mexico 2012 - Partido Revolucionario Institucional")
ggsave(filename="MEX-PRI.png", plot = c, scale = 2)

a <- a + labs(title = "Partido de la Revolucion Democratica")
b <- b + labs(title = "Partido Accion Nacional")
c <- c + labs(title = "Partido Revolucionario Institucional")

all<-grid.arrange(a,b,c, ncol=2, nrow=2, top=textGrob("Mexico 2012 - PSNSsw ~ 0.85",gp=gpar(fontsize=20)))
ggsave(filename="MEX-2012.png", plot = all, scale = 2)

a <- a + scale_fill_gradient(low = "white", high = "black", limits =c(0,maxPVS), na.value = "white")
b <- b + scale_fill_gradient(low = "white", high = "black", limits =c(0,maxPVS), na.value = "white")
c <- c + scale_fill_gradient(low = "white", high = "black", limits =c(0,maxPVS), na.value = "white")

all<-grid.arrange(a,b,c, ncol=2, nrow=2, top=textGrob("Mexico 2012 - PSNSsw ~ 0.85",gp=gpar(fontsize=20)))
ggsave(filename="MEX-2012-GS.png", plot = all, scale = 2)