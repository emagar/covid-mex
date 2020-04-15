library(lubridate)
# read maps
library(spdep); library(maptools)
# used to determine what datum map is in 
library(rgdal)
#gpclibPermit()
# to use osm backgrounds
library(rJava)
library(OpenStreetMap)
library(rgdal)

# shave map's bb to exclude pacific island 
shave <- function(m = NA, p = .5, s = 0, eastwest = TRUE){ # m is map to be shaved, p the rightmost part (share) to keep, -1<s<1 a shift rightward, eastwest=FALSE makes shift northsouth
    m <- m; p <- p;
    dim <- ifelse(eastwest==TRUE, 1, 2) 
    b <- as.data.frame(m@bbox)
    b[dim,] <- b[dim,] - s*(b$max[dim] - b$min[dim])       # shift map rightward (bbox leftward)
    b$min[dim] <- b$max[dim] - p*(b$max[dim] - b$min[dim]) # keeps only 100*p% of horizontal length
    m@bbox <- as.matrix(b)
    #ed.map$col <- m
    return(m)
}


rm(list = ls())
dd <- "/home/eric/Downloads/Desktop/data/mex/covid-amlo/data/"
setwd(dd)
#
# read municipal data
covid <- read.csv(file = "COVID19_Mexico.csv", header = TRUE, stringsAsFactors = FALSE)
covid[1:2,]
# rename vars, recode
table(covid$ENTIDAD_UM)
covid$edon_unidad <- covid$ENTIDAD_UM; covid$ENTIDAD_UM <- NULL # unidad médica atención
table(covid$SEXO)
covid$dmujer <- 0; covid$dmujer[covid$SEXO==1] <- 1; covid$SEXO <- NULL
table(covid$ENTIDAD_RES)
covid$edon <- covid$ENTIDAD_RES; covid$ENTIDAD_RES <- NULL # entidad de residencia
table(covid$MUNICIPIO_RES)
covid$inegi <- covid$MUNICIPIO_RES; covid$MUNICIPIO_RES <- NULL # municipio de residencia
covid$inegi[covid$inegi==997|covid$inegi==998|covid$inegi==999] <- NA
#table(covid$inegi, useNA = "always")
covid$inegi <- covid$edon*1000 + covid$inegi # add edon
covid$fsintomas <- ymd(covid$FECHA_SINTOMAS); covid$FECHA_SINTOMAS <- NULL
covid$fingreso <- ymd(covid$FECHA_INGRESO); covid$FECHA_INGRESO <- NULL
covid$ddefun <- 1; covid$ddefun[grep("9999", covid$FECHA_DEF)] <- 0
table(covid$FECHA_DEF, covid$ddefun, useNA = "always")
covid$fdefun <- ymd(NA); covid$fdefun[covid$ddefun==1] <- ymd(covid$FECHA_DEF[covid$ddefun==1])
covid$FECHA_DEF <- NULL
table(covid$EDAD, useNA = "always")
covid$edad <- covid$EDAD; covid$EDAD <- NULL
covid$dindig <- 0; covid$dindig[covid$HABLA_LENGUA_INDI==1] <- 1; covid$HABLA_LENGUA_INDI <- NULL
table(covid$RESULTADO, useNA = "always")
covid$resultado <- factor(covid$RESULTADO, levels = 1:3, labels = c("positivo", "no positivo", "pendiente"))
covid$RESULTADO <- NULL
covid$duci <- 0; covid$duci[covid$UCI==1] <- 1 # entró a unidad cuidados intensivos
covid$UCI <- NULL
# drop other variables
covid[1:2,]
sel <- which(colnames(covid) %in% c("FECHA_ACTUALIZACION", "ORIGEN", "SECTOR", "ENTIDAD_NAC", "TIPO_PACIENTE", "INTUBADO", "NEUMONIA", "NACIONALIDAD", "EMBARAZO", "DIABETES", "EPOC", "ASMA", "INMUSUPR", "HIPERTENSION", "OTRA_CON", "CARDIOVASCULAR", "OBESIDAD", "RENAL_CRONICA", "TABAQUISMO", "OTRO_CASO", "MIGRANTE", "PAIS_NACIONALIDAD", "PAIS_ORIGEN"))
covid <- covid[,-sel]
rm(sel)
#
# get inegi to ife codes for municipios
source("/home/eric/Downloads/Desktop/MXelsCalendGovt/elecReturns/ancillary/ife_to_inegi.r")
covid$ife <- inegi.to.ife(covid$inegi)
covid$mun <- inegi.to.mun(covid$inegi)
covid[1:3,]
rm(inegi.to.ife, inegi.to.mun, ife.to.mun, ife.to.inegi)
covid[1,]
#
# parece que gob reporta solo los casos positivos en prueba. No los pendientes. Ni los muertos (neumonía atípica?) negativos.
table(covid$ddefun, covid$resultado, useNA = "always")
#
# chrono cumulative frequencies
# very conservative: positive tests only
table(covid$fingreso[covid$resultado=="positivo"], useNA = "always")
#
cumul <- eq # all municipios
cumul$edon <- as.integer(cumul$inegi/1000)
#
dates <- seq(from = ymd("2020-02-27"), to = today(), by = 1)
#
# add columns for successive days
tmp <- as.data.frame(matrix(0, nrow = nrow(cumul), ncol = length(dates)))
cumul <- cbind(cumul, tmp)
#
##################################################################################
## n positivos --- ojo: does not exclude recoveries (not reported) nor deceased ##
##################################################################################
tmp.pos <- covid[covid$resultado=="positivo",] # subset positive test only
sel <- which(is.na(tmp.pos$inegi)); tmp.pos <- tmp.pos[-sel,] # drop cases with missing municipio
#
for (i in 1:length(dates)){
    # i <- 1 # debug
    message(sprintf("loop %s of %s", i, length(dates)))
    sel <- which(tmp.pos$fingreso <= dates[i])
    tmp <- tmp.pos[sel,] # subset data
    tmp <- tmp[order(tmp$inegi),] # sort
    tmp$one <- 1
    tmp$cumul <- ave(tmp$one, tmp$inegi, FUN=sum, na.rm=TRUE)
    tmp <- tmp[duplicated(tmp$inegi)==FALSE,] # keep one obs per municipio
    #tmp
    #
    sel <- which(cumul$inegi %in% tmp$inegi) # target municipios
    #
    cumul[,-1:-4][sel,i]  <- tmp$cumul # replace target municipios in ith column
    #cumul$inegi[sel] # debug
    #tmp$inegi[order(tmp$inegi)] # debug
    #tmp[149,] # debug
}
#
# add redundant row mapping what day column corresponds to
tmp <- cumul[1,]; tmp[] <- NA
tmp2 <- as.character(dates); tmp2 <- as.numeric(gsub("-", "", tmp2))
tmp[,-1:-4] <- tmp2
cumul <- rbind(tmp, cumul)
rm(tmp,tmp2)
cumul[1:3,]
#
# rename
npos.cum <- cumul


## # read sospechosos
## sos <- read.csv(file = "sospechosos.csv", header = FALSE, stringsAsFactors = FALSE)
## colnames(sos)  <- c("txt")
## #
## # drop accented upper cases
## sos$txt <- gsub(pattern = "[Á]", replacement = "A", sos$txt)
## sos$txt <- gsub(pattern = "[É]", replacement = "E", sos$txt)
## sos$txt <- gsub(pattern = "[Í]", replacement = "I", sos$txt)
## sos$txt <- gsub(pattern = "[Ó]", replacement = "O", sos$txt)
## sos$txt <- gsub(pattern = "[Ú]", replacement = "U", sos$txt)
## #
## # break data into columns 
## sos$txt2 <- sos$txt # duplicate text before manipulation
## #
## # n
## sos$n <- gsub(pattern = "^([0-9]+) .*", replacement = "\\1", sos$txt)
## sos$n <- as.numeric(sos$n)
## sos$txt <- gsub(pattern = "^[0-9]+ (.*)", replacement = "\\1", sos$txt) # drop duplicated n
## #
## # edoloc
## sos$edoloc <- gsub(pattern = "^([A-ZÑ. ]+) [MF] .*$", replacement = "\\1", sos$txt)
## sos$txt <- gsub(pattern = "^[A-ZÑ. ]+ ([MF] .*)$", replacement = "\\1", sos$txt) # drop duplicated edoloc
## #
## # edo
## sos$edo <- sos$edoloc
## sos$edo <- sub(pattern = "^AGUASCALIENTES.+$",      replacement = "ags", sos$edo)
## sos$edo <- sub(pattern = "^BAJA CALIFORNIA SUR.+$", replacement = "bcs", sos$edo)
## sos$edo <- sub(pattern = "^BAJA CALIFORNIA.+$",     replacement = "bc",  sos$edo)
## sos$edo <- sub(pattern = "^CAMPECHE.+$",            replacement = "cam", sos$edo)
## sos$edo <- sub(pattern = "^COAHUILA.+$",            replacement = "coa", sos$edo)
## sos$edo <- sub(pattern = "^COLIMA.+$",              replacement = "col", sos$edo)
## sos$edo <- sub(pattern = "^CHIAPAS.+$",             replacement = "cps", sos$edo)
## sos$edo <- sub(pattern = "^CHIHUAHUA.+$",           replacement = "cua", sos$edo)
## sos$edo <- sub(pattern = "^CIUDAD DE MEXICO.+$",    replacement = "df",  sos$edo)
## sos$edo <- sub(pattern = "^DURANGO.+$",             replacement = "dgo", sos$edo)
## sos$edo <- sub(pattern = "^GUANAJUATO.+$",          replacement = "gua", sos$edo)
## sos$edo <- sub(pattern = "^GUERRERO.+$",            replacement = "gue", sos$edo)
## sos$edo <- sub(pattern = "^HIDALGO.+$",             replacement = "hgo", sos$edo)
## sos$edo <- sub(pattern = "^JALISCO.+$",             replacement = "jal", sos$edo)
## sos$edo <- sub(pattern = "^MEXICO.+$",              replacement = "mex", sos$edo)
## sos$edo <- sub(pattern = "^MICHOACAN.+$",           replacement = "mic", sos$edo)
## sos$edo <- sub(pattern = "^MORELOS.+$",             replacement = "mor", sos$edo)
## sos$edo <- sub(pattern = "^NAYARIT.+$",             replacement = "nay", sos$edo)
## sos$edo <- sub(pattern = "^NUEVO LEON.+$",          replacement = "nl",  sos$edo)
## sos$edo <- sub(pattern = "^OAXACA.+$",              replacement = "oax", sos$edo)
## sos$edo <- sub(pattern = "^PUEBLA.+$",              replacement = "pue", sos$edo)
## sos$edo <- sub(pattern = "^QUERETARO.+$",           replacement = "que", sos$edo)
## sos$edo <- sub(pattern = "^QUINTANA ROO.+$",        replacement = "qui", sos$edo)
## sos$edo <- sub(pattern = "^SAN LUIS POTOSI.+$",     replacement = "san", sos$edo)
## sos$edo <- sub(pattern = "^SINALOA.+$",             replacement = "sin", sos$edo)
## sos$edo <- sub(pattern = "^SONORA.+$",              replacement = "son", sos$edo)
## sos$edo <- sub(pattern = "^TABASCO.+$",             replacement = "tab", sos$edo)
## sos$edo <- sub(pattern = "^TAMAULIPAS.+$",          replacement = "tam", sos$edo)
## sos$edo <- sub(pattern = "^TLAXCALA.+$",            replacement = "tla", sos$edo)
## sos$edo <- sub(pattern = "^VERACRUZ.+$",            replacement = "ver", sos$edo)
## sos$edo <- sub(pattern = "^YUCATAN.+$",             replacement = "yuc", sos$edo)
## sos$edo <- sub(pattern = "^ZACATECAS.+$",           replacement = "zac", sos$edo)
## #
## # edon
## sos$edon <- NA
## for (i in 1:length(sos$edo)){
##     sos$edon[i] <- which( c("ags", "bc", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac") %in% sos$edo[i])
## }
## #
## # loc
## sos$loc <- sos$edoloc
## sos$loc <- sub(pattern = "^AGUASCALIENTES (.+)$",      replacement = "\\1", sos$loc)
## sos$loc <- sub(pattern = "^BAJA CALIFORNIA SUR (.+)$", replacement = "\\1", sos$loc)
## sos$loc <- sub(pattern = "^BAJA CALIFORNIA (.+)$",     replacement = "\\1", sos$loc)
## sos$loc <- sub(pattern = "^CAMPECHE (.+)$",            replacement = "\\1", sos$loc)
## sos$loc <- sub(pattern = "^COAHUILA (.+)$",            replacement = "\\1", sos$loc)
## sos$loc <- sub(pattern = "^COLIMA (.+)$",              replacement = "\\1", sos$loc)
## sos$loc <- sub(pattern = "^CHIAPAS (.+)$",             replacement = "\\1", sos$loc)
## sos$loc <- sub(pattern = "^CHIHUAHUA (.+)$",           replacement = "\\1", sos$loc)
## sos$loc <- sub(pattern = "^CIUDAD DE MEXICO (.+)$",    replacement = "\\1", sos$loc)
## sos$loc <- sub(pattern = "^DURANGO (.+)$",             replacement = "\\1", sos$loc)
## sos$loc <- sub(pattern = "^GUANAJUATO (.+)$",          replacement = "\\1", sos$loc)
## sos$loc <- sub(pattern = "^GUERRERO (.+)$",            replacement = "\\1", sos$loc)
## sos$loc <- sub(pattern = "^HIDALGO (.+)$",             replacement = "\\1", sos$loc)
## sos$loc <- sub(pattern = "^JALISCO (.+)$",             replacement = "\\1", sos$loc)
## sos$loc <- sub(pattern = "^MEXICO (.+)$",              replacement = "\\1", sos$loc)
## sos$loc <- sub(pattern = "^MICHOACAN (.+)$",           replacement = "\\1", sos$loc)
## sos$loc <- sub(pattern = "^MORELOS (.+)$",             replacement = "\\1", sos$loc)
## sos$loc <- sub(pattern = "^NAYARIT (.+)$",             replacement = "\\1", sos$loc)
## sos$loc <- sub(pattern = "^NUEVO LEON (.+)$",          replacement = "\\1", sos$loc)
## sos$loc <- sub(pattern = "^OAXACA (.+)$",              replacement = "\\1", sos$loc)
## sos$loc <- sub(pattern = "^PUEBLA (.+)$",              replacement = "\\1", sos$loc)
## sos$loc <- sub(pattern = "^QUERETARO (.+)$",           replacement = "\\1", sos$loc)
## sos$loc <- sub(pattern = "^QUINTANA ROO (.+)$",        replacement = "\\1", sos$loc)
## sos$loc <- sub(pattern = "^SAN LUIS POTOSI (.+)$",     replacement = "\\1", sos$loc)
## sos$loc <- sub(pattern = "^SINALOA (.+)$",             replacement = "\\1", sos$loc)
## sos$loc <- sub(pattern = "^SONORA (.+)$",              replacement = "\\1", sos$loc)
## sos$loc <- sub(pattern = "^TABASCO (.+)$",             replacement = "\\1", sos$loc)
## sos$loc <- sub(pattern = "^TAMAULIPAS (.+)$",          replacement = "\\1", sos$loc)
## sos$loc <- sub(pattern = "^TLAXCALA (.+)$",            replacement = "\\1", sos$loc)
## sos$loc <- sub(pattern = "^VERACRUZ (.+)$",            replacement = "\\1", sos$loc)
## sos$loc <- sub(pattern = "^YUCATAN (.+)$",             replacement = "\\1", sos$loc)
## sos$loc <- sub(pattern = "^ZACATECAS (.+)$",           replacement = "\\1", sos$loc)
## #
## # clean
## sos$edoloc <- NULL # drop duplicate info
## #
## # sexo
## sos$sexo <- sub(pattern = "^([FM]) ([0-9]+) .*$", replacement = "\\1", sos$txt)
## sos$edad <- sub(pattern = "^([FM]) ([0-9]+) .*$", replacement = "\\2", sos$txt)
## sos$edad <- as.numeric(sos$edad)
## #
## sos$txt <- gsub(pattern = "^[FM] [0-9]+ (.*)$", replacement = "\\1", sos$txt) # drop duplicated edoloc
## #
## # sintomas
## sos$sintomas <- sub(pattern = "^([0-9/]+) .*$", replacement = "\\1", sos$txt)
## sos$sintomas <- dmy(sos$sintomas)
## table(sos$sintomas)
## sos$txt <- gsub(pattern = "^[0-9/]+ (.*)$", replacement = "\\1", sos$txt) # drop duplicated edoloc
## #
## # status
## sos$status <- sos$txt
## #
## # recover original text
## sos$txt <- sos$txt2
## sos$txt2 <- NULL
## sos <- cbind(sos[,-1], sos[,1]) # move text to last col
##
## # sort
## sos <- sos[order(sos$edon, sos$loc),]
## write.csv(sos, file = "tmp.csv", row.names = FALSE)

################
# national map #
################
ruta <- "/home/eric/Desktop/data/mapas/entidad"
nat.map <- readOGR(dsn = ruta, layer = 'ENTIDAD')
summary(nat.map)
# projects to a different datum with long and lat
nat.map <- spTransform(nat.map, osm()) # project to osm native Mercator
rm(ruta)

# trial
plot(shave(nat.map, p = .94), lwd = .5, border = "gray")
axis(1); axis(2)


##########################
## read municipios maps ##
##########################
md <- "/home/eric/Downloads/Desktop/data/mapas/cartografia28feb2013rojano/"
edos <- c("ags", "bc", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac")
#
i <- 1
edo <- edos[i]
ruta <- paste(md, edo, "/", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = ruta, layer = 'MUNICIPIO')
#summary(tmp) # EXPLORE
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm()) # project to osm native Mercator
# adds quantity to plot (horizontal time series)
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$MUNICIPIO
#
tmp.cum <- cumul[-1,] # drops date tag in line 1 
tmp.cum <- tmp.cum[tmp.cum$edon==i,] # keeps one state 
tmp.cum$munn <- tmp.cum$ife - tmp.cum$edon*1000 # adds munn based on ife, as in maps
#
tmp.data <- merge(x = tmp.data, y = tmp.cum, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.cum)
#plot(tmp)
#summary(tmp)
agsmun.map <- tmp # rename
#
#
i <- i+1
edo <- edos[i]
ruta <- paste(md, edo, "/", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = ruta, layer = 'MUNICIPIO')
#summary(tmp) # EXPLORE
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm()) # project to osm native Mercator
# adds quantity to plot (horizontal time series)
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$MUNICIPIO
#
tmp.cum <- cumul[-1,] # drops date tag in line 1 
tmp.cum <- tmp.cum[tmp.cum$edon==i,] # keeps one state 
tmp.cum$munn <- tmp.cum$ife - tmp.cum$edon*1000 # adds munn based on ife, as in maps
#
tmp.data <- merge(x = tmp.data, y = tmp.cum, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.cum)
#plot(tmp)
bcmun.map <- tmp # rename
#
i <- i+1
edo <- edos[i]
ruta <- paste(md, edo, "/", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = ruta, layer = 'MUNICIPIO')
#summary(tmp) # EXPLORE
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm()) # project to osm native Mercator
# adds quantity to plot (horizontal time series)
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$MUNICIPIO
#
tmp.cum <- cumul[-1,] # drops date tag in line 1 
tmp.cum <- tmp.cum[tmp.cum$edon==i,] # keeps one state 
tmp.cum$munn <- tmp.cum$ife - tmp.cum$edon*1000 # adds munn based on ife, as in maps
#
tmp.data <- merge(x = tmp.data, y = tmp.cum, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.cum)
#plot(tmp)
bcsmun.map <- tmp # rename
#
i <- i+1
edo <- edos[i]
ruta <- paste(md, edo, "/", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = ruta, layer = 'MUNICIPIO')
#summary(tmp) # EXPLORE
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm()) # project to osm native Mercator
# adds quantity to plot (horizontal time series)
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$MUNICIPIO
#
tmp.cum <- cumul[-1,] # drops date tag in line 1 
tmp.cum <- tmp.cum[tmp.cum$edon==i,] # keeps one state 
tmp.cum$munn <- tmp.cum$ife - tmp.cum$edon*1000 # adds munn based on ife, as in maps
#
tmp.data <- merge(x = tmp.data, y = tmp.cum, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.cum)
#plot(tmp)
cammun.map <- tmp # rename
#
i <- i+1
edo <- edos[i]
ruta <- paste(md, edo, "/", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = ruta, layer = 'MUNICIPIO')
#summary(tmp) # EXPLORE
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm()) # project to osm native Mercator
# adds quantity to plot (horizontal time series)
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$MUNICIPIO
#
tmp.cum <- cumul[-1,] # drops date tag in line 1 
tmp.cum <- tmp.cum[tmp.cum$edon==i,] # keeps one state 
tmp.cum$munn <- tmp.cum$ife - tmp.cum$edon*1000 # adds munn based on ife, as in maps
#
tmp.data <- merge(x = tmp.data, y = tmp.cum, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.cum)
#plot(tmp)
coamun.map <- tmp # rename
#
i <- i+1
edo <- edos[i]
ruta <- paste(md, edo, "/", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = ruta, layer = 'MUNICIPIO')
#summary(tmp) # EXPLORE
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm()) # project to osm native Mercator
# adds quantity to plot (horizontal time series)
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$MUNICIPIO
#
tmp.cum <- cumul[-1,] # drops date tag in line 1 
tmp.cum <- tmp.cum[tmp.cum$edon==i,] # keeps one state 
tmp.cum$munn <- tmp.cum$ife - tmp.cum$edon*1000 # adds munn based on ife, as in maps
#
tmp.data <- merge(x = tmp.data, y = tmp.cum, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.cum)
#plot(tmp)
colmun.map <- tmp # rename
#
i <- i+1
edo <- edos[i]
ruta <- paste(md, edo, "/", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = ruta, layer = 'MUNICIPIO')
#summary(tmp) # EXPLORE
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm()) # project to osm native Mercator
# adds quantity to plot (horizontal time series)
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$MUNICIPIO
#
tmp.cum <- cumul[-1,] # drops date tag in line 1 
tmp.cum <- tmp.cum[tmp.cum$edon==i,] # keeps one state 
tmp.cum$munn <- tmp.cum$ife - tmp.cum$edon*1000 # adds munn based on ife, as in maps
#
tmp.data <- merge(x = tmp.data, y = tmp.cum, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.cum)
#plot(tmp)
cpsmun.map <- tmp # rename
#
i <- i+1
edo <- edos[i]
ruta <- paste(md, edo, "/", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = ruta, layer = 'MUNICIPIO')
#summary(tmp) # EXPLORE
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm()) # project to osm native Mercator
# adds quantity to plot (horizontal time series)
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$MUNICIPIO
#
tmp.cum <- cumul[-1,] # drops date tag in line 1 
tmp.cum <- tmp.cum[tmp.cum$edon==i,] # keeps one state 
tmp.cum$munn <- tmp.cum$ife - tmp.cum$edon*1000 # adds munn based on ife, as in maps
#
tmp.data <- merge(x = tmp.data, y = tmp.cum, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.cum)
#plot(tmp)
cuamun.map <- tmp # rename
#
i <- i+1
edo <- edos[i]
ruta <- paste(md, edo, "/", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = ruta, layer = 'MUNICIPIO')
#summary(tmp) # EXPLORE
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm()) # project to osm native Mercator
# adds quantity to plot (horizontal time series)
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$MUNICIPIO
#
tmp.cum <- cumul[-1,] # drops date tag in line 1 
tmp.cum <- tmp.cum[tmp.cum$edon==i,] # keeps one state 
tmp.cum$munn <- tmp.cum$ife - tmp.cum$edon*1000 # adds munn based on ife, as in maps
#
tmp.data <- merge(x = tmp.data, y = tmp.cum, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.cum)
#plot(tmp)
dfmun.map <- tmp # rename
#
i <- i+1
edo <- edos[i]
ruta <- paste(md, edo, "/", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = ruta, layer = 'MUNICIPIO')
#summary(tmp) # EXPLORE
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm()) # project to osm native Mercator
# adds quantity to plot (horizontal time series)
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$MUNICIPIO
#
tmp.cum <- cumul[-1,] # drops date tag in line 1 
tmp.cum <- tmp.cum[tmp.cum$edon==i,] # keeps one state 
tmp.cum$munn <- tmp.cum$ife - tmp.cum$edon*1000 # adds munn based on ife, as in maps
#
tmp.data <- merge(x = tmp.data, y = tmp.cum, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.cum)
#plot(tmp)
dgomun.map <- tmp # rename
#
i <- i+1
edo <- edos[i]
ruta <- paste(md, edo, "/", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = ruta, layer = 'MUNICIPIO')
#summary(tmp) # EXPLORE
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm()) # project to osm native Mercator
# adds quantity to plot (horizontal time series)
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$MUNICIPIO
#
tmp.cum <- cumul[-1,] # drops date tag in line 1 
tmp.cum <- tmp.cum[tmp.cum$edon==i,] # keeps one state 
tmp.cum$munn <- tmp.cum$ife - tmp.cum$edon*1000 # adds munn based on ife, as in maps
#
tmp.data <- merge(x = tmp.data, y = tmp.cum, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.cum)
#plot(tmp)
guamun.map <- tmp # rename
#
i <- i+1
edo <- edos[i]
ruta <- paste(md, edo, "/", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = ruta, layer = 'MUNICIPIO')
#summary(tmp) # EXPLORE
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm()) # project to osm native Mercator
# adds quantity to plot (horizontal time series)
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$MUNICIPIO
#
tmp.cum <- cumul[-1,] # drops date tag in line 1 
tmp.cum <- tmp.cum[tmp.cum$edon==i,] # keeps one state 
tmp.cum$munn <- tmp.cum$ife - tmp.cum$edon*1000 # adds munn based on ife, as in maps
#
tmp.data <- merge(x = tmp.data, y = tmp.cum, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.cum)
#plot(tmp)
guemun.map <- tmp # rename
#
i <- i+1
edo <- edos[i]
ruta <- paste(md, edo, "/", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = ruta, layer = 'MUNICIPIO')
#summary(tmp) # EXPLORE
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm()) # project to osm native Mercator
# adds quantity to plot (horizontal time series)
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$MUNICIPIO
#
tmp.cum <- cumul[-1,] # drops date tag in line 1 
tmp.cum <- tmp.cum[tmp.cum$edon==i,] # keeps one state 
tmp.cum$munn <- tmp.cum$ife - tmp.cum$edon*1000 # adds munn based on ife, as in maps
#
tmp.data <- merge(x = tmp.data, y = tmp.cum, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.cum)
#plot(tmp)
hgomun.map <- tmp # rename
#
i <- i+1
edo <- edos[i]
ruta <- paste(md, edo, "/", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = ruta, layer = 'MUNICIPIO')
#summary(tmp) # EXPLORE
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm()) # project to osm native Mercator
# adds quantity to plot (horizontal time series)
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$MUNICIPIO
#
tmp.cum <- cumul[-1,] # drops date tag in line 1 
tmp.cum <- tmp.cum[tmp.cum$edon==i,] # keeps one state 
tmp.cum$munn <- tmp.cum$ife - tmp.cum$edon*1000 # adds munn based on ife, as in maps
#
tmp.data <- merge(x = tmp.data, y = tmp.cum, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.cum)
#plot(tmp)
jalmun.map <- tmp # rename
#
i <- i+1
edo <- edos[i]
ruta <- paste(md, edo, "/", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = ruta, layer = 'MUNICIPIO')
#summary(tmp) # EXPLORE
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm()) # project to osm native Mercator
# adds quantity to plot (horizontal time series)
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$MUNICIPIO
#
tmp.cum <- cumul[-1,] # drops date tag in line 1 
tmp.cum <- tmp.cum[tmp.cum$edon==i,] # keeps one state 
tmp.cum$munn <- tmp.cum$ife - tmp.cum$edon*1000 # adds munn based on ife, as in maps
#
tmp.data <- merge(x = tmp.data, y = tmp.cum, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.cum)
#plot(tmp)
mexmun.map <- tmp # rename
#
i <- i+1
edo <- edos[i]
ruta <- paste(md, edo, "/", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = ruta, layer = 'MUNICIPIO')
#summary(tmp) # EXPLORE
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm()) # project to osm native Mercator
# adds quantity to plot (horizontal time series)
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$MUNICIPIO
#
tmp.cum <- cumul[-1,] # drops date tag in line 1 
tmp.cum <- tmp.cum[tmp.cum$edon==i,] # keeps one state 
tmp.cum$munn <- tmp.cum$ife - tmp.cum$edon*1000 # adds munn based on ife, as in maps
#
tmp.data <- merge(x = tmp.data, y = tmp.cum, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.cum)
#plot(tmp)
micmun.map <- tmp # rename
#
i <- i+1
edo <- edos[i]
ruta <- paste(md, edo, "/", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = ruta, layer = 'MUNICIPIO')
#summary(tmp) # EXPLORE
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm()) # project to osm native Mercator
# adds quantity to plot (horizontal time series)
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$MUNICIPIO
#
tmp.cum <- cumul[-1,] # drops date tag in line 1 
tmp.cum <- tmp.cum[tmp.cum$edon==i,] # keeps one state 
tmp.cum$munn <- tmp.cum$ife - tmp.cum$edon*1000 # adds munn based on ife, as in maps
#
tmp.data <- merge(x = tmp.data, y = tmp.cum, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.cum)
#plot(tmp)
mormun.map <- tmp # rename
#
i <- i+1
edo <- edos[i]
ruta <- paste(md, edo, "/", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = ruta, layer = 'MUNICIPIO')
#summary(tmp) # EXPLORE
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm()) # project to osm native Mercator
# adds quantity to plot (horizontal time series)
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$MUNICIPIO
#
tmp.cum <- cumul[-1,] # drops date tag in line 1 
tmp.cum <- tmp.cum[tmp.cum$edon==i,] # keeps one state 
tmp.cum$munn <- tmp.cum$ife - tmp.cum$edon*1000 # adds munn based on ife, as in maps
#
tmp.data <- merge(x = tmp.data, y = tmp.cum, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.cum)
#plot(tmp)
naymun.map <- tmp # rename
#
i <- i+1
edo <- edos[i]
ruta <- paste(md, edo, "/", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = ruta, layer = 'MUNICIPIO')
#summary(tmp) # EXPLORE
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm()) # project to osm native Mercator
# adds quantity to plot (horizontal time series)
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$MUNICIPIO
#
tmp.cum <- cumul[-1,] # drops date tag in line 1 
tmp.cum <- tmp.cum[tmp.cum$edon==i,] # keeps one state 
tmp.cum$munn <- tmp.cum$ife - tmp.cum$edon*1000 # adds munn based on ife, as in maps
#
tmp.data <- merge(x = tmp.data, y = tmp.cum, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.cum)
#plot(tmp)
nlmun.map <- tmp # rename
#
i <- i+1
edo <- edos[i]
ruta <- paste(md, edo, "/", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = ruta, layer = 'MUNICIPIO')
#summary(tmp) # EXPLORE
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm()) # project to osm native Mercator
# adds quantity to plot (horizontal time series)
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$MUNICIPIO
#
tmp.cum <- cumul[-1,] # drops date tag in line 1 
tmp.cum <- tmp.cum[tmp.cum$edon==i,] # keeps one state 
tmp.cum$munn <- tmp.cum$ife - tmp.cum$edon*1000 # adds munn based on ife, as in maps
#
tmp.data <- merge(x = tmp.data, y = tmp.cum, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.cum)
#plot(tmp)
oaxmun.map <- tmp # rename
#
i <- i+1
edo <- edos[i]
ruta <- paste(md, edo, "/", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = ruta, layer = 'MUNICIPIO')
#summary(tmp) # EXPLORE
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm()) # project to osm native Mercator
# adds quantity to plot (horizontal time series)
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$MUNICIPIO
#
tmp.cum <- cumul[-1,] # drops date tag in line 1 
tmp.cum <- tmp.cum[tmp.cum$edon==i,] # keeps one state 
tmp.cum$munn <- tmp.cum$ife - tmp.cum$edon*1000 # adds munn based on ife, as in maps
#
tmp.data <- merge(x = tmp.data, y = tmp.cum, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.cum)
#plot(tmp)
puemun.map <- tmp # rename
#
i <- i+1
edo <- edos[i]
ruta <- paste(md, edo, "/", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = ruta, layer = 'MUNICIPIO')
#summary(tmp) # EXPLORE
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm()) # project to osm native Mercator
# adds quantity to plot (horizontal time series)
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$MUNICIPIO
#
tmp.cum <- cumul[-1,] # drops date tag in line 1 
tmp.cum <- tmp.cum[tmp.cum$edon==i,] # keeps one state 
tmp.cum$munn <- tmp.cum$ife - tmp.cum$edon*1000 # adds munn based on ife, as in maps
#
tmp.data <- merge(x = tmp.data, y = tmp.cum, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.cum)
#plot(tmp)
quemun.map <- tmp # rename
#
i <- i+1
edo <- edos[i]
ruta <- paste(md, edo, "/", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = ruta, layer = 'MUNICIPIO')
#summary(tmp) # EXPLORE
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm()) # project to osm native Mercator
# adds quantity to plot (horizontal time series)
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$MUNICIPIO
#
tmp.cum <- cumul[-1,] # drops date tag in line 1 
tmp.cum <- tmp.cum[tmp.cum$edon==i,] # keeps one state 
tmp.cum$munn <- tmp.cum$ife - tmp.cum$edon*1000 # adds munn based on ife, as in maps
#
tmp.data <- merge(x = tmp.data, y = tmp.cum, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.cum)
#plot(tmp)
quimun.map <- tmp # rename
#
i <- i+1
edo <- edos[i]
ruta <- paste(md, edo, "/", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = ruta, layer = 'MUNICIPIO')
#summary(tmp) # EXPLORE
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm()) # project to osm native Mercator
# adds quantity to plot (horizontal time series)
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$MUNICIPIO
#
tmp.cum <- cumul[-1,] # drops date tag in line 1 
tmp.cum <- tmp.cum[tmp.cum$edon==i,] # keeps one state 
tmp.cum$munn <- tmp.cum$ife - tmp.cum$edon*1000 # adds munn based on ife, as in maps
#
tmp.data <- merge(x = tmp.data, y = tmp.cum, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.cum)
#plot(tmp)
sanmun.map <- tmp # rename
#
i <- i+1
edo <- edos[i]
ruta <- paste(md, edo, "/", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = ruta, layer = 'MUNICIPIO')
#summary(tmp) # EXPLORE
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm()) # project to osm native Mercator
# adds quantity to plot (horizontal time series)
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$MUNICIPIO
#
tmp.cum <- cumul[-1,] # drops date tag in line 1 
tmp.cum <- tmp.cum[tmp.cum$edon==i,] # keeps one state 
tmp.cum$munn <- tmp.cum$ife - tmp.cum$edon*1000 # adds munn based on ife, as in maps
#
tmp.data <- merge(x = tmp.data, y = tmp.cum, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.cum)
#plot(tmp)
sinmun.map <- tmp # rename
#
i <- i+1
edo <- edos[i]
ruta <- paste(md, edo, "/", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = ruta, layer = 'MUNICIPIO')
#summary(tmp) # EXPLORE
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm()) # project to osm native Mercator
# adds quantity to plot (horizontal time series)
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$MUNICIPIO
#
tmp.cum <- cumul[-1,] # drops date tag in line 1 
tmp.cum <- tmp.cum[tmp.cum$edon==i,] # keeps one state 
tmp.cum$munn <- tmp.cum$ife - tmp.cum$edon*1000 # adds munn based on ife, as in maps
#
tmp.data <- merge(x = tmp.data, y = tmp.cum, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.cum)
#plot(tmp)
sonmun.map <- tmp # rename
#
i <- i+1
edo <- edos[i]
ruta <- paste(md, edo, "/", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = ruta, layer = 'MUNICIPIO')
#summary(tmp) # EXPLORE
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm()) # project to osm native Mercator
# adds quantity to plot (horizontal time series)
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$MUNICIPIO
#
tmp.cum <- cumul[-1,] # drops date tag in line 1 
tmp.cum <- tmp.cum[tmp.cum$edon==i,] # keeps one state 
tmp.cum$munn <- tmp.cum$ife - tmp.cum$edon*1000 # adds munn based on ife, as in maps
#
tmp.data <- merge(x = tmp.data, y = tmp.cum, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.cum)
#plot(tmp)
tabmun.map <- tmp # rename
#
i <- i+1
edo <- edos[i]
ruta <- paste(md, edo, "/", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = ruta, layer = 'MUNICIPIO')
#summary(tmp) # EXPLORE
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm()) # project to osm native Mercator
# adds quantity to plot (horizontal time series)
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$MUNICIPIO
#
tmp.cum <- cumul[-1,] # drops date tag in line 1 
tmp.cum <- tmp.cum[tmp.cum$edon==i,] # keeps one state 
tmp.cum$munn <- tmp.cum$ife - tmp.cum$edon*1000 # adds munn based on ife, as in maps
#
tmp.data <- merge(x = tmp.data, y = tmp.cum, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.cum)
#plot(tmp)
tammun.map <- tmp # rename
#
i <- i+1
edo <- edos[i]
ruta <- paste(md, edo, "/", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = ruta, layer = 'MUNICIPIO')
#summary(tmp) # EXPLORE
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm()) # project to osm native Mercator
# adds quantity to plot (horizontal time series)
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$MUNICIPIO
#
tmp.cum <- cumul[-1,] # drops date tag in line 1 
tmp.cum <- tmp.cum[tmp.cum$edon==i,] # keeps one state 
tmp.cum$munn <- tmp.cum$ife - tmp.cum$edon*1000 # adds munn based on ife, as in maps
#
tmp.data <- merge(x = tmp.data, y = tmp.cum, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.cum)
#plot(tmp)
tlamun.map <- tmp # rename
#
i <- i+1
edo <- edos[i]
ruta <- paste(md, edo, "/", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = ruta, layer = 'MUNICIPIO')
#summary(tmp) # EXPLORE
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm()) # project to osm native Mercator
# adds quantity to plot (horizontal time series)
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$MUNICIPIO
#
tmp.cum <- cumul[-1,] # drops date tag in line 1 
tmp.cum <- tmp.cum[tmp.cum$edon==i,] # keeps one state 
tmp.cum$munn <- tmp.cum$ife - tmp.cum$edon*1000 # adds munn based on ife, as in maps
#
tmp.data <- merge(x = tmp.data, y = tmp.cum, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.cum)
#plot(tmp)
vermun.map <- tmp # rename
#
i <- i+1
edo <- edos[i]
ruta <- paste(md, edo, "/", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = ruta, layer = 'MUNICIPIO')
#summary(tmp) # EXPLORE
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm()) # project to osm native Mercator
# adds quantity to plot (horizontal time series)
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$MUNICIPIO
#
tmp.cum <- cumul[-1,] # drops date tag in line 1 
tmp.cum <- tmp.cum[tmp.cum$edon==i,] # keeps one state 
tmp.cum$munn <- tmp.cum$ife - tmp.cum$edon*1000 # adds munn based on ife, as in maps
#
tmp.data <- merge(x = tmp.data, y = tmp.cum, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.cum)
#plot(tmp)
yucmun.map <- tmp # rename
#
i <- i+1
edo <- edos[i]
ruta <- paste(md, edo, "/", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = ruta, layer = 'MUNICIPIO')
#summary(tmp) # EXPLORE
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm()) # project to osm native Mercator
# adds quantity to plot (horizontal time series)
tmp.data <- tmp@data
tmp.data$orden <- 1:nrow(tmp.data)
tmp.data$munn <- tmp.data$MUNICIPIO
#
tmp.cum <- cumul[-1,] # drops date tag in line 1 
tmp.cum <- tmp.cum[tmp.cum$edon==i,] # keeps one state 
tmp.cum$munn <- tmp.cum$ife - tmp.cum$edon*1000 # adds munn based on ife, as in maps
#
tmp.data <- merge(x = tmp.data, y = tmp.cum, by = "munn", all.x = TRUE, all.y = FALSE)
tmp.data <- tmp.data[order(tmp.data$orden),]; tmp.data$orden <- NULL
tmp@data <- tmp.data
rm(tmp.data, tmp.cum)
#plot(tmp)
zacmun.map <- tmp # rename
#
# clean
rm(tmp)
#
# consolidate in list
mu.map <- list()
mu.map$ags <- agsmun.map
mu.map$bc <- bcmun.map
mu.map$bcs <- bcsmun.map
mu.map$cam <- cammun.map
mu.map$coa <- coamun.map
mu.map$col <- colmun.map
mu.map$cps <- cpsmun.map
mu.map$cua <- cuamun.map
mu.map$df <- dfmun.map
mu.map$dgo <- dgomun.map
mu.map$gua <- guamun.map
mu.map$gue <- guemun.map
mu.map$hgo <- hgomun.map
mu.map$jal <- jalmun.map
mu.map$mex <- mexmun.map
mu.map$mic <- micmun.map
mu.map$mor <- mormun.map
mu.map$nay <- naymun.map
mu.map$nl <- nlmun.map
mu.map$oax <- oaxmun.map
mu.map$pue <- puemun.map
mu.map$que <- quemun.map
mu.map$qui <- quimun.map
mu.map$san <- sanmun.map
mu.map$sin <- sinmun.map
mu.map$son <- sonmun.map
mu.map$tab <- tabmun.map
mu.map$tam <- tammun.map
mu.map$tla <- tlamun.map
mu.map$ver <- vermun.map
mu.map$yuc <- yucmun.map
mu.map$zac <- zacmun.map
#
rm(agsmun.map, bcmun.map, bcsmun.map, cammun.map, coamun.map, colmun.map, cpsmun.map, cuamun.map, dfmun.map, dgomun.map, guamun.map, guemun.map, hgomun.map, jalmun.map, mexmun.map, micmun.map, mormun.map, naymun.map, nlmun.map, oaxmun.map, puemun.map, quemun.map, quimun.map, sanmun.map, sinmun.map, sonmun.map, tabmun.map, tammun.map, tlamun.map, vermun.map, yucmun.map, zacmun.map)





# search localidad in municipio map (tried with localidad map and most names are municipios)
sos$munn <- NA
for (i in 1:length(sos$munn)){
    #i <- 100 # debug
    e <- which(edos==sos$edo[i])
    tmp <- as.character(mun.map[[e]]$NOMBRE)
    Encoding(tmp) <- "latin1"
    sel <- grep(sos$loc[i], tmp)
    if (length(sel)==1) sos$munn[i] <- mun.map[[e]]$MUNICIPIO[sel]
}
summary(sos$munn)

sel <- which(is.na(sos$munn))

sos[sel[100],]

plot(zacmun.map)

sos[1,]


