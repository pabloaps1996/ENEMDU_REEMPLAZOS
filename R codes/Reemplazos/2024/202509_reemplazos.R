rm(list = ls())

library(dplyr)
library(tidyverse)
library(readxl)
library(openxlsx)
library(foreign)
library(sampling)
library(rio)


reemplazos <- function(upm,numero,antes,mes,fecha,variable,año,mes_g){
        #abro la base del marco para detectar los conglomerados (con la sintaxis de rds)
        m <- last(list.files("./Marcos finales/UPM"))
        marco <- readRDS(paste0("./Marcos finales/UPM/",m))


        #se agrupa por domestrato para detectar el domestrato que hay que reemplazar
        tamano_reem <- marco %>%
                #primero se filtra los subconglomerados que son para el reemplazo
                filter(id_upm %in% upm) %>%
                #segundo se agrupa segun domestrato para conocer a cual hay que intervenir
                group_by(estrato) %>%
                summarise(n=n())

        #se abre el marco y se determina los disponibles para seleccionar
        marco_sel_reem <- marco %>%
                filter(seleccionable == "1") %>%
                filter(estrato %in% tamano_reem$estrato)




        #seleccion de upm
        #se genera un vector de indices en funcion de los domestrato
        indice <- tamano_reem$estrato
        i=1
        for (i in 1:length(indice)){
                #determinamos los conglomerados disponibles en el estrato

                disponible = marco_sel_reem %>%
                        filter(estrato==indice[i] & seleccionable=="1")


                #se determina el número de conglomerados a seleccionar en el estrato
                n = as.integer(tamano_reem[tamano_reem$estrato==indice[i],2])
                #se determina el número de conglomerados disponibles en el estrato
                N = dim(disponible)[1]
                #función srswor selecciona de forma aleatoria n en N
                disponible <- mutate(disponible, congl_selec = srswor(n, N))
                if(i==1){
                        z = disponible
                }else{
                        z = rbind(z,disponible)
                }
        }

        conglom_selec <- z %>%
                select(id_upm, congl_selec) %>%
                filter(congl_selec == 1)


        # conglom_selec$id_upm[1] <- "080560900102" ### DIFICIL ACCESO

        ## Creacion de la nueva carpeta en la cual se guaradará la informacion
        path <- paste0("./Output/4.Reemplazos/",año,"/",mes,"/")
        dir.create(file.path(paste0(dirname(path),"/",mes), numero))


        saveRDS(conglom_selec,
                paste0("./Output/4.Reemplazos/",año,"/",mes,"/",numero,"/conglomerados_seleccionados_2025.rds"))

        #abro panel viviendas

        # panel_viv = readRDS("F:/!ENEMDU/marco viviendas/marco_viviendas_20191211_V18.rds") %>%
        #         filter(seleccionable=="0")

        # quitar <- panel_viv$id_viv

        #abro marco de viviendas
        v <- last(list.files("./Marcos finales/Viviendas"))
        marco_viv <- readRDS(paste0("./Marcos finales/Viviendas/",v))

        #quitar las viviendas de la parroquia San Lucas
        marco_viv <- filter(marco_viv, parroq_cartografia!="110157")

        #se determina en el marco los conglomerados seleccionados
        marco_viv_enemdu_reempl = marco_viv %>%
                right_join(conglom_selec, by="id_upm")

        ############
        marco_viv_enemdu_reempl = marco_viv_enemdu_reempl
        ############

        #en el marco_viv_genero_reempl se filtra los id_vivienda que han sido utilizados
        #en el panel de viviendas
        # marco_viv_enemdu_reempl <- marco_viv_enemdu_reempl %>%
        #         filter(!(id_viv %in% quitar))

        # seleccion de 10 viviendas
        indice2 <- sort(unique(marco_viv_enemdu_reempl$id_upm))
        for (i in 1:length(indice2)){
                apoyo=filter(marco_viv_enemdu_reempl, id_upm==indice2[i])
                mues <- sample_n(apoyo,10,replace = F)
                if (i==1){
                        muestra.f <- mues
                } else {
                        muestra.f <- rbind(muestra.f,mues)
                }

        }

        print("programador promedio")
        #selección final de las viviendas
        saveRDS(muestra.f,
                paste0("./Output/4.Reemplazos/",año,"/",mes,"/",numero,"/muestra_viviendas_reemplazo_2023.rds"))

        viviendas <- muestra.f %>%
                mutate(hogar=1,
                       viv_rama_actividad=0,
                       test_agua=0,
                       test_agua_rem=0,
                       telefono1=0,
                       telefono2=0,
                       vivienda=NA,
                       panelviv=NA) %>%
                arrange(estrato) %>%
                mutate(s=rep(1:n_distinct(id_upm), each = 10))

        names(muestra.f)

        # Lectura del archivo de la rotacion de paneles

        rotacion_paneles = import("./bases/insumo reemplazos/rotacion_paneles_ENEMDU_2021_2024.xlsx")
        variable_e = rotacion_paneles %>%
                filter(variable_enemdu %in% variable)
        mes_panel = unique(variable_e$variable_enemdu)
        # Asignacion del panel

        paneles <- marco %>% filter(id_upm %in% upm)
        paneles <- paneles %>%
                select(id_upm,estrato,panel= all_of(mes_panel)) %>%
                arrange(estrato) %>%
                mutate(s=seq(1:dim(paneles)[1])) %>%
                select(s,panel)

        viviendas <- left_join(viviendas,paneles,by="s")



        # Formato
        viviendas=select(viviendas,id_viv,id_upm, provin, canton, parroq, zona, sector, manzana, num_edif, numviv,
                         calle, piso, jefehoga,numnum, numper,cartografia, panel_anual=panel,vivienda, panelviv,
                         telefono1,telefono2,parroq_cartografia, area)

        table(viviendas$panel)
        # creacion de los nombres de provincia, canton, parroquia
        nombres_provin<-read.table("./Insumos/Nombre_provincias.csv",
                                   sep=";",
                                   header=T,
                                   colClasses = c("character","character"),
                                   col.names = c("provin","nprovin"),
                                   encoding = "UTF-8")
        nombres_canton<-read.table("./Insumos/Nombre_cantones.csv",
                                   sep=";",
                                   header=T,
                                   colClasses = c("character","character"),
                                   col.names = c("id_canton","ncanton"),
                                   encoding = "UTF-8")
        nombres_parroq<-read.table("./Insumos/Nombre_parroquias_20180221.csv",
                                   sep=";",
                                   header=T,
                                   colClasses = c("character","character"),
                                   col.names = c("id_parroq","nparroq"),
                                   encoding = "UTF-8")

        viviendas <- mutate(viviendas,id_canton=paste0(provin,canton),
                            id_parroq=paste0(provin,canton,parroq))
        viviendas <- merge(viviendas,nombres_provin,
                           by="provin",
                           all.x=T)
        viviendas <- merge(viviendas,nombres_canton,
                           by="id_canton",
                           all.x=T)
        viviendas <- merge(viviendas,nombres_parroq,
                           by="id_parroq",
                           all.x=T)

        viviendas <- viviendas %>%
                mutate(nparroq=ifelse(id_parroq=="230200", "LA CONCORDIA",
                                      ifelse(id_parroq=="100300", "GARCÍA MORENO",
                                             ifelse(id_parroq=="130400", "EL CARMEN",
                                                    ifelse(id_parroq=="030300", "CAÑAR",
                                                           ifelse(id_parroq=="030400", "LA TRONCAL",
                                                                  ifelse(id_parroq=="090900", "EL TRIUNFO",nparroq)))))))

        names(viviendas)[names(viviendas) == 'id_upm'] <- 'id_conglomerado'


        #creacion variable periodo regional
        viviendas <- mutate(viviendas,periodo=1,
                            regional=ifelse(provin=="04" | provin=="08" | provin=="10" | provin=="17" | provin=="21","1",
                                            ifelse(provin=="02" | provin=="09" | provin=="12" | provin=="13" | provin=="20" | provin=="24" | provin=="23" | provin=="90","2",
                                                   ifelse(provin=="05" | provin=="06" | provin=="15" | provin=="16" | provin=="18" | provin=="22","3",
                                                          ifelse(provin=="01" | provin=="03" | provin=="07" | provin=="11" | provin=="14" | provin=="19","4","lol")))))

        #creación variable nregiona
        viviendas <- mutate(viviendas,nregiona=ifelse(regional=="1","PLANTA CENTRAL",
                                                      ifelse(regional=="2","LITORAL",
                                                             ifelse(regional=="3","CENTRO",
                                                                    ifelse(regional=="4","SUR",NA)))))

        #creación variable dominio
        viviendas <- viviendas %>%
                mutate(rnatura = ifelse(provin=="01" | provin=="02" | provin=="03" | provin=="04" |
                                                provin=="05" | provin=="06"| provin=="10" | provin=="11"|
                                                provin=="17" | provin=="18" | provin=="23", "1",
                                        ifelse(provin=="07" |provin=="08" | provin=="09" | provin=="12" |
                                                       provin=="13" | provin=="24", "2",
                                               ifelse(provin=="14" | provin=="15" | provin=="16" |
                                                              provin=="19" | provin=="21" | provin=="22", "3",
                                                      ifelse(provin=="20", "4", "d")))),
                       dominio = ifelse(id_parroq=="170150" & area==1,"01",
                                        ifelse(id_parroq=="090150" & area==1,"02",
                                               ifelse(id_parroq=="010150" & area==1,"03",
                                                      ifelse(id_parroq=="070150" & area==1,"04",
                                                             ifelse(id_parroq=="180150" & area==1,"05",
                                                                    ifelse(rnatura=="1" & !(id_parroq %in% c("010150", "170150", "180150")) & area==1,"06",
                                                                           ifelse(rnatura=="2" & !(id_parroq %in% c("070150", "090150")) & area==1,"07",
                                                                                  ifelse(rnatura=="3" & area==1,"08",
                                                                                         ifelse(rnatura=="1" & area==2,"09",
                                                                                                ifelse(rnatura=="2" & area==2,"10",
                                                                                                       ifelse(rnatura=="3" & area==2,"11",
                                                                                                              ifelse(rnatura=="4","12",NA)))))))))))))

        #creación variable ndominio
        viviendas <- mutate(viviendas,ndominio=ifelse(dominio=="01","Quito",
                                                      ifelse(dominio=="02","Guayaquil",
                                                             ifelse(dominio=="03","Cuenca",
                                                                    ifelse(dominio=="04","Machala",
                                                                           ifelse(dominio=="05","Ambato",
                                                                                  ifelse(dominio=="06","Resto Sierra Urbano",
                                                                                         ifelse(dominio=="07","Resto Costa Urbano",
                                                                                                ifelse(dominio=="08","Amazonia Urbano",
                                                                                                       ifelse(dominio=="09","Sierra Rural",
                                                                                                              ifelse(dominio=="10","Costa Rural",
                                                                                                                     ifelse(dominio=="11","Amazonia Rural",
                                                                                                                            ifelse(dominio=="12","Región Insular",NA)))))))))))))

        rm(nombres_canton,nombres_parroq,nombres_provin)

        # creación variable reemplazo orden
        ind1 <- unique(viviendas$id_conglomerado)
        for (i in 1:length(ind1)){
                print(i)
                aux <- filter(viviendas, id_conglomerado==ind1[i])
                aux$panelviv <- c(1:10)
                aux$vivienda <- c(1:10)
                if(i==1){
                        muestra <- aux
                } else {
                        muestra <- rbind(muestra,aux)
                }
        }


        viviendas <- muestra

        viviendas <- mutate(viviendas, panelviv=as.numeric(panelviv))

        # para verificar que panelviv es unica por conglomerado
        aggr <- summarise(group_by(viviendas, id_conglomerado, panelviv), n=n())
        aggr <- filter(aggr, !is.na(panelviv))
        t <- n_distinct(aggr$n)
        if(t==1){
                print("Panelviv es única")
        } else {
                stop("Panelviv no es única")
        }

        # Bucle con los conglomerados que tienen completos las 10 viviendas
        index <- unique(viviendas$id_conglomerado)
        for(i in 1:length(index)){
                loli <- filter(viviendas,id_conglomerado==index[i])
                loli <- arrange(loli,panelviv)
                unico <- unique(loli$panelviv)
                unico <- unico[!is.na(unico)]
                peer <- c(1:10)[!(1:10 %in% unico)]

                if(length(peer)==1){
                        s <- c(unico[order(unico)],peer)
                }else{
                        s <- c(unico[order(unico)],sample(peer,length(peer),F))
                }

                s1 <- s
                s1[s1<=7]<-0
                s1[s1==8]<-1
                s1[s1==9]<-2
                s1[s1==10]<-3

                loli <- cbind(loli,orden=s,reemplaz=s1)
                loli <- arrange(loli,orden)
                if(i==1){
                        z <- loli
                }
                else{
                        z <- rbind(z,loli)
                }
        }

        viviendas <- z
        rm(i,index,peer,s,s1,unico,z,loli)

        # Incorporar el digito de la rotación de viviendas
        viviendas = viviendas %>%
                # select(-panel_anual) %>%
                left_join(select(
                        filter(
                                rotacion_paneles, variable_enemdu == variable
                        ), panel_upm, codigo_car
                ), by = c("panel_anual"="panel_upm")) %>%
                mutate(panel_anual = codigo_car) %>%
                select(-codigo_car)


        # renombramos la variable panel (octubre_19_i) a panel_x (asi se llama en el sistema)
        names(viviendas)[names(viviendas) == 'panel_anual'] <- 'panel_x'

        # creación variable vivienda panelviv

        viviendas <- viviendas %>%
                left_join(select(
                        filter(
                                rotacion_paneles, variable_enemdu == variable
                        ), codigo_car, codigo_num
                ), by = c("panel_x"="codigo_car")) %>%
                rename(panel = codigo_num)

        viviendas <- mutate(viviendas, vivienda = str_pad(orden,2,"left",pad="0"))
        viviendas <- mutate(viviendas, vivienda1 = ifelse(vivienda=="08","R1",
                                                          ifelse(vivienda=="09","R2",
                                                                 ifelse(vivienda=="10","R3",vivienda))))

        viviendas <- mutate(viviendas,panelviv=paste0(panel_x,vivienda1))

        # creación variable necesarias para el sistema
        viviendas <- mutate(viviendas,hogar=1,
                            viv_rama_actividad=0,
                            test_agua=0,
                            test_agua_rem=0,
                            telefono1=0,
                            telefono2=0)

        #creación variable ncod. cart.
        viviendas <- mutate(viviendas,cod_cart=ifelse(cartografia=='CPVCENEC',1,
                                                      ifelse(cartografia=='ACTENEMDU',2,
                                                             ifelse(cartografia=='ECV20132014',3,
                                                                    ifelse(cartografia=='PROYEC2015',4,
                                                                           ifelse(cartografia=='CENSOGAL15',5,
                                                                                  ifelse(cartografia=='ACTUAL2017',6,NA)))))))


        # correccion la concordia.- solo regional y neregiona               OJO!!!!!!!
        viviendas <- mutate(viviendas,
                            regional=ifelse(provin=="08" & canton=="08","2",regional),
                            nregiona=ifelse(provin=="08" & canton=="08","LITORAL",nregiona))


        # validaciones finales sobre la base
        p <- sum(as.vector(apply(is.na(viviendas),2,sum)))
        if(p==0){
                print("No existen valores perdidos")
        } else {
                stop("Existen valores perdidos")
        }

        viviendas <- as.data.frame(sapply(viviendas,function(x){gsub('"','',x)}))
        viviendas <- as.data.frame(sapply(viviendas,function(x){gsub(';','',x)}))

        viviendas.muestra  <- viviendas %>%
                select(id_conglomerado,provin,canton,parroq,zona,sector,area,nprovin,ncanton,nparroq,periodo,
                       regional,nregiona,dominio,ndominio,orden,panelviv,manzana,num_edif,vivienda,numper,calle,cod_cart,
                       piso,numnum,jefehoga,panel,panel_x,numviv,reemplaz,hogar,viv_rama_actividad,cartografia,
                       test_agua,telefono1,telefono2,parroq_cartografia)

        rm(viviendas)


        cong <- summarise(group_by(viviendas.muestra, id_conglomerado), n=n())
        u <- mean(unique(cong$n))
        if(u==10){
                print("Todos las UPM tienen 10 viviendas")
        } else {
                stop("Alguna UPM no tiene 10 viviendas")
        }

        viviendas.muestra <- viviendas.muestra %>%
                mutate(calle=str_trim(calle, side = "both"),
                       jefehoga=str_trim(jefehoga, side = "both"),
                       jefehoga = ifelse(substr(jefehoga, 1, 1)=="", "NN", jefehoga))

        viviendas.muestra <- viviendas.muestra %>%
                mutate(jefehoga=gsub("<d1>","Ñ",jefehoga),
                       jefehoga=gsub("<c1>","Á",jefehoga),
                       jefehoga=gsub("<c0>","Á",jefehoga), # À
                       jefehoga=gsub("<c9>","É",jefehoga),
                       jefehoga=gsub("<c8>","É",jefehoga), # È
                       jefehoga=gsub("<cd>","Í",jefehoga),
                       jefehoga=gsub("<cc>","Í",jefehoga), # Ì
                       jefehoga=gsub("<d3>","Ó",jefehoga),
                       jefehoga=gsub("<d2>","Ó",jefehoga), # Ò
                       jefehoga=gsub("<da>","Ú",jefehoga),
                       jefehoga=gsub("<d9>","Ú",jefehoga), # Ù
                       jefehoga=gsub("<dc>","Ü",jefehoga), # Ü
                       jefehoga=gsub("<c7>","",jefehoga), # c de Barcsa
                       jefehoga=gsub("<U\\+00BA>","",jefehoga), # °
                       jefehoga=gsub("<U\\+00B0>","",jefehoga), # °
                       jefehoga=gsub("<U\\+00B4>","",jefehoga), # ,
                       jefehoga=gsub("<U\\+00B7>","",jefehoga), # middle dot
                       jefehoga=gsub("<U\\+00A1>","",jefehoga), # ¡
                       jefehoga=gsub("<U\\+00A8>","",jefehoga), # ¨
                       jefehoga=gsub("<U\\+00AA>","",jefehoga), # a chiquita
                       jefehoga=gsub("<U\\+0096>","",jefehoga), # a chiquita
                       jefehoga=gsub('"',"",jefehoga),
                       jefehoga=gsub("'","",jefehoga),
                       jefehoga=gsub(";","",jefehoga),
                       jefehoga=gsub(",","",jefehoga),
                       jefehoga=gsub("<","",jefehoga),
                       calle=gsub("<d1>","Ñ",calle),
                       calle=gsub("<c1>","Á",calle),
                       calle=gsub("<c0>","Á",calle), # À
                       calle=gsub("<c9>","É",calle),
                       calle=gsub("<c8>","É",calle), # È
                       calle=gsub("<cd>","Í",calle),
                       calle=gsub("<cc>","Í",calle), # Ì
                       calle=gsub("<d3>","Ó",calle),
                       calle=gsub("<d2>","Ó",calle), # Ò
                       calle=gsub("<da>","Ú",calle),
                       calle=gsub("<d9>","Ú",calle), # Ù
                       calle=gsub("<dc>","Ü",calle), # Ü
                       calle=gsub("<c7>","",calle), # c de Barcsa
                       calle=gsub("<U\\+00BA>","",calle), # °
                       calle=gsub("<U\\+00B0>","",calle), # °
                       calle=gsub("<U\\+00B4>","",calle), # ,
                       calle=gsub("<U\\+00B7>","",calle), # middle dot
                       calle=gsub("<U\\+00A1>","",calle), # ¡
                       calle=gsub("<U\\+00A8>","",calle), # ¨
                       calle=gsub("<U\\+00AA>","",calle), # a chiquita
                       calle=gsub("<U\\+0096>","",calle), # a chiquita
                       calle=gsub('"',"",calle),
                       calle=gsub("'","",calle),
                       calle=gsub(";","",calle),
                       calle=gsub(",","",calle),
                       calle=gsub("<","",calle))


        # base final
        saveRDS(viviendas.muestra,
                paste0("./Output/4.Reemplazos/",año,"/",mes,"/",numero,"/reemplazos_2023.rds"))

        # Apertura de la base anterior
        base.antes <- import(paste0("./Output/4.Reemplazos/",año,"/",mes,"/",antes,"/muestra_",mes_g,".rds"))



        # eliminacion
        base.antes <- filter(base.antes, !id_conglomerado %in% upm)

        # Apertura de los reemplazos
        base.reemp <- readRDS(paste0("./Output/4.Reemplazos/",año,"/",mes,"/",numero,"/reemplazos_2023.rds"))

        pluscode <- import("D:\\resp_ppenafiel\\respaldos2\\Nuev\\ENEMDU_REEMPLAZOS\\Insumos\\PLUSCODES_BD_CARTOGRAFICAS.csv") |>
                select(union, pluscode = pluscodes, link = ruteo) |>
                filter(!duplicated(union))

        base.reemp<- base.reemp |>
                mutate( manzana = ifelse(id_conglomerado == "040155000102" , "0004", manzana),
                        id_man = paste0(provin, canton, parroq, zona, sector, manzana),
                        union = paste0(id_man, cartografia))


        aux0 <- left_join(base.reemp, pluscode , by = "union")

        aux1 <- left_join(base.reemp, pluscode , by = "union") |>
                filter(is.na(pluscode))


        aux2 <- aux1 |>
                mutate(union = ifelse(is.na(pluscode) & cartografia == "ACTENEMDU" , paste0(id_man, "CPVCENEC") , union ))


        aux3 <- aux0 |> filter(!union %in% aux1$union) |>
                rbind(aux2) |>  select(-pluscode, -link)


        base.reemp <- left_join(aux3, pluscode , by = "union")|>
                mutate(pluscode = ifelse(is.na(pluscode), 0,pluscode ),
                       link = ifelse(is.na(link), 0,link )) |>  select(-union, -id_man)

        z <- names(base.antes)
        z1 <- names(base.reemp)
        if(identical(z,z1)==T){
                print("Los nombres de las variables son las mismas")
        } else {
                stop("Los nombres de las variables son las mismas")
        }

        base <- rbind(base.antes, base.reemp)
        base <- arrange(base, id_conglomerado)

        print(table(base$panel_x, useNA = "ifany"))
        print(table(base$panel, useNA = "ifany"))
        print(table(base$panelviv, useNA = "ifany"))

        # Guardar los archivos de la muestra
        write.xlsx(base,
                   paste0("./Output/4.Reemplazos/",año,"/",mes,"/",numero,"/muestra_",mes_g,".xlsx"))

        saveRDS(base, file =paste0("./Output/4.Reemplazos/",año,"/",mes,"/",numero,"/muestra_",mes_g,".rds"))


        export(base, paste0("./Output/4.Reemplazos/",año,"/",mes,"/",numero,"/muestra_",mes_g,".sav"))

        # Guardar el archivo para DIRAD
        write.xlsx(base.reemp,
                   paste0("./Output/4.Reemplazos/",año,"/",mes,"/",numero,"/conglomerados_DIRAD.xlsx"))
}



upm <- c("220351900401")
numero <- c("Reem1")
antes <- c("Reem0")
mes <- c("10. Octubre")
fecha <- c("20251003")
variable = c("enemdu_25_10_f")
año = c("5. 2025")
mes_g = c("octubre")

reemp <- reemplazos(upm,numero,antes,mes,fecha,variable,año,mes_g)



