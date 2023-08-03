#### hot deck ####
impHD_secuencial<-function(base, variablesAImputar=NULL, dominio=c( ),
                           limites=NULL, ordenadoras=c()){
  
  if(is.null(variablesAImputar)){
    stop("Debe introducir una base, un dominio, variables ordenadoras y una variable a imputar para que la función se ejecute.")
  }
  library(VIM)
  
  final<-length(dominio)
  
  baseI<-hotdeck(
    base,
    variable = variablesAImputar,
    ord_var = ordenadoras,##Area,aglomerado##secuencial o aleatorio
    domain_var = dominio,
    donorcond = limites,##controlar
    imp_var = TRUE,
    imp_suffix = "impHD"
  )
  
  sum(is.na(baseI[variablesAImputar[1]]))->control
  
  if(control>0 & length(dominio)>2){
    
    print(paste0("Hay ", control, " nulos no imputados"))
    n=1
    for(n in 1:length(dominio)){
      k<-length(dominio)
      dominio<-dominio[1:(length(dominio)-1)]
      
      baseI<-hotdeck(
        base,
        variable = variablesAImputar,
        ord_var = ordenadoras,
        domain_var = dominio,
        makeNA = NULL,
        NAcond =  NULL,
        impNA = TRUE,
        donorcond = limites,##controlar
        imp_var = TRUE,
        imp_suffix = "impHD"
      )
      print(paste0("prueba HD ", n))
      n=n+1
      sum(is.na(baseI[variablesAImputar[1]]))->control
      print(control)
      if(control==0){
        return(baseI)
        break
      }else if(n==final){
        warning("HAY CASOS SIN IMPUTAR")
        return(baseI)
        break
      }
    }
  }else if(control==0){
    print("No hubo nulos")
    return(baseI)
    
  }else if(control>0 & length(dominio)<3){
    print("Se achico demasiado el dominio")
    return(baseI)
  }
  
  
  
}
####laburo ramas ####
corregir.rama <- function(base){

  columnas<-names(base)

  if(!('PP04B_COD' %in% columnas)){
    stop("La tabla debe tener la columna PP04B_COD")
  }

  base$PP04B_COD <- as.character(base$PP04B_COD)

  base$PP04B_COD <- case_when(nchar(base$PP04B_COD) == 1 ~ paste0("0", base$PP04B_COD),

                              nchar(base$PP04B_COD) == 2 ~ base$PP04B_COD,

                              nchar(base$PP04B_COD) == 3 ~ paste0("0", base$PP04B_COD),

                              nchar(base$PP04B_COD) == 4 ~ base$PP04B_COD)

  return(base)

}
rama.caes1.0 <- function(base){

  base$rama.caes <- case_when(

    ## A --> Agricultura, caza, silvicultura y pesca.

    (base$PP04B_COD %in% c("01", "0101", "0102", "0103", "0104", "0105",

                           "02", "0200", "03", "0300")) ~ 1,

    ## B --> Explotación de minas y canteras.

    (base$PP04B_COD %in% c("05", "0500", "500", "06", "0600", "07", "0700", "08",

                           "0800", "09", "0900")) ~ 2,

    ## C --> Industria Manufacturera.

    (base$PP04B_COD %in% c("10", "1001", "1002", "1003", "1009", "11", "1100", "12",

                           "1200", "13", "1300", "14", "1400", "15", "1501", "1502",

                           "16", "1600", "17", "1700", "18", "1800", "19", "1901",

                           "1902", "20", "2001", "2002", "2009", "21", "2100", "22",

                           "2201", "2202", "23", "2301", "2309", "24", "2400", "25",

                           "2500", "26", "2601", "2602", "2603", "2604", "27", "2701",

                           "2709", "28", "2800", "29", "2900", "30", "3001", "3002",

                           "3003", "3009", "31", "3100", "32", "3200", "33", "3300")) ~ 3,

    ## D --> Suministro de electricidad, gas, vapor y aire acondicionado.

    (base$PP04B_COD %in% c("35", "3501", "3502")) ~ 4,

    ## E --> Suministro de agua; alcantarillado, gestión de desechos y actividades de saneamiento.

    (base$PP04B_COD %in% c("36", "3600", "37", "3700", "38", "3800", "39", "3900")) ~ 5,

    ## F --> Construcción.

    (base$PP04B_COD %in% c("40", "4000")) ~ 6,

    ## G --> Comercio al por mayor y al por menor; reparaciòn de vehículos automotores y motocicletas.

    (base$PP04B_COD %in% c("45", "4501", "4502", "4503", "4504", "48", "4801", "4802",

                           "4803", "4804", "4805", "4806", "4807", "4808",

                           "4809", "4810", "4811")) ~ 7,

    ## H --> Transporte y almacenamiento.

    (base$PP04B_COD %in% c("49", "4901", "4902", "4903", "4904", "4905", "4909", "50",

                           "5000", "51", "5100", "52", "5201", "5202", "53", "5300")) ~ 8,

    ## I --> Alojamiento y servicios de comidas.

    (base$PP04B_COD %in% c("55", "5500", "56", "5601", "5602")) ~ 9,

    ## J --> Información y comunicación.

    (base$PP04B_COD %in% c("58", "5800", "59", "5900", "60", "6000", "61", "6100",

                           "62", "6200", "63", "6300")) ~ 10,

    ## K --> Actividades financieras y de seguros.

    (base$PP04B_COD %in% c("64", "6400", "65", "6500", "66", "6600")) ~ 11,

    ## L --> Actividades inmobiliarias.

    (base$PP04B_COD %in% c("68", "6800")) ~ 12,

    ## M --> Actividades profesionales, científicas y ténicas.

    (base$PP04B_COD %in% c("69", "6900", "70", "7000", "71", "7100", "72", "7200", "73",

                           "7301", "7302", "74", "7400", "75", "7500")) ~ 13,

    ## N --> Actividades administrativas y servicios de apoyo.

    (base$PP04B_COD %in% c("77", "7701", "7702", "78", "7800", "79", "7900", "80", "8000",

                           "81", "8101", "8102", "82", "8200")) ~ 14,

    ## O --> Administración pública y defensa; planes de seguro social obligatorio.

    (base$PP04B_COD %in% c("83", "8300","84", "8401", "8402", "8403")) ~ 15,

    ## P --> Enseñanza.

    (base$PP04B_COD %in% c("85", "8501", "8509")) ~ 16,

    ## Q --> Salud humana y servicios sociales.

    (base$PP04B_COD %in% c("86", "8600", "87", "8700", "88", "8800")) ~ 17,

    ## R --> Artes, entretenimiento y recreación.

    (base$PP04B_COD %in% c("90", "9000", "91", "9100", "92", "9200", "93", "9301", "9302")) ~ 18,

    ## S --> Otras actividades de servicios.

    (base$PP04B_COD %in% c("94", "9401", "9402", "9409", "95", "9501", "9502", "9503",

                           "96", "9601", "9602", "9603", "9606", "9609")) ~ 19,

    ## T --> Actividades de los hogares como empleadores de personal doméstico; actividades de los hogares como productores de bienes o servicios para uso propio.

    (base$PP04B_COD %in% c("97", "9700", "98", "9800")) ~ 20,

    ## U --> Actividades de organizaciones y organismos extraterritoriales.

    (base$PP04B_COD %in% c("99", "9900")) ~ 21,

    ## V-Z --> Descripción de Actividad Vacía / Actividad no Especificada claramente.

    base$PP04B_COD == "9999" ~ 22)

  return(base)

}
rama.caes1.0.pub <- function(base){

  base$rama.eph <- case_when(

    base$rama.caes == 1  | base$rama.caes == 2 ~ 1,                     # Actividades primarias

    base$rama.caes == 3  ~ 2,                                     # Industria manufacturera

    base$rama.caes == 6  ~ 3,                                     # Construcción

    base$rama.caes == 7  ~ 4,                                     # Comercio

    base$rama.caes == 9  ~ 5,                                     # Hoteles y restaurantes

    base$rama.caes == 8  | base$rama.caes == 10 ~ 6,                    # Transporte, almacenamiento y comunicaciones

    base$rama.caes >= 11 & base$rama.caes <= 14 ~ 7,                    # Servicios financieros, de alquiler y empresariales

    base$rama.caes == 15 ~ 8,                                     # Administración pública, defensa y seguridad social

    base$rama.caes == 16 ~ 9,                                     # Enseñanza

    base$rama.caes == 17 ~ 10,                                    # Servicios sociales y de salud

    base$rama.caes == 20 ~ 11,                                    # Servicio doméstico

    base$rama.caes == 18 | base$rama.caes == 19 ~ 12,                   # Otros servicios comunitarios, sociales y personales

    base$rama.caes == 4  | base$rama.caes == 5  | base$rama.caes == 21 ~ 13,  # Otras ramas

    base$rama.caes == 22  ~ 14)                                   # Actividades no bien especificadas

  base$rama.caes<-as.factor(base$rama.caes)
  return(base)
}
rama.eph.nombre <- function(base){

  base$rama.nombre <- case_when(

    base$rama.eph == 1  ~ "Actividades primarias",

    base$rama.eph == 2  ~ "Industria manufacturera",

    base$rama.eph == 3  ~ "Construccion",

    base$rama.eph == 4  ~ "Comercio",

    base$rama.eph == 5  ~ "Hoteles y restaurantes",

    base$rama.eph == 6  ~ "Transporte, almacenamiento y comunicaciones",

    base$rama.eph == 7  ~ "Servicios financieros, de alquiler y empresariales",

    base$rama.eph == 8  ~ "Administracion publica, defensa y seguridad social",

    base$rama.eph == 9  ~ "Ensenanza",

    base$rama.eph == 10 ~ "Servicios sociales y de salud",

    base$rama.eph == 11 ~ "Servicio domestico",

    base$rama.eph == 12 ~ "Otros servicios comunitarios, sociales y personales",

    base$rama.eph == 13 ~ "Otras ramas",

    base$rama.eph == 14 ~ "Actividades no bien especificadas")



  base$rama.nombre <- factor(base$rama.nombre,

                             levels = c("Actividades primarias",

                                        "Industria manufacturera",

                                        "Construccion",

                                        "Comercio",

                                        "Hoteles y restaurantes",

                                        "Transporte, almacenamiento y comunicaciones",

                                        "Servicios financieros, de alquiler y empresariales",

                                        "Administracion publica, defensa y seguridad social",

                                        "Ensenanza",

                                        "Servicios sociales y de salud",

                                        "Servicio domestico",

                                        "Otros servicios comunitarios, sociales y personales",

                                        "Otras ramas",

                                        "Actividades no bien especificadas"))

  return(base)

}
entra.cuchillo.salen.las.ramas <- function(base){



  base <- corregir.rama(base)

  base <- rama.caes1.0(base)

  base <- rama.caes1.0.pub(base)

  base <- rama.eph.nombre(base)

  return(base)


}
#### otras ####
auxiliares<-function(base){


  base$d5<-as.numeric(substr(base$PP04D_COD,nchar(base$PP04D_COD),nchar(base$PP04D_COD)))

  base$CALIFICACION<-as.numeric(substr(base$PP04D_COD,nchar(base$PP04D_COD),nchar(base$PP04D_COD)))
  base$JERARQUIA<-as.numeric(substr(base$PP04D_COD,nchar(base$PP04D_COD)-2,nchar(base$PP04D_COD)-2))
  # base$JERARQUIA<-ifelse(is.na(base$JERARQUIA), 0, base$JERARQUIA)

  base$CALIFICACION<-as.factor(base$CALIFICACION)
  base$JERARQUIA<-as.factor(base$JERARQUIA)

  base$PP3E_TOT<-ifelse(base$PP3E_TOT>100,NA,base$PP3E_TOT)


  base %>%  mutate(rol = case_when(
    ESTADO==1 & CAT_OCUP== 1 ~ "Patron",
    ESTADO==1 & CAT_OCUP==2 & (NIVEL_ED==6 | NIVEL_ED==5) ~ "CP_calif",
    ESTADO==1 & CAT_OCUP==2 & NIVEL_ED!=6 & NIVEL_ED!=5 ~ "CP_no_calif",
    ESTADO==1 & CAT_OCUP== 3 & (JERARQUIA==2 | JERARQUIA==0) ~  "Direcci?n",
    ESTADO==1 & CAT_OCUP== 3 & JERARQUIA==3 & CALIFICACION==1 ~  "Profesional",
    ESTADO==1 & CAT_OCUP== 3 & JERARQUIA==3 & CALIFICACION==2 ~  "tecnicos",
    ESTADO==1 & CAT_OCUP== 3 & JERARQUIA==3 & CALIFICACION==3 ~  "operativos",
    ESTADO==1 & CAT_OCUP== 3 & JERARQUIA==3 & CALIFICACION==4 ~  "Obrero NC",
    TRUE ~ "Otro"
  ))->base



  base %>%  mutate(INF = case_when(
    PP04C<6 & d5>1 & d5<6 & CAT_OCUP== 1  ~  "Patron I",
    PP04C>5 & CAT_OCUP== 1  ~  "Patr?n F",
    CAT_OCUP== 1  ~  "otros p",
    PP04C<6 & d5>1 & d5<6 & CAT_OCUP== 2  ~  "CP INF",
    PP04C>5 & (d5<1 | d5>=6) & CAT_OCUP== 2  ~  "CP formal",
    CAT_OCUP== 2  ~  "CP otros",
    CAT_OCUP== 3 & PP07H==2 &  PP04C>5 & PP04B1==2  ~  "inf_for",
    CAT_OCUP== 3 & PP07H==2 &  PP04C<6 & PP04B1==2 ~ "inf_inf",
    CAT_OCUP== 3 & PP07H==2 & PP04B1==1 ~ "inf_hog",
    CAT_OCUP== 3 & PP07H==1 & PP04B1==1 ~ "form_hog",
    CAT_OCUP== 3 & PP07H==1 &  PP04C>5 & PP04B1==2   ~ "form_form",
    CAT_OCUP== 3 & PP07H==1 &  PP04C<6 & PP04B1==2   ~ "form_inf",
    CAT_OCUP==3 ~  "otros a"))->base

  base$CH03<-as.factor(base$CH03)
  base$CAT_OCUP<-as.factor(base$CAT_OCUP)
  base$NIVEL_ED<-as.factor(base$NIVEL_ED)
  base$CH04<-as.factor(base$CH04)
  base$AGLOMERADO<-as.factor(base$AGLOMERADO)
  base$REGION<-as.factor(base$REGION)
  base$INF<-as.factor(base$INF)
  base$rol<-as.factor(base$rol)

  base$PP05C_1<-as.factor(base$PP05C_1)
  base$PP05C_2<-as.factor(base$PP05C_2)
  base$PP05C_3<-as.factor(base$PP05C_3)
  

  # base$CH06<-(base$CH06-min(base$CH06))/(max(base$CH06)- min(base$CH06))

  base$TECNO<-substr(base$PP04D_COD,4,4)
  base$car1<-substr(base$PP04D_COD,1,1)
  base$car2<-substr(base$PP04D_COD,1,2)

  base$TECNO<-as.factor(base$TECNO)

  base$car1<-as.factor(base$car1)
  base$car2<-as.factor(base$car2)

  base<-entra.cuchillo.salen.las.ramas(base)

  base$rama.eph<-as.factor(base$rama.eph)
  base$CH03<-as.factor(base$CH03)
  base$CH08<-as.factor(base$CH08)
  base$CH15<-as.factor(base$CH15)

  base$CH16<-as.factor(base$CH16)


  base$PP07A<-as.factor(base$PP07A)
  base$PP07E<-as.factor(base$PP07E)
  base$PP07H<-as.factor(base$PP07H)

  base$PP04C<-as.factor(base$PP04C)

  return(base)

}

ponerNulos<-function(base){

  columnas<-names(base)

  if(!('P21' %in% columnas)){

    stop("Se requiere la variable P21 para poder utilizar dicha funcion")


  }

  base<-base %>% filter(ESTADO==1)

  base$CONTESTA<-ifelse(base$P21>0,1,0)
  #saquè ch06
  modelo.logit<-glm(CONTESTA ~ REGION +CH06 +CH04 + CH03 +NIVEL_ED, data = base, family = "binomial")

  base$probabilidad<-predict(modelo.logit, base, type="response")

  # tasa<-sum(base$P21==-9)/NROW(base)
  baseFiltrada<-base %>% filter(P21>0)
  baseFiltrada$P21_i<-baseFiltrada$P21

  baseFiltrada$limite<-runif(NROW(baseFiltrada),0,1)

  baseFiltrada$P21_i<-ifelse(baseFiltrada$probabilidad<baseFiltrada$limite,NA,baseFiltrada$P21_i)

  baseFiltrada$imp<-ifelse(is.na(baseFiltrada$P21_i),1,0)

  return(baseFiltrada)
}
