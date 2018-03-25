preparacion de datos
================

Serie 2001 a 2016
-----------------

Los datos tienen la siguiente pinta

``` r
library(pxR)
```

    ## Loading required package: stringr

    ## Loading required package: reshape2

    ## Loading required package: RJSONIO

    ## Loading required package: plyr

``` r
df<-as.data.frame(read.px('./raw-data/01001.px'))
```

    ## Warning in grepl("CHARSET.*ANSI", charset, ignore.case = T): string de
    ## entrada 4 es inválida en este locale

``` r
df<-unique(df[df$Ramas.de.actividad=='PRODUCTO INTERIOR BRUTO A PRECIOS DE MERCADO',])
names(df)<-c('period', 'kind','region','value')
series_2001_2010<-dcast(df,region~period, value.var = 'value')

head(series_2001_2010,3)
```

    ##                    region 2016 (1ª E)   2015(A)   2014(P)      2013
    ## 1               Andalucía   148467617 143846139 138630113 137376858
    ## 2                  Aragón    34686536  33688924  32737472  32583929
    ## 3 Asturias, Principado de    21696244  21278658  20560557  20668712
    ##        2012      2011      2010      2009      2008      2007      2006
    ## 1 139710392 144651807 146124642 146315391 152137231 148644794 139066918
    ## 2  32534897  33917092  34406424  34137634  35615255  34228275  31407151
    ## 3  21419093  22464790  22868674  22723922  23989386  23239346  21700331
    ##        2005      2004      2003      2002     2001     2000
    ## 1 128986608 118724376 109789588 100887974 93492514 86331553
    ## 2  28907051  26800618  25087830  23410533 21557351 20042880
    ## 3  19955477  18386025  17269896  16315232 15389277 14266600

Serie 1995-2004
---------------

Solo tomamos años anterioes al 2000

``` r
df<-as.data.frame(read.px('./raw-data/re001.px'))
names(df)<-c('period', 'region', 'value')
series_1995_2000<-dcast(df,region~period, value.var = 'value')[,-c(2,3,4,5,6)]
head(series_1995_2000,3)
```

    ##                region     1999     1998     1997     1996     1995
    ## 1           ANDALUCIA 75241844 70358958 66501528 62389878 58703745
    ## 2              ARAGON 17636274 16824619 16119777 15181169 14302189
    ## 3 ASTURIAS (Ppdo. de) 12714496 12394228 11457960 11081648 10583097

Serie 1980-1996
---------------

Solo tomamos años anterioes a 1995

Este dataset incluye provincias, asi que tenemos que elegir "manualmente" las CCAA

Esta serie utiliza millones de pesetas, como unidad, que pasamos a miles de euros por homegenidad con las anteriores

``` r
df<-as.data.frame(read.px('./raw-data/re002.px'))
names(df)<-c('period', 'region', 'value')
series_1980_1996<-dcast(df,region~period, value.var = 'value')[,-c(2,3)][c(1,10,14,15,16,19,20,30,36,41,45,48,53,54,55,56,60),]
series_1980_1996[,-1]<-round(series_1980_1996[,-1]/0.166386)
head(series_1980_1996,3)
```

    ##                 region 1994(Provisional)     1993     1992     1991
    ## 1            Andalucía          52275101 49018313 48036986 45139988
    ## 10              Aragón          13207241 12473243 12140090 11391722
    ## 14 Asturias (Ppdo. de)           9791839  9257516  9085476  8349963
    ##        1990     1989     1988     1987     1986     1985     1984     1983
    ## 1  41132511 35921105 32427302 29334379 25803096 22631658 19869941 17656534
    ## 10 10396169  9488449  8478388  7405689  6670128  5866257  5413929  4723847
    ## 14  7727543  7170327  6432729  5867164  5537707  4808704  4197775  3762799
    ##        1982     1981     1980
    ## 1  15353347 13048063 11657633
    ## 10  3999273  3416904  3115965
    ## 14  3384798  2886367  2542389

Uniendo las series
------------------

Combinamos las 17 comunidades y etiquetamos adecuadamente los periodos para los que solo hay informacion estimada

Exportamos los datos a una tabla feather

``` r
table<-cbind(series_2001_2010[1:17,], series_1995_2000[1:17,-1], series_1980_1996[1:17,-1])
table.m<-melt(table)
```

    ## Using region as id variables

``` r
names(table.m)[2:3]<-c("period","gdp_current_price")
table.m$year<-as.numeric(gsub(" *\\(.*\\)","",as.character(table.m$period)))
table.m$kind<-"real"
table.m$kind[grep(" *\\(.*\\)",as.character(table.m$period))]<-"estimated"
library(feather)
write_feather(table.m,'gdp_per_region_1980_to_2016.feather')
table.m
```

    ##                          region            period gdp_current_price year
    ## 1                     Andalucía       2016 (1ª E)         148467617 2016
    ## 2                        Aragón       2016 (1ª E)          34686536 2016
    ## 3       Asturias, Principado de       2016 (1ª E)          21696244 2016
    ## 4                Balears, Illes       2016 (1ª E)          28460988 2016
    ## 5                      Canarias       2016 (1ª E)          42606745 2016
    ## 6                     Cantabria       2016 (1ª E)          12538918 2016
    ## 7               Castilla y León       2016 (1ª E)          55391770 2016
    ## 8          Castilla - La Mancha       2016 (1ª E)          38014904 2016
    ## 9                      Cataluña       2016 (1ª E)         211915475 2016
    ## 10         Comunitat Valenciana       2016 (1ª E)         105077178 2016
    ## 11                  Extremadura       2016 (1ª E)          17712047 2016
    ## 12                      Galicia       2016 (1ª E)          57967479 2016
    ## 13         Madrid, Comunidad de       2016 (1ª E)         210812904 2016
    ## 14            Murcia, Región de       2016 (1ª E)          28526935 2016
    ## 15  Navarra, Comunidad Foral de       2016 (1ª E)          19017603 2016
    ## 16                   País Vasco       2016 (1ª E)          68897003 2016
    ## 17                    Rioja, La       2016 (1ª E)           8032459 2016
    ## 18                    Andalucía           2015(A)         143846139 2015
    ## 19                       Aragón           2015(A)          33688924 2015
    ## 20      Asturias, Principado de           2015(A)          21278658 2015
    ## 21               Balears, Illes           2015(A)          27228681 2015
    ## 22                     Canarias           2015(A)          40880389 2015
    ## 23                    Cantabria           2015(A)          12225991 2015
    ## 24              Castilla y León           2015(A)          53563062 2015
    ## 25         Castilla - La Mancha           2015(A)          36958335 2015
    ## 26                     Cataluña           2015(A)         204189146 2015
    ## 27         Comunitat Valenciana           2015(A)         101369085 2015
    ## 28                  Extremadura           2015(A)          17273391 2015
    ## 29                      Galicia           2015(A)          55820654 2015
    ## 30         Madrid, Comunidad de           2015(A)         202965056 2015
    ## 31            Murcia, Región de           2015(A)          27528246 2015
    ## 32  Navarra, Comunidad Foral de           2015(A)          18484789 2015
    ## 33                   País Vasco           2015(A)          66553284 2015
    ## 34                    Rioja, La           2015(A)           7889806 2015
    ## 35                    Andalucía           2014(P)         138630113 2014
    ## 36                       Aragón           2014(P)          32737472 2014
    ## 37      Asturias, Principado de           2014(P)          20560557 2014
    ## 38               Balears, Illes           2014(P)          26262492 2014
    ## 39                     Canarias           2014(P)          39738229 2014
    ## 40                    Cantabria           2014(P)          11936599 2014
    ## 41              Castilla y León           2014(P)          51885273 2014
    ## 42         Castilla - La Mancha           2014(P)          35698046 2014
    ## 43                     Cataluña           2014(P)         196714713 2014
    ## 44         Comunitat Valenciana           2014(P)          97245778 2014
    ## 45                  Extremadura           2014(P)          16651921 2014
    ## 46                      Galicia           2014(P)          53864664 2014
    ## 47         Madrid, Comunidad de           2014(P)         195367513 2014
    ## 48            Murcia, Región de           2014(P)          26559415 2014
    ## 49  Navarra, Comunidad Foral de           2014(P)          17833177 2014
    ## 50                   País Vasco           2014(P)          63907507 2014
    ## 51                    Rioja, La           2014(P)           7635481 2014
    ## 52                    Andalucía              2013         137376858 2013
    ## 53                       Aragón              2013          32583929 2013
    ## 54      Asturias, Principado de              2013          20668712 2013
    ## 55               Balears, Illes              2013          25507987 2013
    ## 56                     Canarias              2013          39555918 2013
    ## 57                    Cantabria              2013          11750298 2013
    ## 58              Castilla y León              2013          51851079 2013
    ## 59         Castilla - La Mancha              2013          36581196 2013
    ## 60                     Cataluña              2013         193126366 2013
    ## 61         Comunitat Valenciana              2013          95247067 2013
    ## 62                  Extremadura              2013          16781115 2013
    ## 63                      Galicia              2013          53710116 2013
    ## 64         Madrid, Comunidad de              2013         192923737 2013
    ## 65            Murcia, Región de              2013          26479434 2013
    ## 66  Navarra, Comunidad Foral de              2013          17480886 2013
    ## 67                   País Vasco              2013          62647749 2013
    ## 68                    Rioja, La              2013           7517137 2013
    ## 69                    Andalucía              2012         139710392 2012
    ## 70                       Aragón              2012          32534897 2012
    ## 71      Asturias, Principado de              2012          21419093 2012
    ## 72               Balears, Illes              2012          25646507 2012
    ## 73                     Canarias              2012          39799366 2012
    ## 74                    Cantabria              2012          12152197 2012
    ## 75              Castilla y León              2012          53445730 2012
    ## 76         Castilla - La Mancha              2012          37503325 2012
    ## 77                     Cataluña              2012         195209451 2012
    ## 78         Comunitat Valenciana              2012          96427837 2012
    ## 79                  Extremadura              2012          16874848 2012
    ## 80                      Galicia              2012          54023202 2012
    ## 81         Madrid, Comunidad de              2012         195653479 2012
    ## 82            Murcia, Región de              2012          26547688 2012
    ## 83  Navarra, Comunidad Foral de              2012          17573037 2012
    ## 84                   País Vasco              2012          63818464 2012
    ## 85                    Rioja, La              2012           7655224 2012
    ## 86                    Andalucía              2011         144651807 2011
    ## 87                       Aragón              2011          33917092 2011
    ## 88      Asturias, Principado de              2011          22464790 2011
    ## 89               Balears, Illes              2011          26030098 2011
    ## 90                     Canarias              2011          41047616 2011
    ## 91                    Cantabria              2011          12591556 2011
    ## 92              Castilla y León              2011          55076407 2011
    ## 93         Castilla - La Mancha              2011          38773960 2011
    ## 94                     Cataluña              2011         200184689 2011
    ## 95         Comunitat Valenciana              2011         100664633 2011
    ## 96                  Extremadura              2011          17563209 2011
    ## 97                      Galicia              2011          55828124 2011
    ## 98         Madrid, Comunidad de              2011         198942916 2011
    ## 99            Murcia, Región de              2011          27243278 2011
    ## 100 Navarra, Comunidad Foral de              2011          18220597 2011
    ## 101                  País Vasco              2011          65176367 2011
    ## 102                   Rioja, La              2011           7913467 2011
    ## 103                   Andalucía              2010         146124642 2010
    ## 104                      Aragón              2010          34406424 2010
    ## 105     Asturias, Principado de              2010          22868674 2010
    ## 106              Balears, Illes              2010          26194558 2010
    ## 107                    Canarias              2010          41248693 2010
    ## 108                   Cantabria              2010          12826271 2010
    ## 109             Castilla y León              2010          55558135 2010
    ## 110        Castilla - La Mancha              2010          39230002 2010
    ## 111                    Cataluña              2010         203324091 2010
    ## 112        Comunitat Valenciana              2010         102328966 2010
    ## 113                 Extremadura              2010          18026718 2010
    ## 114                     Galicia              2010          57025172 2010
    ## 115        Madrid, Comunidad de              2010         197948300 2010
    ## 116           Murcia, Región de              2010          27984477 2010
    ## 117 Navarra, Comunidad Foral de              2010          18256818 2010
    ## 118                  País Vasco              2010          65680491 2010
    ## 119                   Rioja, La              2010           8013688 2010
    ## 120                   Andalucía              2009         146315391 2009
    ## 121                      Aragón              2009          34137634 2009
    ## 122     Asturias, Principado de              2009          22723922 2009
    ## 123              Balears, Illes              2009          26153141 2009
    ## 124                    Canarias              2009          40694618 2009
    ## 125                   Cantabria              2009          12809601 2009
    ## 126             Castilla y León              2009          55457671 2009
    ## 127        Castilla - La Mancha              2009          39210517 2009
    ## 128                    Cataluña              2009         202028299 2009
    ## 129        Comunitat Valenciana              2009         102781394 2009
    ## 130                 Extremadura              2009          17777510 2009
    ## 131                     Galicia              2009          56739461 2009
    ## 132        Madrid, Comunidad de              2009         199530665 2009
    ## 133           Murcia, Región de              2009          27797007 2009
    ## 134 Navarra, Comunidad Foral de              2009          18204976 2009
    ## 135                  País Vasco              2009          64935346 2009
    ## 136                   Rioja, La              2009           7950219 2009
    ## 137                   Andalucía              2008         152137231 2008
    ## 138                      Aragón              2008          35615255 2008
    ## 139     Asturias, Principado de              2008          23989386 2008
    ## 140              Balears, Illes              2008          27193863 2008
    ## 141                    Canarias              2008          42582341 2008
    ## 142                   Cantabria              2008          13279223 2008
    ## 143             Castilla y León              2008          57092217 2008
    ## 144        Castilla - La Mancha              2008          40389312 2008
    ## 145                    Cataluña              2008         209004722 2008
    ## 146        Comunitat Valenciana              2008         108507820 2008
    ## 147                 Extremadura              2008          18154860 2008
    ## 148                     Galicia              2008          58583574 2008
    ## 149        Madrid, Comunidad de              2008         202034516 2008
    ## 150           Murcia, Región de              2008          29137167 2008
    ## 151 Navarra, Comunidad Foral de              2008          18738715 2008
    ## 152                  País Vasco              2008          67698141 2008
    ## 153                   Rioja, La              2008           8275326 2008
    ## 154                   Andalucía              2007         148644794 2007
    ## 155                      Aragón              2007          34228275 2007
    ## 156     Asturias, Principado de              2007          23239346 2007
    ## 157              Balears, Illes              2007          26144862 2007
    ## 158                    Canarias              2007          41656214 2007
    ## 159                   Cantabria              2007          12845737 2007
    ## 160             Castilla y León              2007          55831514 2007
    ## 161        Castilla - La Mancha              2007          38706853 2007
    ## 162                    Cataluña              2007         203402667 2007
    ## 163        Comunitat Valenciana              2007         105192625 2007
    ## 164                 Extremadura              2007          17482445 2007
    ## 165                     Galicia              2007          56233842 2007
    ## 166        Madrid, Comunidad de              2007         194533412 2007
    ## 167           Murcia, Región de              2007          27989687 2007
    ## 168 Navarra, Comunidad Foral de              2007          17958589 2007
    ## 169                  País Vasco              2007          65091957 2007
    ## 170                   Rioja, La              2007           7963068 2007
    ## 171                   Andalucía              2006         139066918 2006
    ## 172                      Aragón              2006          31407151 2006
    ## 173     Asturias, Principado de              2006          21700331 2006
    ## 174              Balears, Illes              2006          24429529 2006
    ## 175                    Canarias              2006          39248086 2006
    ## 176                   Cantabria              2006          11976124 2006
    ## 177             Castilla y León              2006          52148058 2006
    ## 178        Castilla - La Mancha              2006          35434272 2006
    ## 179                    Cataluña              2006         189854079 2006
    ## 180        Comunitat Valenciana              2006          98381803 2006
    ## 181                 Extremadura              2006          16231118 2006
    ## 182                     Galicia              2006          52169100 2006
    ## 183        Madrid, Comunidad de              2006         181318153 2006
    ## 184           Murcia, Región de              2006          25936676 2006
    ## 185 Navarra, Comunidad Foral de              2006          16816112 2006
    ## 186                  País Vasco              2006          60937706 2006
    ## 187                   Rioja, La              2006           7419962 2006
    ## 188                   Andalucía              2005         128986608 2005
    ## 189                      Aragón              2005          28907051 2005
    ## 190     Asturias, Principado de              2005          19955477 2005
    ## 191              Balears, Illes              2005          22602678 2005
    ## 192                    Canarias              2005          36767517 2005
    ## 193                   Cantabria              2005          11169636 2005
    ## 194             Castilla y León              2005          48866640 2005
    ## 195        Castilla - La Mancha              2005          32485453 2005
    ## 196                    Cataluña              2005         175031658 2005
    ## 197        Comunitat Valenciana              2005          90535410 2005
    ## 198                 Extremadura              2005          15215503 2005
    ## 199                     Galicia              2005          48125508 2005
    ## 200        Madrid, Comunidad de              2005         166138717 2005
    ## 201           Murcia, Región de              2005          23867292 2005
    ## 202 Navarra, Comunidad Foral de              2005          15635137 2005
    ## 203                  País Vasco              2005          56211666 2005
    ## 204                   Rioja, La              2005           6855308 2005
    ## 205                   Andalucía              2004         118724376 2004
    ## 206                      Aragón              2004          26800618 2004
    ## 207     Asturias, Principado de              2004          18386025 2004
    ## 208              Balears, Illes              2004          20983851 2004
    ## 209                    Canarias              2004          34305219 2004
    ## 210                   Cantabria              2004          10339090 2004
    ## 211             Castilla y León              2004          45712531 2004
    ## 212        Castilla - La Mancha              2004          29647186 2004
    ## 213                    Cataluña              2004         162716105 2004
    ## 214        Comunitat Valenciana              2004          83896036 2004
    ## 215                 Extremadura              2004          13986836 2004
    ## 216                     Galicia              2004          44351222 2004
    ## 217        Madrid, Comunidad de              2004         153815368 2004
    ## 218           Murcia, Región de              2004          21758036 2004
    ## 219 Navarra, Comunidad Foral de              2004          14514312 2004
    ## 220                  País Vasco              2004          52130831 2004
    ## 221                   Rioja, La              2004           6358571 2004
    ## 222                   Andalucía              2003         109789588 2003
    ## 223                      Aragón              2003          25087830 2003
    ## 224     Asturias, Principado de              2003          17269896 2003
    ## 225              Balears, Illes              2003          19692948 2003
    ## 226                    Canarias              2003          32434415 2003
    ## 227                   Cantabria              2003           9704150 2003
    ## 228             Castilla y León              2003          42969761 2003
    ## 229        Castilla - La Mancha              2003          27621593 2003
    ## 230                    Cataluña              2003         151676907 2003
    ## 231        Comunitat Valenciana              2003          78242090 2003
    ## 232                 Extremadura              2003          13094947 2003
    ## 233                     Galicia              2003          41225500 2003
    ## 234        Madrid, Comunidad de              2003         143196121 2003
    ## 235           Murcia, Región de              2003          20241248 2003
    ## 236 Navarra, Comunidad Foral de              2003          13586433 2003
    ## 237                  País Vasco              2003          48879847 2003
    ## 238                   Rioja, La              2003           5994163 2003
    ## 239                   Andalucía              2002         100887974 2002
    ## 240                      Aragón              2002          23410533 2002
    ## 241     Asturias, Principado de              2002          16315232 2002
    ## 242              Balears, Illes              2002          18780108 2002
    ## 243                    Canarias              2002          30245930 2002
    ## 244                   Cantabria              2002           9198499 2002
    ## 245             Castilla y León              2002          40385217 2002
    ## 246        Castilla - La Mancha              2002          25415826 2002
    ## 247                    Cataluña              2002         141450434 2002
    ## 248        Comunitat Valenciana              2002          73246538 2002
    ## 249                 Extremadura              2002          12237785 2002
    ## 250                     Galicia              2002          38451199 2002
    ## 251        Madrid, Comunidad de              2002         133558896 2002
    ## 252           Murcia, Región de              2002          18598188 2002
    ## 253 Navarra, Comunidad Foral de              2002          12741253 2002
    ## 254                  País Vasco              2002          46167184 2002
    ## 255                   Rioja, La              2002           5533879 2002
    ## 256                   Andalucía              2001          93492514 2001
    ## 257                      Aragón              2001          21557351 2001
    ## 258     Asturias, Principado de              2001          15389277 2001
    ## 259              Balears, Illes              2001          17789707 2001
    ## 260                    Canarias              2001          28278218 2001
    ## 261                   Cantabria              2001           8604007 2001
    ## 262             Castilla y León              2001          37970750 2001
    ## 263        Castilla - La Mancha              2001          23582775 2001
    ## 264                    Cataluña              2001         132311008 2001
    ## 265        Comunitat Valenciana              2001          68323833 2001
    ## 266                 Extremadura              2001          11484483 2001
    ## 267                     Galicia              2001          35968041 2001
    ## 268        Madrid, Comunidad de              2001         124397963 2001
    ## 269           Murcia, Región de              2001          17085644 2001
    ## 270 Navarra, Comunidad Foral de              2001          11906276 2001
    ## 271                  País Vasco              2001          43591343 2001
    ## 272                   Rioja, La              2001           5224763 2001
    ## 273                   Andalucía              2000          86331553 2000
    ## 274                      Aragón              2000          20042880 2000
    ## 275     Asturias, Principado de              2000          14266600 2000
    ## 276              Balears, Illes              2000          16492806 2000
    ## 277                    Canarias              2000          25962458 2000
    ## 278                   Cantabria              2000           7945277 2000
    ## 279             Castilla y León              2000          35646082 2000
    ## 280        Castilla - La Mancha              2000          21713679 2000
    ## 281                    Cataluña              2000         122056805 2000
    ## 282        Comunitat Valenciana              2000          62531733 2000
    ## 283                 Extremadura              2000          10745432 2000
    ## 284                     Galicia              2000          33391170 2000
    ## 285        Madrid, Comunidad de              2000         114203390 2000
    ## 286           Murcia, Región de              2000          15675781 2000
    ## 287 Navarra, Comunidad Foral de              2000          11157493 2000
    ## 288                  País Vasco              2000          40711377 2000
    ## 289                   Rioja, La              2000           4889889 2000
    ## 290                   Andalucía              1999          75241844 1999
    ## 291                      Aragón              1999          17636274 1999
    ## 292     Asturias, Principado de              1999          12714496 1999
    ## 293              Balears, Illes              1999          13961277 1999
    ## 294                    Canarias              1999          22909101 1999
    ## 295                   Cantabria              1999           7084089 1999
    ## 296             Castilla y León              1999          32563366 1999
    ## 297        Castilla - La Mancha              1999          19653559 1999
    ## 298                    Cataluña              1999         105698177 1999
    ## 299        Comunitat Valenciana              1999          54434391 1999
    ## 300                 Extremadura              1999           9777833 1999
    ## 301                     Galicia              1999          30910000 1999
    ## 302        Madrid, Comunidad de              1999          97169687 1999
    ## 303           Murcia, Región de              1999          13281568 1999
    ## 304 Navarra, Comunidad Foral de              1999           9541669 1999
    ## 305                  País Vasco              1999          36312901 1999
    ## 306                   Rioja, La              1999           4273883 1999
    ## 307                   Andalucía              1998          70358958 1998
    ## 308                      Aragón              1998          16824619 1998
    ## 309     Asturias, Principado de              1998          12394228 1998
    ## 310              Balears, Illes              1998          12717956 1998
    ## 311                    Canarias              1998          20675958 1998
    ## 312                   Cantabria              1998           6571597 1998
    ## 313             Castilla y León              1998          30609425 1998
    ## 314        Castilla - La Mancha              1998          18667214 1998
    ## 315                    Cataluña              1998          98608532 1998
    ## 316        Comunitat Valenciana              1998          50817383 1998
    ## 317                 Extremadura              1998           9030424 1998
    ## 318                     Galicia              1998          28966227 1998
    ## 319        Madrid, Comunidad de              1998          90596587 1998
    ## 320           Murcia, Región de              1998          12435298 1998
    ## 321 Navarra, Comunidad Foral de              1998           8999222 1998
    ## 322                  País Vasco              1998          33613629 1998
    ## 323                   Rioja, La              1998           4001479 1998
    ## 324                   Andalucía              1997          66501528 1997
    ## 325                      Aragón              1997          16119777 1997
    ## 326     Asturias, Principado de              1997          11457960 1997
    ## 327              Balears, Illes              1997          11816498 1997
    ## 328                    Canarias              1997          18993648 1997
    ## 329                   Cantabria              1997           6093622 1997
    ## 330             Castilla y León              1997          29113845 1997
    ## 331        Castilla - La Mancha              1997          17395336 1997
    ## 332                    Cataluña              1997          93513809 1997
    ## 333        Comunitat Valenciana              1997          47233520 1997
    ## 334                 Extremadura              1997           8463357 1997
    ## 335                     Galicia              1997          27342372 1997
    ## 336        Madrid, Comunidad de              1997          83274418 1997
    ## 337           Murcia, Región de              1997          11570962 1997
    ## 338 Navarra, Comunidad Foral de              1997           8513113 1997
    ## 339                  País Vasco              1997          31066603 1997
    ## 340                   Rioja, La              1997           3775764 1997
    ## 341                   Andalucía              1996          62389878 1996
    ## 342                      Aragón              1996          15181169 1996
    ## 343     Asturias, Principado de              1996          11081648 1996
    ## 344              Balears, Illes              1996          10748749 1996
    ## 345                    Canarias              1996          17684474 1996
    ## 346                   Cantabria              1996           5751285 1996
    ## 347             Castilla y León              1996          27985051 1996
    ## 348        Castilla - La Mancha              1996          16480110 1996
    ## 349                    Cataluña              1996          88418707 1996
    ## 350        Comunitat Valenciana              1996          43870044 1996
    ## 351                 Extremadura              1996           8033196 1996
    ## 352                     Galicia              1996          25857185 1996
    ## 353        Madrid, Comunidad de              1996          77804094 1996
    ## 354           Murcia, Región de              1996          10659781 1996
    ## 355 Navarra, Comunidad Foral de              1996           7948073 1996
    ## 356                  País Vasco              1996          29064201 1996
    ## 357                   Rioja, La              1996           3530936 1996
    ## 358                   Andalucía              1995          58703745 1995
    ## 359                      Aragón              1995          14302189 1995
    ## 360     Asturias, Principado de              1995          10583097 1995
    ## 361              Balears, Illes              1995          10062087 1995
    ## 362                    Canarias              1995          16626276 1995
    ## 363                   Cantabria              1995           5465372 1995
    ## 364             Castilla y León              1995          26714076 1995
    ## 365        Castilla - La Mancha              1995          15435841 1995
    ## 366                    Cataluña              1995          82752572 1995
    ## 367        Comunitat Valenciana              1995          41374468 1995
    ## 368                 Extremadura              1995           7530804 1995
    ## 369                     Galicia              1995          24565897 1995
    ## 370        Madrid, Comunidad de              1995          73522117 1995
    ## 371           Murcia, Región de              1995          10029536 1995
    ## 372 Navarra, Comunidad Foral de              1995           7454734 1995
    ## 373                  País Vasco              1995          27646756 1995
    ## 374                   Rioja, La              1995           3343425 1995
    ## 375                   Andalucía 1994(Provisional)          52275101 1994
    ## 376                      Aragón 1994(Provisional)          13207241 1994
    ## 377     Asturias, Principado de 1994(Provisional)           9791839 1994
    ## 378              Balears, Illes 1994(Provisional)           9723240 1994
    ## 379                    Canarias 1994(Provisional)          14513132 1994
    ## 380                   Cantabria 1994(Provisional)           5083931 1994
    ## 381             Castilla y León 1994(Provisional)          23099768 1994
    ## 382        Castilla - La Mancha 1994(Provisional)          13925793 1994
    ## 383                    Cataluña 1994(Provisional)          74690563 1994
    ## 384        Comunitat Valenciana 1994(Provisional)          37914789 1994
    ## 385                 Extremadura 1994(Provisional)           7546993 1994
    ## 386                     Galicia 1994(Provisional)          21273875 1994
    ## 387        Madrid, Comunidad de 1994(Provisional)          61927037 1994
    ## 388           Murcia, Región de 1994(Provisional)           9528542 1994
    ## 389 Navarra, Comunidad Foral de 1994(Provisional)           6301245 1994
    ## 390                  País Vasco 1994(Provisional)          24133280 1994
    ## 391                   Rioja, La 1994(Provisional)           2957599 1994
    ## 392                   Andalucía              1993          49018313 1993
    ## 393                      Aragón              1993          12473243 1993
    ## 394     Asturias, Principado de              1993           9257516 1993
    ## 395              Balears, Illes              1993           9016119 1993
    ## 396                    Canarias              1993          13618309 1993
    ## 397                   Cantabria              1993           4764614 1993
    ## 398             Castilla y León              1993          22133401 1993
    ## 399        Castilla - La Mancha              1993          13285853 1993
    ## 400                    Cataluña              1993          69719117 1993
    ## 401        Comunitat Valenciana              1993          35965063 1993
    ## 402                 Extremadura              1993           7051909 1993
    ## 403                     Galicia              1993          20213972 1993
    ## 404        Madrid, Comunidad de              1993          57973994 1993
    ## 405           Murcia, Región de              1993           8918112 1993
    ## 406 Navarra, Comunidad Foral de              1993           5930733 1993
    ## 407                  País Vasco              1993          22802495 1993
    ## 408                   Rioja, La              1993           2759619 1993
    ## 409                   Andalucía              1992          48036986 1992
    ## 410                      Aragón              1992          12140090 1992
    ## 411     Asturias, Principado de              1992           9085476 1992
    ## 412              Balears, Illes              1992           8647897 1992
    ## 413                    Canarias              1992          12661810 1992
    ## 414                   Cantabria              1992           4690869 1992
    ## 415             Castilla y León              1992          20920781 1992
    ## 416        Castilla - La Mancha              1992          13052685 1992
    ## 417                    Cataluña              1992          67887749 1992
    ## 418        Comunitat Valenciana              1992          34966073 1992
    ## 419                 Extremadura              1992           6840732 1992
    ## 420                     Galicia              1992          19546596 1992
    ## 421        Madrid, Comunidad de              1992          56014911 1992
    ## 422           Murcia, Región de              1992           8699752 1992
    ## 423 Navarra, Comunidad Foral de              1992           5842006 1992
    ## 424                  País Vasco              1992          22191176 1992
    ## 425                   Rioja, La              1992           2688844 1992
    ## 426                   Andalucía              1991          45139988 1991
    ## 427                      Aragón              1991          11391722 1991
    ## 428     Asturias, Principado de              1991           8349963 1991
    ## 429              Balears, Illes              1991           7914572 1991
    ## 430                    Canarias              1991          11465177 1991
    ## 431                   Cantabria              1991           4296185 1991
    ## 432             Castilla y León              1991          19541494 1991
    ## 433        Castilla - La Mancha              1991          12132265 1991
    ## 434                    Cataluña              1991          62747292 1991
    ## 435        Comunitat Valenciana              1991          32661600 1991
    ## 436                 Extremadura              1991           6284681 1991
    ## 437                     Galicia              1991          18078438 1991
    ## 438        Madrid, Comunidad de              1991          51799833 1991
    ## 439           Murcia, Región de              1991           8141154 1991
    ## 440 Navarra, Comunidad Foral de              1991           5460622 1991
    ## 441                  País Vasco              1991          21008180 1991
    ## 442                   Rioja, La              1991           2494970 1991
    ## 443                   Andalucía              1990          41132511 1990
    ## 444                      Aragón              1990          10396169 1990
    ## 445     Asturias, Principado de              1990           7727543 1990
    ## 446              Balears, Illes              1990           7108362 1990
    ## 447                    Canarias              1990          10511612 1990
    ## 448                   Cantabria              1990           4004676 1990
    ## 449             Castilla y León              1990          17888891 1990
    ## 450        Castilla - La Mancha              1990          11128112 1990
    ## 451                    Cataluña              1990          57174498 1990
    ## 452        Comunitat Valenciana              1990          29753807 1990
    ## 453                 Extremadura              1990           5691308 1990
    ## 454                     Galicia              1990          16611975 1990
    ## 455        Madrid, Comunidad de              1990          47072266 1990
    ## 456           Murcia, Región de              1990           7517003 1990
    ## 457 Navarra, Comunidad Foral de              1990           4984211 1990
    ## 458                  País Vasco              1990          19320165 1990
    ## 459                   Rioja, La              1990           2273046 1990
    ## 460                   Andalucía              1989          35921105 1989
    ## 461                      Aragón              1989           9488449 1989
    ## 462     Asturias, Principado de              1989           7170327 1989
    ## 463              Balears, Illes              1989           6258279 1989
    ## 464                    Canarias              1989           9618736 1989
    ## 465                   Cantabria              1989           3712909 1989
    ## 466             Castilla y León              1989          16524239 1989
    ## 467        Castilla - La Mancha              1989          10026306 1989
    ## 468                    Cataluña              1989          51458536 1989
    ## 469        Comunitat Valenciana              1989          26591570 1989
    ## 470                 Extremadura              1989           5126164 1989
    ## 471                     Galicia              1989          15278569 1989
    ## 472        Madrid, Comunidad de              1989          41712512 1989
    ## 473           Murcia, Región de              1989           6550605 1989
    ## 474 Navarra, Comunidad Foral de              1989           4652669 1989
    ## 475                  País Vasco              1989          17622997 1989
    ## 476                   Rioja, La              1989           2033524 1989
    ## 477                   Andalucía              1988          32427302 1988
    ## 478                      Aragón              1988           8478388 1988
    ## 479     Asturias, Principado de              1988           6432729 1988
    ## 480              Balears, Illes              1988           5716328 1988
    ## 481                    Canarias              1988           8839097 1988
    ## 482                   Cantabria              1988           3260298 1988
    ## 483             Castilla y León              1988          15021108 1988
    ## 484        Castilla - La Mancha              1988           8831999 1988
    ## 485                    Cataluña              1988          45096024 1988
    ## 486        Comunitat Valenciana              1988          23689697 1988
    ## 487                 Extremadura              1988           4693364 1988
    ## 488                     Galicia              1988          13684409 1988
    ## 489        Madrid, Comunidad de              1988          37094966 1988
    ## 490           Murcia, Región de              1988           5811943 1988
    ## 491 Navarra, Comunidad Foral de              1988           3996202 1988
    ## 492                  País Vasco              1988          15573600 1988
    ## 493                   Rioja, La              1988           1826578 1988
    ## 494                   Andalucía              1987          29334379 1987
    ## 495                      Aragón              1987           7405689 1987
    ## 496     Asturias, Principado de              1987           5867164 1987
    ## 497              Balears, Illes              1987           5149670 1987
    ## 498                    Canarias              1987           7791966 1987
    ## 499                   Cantabria              1987           2807075 1987
    ## 500             Castilla y León              1987          13723468 1987
    ## 501        Castilla - La Mancha              1987           7806751 1987
    ## 502                    Cataluña              1987          40004946 1987
    ## 503        Comunitat Valenciana              1987          21472035 1987
    ## 504                 Extremadura              1987           4101589 1987
    ## 505                     Galicia              1987          12265665 1987
    ## 506        Madrid, Comunidad de              1987          33667033 1987
    ## 507           Murcia, Región de              1987           5356917 1987
    ## 508 Navarra, Comunidad Foral de              1987           3700167 1987
    ## 509                  País Vasco              1987          14320808 1987
    ## 510                   Rioja, La              1987           1644303 1987
    ## 511                   Andalucía              1986          25803096 1986
    ## 512                      Aragón              1986           6670128 1986
    ## 513     Asturias, Principado de              1986           5537707 1986
    ## 514              Balears, Illes              1986           4537930 1986
    ## 515                    Canarias              1986           6940939 1986
    ## 516                   Cantabria              1986           2548616 1986
    ## 517             Castilla y León              1986          12348972 1986
    ## 518        Castilla - La Mancha              1986           6805170 1986
    ## 519                    Cataluña              1986          35444322 1986
    ## 520        Comunitat Valenciana              1986          19212362 1986
    ## 521                 Extremadura              1986           3578192 1986
    ## 522                     Galicia              1986          11205829 1986
    ## 523        Madrid, Comunidad de              1986          29944575 1986
    ## 524           Murcia, Región de              1986           4834217 1986
    ## 525 Navarra, Comunidad Foral de              1986           3174468 1986
    ## 526                  País Vasco              1986          13416706 1986
    ## 527                   Rioja, La              1986           1544529 1986
    ## 528                   Andalucía              1985          22631658 1985
    ## 529                      Aragón              1985           5866257 1985
    ## 530     Asturias, Principado de              1985           4808704 1985
    ## 531              Balears, Illes              1985           4056567 1985
    ## 532                    Canarias              1985           5862086 1985
    ## 533                   Cantabria              1985           2360307 1985
    ## 534             Castilla y León              1985          10977835 1985
    ## 535        Castilla - La Mancha              1985           6247046 1985
    ## 536                    Cataluña              1985          30134368 1985
    ## 537        Comunitat Valenciana              1985          17229328 1985
    ## 538                 Extremadura              1985           3302964 1985
    ## 539                     Galicia              1985          10122606 1985
    ## 540        Madrid, Comunidad de              1985          24976302 1985
    ## 541           Murcia, Región de              1985           4047949 1985
    ## 542 Navarra, Comunidad Foral de              1985           2784453 1985
    ## 543                  País Vasco              1985          11889708 1985
    ## 544                   Rioja, La              1985           1518084 1985
    ## 545                   Andalucía              1984          19869941 1984
    ## 546                      Aragón              1984           5413929 1984
    ## 547     Asturias, Principado de              1984           4197775 1984
    ## 548              Balears, Illes              1984           3421808 1984
    ## 549                    Canarias              1984           5479740 1984
    ## 550                   Cantabria              1984           2234924 1984
    ## 551             Castilla y León              1984           9722994 1984
    ## 552        Castilla - La Mancha              1984           5358678 1984
    ## 553                    Cataluña              1984          28041800 1984
    ## 554        Comunitat Valenciana              1984          15534847 1984
    ## 555                 Extremadura              1984           2926328 1984
    ## 556                     Galicia              1984           9280366 1984
    ## 557        Madrid, Comunidad de              1984          23118315 1984
    ## 558           Murcia, Región de              1984           3687834 1984
    ## 559 Navarra, Comunidad Foral de              1984           2548105 1984
    ## 560                  País Vasco              1984          10711713 1984
    ## 561                   Rioja, La              1984           1264469 1984
    ## 562                   Andalucía              1983          17656534 1983
    ## 563                      Aragón              1983           4723847 1983
    ## 564     Asturias, Principado de              1983           3762799 1983
    ## 565              Balears, Illes              1983           2930138 1983
    ## 566                    Canarias              1983           5042311 1983
    ## 567                   Cantabria              1983           1979379 1983
    ## 568             Castilla y León              1983           8478490 1983
    ## 569        Castilla - La Mancha              1983           4692059 1983
    ## 570                    Cataluña              1983          24480082 1983
    ## 571        Comunitat Valenciana              1983          13582880 1983
    ## 572                 Extremadura              1983           2189054 1983
    ## 573                     Galicia              1983           8196140 1983
    ## 574        Madrid, Comunidad de              1983          20673067 1983
    ## 575           Murcia, Región de              1983           3245165 1983
    ## 576 Navarra, Comunidad Foral de              1983           2290661 1983
    ## 577                  País Vasco              1983           9882712 1983
    ## 578                   Rioja, La              1983           1119698 1983
    ## 579                   Andalucía              1982          15353347 1982
    ## 580                      Aragón              1982           3999273 1982
    ## 581     Asturias, Principado de              1982           3384798 1982
    ## 582              Balears, Illes              1982           2506154 1982
    ## 583                    Canarias              1982           4290854 1982
    ## 584                   Cantabria              1982           1742250 1982
    ## 585             Castilla y León              1982           7396355 1982
    ## 586        Castilla - La Mancha              1982           4165699 1982
    ## 587                    Cataluña              1982          21657970 1982
    ## 588        Comunitat Valenciana              1982          11770275 1982
    ## 589                 Extremadura              1982           1936623 1982
    ## 590                     Galicia              1982           7390718 1982
    ## 591        Madrid, Comunidad de              1982          17858275 1982
    ## 592           Murcia, Región de              1982           2768809 1982
    ## 593 Navarra, Comunidad Foral de              1982           2053520 1982
    ## 594                  País Vasco              1982           8890514 1982
    ## 595                   Rioja, La              1982            949797 1982
    ## 596                   Andalucía              1981          13048063 1981
    ## 597                      Aragón              1981           3416904 1981
    ## 598     Asturias, Principado de              1981           2886367 1981
    ## 599              Balears, Illes              1981           2108939 1981
    ## 600                    Canarias              1981           3714459 1981
    ## 601                   Cantabria              1981           1548562 1981
    ## 602             Castilla y León              1981           6153102 1981
    ## 603        Castilla - La Mancha              1981           3612341 1981
    ## 604                    Cataluña              1981          19235885 1981
    ## 605        Comunitat Valenciana              1981          10485377 1981
    ## 606                 Extremadura              1981           1664671 1981
    ## 607                     Galicia              1981           6282668 1981
    ## 608        Madrid, Comunidad de              1981          15126116 1981
    ## 609           Murcia, Región de              1981           2433498 1981
    ## 610 Navarra, Comunidad Foral de              1981           1826013 1981
    ## 611                  País Vasco              1981           7717921 1981
    ## 612                   Rioja, La              1981            816679 1981
    ## 613                   Andalucía              1980          11657633 1980
    ## 614                      Aragón              1980           3115965 1980
    ## 615     Asturias, Principado de              1980           2542389 1980
    ## 616              Balears, Illes              1980           1796906 1980
    ## 617                    Canarias              1980           3284417 1980
    ## 618                   Cantabria              1980           1338009 1980
    ## 619             Castilla y León              1980           5662880 1980
    ## 620        Castilla - La Mancha              1980           3288263 1980
    ## 621                    Cataluña              1980          17405395 1980
    ## 622        Comunitat Valenciana              1980           9084238 1980
    ## 623                 Extremadura              1980           1508366 1980
    ## 624                     Galicia              1980           5465634 1980
    ## 625        Madrid, Comunidad de              1980          13456968 1980
    ## 626           Murcia, Región de              1980           2185502 1980
    ## 627 Navarra, Comunidad Foral de              1980           1580566 1980
    ## 628                  País Vasco              1980           6764331 1980
    ## 629                   Rioja, La              1980            711220 1980
    ##          kind
    ## 1   estimated
    ## 2   estimated
    ## 3   estimated
    ## 4   estimated
    ## 5   estimated
    ## 6   estimated
    ## 7   estimated
    ## 8   estimated
    ## 9   estimated
    ## 10  estimated
    ## 11  estimated
    ## 12  estimated
    ## 13  estimated
    ## 14  estimated
    ## 15  estimated
    ## 16  estimated
    ## 17  estimated
    ## 18  estimated
    ## 19  estimated
    ## 20  estimated
    ## 21  estimated
    ## 22  estimated
    ## 23  estimated
    ## 24  estimated
    ## 25  estimated
    ## 26  estimated
    ## 27  estimated
    ## 28  estimated
    ## 29  estimated
    ## 30  estimated
    ## 31  estimated
    ## 32  estimated
    ## 33  estimated
    ## 34  estimated
    ## 35  estimated
    ## 36  estimated
    ## 37  estimated
    ## 38  estimated
    ## 39  estimated
    ## 40  estimated
    ## 41  estimated
    ## 42  estimated
    ## 43  estimated
    ## 44  estimated
    ## 45  estimated
    ## 46  estimated
    ## 47  estimated
    ## 48  estimated
    ## 49  estimated
    ## 50  estimated
    ## 51  estimated
    ## 52       real
    ## 53       real
    ## 54       real
    ## 55       real
    ## 56       real
    ## 57       real
    ## 58       real
    ## 59       real
    ## 60       real
    ## 61       real
    ## 62       real
    ## 63       real
    ## 64       real
    ## 65       real
    ## 66       real
    ## 67       real
    ## 68       real
    ## 69       real
    ## 70       real
    ## 71       real
    ## 72       real
    ## 73       real
    ## 74       real
    ## 75       real
    ## 76       real
    ## 77       real
    ## 78       real
    ## 79       real
    ## 80       real
    ## 81       real
    ## 82       real
    ## 83       real
    ## 84       real
    ## 85       real
    ## 86       real
    ## 87       real
    ## 88       real
    ## 89       real
    ## 90       real
    ## 91       real
    ## 92       real
    ## 93       real
    ## 94       real
    ## 95       real
    ## 96       real
    ## 97       real
    ## 98       real
    ## 99       real
    ## 100      real
    ## 101      real
    ## 102      real
    ## 103      real
    ## 104      real
    ## 105      real
    ## 106      real
    ## 107      real
    ## 108      real
    ## 109      real
    ## 110      real
    ## 111      real
    ## 112      real
    ## 113      real
    ## 114      real
    ## 115      real
    ## 116      real
    ## 117      real
    ## 118      real
    ## 119      real
    ## 120      real
    ## 121      real
    ## 122      real
    ## 123      real
    ## 124      real
    ## 125      real
    ## 126      real
    ## 127      real
    ## 128      real
    ## 129      real
    ## 130      real
    ## 131      real
    ## 132      real
    ## 133      real
    ## 134      real
    ## 135      real
    ## 136      real
    ## 137      real
    ## 138      real
    ## 139      real
    ## 140      real
    ## 141      real
    ## 142      real
    ## 143      real
    ## 144      real
    ## 145      real
    ## 146      real
    ## 147      real
    ## 148      real
    ## 149      real
    ## 150      real
    ## 151      real
    ## 152      real
    ## 153      real
    ## 154      real
    ## 155      real
    ## 156      real
    ## 157      real
    ## 158      real
    ## 159      real
    ## 160      real
    ## 161      real
    ## 162      real
    ## 163      real
    ## 164      real
    ## 165      real
    ## 166      real
    ## 167      real
    ## 168      real
    ## 169      real
    ## 170      real
    ## 171      real
    ## 172      real
    ## 173      real
    ## 174      real
    ## 175      real
    ## 176      real
    ## 177      real
    ## 178      real
    ## 179      real
    ## 180      real
    ## 181      real
    ## 182      real
    ## 183      real
    ## 184      real
    ## 185      real
    ## 186      real
    ## 187      real
    ## 188      real
    ## 189      real
    ## 190      real
    ## 191      real
    ## 192      real
    ## 193      real
    ## 194      real
    ## 195      real
    ## 196      real
    ## 197      real
    ## 198      real
    ## 199      real
    ## 200      real
    ## 201      real
    ## 202      real
    ## 203      real
    ## 204      real
    ## 205      real
    ## 206      real
    ## 207      real
    ## 208      real
    ## 209      real
    ## 210      real
    ## 211      real
    ## 212      real
    ## 213      real
    ## 214      real
    ## 215      real
    ## 216      real
    ## 217      real
    ## 218      real
    ## 219      real
    ## 220      real
    ## 221      real
    ## 222      real
    ## 223      real
    ## 224      real
    ## 225      real
    ## 226      real
    ## 227      real
    ## 228      real
    ## 229      real
    ## 230      real
    ## 231      real
    ## 232      real
    ## 233      real
    ## 234      real
    ## 235      real
    ## 236      real
    ## 237      real
    ## 238      real
    ## 239      real
    ## 240      real
    ## 241      real
    ## 242      real
    ## 243      real
    ## 244      real
    ## 245      real
    ## 246      real
    ## 247      real
    ## 248      real
    ## 249      real
    ## 250      real
    ## 251      real
    ## 252      real
    ## 253      real
    ## 254      real
    ## 255      real
    ## 256      real
    ## 257      real
    ## 258      real
    ## 259      real
    ## 260      real
    ## 261      real
    ## 262      real
    ## 263      real
    ## 264      real
    ## 265      real
    ## 266      real
    ## 267      real
    ## 268      real
    ## 269      real
    ## 270      real
    ## 271      real
    ## 272      real
    ## 273      real
    ## 274      real
    ## 275      real
    ## 276      real
    ## 277      real
    ## 278      real
    ## 279      real
    ## 280      real
    ## 281      real
    ## 282      real
    ## 283      real
    ## 284      real
    ## 285      real
    ## 286      real
    ## 287      real
    ## 288      real
    ## 289      real
    ## 290      real
    ## 291      real
    ## 292      real
    ## 293      real
    ## 294      real
    ## 295      real
    ## 296      real
    ## 297      real
    ## 298      real
    ## 299      real
    ## 300      real
    ## 301      real
    ## 302      real
    ## 303      real
    ## 304      real
    ## 305      real
    ## 306      real
    ## 307      real
    ## 308      real
    ## 309      real
    ## 310      real
    ## 311      real
    ## 312      real
    ## 313      real
    ## 314      real
    ## 315      real
    ## 316      real
    ## 317      real
    ## 318      real
    ## 319      real
    ## 320      real
    ## 321      real
    ## 322      real
    ## 323      real
    ## 324      real
    ## 325      real
    ## 326      real
    ## 327      real
    ## 328      real
    ## 329      real
    ## 330      real
    ## 331      real
    ## 332      real
    ## 333      real
    ## 334      real
    ## 335      real
    ## 336      real
    ## 337      real
    ## 338      real
    ## 339      real
    ## 340      real
    ## 341      real
    ## 342      real
    ## 343      real
    ## 344      real
    ## 345      real
    ## 346      real
    ## 347      real
    ## 348      real
    ## 349      real
    ## 350      real
    ## 351      real
    ## 352      real
    ## 353      real
    ## 354      real
    ## 355      real
    ## 356      real
    ## 357      real
    ## 358      real
    ## 359      real
    ## 360      real
    ## 361      real
    ## 362      real
    ## 363      real
    ## 364      real
    ## 365      real
    ## 366      real
    ## 367      real
    ## 368      real
    ## 369      real
    ## 370      real
    ## 371      real
    ## 372      real
    ## 373      real
    ## 374      real
    ## 375 estimated
    ## 376 estimated
    ## 377 estimated
    ## 378 estimated
    ## 379 estimated
    ## 380 estimated
    ## 381 estimated
    ## 382 estimated
    ## 383 estimated
    ## 384 estimated
    ## 385 estimated
    ## 386 estimated
    ## 387 estimated
    ## 388 estimated
    ## 389 estimated
    ## 390 estimated
    ## 391 estimated
    ## 392      real
    ## 393      real
    ## 394      real
    ## 395      real
    ## 396      real
    ## 397      real
    ## 398      real
    ## 399      real
    ## 400      real
    ## 401      real
    ## 402      real
    ## 403      real
    ## 404      real
    ## 405      real
    ## 406      real
    ## 407      real
    ## 408      real
    ## 409      real
    ## 410      real
    ## 411      real
    ## 412      real
    ## 413      real
    ## 414      real
    ## 415      real
    ## 416      real
    ## 417      real
    ## 418      real
    ## 419      real
    ## 420      real
    ## 421      real
    ## 422      real
    ## 423      real
    ## 424      real
    ## 425      real
    ## 426      real
    ## 427      real
    ## 428      real
    ## 429      real
    ## 430      real
    ## 431      real
    ## 432      real
    ## 433      real
    ## 434      real
    ## 435      real
    ## 436      real
    ## 437      real
    ## 438      real
    ## 439      real
    ## 440      real
    ## 441      real
    ## 442      real
    ## 443      real
    ## 444      real
    ## 445      real
    ## 446      real
    ## 447      real
    ## 448      real
    ## 449      real
    ## 450      real
    ## 451      real
    ## 452      real
    ## 453      real
    ## 454      real
    ## 455      real
    ## 456      real
    ## 457      real
    ## 458      real
    ## 459      real
    ## 460      real
    ## 461      real
    ## 462      real
    ## 463      real
    ## 464      real
    ## 465      real
    ## 466      real
    ## 467      real
    ## 468      real
    ## 469      real
    ## 470      real
    ## 471      real
    ## 472      real
    ## 473      real
    ## 474      real
    ## 475      real
    ## 476      real
    ## 477      real
    ## 478      real
    ## 479      real
    ## 480      real
    ## 481      real
    ## 482      real
    ## 483      real
    ## 484      real
    ## 485      real
    ## 486      real
    ## 487      real
    ## 488      real
    ## 489      real
    ## 490      real
    ## 491      real
    ## 492      real
    ## 493      real
    ## 494      real
    ## 495      real
    ## 496      real
    ## 497      real
    ## 498      real
    ## 499      real
    ## 500      real
    ## 501      real
    ## 502      real
    ## 503      real
    ## 504      real
    ## 505      real
    ## 506      real
    ## 507      real
    ## 508      real
    ## 509      real
    ## 510      real
    ## 511      real
    ## 512      real
    ## 513      real
    ## 514      real
    ## 515      real
    ## 516      real
    ## 517      real
    ## 518      real
    ## 519      real
    ## 520      real
    ## 521      real
    ## 522      real
    ## 523      real
    ## 524      real
    ## 525      real
    ## 526      real
    ## 527      real
    ## 528      real
    ## 529      real
    ## 530      real
    ## 531      real
    ## 532      real
    ## 533      real
    ## 534      real
    ## 535      real
    ## 536      real
    ## 537      real
    ## 538      real
    ## 539      real
    ## 540      real
    ## 541      real
    ## 542      real
    ## 543      real
    ## 544      real
    ## 545      real
    ## 546      real
    ## 547      real
    ## 548      real
    ## 549      real
    ## 550      real
    ## 551      real
    ## 552      real
    ## 553      real
    ## 554      real
    ## 555      real
    ## 556      real
    ## 557      real
    ## 558      real
    ## 559      real
    ## 560      real
    ## 561      real
    ## 562      real
    ## 563      real
    ## 564      real
    ## 565      real
    ## 566      real
    ## 567      real
    ## 568      real
    ## 569      real
    ## 570      real
    ## 571      real
    ## 572      real
    ## 573      real
    ## 574      real
    ## 575      real
    ## 576      real
    ## 577      real
    ## 578      real
    ## 579      real
    ## 580      real
    ## 581      real
    ## 582      real
    ## 583      real
    ## 584      real
    ## 585      real
    ## 586      real
    ## 587      real
    ## 588      real
    ## 589      real
    ## 590      real
    ## 591      real
    ## 592      real
    ## 593      real
    ## 594      real
    ## 595      real
    ## 596      real
    ## 597      real
    ## 598      real
    ## 599      real
    ## 600      real
    ## 601      real
    ## 602      real
    ## 603      real
    ## 604      real
    ## 605      real
    ## 606      real
    ## 607      real
    ## 608      real
    ## 609      real
    ## 610      real
    ## 611      real
    ## 612      real
    ## 613      real
    ## 614      real
    ## 615      real
    ## 616      real
    ## 617      real
    ## 618      real
    ## 619      real
    ## 620      real
    ## 621      real
    ## 622      real
    ## 623      real
    ## 624      real
    ## 625      real
    ## 626      real
    ## 627      real
    ## 628      real
    ## 629      real
