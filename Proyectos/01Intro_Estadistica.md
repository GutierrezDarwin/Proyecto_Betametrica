Proyecto Estadística
================
Ing. Darwin Gutiérrez
2024-04-11

# Cargando mi base de datos

``` r
#file.choose()
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
data <- read.csv("D:\\Experto en Ciencia de Datos\\MODULO 01 DataC. Estadística para la toma de decisiones\\Material\\BASES_DATOS_BASES_ZS2ZD2\\Iowa_Liquor_Sales.csv",stringsAsFactors = T, header = T)

datos <- data %>% mutate(Sale..Dollars.=(as.numeric(substr(data$Sale..Dollars.,2,15))),
                         City=toupper(City),
                         Store.Name=toupper(Store.Name),
                         Date=as.Date(Date,format="%m/%d/%Y"),
                         anio=lubridate::year(Date)
                         ) %>% 
  rename(ventas= Sale..Dollars.,
         ciudad=City,
         categoria=Category.Name,
         nombre_tienda=Store.Name)
```

# 1. ¿Cuál es el top 5(promedio de ventas), para el año 2016, para la ciudad

# CEDAR RAPIDS?

``` r
datos %>% 
  group_by(ciudad,anio,nombre_tienda) %>% 
  summarise(promedio_ventas=mean(ventas,na.rm = T,trim=0.9))%>% 
  filter(ciudad=="CEDAR RAPIDS" & anio==2016)%>% 
  top_n(5,promedio_ventas) %>% 
  arrange(desc(promedio_ventas))
```

    ## `summarise()` has grouped output by 'ciudad', 'anio'. You can override using
    ## the `.groups` argument.

    ## # A tibble: 5 × 4
    ## # Groups:   ciudad, anio [1]
    ##   ciudad        anio nombre_tienda                       promedio_ventas
    ##   <chr>        <dbl> <chr>                                         <dbl>
    ## 1 CEDAR RAPIDS  2016 FAREWAY STORES #151 / CEDAR RAPIDS             225 
    ## 2 CEDAR RAPIDS  2016 SAM'S CLUB 8162 / CEDAR RAPIDS                 171 
    ## 3 CEDAR RAPIDS  2016 BENZ DISTRIBUTING                              162 
    ## 4 CEDAR RAPIDS  2016 CASEY'S GENERAL STORE #2763 / CEDAR            149.
    ## 5 CEDAR RAPIDS  2016 TARGET STORE T-1771 / CEDAR RAPIDS             149.

\#¿Cuáles fueron los 5 últimos vendedores(promedio de ventas, para el
2016, para davenport)?

``` r
datos %>% 
  group_by(ciudad,anio,Vendor.Name) %>% 
  summarise(promedio_ventas = mean(ventas,na.rm = T)) %>% 
  filter(ciudad=="DAVENPORT" & anio==2016) %>% 
  arrange(promedio_ventas) %>% 
  head(.,n=5)
```

    ## `summarise()` has grouped output by 'ciudad', 'anio'. You can override using
    ## the `.groups` argument.

    ## # A tibble: 5 × 4
    ## # Groups:   ciudad, anio [1]
    ##   ciudad     anio Vendor.Name                   promedio_ventas
    ##   <chr>     <dbl> <fct>                                   <dbl>
    ## 1 DAVENPORT  2016 Luxco-St Louis                           36.5
    ## 2 DAVENPORT  2016 A HARDY USA LTD                          37.0
    ## 3 DAVENPORT  2016 Rumcoqui and Co                          38.3
    ## 4 DAVENPORT  2016 Prestige Wine & Spirits Group            42.1
    ## 5 DAVENPORT  2016 Dehner Distillery                        42.7

\#¿Cues es el top 5 de productos más vendidos, para el 2016 y 2017, por
ciudad?

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ forcats   1.0.0     ✔ readr     2.1.5
    ## ✔ ggplot2   3.4.4     ✔ stringr   1.5.1
    ## ✔ lubridate 1.9.3     ✔ tibble    3.2.1
    ## ✔ purrr     1.0.2     ✔ tidyr     1.3.1
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
# top 5 productos mas vendidos para el año 2016

datos %>% 
  group_by(ciudad,anio,categoria)%>% 
  summarise(total_vendida=sum(Bottles.Sold,na.rm = T)) %>% 
  filter(anio==2016) %>% 
  arrange(desc(total_vendida))%>% 
  top_n(5,total_vendida)%>% 
  pivot_wider(names_from = ciudad,
              values_from = total_vendida,
              names_prefix = "ciudad_",
              values_fill=0)
```

    ## `summarise()` has grouped output by 'ciudad', 'anio'. You can override using
    ## the `.groups` argument.

    ## # A tibble: 5 × 5
    ## # Groups:   anio [1]
    ##    anio categoria      ciudad_DAVENPORT `ciudad_CEDAR RAPIDS` ciudad_WATERLOO
    ##   <dbl> <fct>                     <int>                 <int>           <int>
    ## 1  2016 VODKA FLAVORED            38105                 37949           20579
    ## 2  2016 FLAVORED RUM              12573                 16478            9941
    ## 3  2016 CREAM LIQUEURS             7309                  9477            4603
    ## 4  2016 Cream Liqueurs             1857                  3052            1154
    ## 5  2016 Flavored Rum               1458                  2555             867

``` r
# top 5 productos mas vendidos para el año 2017

datos %>% 
  group_by(ciudad,anio,categoria)%>% 
  summarise(total_vendida=sum(Bottles.Sold,na.rm = T)) %>% 
  filter(anio==2017) %>% 
  arrange(desc(total_vendida))%>%
  top_n(5,total_vendida)%>%
  pivot_wider(names_from = ciudad,
              values_from = total_vendida,
              names_prefix = "ciudad_",
              values_fill=0)
```

    ## `summarise()` has grouped output by 'ciudad', 'anio'. You can override using
    ## the `.groups` argument.

    ## # A tibble: 2 × 5
    ## # Groups:   anio [1]
    ##    anio categoria      `ciudad_CEDAR RAPIDS` ciudad_DAVENPORT ciudad_WATERLOO
    ##   <dbl> <fct>                          <int>            <int>           <int>
    ## 1  2017 Flavored Rum                    1031              511             355
    ## 2  2017 Cream Liqueurs                   823              465             303

``` r
# NOTA: No hay registros de ventas en 3 productos en el año 2017. 
```
