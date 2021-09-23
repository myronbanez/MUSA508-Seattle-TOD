Transit Oriented Development Around Seattle’s Streetcar
================
Myron Bañez

## Setup

## Styling Visualization

``` r
mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 16,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.text.x = element_text(size = 14))
}

plotTheme <- function(base_size = 10) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 12,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    strip.text.x = element_text(size = 14)
  )
}

# Load Quantile break functions
qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]],c(.01,.2,.4,.6,.8), na.rm=T), digits = 3))
  }
}

q5 <- function(variable) {as.factor(ntile(variable, 5))}

# Load hexadecimal color palette
palette5 <- c("#ccdbdc","#9ad1d4","#80ced7","#007ea7","#003249")

# Load census API key
census_api_key("05b9c101eb2ee7dc7abb88140da527ce637ac07f", overwrite = TRUE)
```

    ## To install your API key for use in future sessions, run this function with `install = TRUE`.

## 2009 Tracts

``` r
tracts09 <-  
  get_acs(geography = "tract", variables = c("B25026_001E","B02001_002E","B15001_050E",
                                             "B15001_009E","B19013_001E","B25058_001E",
                                             "B06012_002E"), 
                year=2009, state=53, county=033, geometry=T) %>% 
  st_transform('ESRI:102748') 
```

    ## Getting data from the 2005-2009 5-year ACS

    ## Downloading feature geometry from the Census website.  To cache shapefiles for use in future sessions, set `options(tigris_use_cache = TRUE)`.

    ##   |                                                                              |                                                                      |   0%  |                                                                              |=                                                                     |   1%  |                                                                              |==                                                                    |   3%  |                                                                              |===                                                                   |   5%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |======                                                                |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |==========                                                            |  14%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  17%  |                                                                              |==============                                                        |  19%  |                                                                              |==============                                                        |  20%  |                                                                              |===============                                                       |  21%  |                                                                              |===============                                                       |  22%  |                                                                              |================                                                      |  23%  |                                                                              |=================                                                     |  24%  |                                                                              |==================                                                    |  25%  |                                                                              |==================                                                    |  26%  |                                                                              |====================                                                  |  29%  |                                                                              |=====================                                                 |  30%  |                                                                              |======================                                                |  31%  |                                                                              |=======================                                               |  32%  |                                                                              |=======================                                               |  34%  |                                                                              |=============================                                         |  41%  |                                                                              |===============================                                       |  44%  |                                                                              |================================                                      |  45%  |                                                                              |====================================                                  |  52%  |                                                                              |=====================================                                 |  53%  |                                                                              |======================================                                |  54%  |                                                                              |=======================================                               |  56%  |                                                                              |=========================================                             |  59%  |                                                                              |============================================                          |  62%  |                                                                              |=============================================                         |  64%  |                                                                              |==============================================                        |  66%  |                                                                              |==================================================                    |  72%  |                                                                              |===================================================                   |  73%  |                                                                              |=====================================================                 |  75%  |                                                                              |=========================================================             |  81%  |                                                                              |==========================================================            |  83%  |                                                                              |===========================================================           |  85%  |                                                                              |=============================================================         |  87%  |                                                                              |==============================================================        |  88%  |                                                                              |================================================================      |  91%  |                                                                              |==================================================================    |  94%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  98%  |                                                                              |======================================================================| 100%

``` r
tracts09_1<-subset(tracts09, GEOID == "53033000100" | GEOID == "53033000200" |  GEOID == "53033000300" | GEOID == "53033000401" | GEOID == "53033000402" | GEOID == 53033000500 | GEOID == 53033000600 | GEOID == 53033000700 | GEOID == 53033000800 | GEOID == 53033000900 | GEOID == 53033001000 | GEOID == 53033001100 | GEOID == 53033001200 | GEOID == 53033001300 | GEOID == 53033001400 | GEOID == 53033001500 | GEOID == 53033001600 | GEOID == 53033001700 | GEOID == 53033001800 | GEOID == 53033001900 | GEOID == 53033002000 | GEOID == 53033002100 | GEOID == 53033002200 | GEOID == 53033002300 | GEOID == 53033002400 | GEOID == 53033002500 | GEOID == 53033002600 | GEOID == 53033002700 | GEOID == 53033002800 | GEOID == 53033002900 | GEOID == 53033003000 | GEOID == 53033003100 | GEOID == 53033003200 | GEOID == 53033003300 | GEOID == 53033003400 | GEOID == 53033003500 | GEOID == 53033003600 | GEOID == 53033003700 | GEOID == 53033003800 | GEOID == 53033003900 | GEOID == 53033004000 | GEOID == 53033004100 | GEOID == 53033004200 | GEOID == 53033004300 | GEOID == 53033004400 | GEOID == 53033004500 | GEOID == 53033004600 | GEOID == 53033004700 | GEOID == 53033004800 | GEOID == 53033004900 | GEOID == 53033005000 | GEOID == 53033005100 | GEOID == 53033005200 | GEOID == 53033005301 | GEOID == 53033005302 | GEOID == 53033005400 | GEOID == 53033005500 | GEOID == 53033005600 | GEOID == 53033005700 | GEOID == 53033005801 | GEOID == 53033005802 | GEOID == 53033005900 | GEOID == 53033006000 | GEOID == 53033006100 | GEOID == 53033006200 | GEOID == 53033006300 | GEOID == 53033006400 | GEOID == 53033006500 | GEOID == 53033006600 | GEOID == 53033006700 | GEOID == 53033006800 | GEOID == 53033006900 | GEOID == 53033007000 | GEOID == 53033007100 | GEOID == 53033007200 | GEOID == 53033007300 | GEOID == 53033007400 | GEOID == 53033007500 | GEOID == 53033007600 | GEOID == 53033007700 | GEOID == 53033007800 | GEOID == 53033007900 | GEOID == 53033008001 | GEOID == 53033008002 | GEOID == 53033008001 | GEOID == 53033008100  | GEOID == 53033008200 | GEOID == 53033008300 | GEOID == 53033008400 | GEOID == 53033008500 | GEOID == 53033008600 | GEOID == 53033008700 | GEOID == 53033008800 | GEOID == 53033008900 | GEOID == 53033009000 | GEOID == 53033009100 | GEOID == 53033009200 | GEOID == 53033009300 | GEOID == 53033009400 | GEOID == 53033009500 | GEOID == 53033009600 | GEOID == 53033009701 | GEOID == 53033009702 | GEOID == 53033009800 | GEOID == 53033009900 | GEOID == 53033010000 | GEOID == 53033010100 | GEOID == 53033010200 | GEOID == 53033010300 | GEOID == 53033010400 | GEOID == 53033010500 | GEOID == 53033010600 | GEOID == 53033010700 | GEOID == 53033010800 | GEOID == 53033010900 | GEOID == 53033011000 | GEOID == 53033011101 | GEOID == 53033011102 | GEOID == 53033011200 | GEOID == 53033011300 | GEOID == 53033011400 | GEOID == 53033011500 | GEOID == 53033011600 | GEOID == 53033011700 | GEOID == 53033011800 | GEOID == 53033011900 | GEOID == 53033012000 | GEOID == 53033012100 )                                  
 
totalPop09 <-
  tracts09_1 %>%
  filter(variable == "B25026_001")
```

## ggplot

``` r
A <- 
  ggplot() +
  geom_sf(data = totalPop09, aes(fill = estimate)) +
  theme(plot.title = element_text(size=22))

B <- 
  ggplot() +
  geom_sf(data = totalPop09, aes(fill = q5(estimate))) +
  theme(plot.title = element_text(size=22))

C <-
  ggplot() +
  geom_sf(data = totalPop09, aes(fill = q5(estimate))) +
  scale_fill_manual(values = palette5,
                    labels = qBr(totalPop09, "estimate"),
                    name = "Total\nPopluation\n(Quintile Breaks)") +
  theme(plot.title = element_text(size=22))

D <- 
  ggplot() +
  geom_sf(data = totalPop09, aes(fill = q5(estimate)), color = NA) +
  scale_fill_manual(values = palette5,
                    labels = qBr(totalPop09, "estimate"),
                    name = "Popluation\n(Quintile Breaks)") +
  labs(title = "Total Population", subtitle = "Seattle; 2009") +
  mapTheme() + theme(plot.title = element_text(size=22))

tracts09_1 <- 
  tracts09_1 %>%
  dplyr::select( -NAME, -moe) %>%
  spread(variable, estimate) %>%
  dplyr::select(-geometry) %>%
  rename(TotalPop = B25026_001, 
         Whites = B02001_002,
         FemaleBachelors = B15001_050, 
         MaleBachelors = B15001_009,
         MedHHInc = B19013_001, 
         MedRent = B25058_001,
         TotalPoverty = B06012_002)

tracts09_1 <- 
  tracts09_1 %>%
  mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop, 0),
         pctBachelors = ifelse(TotalPop > 0, ((FemaleBachelors + MaleBachelors) / TotalPop), 0),
         pctPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
         year = "2009") %>%
  dplyr::select(-Whites,-FemaleBachelors,-MaleBachelors,-TotalPoverty)
```

## 2017 Tracts

``` r
tracts17 <- 
  get_acs(geography = "tract", variables = c("B25026_001E","B02001_002E","B15001_050E",
                                             "B15001_009E","B19013_001E","B25058_001E",
                                             "B06012_002E"), 
          year=2017, state=53, county=033, geometry=T, output="wide") %>%
  st_transform('ESRI:102748')
```

    ## Getting data from the 2013-2017 5-year ACS

    ## Downloading feature geometry from the Census website.  To cache shapefiles for use in future sessions, set `options(tigris_use_cache = TRUE)`.

    ##   |                                                                              |                                                                      |   0%  |                                                                              |=                                                                     |   1%  |                                                                              |====                                                                  |   5%  |                                                                              |======                                                                |   8%  |                                                                              |======                                                                |   9%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  17%  |                                                                              |=============                                                         |  18%  |                                                                              |===============                                                       |  21%  |                                                                              |================                                                      |  22%  |                                                                              |==================                                                    |  25%  |                                                                              |===================                                                   |  26%  |                                                                              |====================                                                  |  28%  |                                                                              |====================                                                  |  29%  |                                                                              |=====================                                                 |  30%  |                                                                              |=======================                                               |  32%  |                                                                              |=========================                                             |  35%  |                                                                              |=========================                                             |  36%  |                                                                              |==========================                                            |  38%  |                                                                              |===========================                                           |  39%  |                                                                              |============================                                          |  40%  |                                                                              |=============================                                         |  42%  |                                                                              |==============================                                        |  43%  |                                                                              |===============================                                       |  44%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  47%  |                                                                              |==================================                                    |  48%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================                                  |  51%  |                                                                              |======================================                                |  54%  |                                                                              |=======================================                               |  55%  |                                                                              |=======================================                               |  56%  |                                                                              |========================================                              |  58%  |                                                                              |==========================================                            |  59%  |                                                                              |==========================================                            |  60%  |                                                                              |===========================================                           |  62%  |                                                                              |============================================                          |  63%  |                                                                              |=============================================                         |  64%  |                                                                              |==============================================                        |  66%  |                                                                              |===============================================                       |  67%  |                                                                              |================================================                      |  68%  |                                                                              |==================================================                    |  71%  |                                                                              |===================================================                   |  72%  |                                                                              |====================================================                  |  74%  |                                                                              |=====================================================                 |  76%  |                                                                              |======================================================                |  78%  |                                                                              |=======================================================               |  78%  |                                                                              |========================================================              |  80%  |                                                                              |=========================================================             |  81%  |                                                                              |==========================================================            |  83%  |                                                                              |===========================================================           |  84%  |                                                                              |============================================================          |  86%  |                                                                              |=============================================================         |  87%  |                                                                              |==============================================================        |  88%  |                                                                              |===============================================================       |  90%  |                                                                              |================================================================      |  91%  |                                                                              |=================================================================     |  92%  |                                                                              |==================================================================    |  95%  |                                                                              |====================================================================  |  98%  |                                                                              |======================================================================| 100%

``` r
tracts17_1<-subset(tracts17, GEOID == "53033000100" | GEOID == "53033000200" |  GEOID == "53033000300" | GEOID == "53033000401" | GEOID == "53033000402" | GEOID == 53033000500 | GEOID == 53033000600 | GEOID == 53033000700 | GEOID == 53033000800 | GEOID == 53033000900 | GEOID == 53033001000 | GEOID == 53033001100 | GEOID == 53033001200 | GEOID == 53033001300 | GEOID == 53033001400 | GEOID == 53033001500 | GEOID == 53033001600 | GEOID == 53033001700 | GEOID == 53033001800 | GEOID == 53033001900 | GEOID == 53033002000 | GEOID == 53033002100 | GEOID == 53033002200 | GEOID == 53033002300 | GEOID == 53033002400 | GEOID == 53033002500 | GEOID == 53033002600 | GEOID == 53033002700 | GEOID == 53033002800 | GEOID == 53033002900 | GEOID == 53033003000 | GEOID == 53033003100 | GEOID == 53033003200 | GEOID == 53033003300 | GEOID == 53033003400 | GEOID == 53033003500 | GEOID == 53033003600 | GEOID == 53033003700 | GEOID == 53033003800 | GEOID == 53033003900 | GEOID == 53033004000 | GEOID == 53033004100 | GEOID == 53033004200 | GEOID == 53033004300 | GEOID == 53033004400 | GEOID == 53033004500 | GEOID == 53033004600 | GEOID == 53033004700 | GEOID == 53033004800 | GEOID == 53033004900 | GEOID == 53033005000 | GEOID == 53033005100 | GEOID == 53033005200 | GEOID == 53033005301 | GEOID == 53033005302 | GEOID == 53033005400 | GEOID == 53033005500 | GEOID == 53033005600 | GEOID == 53033005700 | GEOID == 53033005801 | GEOID == 53033005802 | GEOID == 53033005900 | GEOID == 53033006000 | GEOID == 53033006100 | GEOID == 53033006200 | GEOID == 53033006300 | GEOID == 53033006400 | GEOID == 53033006500 | GEOID == 53033006600 | GEOID == 53033006700 | GEOID == 53033006800 | GEOID == 53033006900 | GEOID == 53033007000 | GEOID == 53033007100 | GEOID == 53033007200 | GEOID == 53033007300 | GEOID == 53033007400 | GEOID == 53033007500 | GEOID == 53033007600 | GEOID == 53033007700 | GEOID == 53033007800 | GEOID == 53033007900 | GEOID == 53033008001 | GEOID == 53033008002 | GEOID == 53033008001 | GEOID == 53033008100  | GEOID == 53033008200 | GEOID == 53033008300 | GEOID == 53033008400 | GEOID == 53033008500 | GEOID == 53033008600 | GEOID == 53033008700 | GEOID == 53033008800 | GEOID == 53033008900 | GEOID == 53033009000 | GEOID == 53033009100 | GEOID == 53033009200 | GEOID == 53033009300 | GEOID == 53033009400 | GEOID == 53033009500 | GEOID == 53033009600 | GEOID == 53033009701 | GEOID == 53033009702 | GEOID == 53033009800 | GEOID == 53033009900 | GEOID == 53033010000 | GEOID == 53033010100 | GEOID == 53033010200 | GEOID == 53033010300 | GEOID == 53033010400 | GEOID == 53033010500 | GEOID == 53033010600 | GEOID == 53033010700 | GEOID == 53033010800 | GEOID == 53033010900 | GEOID == 53033011000 | GEOID == 53033011101 | GEOID == 53033011102 | GEOID == 53033011200 | GEOID == 53033011300 | GEOID == 53033011400 | GEOID == 53033011500 | GEOID == 53033011600 | GEOID == 53033011700 | GEOID == 53033011800 | GEOID == 53033011900 | GEOID == 53033012000 | GEOID == 53033012100 ) %>%
  rename(TotalPop = B25026_001E, 
         Whites = B02001_002E,
         FemaleBachelors = B15001_050E, 
         MaleBachelors = B15001_009E,
         MedHHInc = B19013_001E, 
         MedRent = B25058_001E,
         TotalPoverty = B06012_002E) %>%
  dplyr::select(-NAME, -starts_with("B")) %>%
  mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop,0),
         pctBachelors = ifelse(TotalPop > 0, ((FemaleBachelors + MaleBachelors) / TotalPop),0),
         pctPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
         year = "2017") %>%
  dplyr::select(-Whites, -FemaleBachelors, -MaleBachelors, -TotalPoverty) 
```

## Combine Data

``` r
allTracts <- rbind(tracts09_1,tracts17_1)
```

## Wrangle Seattle TOD Data

``` r
# Set working directory to access data
setwd("~/Desktop/Seattle-main")

# Streetcar Stops
StreetcarStops <- 
  rbind(
    st_read("Streetcar_Stations.geojson") %>% 
      mutate(STATION = "South Lake Union") %>%
      select(STOP, STATION),
    st_read("Streetcar_Stations.geojson") %>%
      mutate(STATION = "First Hill") %>%
      select(STOP, STATION)) %>%
  st_transform(st_crs(tracts09_1))
```

    ## Reading layer `Streetcar_Stations' from data source 
    ##   `/Users/myronbanez/Desktop/Seattle-main/Streetcar_Stations.geojson' 
    ##   using driver `GeoJSON'
    ## Simple feature collection with 26 features and 3 fields
    ## Geometry type: POINT
    ## Dimension:     XY
    ## Bounding box:  xmin: -122.3385 ymin: 47.59561 xmax: -122.3141 ymax: 47.62763
    ## Geodetic CRS:  WGS 84
    ## Reading layer `Streetcar_Stations' from data source 
    ##   `/Users/myronbanez/Desktop/Seattle-main/Streetcar_Stations.geojson' 
    ##   using driver `GeoJSON'
    ## Simple feature collection with 26 features and 3 fields
    ## Geometry type: POINT
    ## Dimension:     XY
    ## Bounding box:  xmin: -122.3385 ymin: 47.59561 xmax: -122.3141 ymax: 47.62763
    ## Geodetic CRS:  WGS 84

``` r
# Visualize Streetcar Stops
ggplot() + 
  geom_sf(data=st_union(tracts09_1)) +
  geom_sf(data=StreetcarStops, 
          aes(color = STATION), 
          show.legend = "point", size= 2) +
  scale_colour_manual(values = c("blue", "orange")) +
  labs(title="Streetcar Stops", 
       subtitle="Seattle, WA", 
       caption="Figure 1") +
  mapTheme()
```

![](SeattleTOD_files/figure-gfm/Wrangling%20TOD%20Data-1.png)<!-- -->

``` r
# Buffers
StreetcarBuffers <- 
  rbind(
    st_buffer(StreetcarStops, 2640) %>%
      mutate(Legend = "Buffer") %>%
      dplyr::select(Legend),
    st_union(st_buffer(StreetcarStops, 2640)) %>%
      st_sf() %>%
      mutate(Legend = "Unioned Buffer"))

# Visualize Buffers
ggplot() +
  geom_sf(data=StreetcarBuffers) +
  geom_sf(data=StreetcarStops, show.legend = "point") +
  facet_wrap(~Legend) + 
  labs(caption = "Figure 2") +
  mapTheme()
```

![](SeattleTOD_files/figure-gfm/Wrangling%20TOD%20Data-2.png)<!-- -->

``` r
# Spatial Operations
buffer <- filter(StreetcarBuffers, Legend=="Unioned Buffer")

clip <- 
  st_intersection(buffer, tracts09_1) %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Clip")
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

``` r
selection <- 
  tracts09_1[buffer,] %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Spatial Selection")

selectCentroids <-
  st_centroid(tracts09_1)[buffer,] %>%
  st_drop_geometry() %>%
  left_join(., dplyr::select(tracts09_1, GEOID)) %>%
  st_sf() %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Select by Centroids")
```

    ## Warning in st_centroid.sf(tracts09_1): st_centroid assumes attributes are
    ## constant over geometries of x

    ## Joining, by = "GEOID"

``` r
alloperations <- rbind(clip, selection, selectCentroids)

# Visualize Spatial Operations
ggplot(tracts09_1) +
  geom_sf(data=st_union(tracts09_1)) +
  geom_sf(data=alloperations) +
  facet_wrap(~Selection_Type) +
  geom_sf(data = alloperations, aes(fill = q5(TotalPop)), color = NA) +
  scale_fill_manual(values = palette5,
                    labels = qBr(alloperations, "TotalPop"),
                    name = "Popluation\n(Quintile Breaks)") +
  labs(title = "Total Population", subtitle = "Seattle; 2009", caption = "Figure 3") +
  mapTheme() + theme(plot.title = element_text(size=22))
```

![](SeattleTOD_files/figure-gfm/Wrangling%20TOD%20Data-3.png)<!-- -->

## TOD Indicator: Maps

``` r
allTracts.group <- 
  rbind(
    st_centroid(allTracts)[StreetcarBuffers,] %>%
      st_drop_geometry() %>%
      left_join(allTracts) %>%
      st_sf() %>%
      mutate(TOD = "TOD"),
    st_centroid(allTracts)[StreetcarBuffers, op = st_disjoint] %>%
      st_drop_geometry() %>%
      left_join(allTracts) %>%
      st_sf() %>%
      mutate(TOD = "Non-TOD")) %>%
  mutate(MedRent.inf = ifelse(year == "2009", MedRent* 1.14, MedRent)) %>%
  mutate(MedHHInc.inf = ifelse(year == "2009", MedHHInc* 1.14, MedHHInc)) %>%
  mutate(pctPoverty.inf = ifelse(year == "2009", pctPoverty* 1.14, pctPoverty))
```

    ## Warning in st_centroid.sf(allTracts): st_centroid assumes attributes are
    ## constant over geometries of x

    ## Joining, by = c("GEOID", "MedHHInc", "TotalPop", "MedRent", "pctWhite", "pctBachelors", "pctPoverty", "year")

    ## Warning in st_centroid.sf(allTracts): st_centroid assumes attributes are
    ## constant over geometries of x

    ## Joining, by = c("GEOID", "MedHHInc", "TotalPop", "MedRent", "pctWhite", "pctBachelors", "pctPoverty", "year")

``` r
ggplot(allTracts.group)+
    geom_sf(data=st_union(tracts09_1)) +
    geom_sf(data=StreetcarBuffers) +
    geom_sf(aes(fill = TOD)) +
    labs(title = "Time/Space Groups", caption = "Figure 4") +
    facet_wrap(~year)+
    mapTheme() + 
    theme(plot.title = element_text(size=22))
```

![](SeattleTOD_files/figure-gfm/TOD%20Indicators:%20Map-1.png)<!-- -->

``` r
# Total Population
ggplot(allTracts.group) +
    geom_sf(data = st_union(tracts09_1)) +
    geom_sf(aes(fill = q5(TotalPop)), color = "NA") +
    geom_sf(data = st_union(StreetcarBuffers), fill = "NA", color = "red") +
    scale_fill_manual(values = palette5,
                      labels = qBr(allTracts.group, "TotalPop"),
                      name = "Total Population\n(Quintile Breaks)") +
    labs(title = "Total Population 2009-2017", caption = "Figure 5") +
    facet_wrap(~year) +
    mapTheme() + 
    theme(plot.title = element_text(size=22))
```

![](SeattleTOD_files/figure-gfm/TOD%20Indicators:%20Map-2.png)<!-- -->

``` r
# Median Rent
ggplot(allTracts.group) +
    geom_sf(data = st_union(tracts09_1)) +
    geom_sf(aes(fill = q5(MedRent.inf)), color = "NA") +
    geom_sf(data = st_union(StreetcarBuffers), fill = "NA", color = "red") +
    scale_fill_manual(values = palette5,
                      labels = qBr(allTracts.group, "MedRent.inf"),
                      name = "Rent\n(Quintile Breaks)") +
    labs(title = "Median Rent 2009-2017", subtitle = "Real Dollars", caption = "Figure 6") +
    facet_wrap(~year) +
    mapTheme() + 
    theme(plot.title = element_text(size=22))
```

![](SeattleTOD_files/figure-gfm/TOD%20Indicators:%20Map-3.png)<!-- -->

``` r
# Median Household Income
ggplot(allTracts.group) +
    geom_sf(data = st_union(tracts09_1)) +
    geom_sf(aes(fill = q5(MedHHInc.inf)), color = "NA") +
    geom_sf(data = st_union(StreetcarBuffers), fill = "NA", color = "red") +
    scale_fill_manual(values = palette5,
                      labels = qBr(allTracts.group, "MedHHInc.inf"),
                      name = "Income\n(Quintile Breaks)") +
    labs(title = "Median Household Income 2009-2017", subtitle = "Real Dollars", caption = "Figure 7") +
    facet_wrap(~year) +
    mapTheme() + 
    theme(plot.title = element_text(size=22))
```

![](SeattleTOD_files/figure-gfm/TOD%20Indicators:%20Map-4.png)<!-- -->

``` r
# Percent Poverty
pov_plot <- allTracts.group %>% 
  mutate(pov_100 = pctPoverty.inf*100)
ggplot(allTracts.group) +
    geom_sf(data = st_union(tracts09_1)) +
    geom_sf(aes(fill = q5(pctPoverty.inf)), color = "NA") +
    geom_sf(data = st_union(StreetcarBuffers), fill = "NA", color = "red") +
    scale_fill_manual(values = palette5,
                      labels = qBr(pov_plot, "pctPoverty.inf", rnd=F),
                      name = "% Poverty\n(Quintile Breaks)") +
    labs(title = "Percent Poverty 2009-2017", subtitle = "Real Dollars", caption = "Figure 8") +
    facet_wrap(~year) +
    mapTheme() + 
    theme(plot.title = element_text(size=22))
```

![](SeattleTOD_files/figure-gfm/TOD%20Indicators:%20Map-5.png)<!-- -->

## TOD Indicator: Tables

``` r
allTracts.Summary <- 
  st_drop_geometry(allTracts.group) %>%
  group_by(year, TOD) %>%
  summarize(Population = mean(TotalPop, na.rm = T),
            Rent = mean(MedRent.inf, na.rm = T),
            HHIncome = mean(MedHHInc, na.rm = T),
            PerPoverty = mean(pctPoverty, na.rm = T))
```

    ## `summarise()` has grouped output by 'year'. You can override using the `.groups` argument.

``` r
kable(allTracts.Summary) %>%
  kable_styling() %>%
  footnote(general_title = "\n",
           general = "Figure 9")
```

<table class="table" style="margin-left: auto; margin-right: auto;border-bottom: 0;">
<thead>
<tr>
<th style="text-align:left;">
year
</th>
<th style="text-align:left;">
TOD
</th>
<th style="text-align:right;">
Population
</th>
<th style="text-align:right;">
Rent
</th>
<th style="text-align:right;">
HHIncome
</th>
<th style="text-align:right;">
PerPoverty
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
2009
</td>
<td style="text-align:left;">
Non-TOD
</td>
<td style="text-align:right;">
4611.040
</td>
<td style="text-align:right;">
1023.6189
</td>
<td style="text-align:right;">
63352.31
</td>
<td style="text-align:right;">
0.1339829
</td>
</tr>
<tr>
<td style="text-align:left;">
2009
</td>
<td style="text-align:left;">
TOD
</td>
<td style="text-align:right;">
3388.375
</td>
<td style="text-align:right;">
817.0237
</td>
<td style="text-align:right;">
34840.12
</td>
<td style="text-align:right;">
0.2667539
</td>
</tr>
<tr>
<td style="text-align:left;">
2017
</td>
<td style="text-align:left;">
Non-TOD
</td>
<td style="text-align:right;">
5135.069
</td>
<td style="text-align:right;">
1321.4310
</td>
<td style="text-align:right;">
87887.71
</td>
<td style="text-align:right;">
0.1202081
</td>
</tr>
<tr>
<td style="text-align:left;">
2017
</td>
<td style="text-align:left;">
TOD
</td>
<td style="text-align:right;">
4400.000
</td>
<td style="text-align:right;">
1254.5333
</td>
<td style="text-align:right;">
62249.53
</td>
<td style="text-align:right;">
0.1895205
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="padding: 0; " colspan="100%">
<span style="font-style: italic;"><br></span>
</td>
</tr>
<tr>
<td style="padding: 0; " colspan="100%">
<sup></sup> Figure 9
</td>
</tr>
</tfoot>
</table>

``` r
allTracts.Summary %>%
  unite(year.TOD, year, TOD, sep = ": ", remove = T) %>%
  gather(Variable, Value, -year.TOD) %>%
  mutate(Value = round(Value, 2)) %>%
  spread(year.TOD, Value) %>%
  kable() %>%
  kable_styling() %>%
  footnote(general_title = "\n",
           general = "Figure 10")
```

<table class="table" style="margin-left: auto; margin-right: auto;border-bottom: 0;">
<thead>
<tr>
<th style="text-align:left;">
Variable
</th>
<th style="text-align:right;">
2009: Non-TOD
</th>
<th style="text-align:right;">
2009: TOD
</th>
<th style="text-align:right;">
2017: Non-TOD
</th>
<th style="text-align:right;">
2017: TOD
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
HHIncome
</td>
<td style="text-align:right;">
63352.31
</td>
<td style="text-align:right;">
34840.12
</td>
<td style="text-align:right;">
87887.71
</td>
<td style="text-align:right;">
62249.53
</td>
</tr>
<tr>
<td style="text-align:left;">
PerPoverty
</td>
<td style="text-align:right;">
0.13
</td>
<td style="text-align:right;">
0.27
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
0.19
</td>
</tr>
<tr>
<td style="text-align:left;">
Population
</td>
<td style="text-align:right;">
4611.04
</td>
<td style="text-align:right;">
3388.38
</td>
<td style="text-align:right;">
5135.07
</td>
<td style="text-align:right;">
4400.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Rent
</td>
<td style="text-align:right;">
1023.62
</td>
<td style="text-align:right;">
817.02
</td>
<td style="text-align:right;">
1321.43
</td>
<td style="text-align:right;">
1254.53
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="padding: 0; " colspan="100%">
<span style="font-style: italic;"><br></span>
</td>
</tr>
<tr>
<td style="padding: 0; " colspan="100%">
<sup></sup> Figure 10
</td>
</tr>
</tfoot>
</table>

## TOD Indicator: Plots

``` r
allTracts.Summary %>%
  gather(Variable, Value, -year, -TOD) %>%
  ggplot(aes(year, Value, fill = TOD)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~Variable, scales = "free", ncol=5) +
    scale_fill_manual(values = c("#9ad1d4", "#003249")) +
    labs(title = "Indicator differences across time and space", caption = "Figure 11") +
    plotTheme() + theme(legend.position="bottom")
```

![](SeattleTOD_files/figure-gfm/TOD%20Indicators%20-%20Plot-1.png)<!-- -->

## Graduated Symbol Map

``` r
# Population 1/2 Mile
centers1 <- st_centroid(alloperations)
```

    ## Warning in st_centroid.sf(alloperations): st_centroid assumes attributes are
    ## constant over geometries of x

``` r
ggplot(tracts09_1) +
  geom_sf(data=st_union(tracts09_1), fill = "white") +
  geom_sf(data=tracts09_1, aes(fill = q5(TotalPop)), color = NA) +
  geom_sf(data=centers1, aes(size = TotalPop), shape = 21,
          fill = "darkgreen", alpha = 0.7, show.legend = "point") +
  scale_size_continuous(range = c(1,6)) +
  scale_fill_manual(values = palette5,
                    labels = qBr(alloperations, "TotalPop"),
                    name = "Popluation\n(Quintile Breaks)") +
  labs(title = "Total Population", subtitle = "Seattle; 2009", caption = "Figure 12") +
  mapTheme() + theme(plot.title = element_text(size=22))
```

![](SeattleTOD_files/figure-gfm/Graduated%20Symbol%20Map-1.png)<!-- -->

``` r
# Rent 1/2 Mile
clip_Rent <- 
  st_intersection(StreetcarBuffers, tracts09_1) %>%
  dplyr::select(MedHHInc) %>%
  mutate(Selection_Type = "Clip")
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

``` r
selection_Rent <- 
  tracts09_1[StreetcarBuffers,] %>%
  dplyr::select(MedHHInc) %>%
  mutate(Selection_Type = "Spatial Selection")

selectCentroids_Rent <-
  st_centroid(tracts09_1)[StreetcarBuffers,] %>%
  st_drop_geometry() %>%
  left_join(., dplyr::select(tracts09_1, GEOID)) %>%
  st_sf() %>%
  dplyr::select(MedHHInc) %>%
  mutate(Selection_Type = "Select by Centroids")
```

    ## Warning in st_centroid.sf(tracts09_1): st_centroid assumes attributes are
    ## constant over geometries of x

    ## Joining, by = "GEOID"

``` r
alloperations_Rent <- rbind(clip_Rent, selection_Rent, selectCentroids_Rent)

centers2 <- st_centroid(alloperations_Rent)
```

    ## Warning in st_centroid.sf(alloperations_Rent): st_centroid assumes attributes
    ## are constant over geometries of x

``` r
ggplot(tracts09_1) +
  geom_sf(data=st_union(tracts09_1), fill = "white") +
  geom_sf(data=tracts09_1, aes(fill = q5(TotalPop)), color = NA) +
  geom_sf(data=centers2, aes(size = MedHHInc), shape = 21,
          fill = "darkgreen", alpha = 0.7, show.legend = "point") +
  scale_size_continuous(range = c(1,4)) +
  scale_fill_manual(values = palette5,
                    labels = qBr(alloperations, "TotalPop"),
                    name = "Popluation\n(Quintile Breaks)") +
  labs(title = "Total Population", subtitle = "Seattle; 2009", caption = "Figure 13") +
  mapTheme() + theme(plot.title = element_text(size=22))
```

![](SeattleTOD_files/figure-gfm/Graduated%20Symbol%20Map-2.png)<!-- -->
