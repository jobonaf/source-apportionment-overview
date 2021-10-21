# read municipalities
mun <- rgdal::readOGR("/lustre/arpa/bonafeg/data/geo/LimitiAmministrativi/Limiti_2018_WGS84_UTM32N/Com01012018_g_WGS84.shp")
mun[mun@data$COD_REG==6,] -> mun
mun <- sp::spTransform(mun, CRS("+init=epsg:32633"))

# read annual kriging
library(raster)
library(glue)
library(futile.logger)
years <- 2014:2019
polls <- c("PM10","PM25","NO2")
Dat <- NULL
for (yy in years) {
  for (pp in polls) {
    ff <- Sys.glob(glue("data/{pp}_{yy}_CivilYear_Avg*asc"))
    flog.info(glue("Reading {ff}"))
    r <- raster(ff)
    crs(r) <- "+init=epsg:32633"
    names(r) <- "Value"
    dat <- as_tibble(rasterToPoints(r)) %>% 
      left_join(as_tibble(rasterToPoints(rasterize(mun,r,"COMUNE")))) %>%
      mutate(Municipality=levels(mun@data$COMUNE)[layer]) %>%
      filter(!is.na(Municipality)) %>%
      mutate(Year=yy, Pollutant=ifelse(pp=="PM25","PM2.5",pp), layer=NULL)
    Dat <- bind_rows(Dat,dat)
  }
}

# percentiles in municipalities
library(purrr)
library(tidyr)
p <- (0:10)/10
p_names <- map_dbl(p, ~.x*100)
p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = p_names)
Dat %>%
  group_by(Pollutant,Municipality) %>%
  summarize_at(vars(Value), funs(!!!p_funs), .groups="drop") %>%
  gather(Percentile,Value,-Pollutant:-Municipality) -> Dat

# spatial SA
library(readr)
spatSA <- read_csv("data/SpatialSourceApp_MunicFVG.csv") %>%
  transmute(Municipality,SpatialSA=Potency,Origin,Pollutant) %>%
  group_by(Municipality,Pollutant) %>%
  mutate(altrove=100-sum(SpatialSA)) %>%
  spread(Origin,SpatialSA) %>%
  gather(Origin,SpatialSA,-Pollutant,-Municipality)
SpatSA <- Dat %>% left_join(spatSA) %>%
  mutate(Share=Value*SpatialSA*0.01)%>%
  ungroup()

# sectoral SA
sectSA <- read_csv("data/SectorSourceApp_MunicFVG.csv")%>%
  filter(Index=="yMean") %>%
  transmute(Municipality,SectoralSA=RegionalImpact*100,Sector=SectorName,
            Pollutant=ifelse(Pollutant=="PM25","PM2.5",Pollutant))
SectSA <- SpatSA %>% filter(Origin=="Friuli - Venezia Giulia") %>% 
  select(Pollutant,Municipality,Percentile,Value,SpatialSA) %>%
  left_join(sectSA) %>%
  mutate(Share=Value*SpatialSA*0.01*SectoralSA*0.01,
         extraregionale=Value*(1-SpatialSA*0.01)) %>%
  select(Pollutant,Municipality,Percentile,Sector,extraregionale,Share) %>%
  filter(!is.na(Sector)) %>%
  spread(Sector,Share) %>%
  gather(Sector,Share,-Pollutant,-Municipality,-Percentile) %>%
  ungroup()

# save
save(SpatSA, SectSA, file = "data/OverviewSourceApp_MunicFVG.rda")
