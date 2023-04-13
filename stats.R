## Warehouse summary stats
## Created by Mike McCarthy, Radical Research LLC
## Created March 2023
## Last Modified April 2023

library(sf)
library(tidyverse)
library(leaflet)

##Import full dataset of existing for basic stats

wh_28k <- st_read(dsn = 'C:/Dev/WarehouseMap/exports_other/final_parcels_28k.geojson') %>% 
  filter(county %in% c('San Bernardino', 'Riverside'))

plannedWH.url <- 'https://raw.githubusercontent.com/RadicalResearchLLC/PlannedWarehouses/main/plannedWarehouses.geojson'
plannedWarehouses <- st_read(plannedWH.url) %>% 
  st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84") 

wh_nogeom <- wh_28k %>% 
  st_set_geometry(value = NULL) #%>% 

warehouse_stats <- wh_nogeom %>% 
  group_by(county) %>% 
  summarize(count = n(), footprint = sum(shape_area))

wh_nogeom %>% 
  ggplot(aes(x = floorSpace.sq.ft, fill = county)) + 
  geom_histogram(bins = 50) +
  theme_bw() +
  facet_wrap(~county) +
  scale_x_continuous(labels = scales::label_comma()) +
  labs(x = 'Warehouse footprint', y = '# of warehouses',
       title = 'Warehouse Counts by county - all sizes') +
  theme(legend.position = 'none')

wh_nogeom %>% 
  filter(floorSpace.sq.ft >= 100000) %>%
  filter(county != 'Orange') %>% 
  ggplot(aes(x = floorSpace.sq.ft, fill = county)) + 
  geom_histogram(bins = 50, color = 'grey30') +
  theme_bw() +
  facet_wrap(~county, nrow = 3) +
  scale_x_log10(labels = scales::label_comma()) +
  labs(x = 'Warehouse size (sq.ft.)', y = '# of warehouses',
       title = 'Warehouse counts') +
  theme(legend.position = 'none')

wh_nogeom2 <- wh_nogeom %>% 
  mutate(size_bin2 = case_when(
    floorSpace.sq.ft < 100000 ~ 'Less than 100,000 sq.ft.',
    floorSpace.sq.ft >= 400000 ~ 'More than 400,000 sq.ft.',
    TRUE ~ 'Between 100,000 and 400,000 sq.ft.'
  )) %>% 
  group_by(size_bin2, county) %>% 
  summarize(count = n(), area = sum(shape_area), .groups = 'drop')

shapeArea <- st_area(plannedWarehouses) 

planned_nogeom <- plannedWarehouses %>% 
  mutate(category = 'Planned or Approved',
         area =  round(as.numeric(10.764*shapeArea), -3)) %>% 
  mutate(floorSpace.sq.ft = 0.55*area) %>% 
  mutate(year_built = 2025) %>% 
  rename(apn = name,
         shape_area = area) %>% 
  st_set_geometry(value = NULL)

planned_tidy1 <- plannedWarehouses %>% 
  mutate(category = 'Planned or Approved',
         area =  round(as.numeric(10.764*shapeArea), -3)) %>% 
  mutate(floorSpace.sq.ft = 0.55*area) %>% 
  mutate(year_built = 2025) %>% 
  rename(apn = name,
         shape_area = area) %>% 
  filter(shape_area > 153840)

WH_100k <- wh_28k %>% 
  filter(shape_area > 153840) %>% 
  select(apn, year_built, shape_area, floorSpace.sq.ft,
         geometry) %>% 
  mutate(category = 'Existing') %>% 
  bind_rows(planned_tidy1)

WH_400k <- WH_100k %>% 
  filter(shape_area> 615000)

wh_nogeom_all <- wh_nogeom %>% 
  select(apn, shape_area, floorSpace.sq.ft, year_built) %>% 
  mutate(category = 'Existing') %>% 
  bind_rows(planned_nogeom)

rm(ls = wh_nogeom, planned_nogeom, wh_nogeom2, wh_28k, warehouse_stats, plannedWarehouses, planned_tidy1)

AB1000_buffer <- WH_100k %>% 
  st_buffer(dist = 304.8) 

AB1748_buffer <- WH_100k %>% 
  filter(shape_area > 600000) %>% 
  st_buffer(dist = 91.44)  

schools <- sf::st_read(dsn = 'C:/Dev/SchoolWarehouse/CSCD_2021.gdb', quiet = TRUE, type = 3) %>%
  # clean_names() %>% 
  filter(County %in% c('Riverside', 'San Bernardino')) %>% 
  st_transform(crs = 4326) %>%
  filter(CDSCode != 33670330131607) %>% 
  filter(CDSCode != 33672310114066) %>% 
  filter(Status == 'Active') %>% 
  select(School, District, City, GradesServed, Level, Shape) %>% 
  st_make_valid()  

schoolsNearWH <- AB1000_buffer %>% 
  st_join(schools, left = FALSE) %>% 
  st_set_geometry(value = NULL) %>% 
  group_by(School,District,City, Level, GradesServed) %>% 
  summarize(count = n(), footprint = sum(shape_area), .groups = 'drop') %>%
  arrange(desc(footprint)) %>% 
  mutate(AB1000 = TRUE)

SchoolsNearWH1000 <- schoolsNearWH %>% 
  left_join(schools) %>%  
  st_as_sf() %>% 
  distinct()

SchoolsNear3 <- AB1748_buffer %>% 
  st_join(schools, left = FALSE) %>% 
  st_set_geometry(value = NULL) %>% 
  group_by(School,District,City, Level, GradesServed) %>% 
  summarize(count = n(), footprint = sum(shape_area), .groups = 'drop') %>%
  arrange(desc(footprint)) %>% 
  mutate(AB1748 = TRUE) 

narrow_schools1000 <- schoolsNearWH %>% 
  select(School, District, AB1000)
narrow_schools1748 <- SchoolsNear3 %>% 
  select(School, District, AB1748)

narrow_schools <- full_join(narrow_schools1000, narrow_schools1748) %>% 
  mutate(legis = ifelse(is.na(AB1748), 'AB1000', 'AB1748')) %>% 
  inner_join(schools) %>%  
  st_as_sf() %>% 
  distinct()

rm(ls = schools, SchoolsNear3, schoolsNearWH,
   narrow_schools1000, narrow_schools1748, SchoolsNearWH1000)

wd <- getwd()
setwd(paste0(wd, '/WarehouseAssemblyBill'))
save.image('.RData')

palSchool <-colorFactor(palette = c('blue', 'darkblue'), domain = narrow_schools$legis)
leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = WH_100k,
              color = 'red',
              stroke = FALSE) %>% 
  addPolygons(data = narrow_schools,
              color = ~palSchool(legis),
              weight = 1,
              fillOpacity = 0.5)
