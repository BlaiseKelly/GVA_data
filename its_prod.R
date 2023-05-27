library(readxl)
library(tidyverse) 
library(stringr)
library(sf)
library(tmap)
library(cols4all)
library(reshape2)

## define url of xls location
url <- 'https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/labourproductivity/datasets/subregionalproductivitylabourproductivitygvaperhourworkedandgvaperfilledjobindicesbyuknuts2andnuts3subregions/current/itlproductivity.xls'
## find working directory and swap back slashes for forward
fold = str_replace_all(getwd(), '/', '\\\\')
## generate temp dir directory
fil = tempfile(pattern = "file", tmpdir = fold, fileext = '.xls')
##download the file from link
download.file(url, fil, mode = 'wb')
## import xls file
prod_dat <- readxl::read_excel(fil,
                      sheet = 'B1', skip = 4)

## Manually download ITL3 geojson file and import using st_read
ITL3_geo <- st_read("International_Territorial_Level_3_January_2021_UK_BGC_V3_2022_-3166856060823825625.geojson") %>% 
  select(ITL321CD)
ITL3_pd <- ITL3_geo %>% 
  left_join(prod_dat, by = c('ITL321CD' = 'ITL code')) ## join it to the data by ITL3 code
## define breaks
bks <- c(0,100 , max(ITL3$index...19))
## generate palette, preferably color blind safe to see options do
## cols4all::c4a_gui()
pal_mean <- cols4all::c4a('carto.safe',  NROW(bks))
## plot
tm_1 <- tm_shape(ITL3) +
  tm_polygons('index...19',palette = pal_mean,alpha = 0.6, border.col = 'white', breaks = bks,  title = 'GVA per hour')+
  tm_layout(legend.outside = FALSE, frame = FALSE, legend.outside.position = 'left', title.size = 1, 
            main.title = 'Where in the UK is productivity\nabove and below the\nnational average', 
            main.title.fontface = "bold",
            panel.label.color = 'white', panel.label.bg.color = 'black', title = 'GVA per hour worked\nby ITL3 region of Great Britain\n(100 is average)',
            bg.color = "white")+
  tm_credits("Source: ONS Subregional productivity: labour productivity indicies by UK ITL3 subregions", size = 4)+
  tm_legend(position = c("right", "centre"))

## save as png
tmap_save(tm_1, 'ITL3.png', width = 4500, height = 5500, dpi = 600)

## use melt function to collapse data frame
ITL3_down <- prod_dat %>% 
  select(-`ITL level`, -`Region name`) %>% 
  mutate(ITL_code = `ITL code`) %>% 
  select(-`ITL code`) %>% 
  melt('ITL_code') %>% #
  mutate(year = paste0('20', sprintf("%02d", as.numeric(gsub('index...', '', variable))))) %>% 
  select(-variable) %>% 
  left_join(ITL3_geo, by = c('ITL_code' = 'ITL321CD')) %>%
  select(-ITL_code)

## tell R which column is geometry
st_geometry(ITL3_down) <- ITL3_down$geometry
## remove empty geometries
ITL3_down = ITL3_down[!st_is_empty(ITL3_down),,drop=FALSE]

## define breaks
bks_all <- c(0,100 , max(ITL3_down$value))

tm_2 <- tm_shape(ITL3_down) +
  tm_polygons('value',palette = pal_mean,alpha = 0.6, border.col = 'white', breaks = bks_all,  title = 'GVA per hour')+
  tm_layout(legend.outside = FALSE, frame = FALSE, legend.outside.position = 'left', title.size = 1, panel.labels = unique(ITL3_down$year),
            main.title = 'Where in the UK is productivity\nabove and below the\nnational average', 
            main.title.fontface = "bold",
            panel.label.color = 'white', panel.label.bg.color = 'black', title = 'GVA per hour worked\nby ITL3 region of Great Britain\n(100 is average)',
            bg.color = "white")+
  tm_credits("Source: ONS Subregional productivity: labour productivity indicies by UK ITL3 subregions", size = 4)+
  tm_legend(position = c("right", "centre"))+
  tm_facets(by = 'year', nrow = 1, ncol = 1)

## save animation
tmap_animation(tm_2, filename = "ITL3.gif", delay = 60)



