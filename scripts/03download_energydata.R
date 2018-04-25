#' download energy generation data from eia.gov

library(tidyverse)
library(lubridate)
library(readxl)

#gis files 'https://nhaap.ornl.gov/sites/default/files/data_repo/EHA/ORNL_EHAHydroPlantV1_FY18Q2.zip'

#' # list of files needed for the analysis
energy_urls <- c(
  'https://www.eia.gov/electricity/data/eia923/archive/xls/f906920_2006.zip',
  'https://www.eia.gov/electricity/data/eia923/archive/xls/f906920_2007.zip',
  'https://www.eia.gov/electricity/data/eia923/archive/xls/f923_2008.zip',
  'https://www.eia.gov/electricity/data/eia923/archive/xls/f923_2009.zip',
  'https://www.eia.gov/electricity/data/eia923/archive/xls/f923_2010.zip',
  'https://www.eia.gov/electricity/data/eia923/archive/xls/f923_2011.zip',
  'https://www.eia.gov/electricity/data/eia923/archive/xls/f923_2012.zip',
  'https://www.eia.gov/electricity/data/eia923/archive/xls/f923_2013.zip',
  'https://www.eia.gov/electricity/data/eia923/archive/xls/f923_2014.zip',
  'https://www.eia.gov/electricity/data/eia923/archive/xls/f923_2015.zip',
  'https://www.eia.gov/electricity/data/eia923/archive/xls/f923_2016.zip',
  'https://www.eia.gov/electricity/data/eia923/xls/f923_2017.zip')

#' # setup directories
dir_energy <- 'data/energy_information'
dir_dl <- file.path(dir_energy,'downloads')
dir.create(dir_dl,rec=T)

#' # download and unzip files from eia.gov
u=energy_urls[1]
purrr::walk(energy_urls,function(u){
  fn_energy <- basename(u)
  str_fn <- str_split(fn_energy,pattern='[_.]',simplify=T)
  fn_xls <- str_fn[1]
  yr <- str_fn[2]
  myfile <- file.path(dir_dl,fn_energy)
  if(!file.exists(myfile)){
    download.file(u,destfile=myfile,method='auto')
  }
  unzip(myfile,exdir=file.path(dir_energy,yr))
  # unlink(file.path(dir_energy,yr),rec=T)
})

#' # list of excel files for each year from each zipfile
energy_excel <- c(
  'f906920_2006.xls',
  'f906920_2007.xls',
  'eia923December2008.xls',
  'EIA923 SCHEDULES 2_3_4_5 M Final 2009 REVISED 05252011.XLS',
  'EIA923 SCHEDULES 2_3_4_5 Final 2010.xls',
  'EIA923_Schedules_2_3_4_5_2011_Final_Revision.xlsx',
  'EIA923_Schedules_2_3_4_5_M_12_2012_Final_Revision.xlsx',
  'EIA923_Schedules_2_3_4_5_2013_Final_Revision.xlsx',
  'EIA923_Schedules_2_3_4_5_M_12_2014_Final_Revision.xlsx',
  'EIA923_Schedules_2_3_4_5_M_12_2015_Final_Revision.xlsx',
  'EIA923_Schedules_2_3_4_5_M_12_2016_Final_Revision.xlsx',
  'EIA923_Schedules_2_3_4_5_M_12_2017_27FEB2018.xlsx'
)

#' ## years for the excel files
yrs <- 2006:2017

#' # import the excel files into a dataframe
#' R C Kirkwood is the name of the hydropower plant at Hetchy Hetchy 
yr=yrs[1]
f=energy_excel[1]
xls_df <- 
  map2_df(yrs,energy_excel,function(yr,f){
    print(paste(yr, f, sep='/'))
    if(as.numeric(yr)<2011){
      skip_num=7
    } else {
      skip_num=5
    }
    readxl::read_excel(file.path(dir_energy,yr,f),skip=skip_num) %>% 
      mutate(yr=yr) %>% 
      dplyr::select(yr,dam = `Plant Name`, contains('netgen',ignore.case=TRUE)) %>% 
      filter(dam == 'R C Kirkwood') %>% 
      mutate_at(.vars=vars(contains('netgen',ignore.case=T)),.funs=funs(as.numeric(.))) %>% 
      gather(mnth,netgen,-dam,-yr) %>%
      separate(mnth,into=c('var','mnth'),remove=T) %>%
      select(-var)
    }) %>%   
mutate(mnth=lubridate::parse_date_time(mnth,orders = 'b!'), #slick lubridate magic!
         mnth=month(mnth,label=T),
         dte=lubridate::ymd(paste(yr,mnth,'01'))) %>% 
  rename(netgen.MWh=netgen) # add the units!

#' # save dataframe
saveRDS(xls_df,'data/energy_information/netgen_2006-2017.rds')
write_tsv(xls_df,'data/energy_information/netgen_2006-2017.txt')
