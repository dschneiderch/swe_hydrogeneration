#' download SNODAS

library(tidyverse)
library(R.utils)

#' # set output location
dir_conus <- 'data/snodas/swe/conus'
dir.create(dir_conus,rec=T,showWarnings = F)

#' # run through the years 
for(yr in 2004:2012){
  mth=1:12
  dte=as.Date(paste(yr,mth,'01',sep='-')) #only the 1st of the month
  mth_str <- strftime(dte,'%m_%b')
  fn <- paste0('SNODAS_',strftime(dte,'%Y%m%01'),'.tar')
  urlfn <- file.path('ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/masked',yr,mth_str,fn)
  
  dir_snodas <- 'data/snodas/downloads'
  dir.create(dir_snodas,rec=T,showWarnings = F)
  
  #' # iterate through each url. each month-yr is separate
  purrr::walk(urlfn,function(dl_fn){
    myfile=file.path(dir_snodas,basename(dl_fn))
    print(myfile)
    if(!file.exists(myfile)){ # don't duplicate download
      tryCatch({
        download.file(url = dl_fn, destfile = myfile, method = 'auto')},
        error = function(e){
          cat("ERROR :",conditionMessage(e), "\n")
        })
    }
    
    #' # make sure the .tar file exists before proceeding witn untar and ungzip
    if(file.exists(myfile)){ #need to check if file exists again because it may not exist on the server. the above check is so it doesn't redownload.
      #' untar to unique folder
      dir_nm <- file.path(dir_snodas,stringr::str_split_fixed(basename(dl_fn),fixed('.'),2)[,1])
      untar(myfile,
            exdir=dir_nm) 
      
      #' # select gzip swe files
      dir_files <- dir(dir_nm,full.names = T)
      swe_ind <- grep(glob2rx('*1034*.gz$'),dir_files) #1034 is code for SWE
      swe_files <- dir_files[swe_ind]
      
      #' # ungzip each set of swe files .hdr and .dat and save to new swe folder
      purrr::walk(swe_files,function(f){
        fn_uncom <- sub('\\.gz$', '', f)
        if(file.exists(fn_uncom)) file.remove(fn_uncom)
        R.utils::gunzip(f,
                        remove = F) %>% 
          file.copy(file.path(dir_conus,basename(fn_uncom)))
        # file.remove(fn_uncom)
      })
      unlink(dir_nm,rec=T) #save space. only keep original .tar
    }
  })
}





