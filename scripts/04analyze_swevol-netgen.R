#' # Analyze monthly power generation and snowpack water content
#' 

library(raster)
library(tidyverse)
library(lubridate)
library(caret)

#' # import snodas swe 
#'

swe_fns <- list.files('data/snodas/swe/tuo',glob2rx('^masked*.tif'),full.names=T)
swe_stack <- raster::stack(swe_fns)

swe_sum <- cellStats(swe_stack,'sum',na.rm=T)
swe_vol <- 
  swe_sum %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column() %>%
  as_tibble() %>% 
  setNames(c('dte','swe.vol')) %>% 
  mutate(swe.vol.km3=swe.vol*res(swe_stack)[1]*res(swe_stack)[2]/1000^3) %>% 
  separate(dte, into=c('a','b','basin','dte')) %>% 
  dplyr::select(-a,-b,-swe.vol) %>% 
  mutate(dte=lubridate::ymd(dte))


#' # import netgen

netgen <- readRDS('data/energy_information/netgen_2006-2017.rds')

#' # combine swe vol and netgen

swe_netgen <- right_join(swe_vol,netgen) %>% 
  mutate(
    wyr=case_when(#it's important to use water year, since snow accumulation begins in fall of the previous year
      mnth < 'Oct' ~ yr,
      mnth >= 'Oct' ~ yr + as.integer(1)
    )
  ) %>% 
  filter(wyr > 2006, wyr < 2018) %>%  #removing 2006 because march 1 snodas isn't available. critical time! in the future you could replace with Mar 2nd
  mutate(wyrmnth = factor(mnth,month.abb[c(10:12,1:9)]))

# Oct 1 2006 is also missing but since Nov 1 had 0 swe we can assume oct 1 had 0 swe
swe_netgen$basin[1] <- 'tuo'
swe_netgen$swe.vol.km3[1] <- 0

# View(swe_netgen)

#' ## plot timeseries of netgen and swe vol

#+ fig.width=16
swe_netgen %>% 
  mutate(swe2plot=swe.vol.km3*30000) %>% 
  gather(var,val,netgen.MWh,swe2plot) %>% 
  ggplot()+
  geom_path(aes(x = dte,y=val,color=var))+
  labs(y='Generated Electrity (MWh)')+
  scale_x_date(date_breaks='3 months')+
  scale_y_continuous(sec.axis = sec_axis(trans=~./30000,name='SWE Volume (cu. km)'))+
  scale_color_brewer(name='',
                     palette='Set1',
                     labels=c('Generated Electricity (MWh)','SWE Volume (cu. km)'))+
  theme(axis.text.x.bottom = element_text(angle=90,vjust=.5),
        axis.line.y.left = element_line(color=RColorBrewer::brewer.pal(3,'Set1')[1]),
        axis.line.y.right = element_line(color=RColorBrewer::brewer.pal(3,'Set1')[2]))

#+ include=F
# gts_netgen <- ggplot(swe_netgen)+
#   geom_path(aes(x = dte,y=netgen.MWh))+
#   scale_x_date(date_breaks='3 months')+
#   theme(axis.text.x.bottom = element_text(angle=90,vjust=.5),
#         axis.title.x=element_blank())
# gts_swe <- ggplot(swe_netgen)+
#   geom_path(aes(x = dte,y=swe.vol.km3))+
#   theme(axis.ticks.x.top = element_line(color='black'))
# gts_stacked <- cowplot::plot_grid(gts_netgen,gts_swe,
#                    nrow=2,
#                    rel_heights = c(.6,.4),
#                    align='v')
# gts_stacked

#' # monthly trends in netgen 
#+ fig.width=12, fig.height=8
ggplot(swe_netgen)+
  geom_path(aes(x=wyrmnth,y=netgen.MWh,group=wyr))+
  geom_point(aes(x=wyrmnth,y=netgen.MWh))+
  scale_color_viridis_d(option = 'A')+
  facet_wrap(.~wyr,scales='fixed')


#' ## are leads in netgen correlated with swe?
swe_netgen_leads <- 
  swe_netgen %>% 
  mutate(netgen.MWh_lead1 = lead(netgen.MWh,1),
         netgen.MWh_lead2 = lead(netgen.MWh,2),
         netgen.MWh_lead3 = lead(netgen.MWh,3)) %>% 
  gather(timelag,netgen,contains('netgen'))

#' 
#+ fig.width=18, fig.height=10
ggplot(swe_netgen_leads)+
  geom_point(aes(x=swe.vol.km3,y=netgen,colour=wyrmnth))+
  geom_path(aes(x=swe.vol.km3,y=netgen))+
  scale_color_viridis_d(option = 'A')+
  facet_grid(timelag~wyr,scales='fixed')



#' # does melt from previous month correlate with netgen?

swe_netgen_melt <- 
  swe_netgen %>% 
  mutate(melt_1mth=swe.vol.km3-lag(swe.vol.km3,1))

# avoid missing data, inflow is 0 for oct 2006
swe_netgen_melt$melt_1mth[1] = 0

#+ fig.width=12, fig.height=8 
ggplot(swe_netgen_melt)+
  # geom_path(aes(x=melt_1mth,y=netgen.MWh))+
  geom_point(aes(x=melt_1mth,y=netgen.MWh,color=wyrmnth))+
  facet_wrap(.~wyr,scales='fixed')+
  scale_color_viridis_d(option = 'A')

#' # model hydropower generation using snow vol and month

run_model <- function(train_df,test_df,preproc,modeltype){
  mdl_eval <- train(netgen.MWh ~ swe.vol.km3*melt_1mth + mnth, data = predict(preproc,newdata=train_df),
                    method = modeltype, 
                    trControl = fitControl)
  preds <- predict(mdl_eval,newdata=predict(preproc,newdata=test_df))
  train_stats <- postResample(pred=preds, obs = test_df$netgen.MWh)
  
  outdf <- as.data.frame(t(train_stats)) %>% 
    as_tibble() %>% 
    mutate(bootdata=list(data_frame(obs=test_df$netgen.MWh,pred=preds,mnth=factor(test_df$mnth,levels=c('Oct','Nov','Dec','Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep')))))
  
  return(outdf)
}

fitControl <- trainControl(
  method = "boot",
  number = 10)

preproc <- preProcess(swe_netgen_melt %>% select(swe.vol.km3,mnth,melt_1mth),
                      method='center','scale')

trainIndex <- createResample(swe_netgen_melt$netgen.MWh, times=10, list=TRUE)

calval_df <- map_df(.x=trainIndex,.id='bootsample',.f=function(splitInd){
  train_df=swe_netgen_melt[splitInd,]
  test_df=swe_netgen_melt[-splitInd,]
  run_model(train_df=train_df,test_df=test_df,preproc=preproc,modeltype='glmnet')
})

calval_df %>% 
  summarise_if(is.numeric,.funs = funs(mean,min,max))

calval_plotdf <- 
  calval_df %>% 
  select(bootsample,bootdata) %>% 
  unnest(bootdata) 

calval_plotdf %>% 
  summarise(r2=cor(obs,pred)^2,mae=mean(abs(obs-pred),na.rm=T))

ggplot(data=calval_plotdf, aes(x=obs,y=pred))+
  geom_point(aes(color=mnth))+
  geom_smooth(method='lm',color='black',se=T)+
  
  
  
  
