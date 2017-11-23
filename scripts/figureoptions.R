dodge <- position_dodge(.3)

colorvalAPA2 <-  c( "#404040","#ca0020","#bababa",  "#f4a582")
#404040 ## darkgrey - yoked-consistent
#bababa ## light grey - yoked-conflict
#ca0020 ## red - consistent 
#f4a582 ## pink - conflict

colorvalAPA4 <-  c( "#bababa","#f4a582")
#bababa ## light grey - yoked-conflict
#f4a582 ## pink - conflict

colorvalAPA5 <-  c( "#404040","#ca0020")
#404040 ## darkgrey - yoked-consistent
#ca0020 ## red - consistent 

colorvalAPA6 <-  c( "#f4a582","#ca0020")
#f4a582 ## pink - conflict
#ca0020 ## red - consistent 

colorvalAPA7 <-  c( "#bababa","#404040")
#bababa ## light grey - yoked-conflict
#404040 ## darkgrey - yoked-consistent

colorvalAPA8 <-  c("WT" = "#bf5700", "FMR1KO" = "#54278f", "FMR1KO_conflict" = "#f4a582","WT_conflict" = "#f4a582",  "FMR1KO_yoked-conflict" = "#bababa","WT_yoked-conflict" = "#bababa" )

colorvalAPA9 <-  c("conflict" = "#f4a582", "yoked-conflict" = "#bababa", "FMR1KO_conflict" = "#54278f","WT_conflict" = "#bf5700",  "FMR1KO_yoked-conflict" = "#54278f","WT_yoked-conflict" = "#bf5700" )

colorvalAPA10 <-  c("WT" = "#bf5700", "FMR1KO" = "#54278f", "FMR1KO_consistent" = "#ca0020","WT_consistent" = "#ca0020",  "FMR1KO_yoked-consistent" = "#404040","WT_yoked-consistent" = "#404040" )

colorvalAPA11 <-  c("WT_consistent" = "#ca0020", "FMR1KO_consistent" = "white",
                   "WT_yoked-consistent" = "#404040", "FMR1KO_yoked-consistent" = "white",
                   "WT_conflict" = "#f4a582",   "FMR1KO_conflict" = "white",  
                   "WT_yoked-conflict" = "#bababa", "FMR1KO_yoked_conflict" = "white" )

colorvalAPA00 <-  c( "#404040","#ca0020", "#bababa", "#f4a582")
#404040 ## darkgrey - Yoked_NoConflict
#ca0020 ## red - Trained_NoConflict 
#bababa ## light grey - Yoked_Conflict
#f4a582 ## pink - Trained_Conflict

ann_colorsGenotype = list(
  Genotype =  c('FMR1' = (values=c("white")), 
            'WT' = (values=c("#404040"))))

ann_colorsdaytime2 = list(
  daytime2 =  c('beforenoon' = (values=c("orange")), 
                'afternoon' = (values=c("red"))))

ann_colorsdaytime3 = list(
  daytime3 =  c('daytime' = (values=c("orange")), 
                'nighttime' = (values=c("blue"))))

ann_colorsdaytime = list(
  daytime =  c('afternoon' = (values=c("red")), 
                'beforenoon' = (values=c("orange")),
               'evening' = (values=c("blue")), 
               'nighttime' = (values=c("black"))))

ann_colorsall = list(
  daytime =  c('afternoon' = (values=c("red")), 
               'beforenoon' = (values=c("orange")),
               'evening' = (values=c("blue")), 
               'nighttime' = (values=c("black"))),
  Genotype =  c('FMR1' = (values=c("white")), 
                'WT' = (values=c("#404040"))))

ann_colorsdaytime3frm1 = list(
  Genotype =  c('FMR1' = (values=c("white")), 
                'WT' = (values=c("#404040"))),
  daytime3 =  c('daytime' = (values=c("orange")), 
                'nighttime' = (values=c("blue"))))
