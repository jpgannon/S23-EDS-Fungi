library(ggplot2)
library(dplyr)
library(stringr)



merge_dat1 <- merge(finalgrowth_dat,finalsurv_dat)

merge_dat2 <- merge(merge_dat1, finalmstr_dat)


tag_id_dat$ID_new <- gsub('_','.',tag_id_dat$Tag.ID)
seedling_dat


merge_dat2$ID2 <- gsub('_','.',merge_dat2$Tag.ID)
merge_dat2$ID2 <- gsub(' ','.',merge_dat2$ID2)
summary(merge_dat2)

merge_dat2$leg_pos <- paste(merge_dat2$myc.legacy.type, merge_dat2$Position)

merge_dat3 <- merge(seedling_dat,merge_dat2, by ='ID2')

merge_dat3$PCG <- (((merge_dat3$HT.cm - merge_dat3$HT.cm.initial)/merge_dat3$HT.cm.initial)*100)

merge_dat3$PCG


#tag_id_dat$ID_new <- gsub('_','.',tag_id_dat$Tag.ID)

merge_dat4$myc_pos <- paste(merge_dat3$myc.species.type, merge_dat3$Position)

merge_dat4$myc_pos


merge_dat5 <- merge_dat4 %>% 
  rename(
    "Tree Species" = "Species.x",
     "Nitrogen isotope Ration" = "N15.corrected",
     "Real Growth (cm)" = "REALGrowth")

merge_dat5 <- merge_dat4


