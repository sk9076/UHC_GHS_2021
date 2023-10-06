pacman::p_load(dplyr, tidyverse, ggplot2, ggthemr, rio, here, fixest, gridExtra, grid,
               magrittr, lfe, HonestDiD, bacondecomp, haven, plotmath,
               openxlsx)

summarize_did <- function(out){
  es <- aggte(out, type = "dynamic") 
  
  temp <- cbind.data.frame(year=es$egt +2020,
                   att=es$att.egt,
                   min_95 = es$att.egt - es$se.egt*es$crit.val.egt,
                   max_95 = es$att.egt + es$se.egt*es$crit.val.egt)
  
  grp <- aggte(out, type = "group")
  
  overall.att <- (grp$att.egt)
  overall_min_95 <- (grp$att.egt - grp$se.egt*grp$crit.val.egt)
  overall_max_95 <- (grp$att.egt + grp$se.egt*grp$crit.val.egt)
  
  data.frame(year = "overall", 
             att = overall.att, 
             min_95 = overall_min_95, 
             max_95 = overall_max_95) %>% 
    rbind(temp %>% filter(year %in% c(2020, 2021))) %>% mutate(
      pval = out$Wpval
    )
}

summarize_did_all <- function(out){
  es <- aggte(out, type = "dynamic") 
  
  temp <- cbind.data.frame(year=es$egt +2020,
                           att=es$att.egt,
                           min_95 = es$att.egt - es$se.egt*es$crit.val.egt,
                           max_95 = es$att.egt + es$se.egt*es$crit.val.egt)
  
  grp <- aggte(out, type = "group")
  
  overall.att <- (grp$att.egt)
  overall_min_95 <- (grp$att.egt - grp$se.egt*grp$crit.val.egt)
  overall_max_95 <- (grp$att.egt + grp$se.egt*grp$crit.val.egt)
  
  data.frame(year = "overall", 
             att = overall.att, 
             min_95 = overall_min_95, 
             max_95 = overall_max_95) %>% 
    rbind(temp) %>% mutate(
      pval = out$Wpval
    )
}
