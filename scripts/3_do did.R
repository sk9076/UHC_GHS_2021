### Note that the estimated coefficient changes everytime you run it. 

# This is adjusted based on the results from script #2
dat_merged %<>% mutate(
  trt_uhc = ifelse(uhc_2019>=75, 2020, 0),
  trt_ghsi = ifelse(ghsi>=60, 2020, 0)
)

## vaccine not aggregated
out_uhc <- att_gt(yname = "coverage",
                  gname = "trt_uhc",
                  idname = "id_country_vac",
                  
                  tname = "year",
                  xformla = ~ who_region + wb_income,
                  
                  data = dat_merged %>% filter(year>=2015) ,
                  est_method = "dr",
                  base_period = "universal")

out_uhc
es_uhc <- aggte(out_uhc, type = "dynamic") 
es_uhc

p_es_uhc <- cbind.data.frame(year=es_uhc$egt +2020,
                             att=es_uhc$att.egt,
                             min_95 = es_uhc$att.egt - es_uhc$se.egt*es_uhc$crit.val.egt,
                             max_95 = es_uhc$att.egt + es_uhc$se.egt*es_uhc$crit.val.egt,
                             post=as.factor(1*(es_uhc$egt >= 0)))


group_effects_uhc <- aggte(out_uhc, type = "group")
summary(group_effects_uhc)

overall.att_uhc <- (group_effects_uhc$att.egt)
overall_min_95_uhc <- (group_effects_uhc$att.egt - group_effects_uhc$se.egt*group_effects_uhc$crit.val.egt)
overall_max_95_uhc <- (group_effects_uhc$att.egt + group_effects_uhc$se.egt*group_effects_uhc$crit.val.egt)

res_list[["uhc"]] <- data.frame(year = "overall", 
                                att = overall.att_uhc, 
                                min_95 = overall_min_95_uhc, 
                                max_95 = overall_max_95_uhc) %>% 
  rbind(p_es_uhc %>% select(-post))

p_did_uhc <- 
  ggplot(p_es_uhc) + 
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  geom_segment(aes(x = 2020, xend = 2021,
                   y = overall.att_uhc, 
                   yend = overall.att_uhc,
                   color = "ATT")) +
  geom_rect(aes(xmin = 2020, xmax = 2021,
                ymin = overall_min_95_uhc, 
                ymax = overall_max_95_uhc, 
                fill = "ATT"), alpha = 0.01)+
  geom_errorbar(aes(year, ymin = min_95, ymax = max_95, color = post), width = 0.5) +
  geom_point(aes(year, (att), color = post)) +
  scale_x_continuous(breaks = seq(2015, 2021, by=1)) +
  scale_color_manual(
    values = c("0" = "#a6b7ed",
               "1" = "#213259",
               "ATT" = "#7391f0"),
    labels = c("Pre", "Post", "ATT")
  ) +
  scale_fill_manual(
    values = c("ATT" = "#7391f0")
  ) +
  theme(legend.title = element_blank()) +
  guides(fill="none")+
  xlab("Week") + ylab("Difference-in-difference \nCoefficient")+
  ggtitle("(A) UHC (UHC SCI 2019 \u2265 75)")


out_ghs <- att_gt(yname = "coverage",
                  gname = "trt_ghsi",
                  idname = "id_country_vac",
                  tname = "year",
                  xformla = ~ who_region + wb_income,
                  
                  #allow_unbalanced_panel = T,
                  data = dat_merged %>% filter(year >=2015),
                  est_method = "dr",
                  base_period = "universal")

out_ghs
es_ghs <- aggte(out_ghs, type = "dynamic") 
es_ghs

p_es_ghs <- cbind.data.frame(year=es_ghs$egt +2020,
                             att=es_ghs$att.egt,
                             min_95 = es_ghs$att.egt - es_ghs$se.egt*es_ghs$crit.val.egt,
                             max_95 = es_ghs$att.egt + es_ghs$se.egt*es_ghs$crit.val.egt,
                             post=as.factor(1*(es_ghs$egt >= 0)))


group_effects_ghs <- aggte(out_ghs, type = "group")
summary(group_effects_ghs)

overall.att_ghs <- (group_effects_ghs$att.egt)
overall_min_95_ghs <- (group_effects_ghs$att.egt - group_effects_ghs$se.egt*group_effects_ghs$crit.val.egt)
overall_max_95_ghs <- (group_effects_ghs$att.egt + group_effects_ghs$se.egt*group_effects_ghs$crit.val.egt)

res_list[["ghs"]] <- data.frame(year = "overall", 
                                att = overall.att_ghs, 
                                min_95 = overall_min_95_ghs, 
                                max_95 = overall_max_95_ghs) %>% 
  rbind(p_es_ghs %>% select(-post))


p_did_ghs <- 
  ggplot(p_es_ghs) + 
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  geom_segment(aes(x = 2020, xend = 2021,
                   y = overall.att_ghs, 
                   yend = overall.att_ghs,
                   color = "ATT")) +
  geom_rect(aes(xmin = 2020, xmax = 2021,
                ymin = overall_min_95_ghs, 
                ymax = overall_max_95_ghs, 
                fill = "ATT"), alpha = 0.01)+
  geom_errorbar(aes(year, ymin = min_95, ymax = max_95, color = post), width = 0.5) +
  geom_point(aes(year, (att), color = post)) +
  scale_x_continuous(breaks = seq(2015, 2021, by=1)) +
  scale_color_manual(
    values = c("0" = "#a6b7ed",
               "1" = "#213259",
               "ATT" = "#7391f0"),
    labels = c("Pre", "Post", "ATT")
  ) +
  scale_fill_manual(
    values = c("ATT" = "#7391f0")
  ) +
  theme(legend.title = element_blank()) +
  guides(fill="none")+
  xlab("Year") + ylab("Difference-in-difference \nCoefficient")+
  ggtitle("(B) GHS (GHSI \u2265 60)")

ggsave(here::here("results", "p_did_uhc.png"),
       p_did_uhc,
       width = 8,
       height = 4,
       dpi = 200)

ggsave(here::here("results", "p_did_ghs.png"),
       p_did_ghs,
       width = 8,
       height = 4,
       dpi = 200)

ggsave(here::here("results", "p_did_combined.png"),
  ggpubr::ggarrange(p_did_uhc, p_did_ghs, nrow = 2),
  width = 8,
  height = 8,
  dpi=200)

res_list %<>% lapply(function(x){
  x %<>% mutate_at(vars(att, min_95, max_95), function(y){round(y, 3)})
})

wb <- createWorkbook()
addWorksheet(wb, 
             sheetName = "UHC_75")
addWorksheet(wb, 
             sheetName = "GHS_60")
writeData(wb, sheet = "UHC_75", res_list[["uhc"]], colNames=T)
writeData(wb, sheet = "GHS_60", res_list[["ghs"]], colNames=T)
saveWorkbook(wb, here::here("results", "result.xlsx"), overwrite = T)
