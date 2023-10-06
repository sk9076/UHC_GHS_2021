### Note that the estimated coefficient changes everytime you run it. 

#UHC median 58
#GHS median 37

slide_trt_uhc <- seq(60, 90, by = 5)
slide_trt_ghs <- seq(40, 80, by = 5)

res_uhc_slide <- data.frame(
  cutoff = rep(slide_trt_uhc, each = 3),
  cat = rep(c("overall", "2020", "2021"), length(slide_trt_uhc)),
  att = NA,
  min = NA,
  max = NA,
  pval = NA
)

res_ghs_slide <- data.frame(
  cutoff = rep(slide_trt_ghs, each = 3),
  cat = rep(c("overall", "2020", "2021"), length(slide_trt_ghs)),
  att = NA,
  min = NA,
  max = NA,
  pval = NA
)

for(i in 1:length(slide_trt_uhc)){
  dat_merged2 <- dat_merged%>% mutate(
    trt_uhc = ifelse(uhc_2019>= slide_trt_uhc[i], 2020, 0)
  )
  
  temp_out_uhc <- att_gt(yname = "coverage",
                    gname = "trt_uhc",
                    idname = "id_country_vac",
                    
                    tname = "year",
                    xformla = ~ who_region + wb_income,
                    
                    data = dat_merged2 %>% filter(year>=2015) ,
                    est_method = "dr",
                    base_period = "universal")
  
 
  temp_res <- summarize_did(temp_out_uhc)
  
  res_uhc_slide[which(res_uhc_slide$cutoff == slide_trt_uhc[i]),-1] <- temp_res
  
}



for(i in 1:length(slide_trt_ghs)){
  dat_merged2 <- dat_merged%>% mutate(
    trt_ghs = ifelse(ghsi>= slide_trt_ghs[i], 2020, 0)
  )
  
  temp_out_ghs <- att_gt(yname = "coverage",
                         gname = "trt_ghs",
                         idname = "id_country_vac",
                         
                         tname = "year",
                         xformla = ~ who_region + wb_income,
                         
                         data = dat_merged2 %>% filter(year>=2015) ,
                         est_method = "dr",
                         base_period = "universal")
  
  
  temp_res <- summarize_did(temp_out_ghs)
  
  res_ghs_slide[which(res_ghs_slide$cutoff == slide_trt_ghs[i]),-1] <- temp_res
  
}

p_uhc_slide <- ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  geom_errorbar(data = res_uhc_slide %>% filter(cat == "overall"),
                aes(cutoff, ymin = min, ymax = max, color = "Overall"), width = 0.2)+
  geom_errorbar(data = res_uhc_slide %>% filter(cat == "2020"),
                aes(cutoff-0.3, ymin = min, ymax = max, color = "2020"), width = 0.2)+
  geom_errorbar(data = res_uhc_slide %>% filter(cat == "2021"),
                aes(cutoff+0.3, ymin = min, ymax = max, color = "2021"), width = 0.2)+
  geom_point(data = res_uhc_slide %>% filter(cat == "overall"),
             aes(cutoff, att, color = "Overall"), size = 3)+
  geom_point(data = res_uhc_slide %>% filter(cat == "2020"),
             aes(cutoff-0.3, att, color = "2020"))+
  geom_point(data = res_uhc_slide %>% filter(cat == "2021"),
             aes(cutoff+0.3, att, color = "2021")) +
  scale_x_continuous(breaks = seq(60,90, by = 5)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  xlab("UHC SCI 2019 (Range 0-100) Cut-off") + ylab("Difference-in-difference (DID)\nCoefficient")

p_ghs_slide <- ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  geom_errorbar(data = res_ghs_slide %>% filter(cat == "overall"),
                aes(cutoff, ymin = min, ymax = max, color = "Overall"), width = 0.2)+
  geom_errorbar(data = res_ghs_slide %>% filter(cat == "2020"),
                aes(cutoff-0.3, ymin = min, ymax = max, color = "2020"), width = 0.2)+
  geom_errorbar(data = res_ghs_slide %>% filter(cat == "2021"),
                aes(cutoff+0.3, ymin = min, ymax = max, color = "2021"), width = 0.2)+
  geom_point(data = res_ghs_slide %>% filter(cat == "overall"),
             aes(cutoff, att, color = "Overall"), size = 3)+
  geom_point(data = res_ghs_slide %>% filter(cat == "2020"),
             aes(cutoff-0.3, att, color = "2020"))+
  geom_point(data = res_ghs_slide %>% filter(cat == "2021"),
             aes(cutoff+0.3, att, color = "2021")) +
  scale_x_continuous(breaks = seq(40,80, by = 5)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  xlab("GHSI 2019 (Range 0-100) Cut-off") + ylab("Difference-in-difference (DID)\nCoefficient")

ggsave(here::here("results", "p_uhc_slide.png"),
       p_uhc_slide,
       width = 8,
       height = 3,
       dpi = 200)

ggsave(here::here("results", "p_ghs_slide.png"),
       p_ghs_slide,
       width = 8,
       height = 3,
       dpi = 200)

wb <- loadWorkbook(here::here("results", "result.xlsx"))
#addWorksheet(wb, "GHS_sliding scale")
writeData(wb, "GHS_sliding scale", res_ghs_slide)
#addWorksheet(wb, "UHC_sliding scale")
writeData(wb, "UHC_sliding scale", res_uhc_slide)
saveWorkbook(wb, here::here("results", "result.xlsx"), overwrite = T)
