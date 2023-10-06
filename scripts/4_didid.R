### Note that the estimated coefficient changes everytime you run it. 

# DIDID

# Code new trt variables
dat_merged2 <- dat_merged %<>% mutate(
  trt_hh = ifelse(uhc_2019>=75 & ghsi>=60, 2020, 0),
  trt_hl = ifelse(uhc_2019>=75 & ghsi<60, 2020, 0),
  trt_lh = ifelse(uhc_2019<75 & ghsi>=60, 2020, 0),
  ctr = ifelse(uhc_2019<75 & ghsi<60, 0, NA)
)


# Comparing vs. the control group (low UHC-low GHS)
res_didid <- data.frame(
  trt = rep(c("High UHC - High GHS",
          "High UHC - Low GHS",
          "Low UHC - High GHS"), each = 3),
  
  cat = rep(c("Overall", "2020", "2021"), 3),
  att = NA,
  min = NA,
  max = NA,
  pval = NA
)

res_didid_full <- data.frame(
  trt = rep(c("High UHC - High GHS",
              "High UHC - Low GHS",
              "Low UHC - High GHS"), each = 8),
  
  cat = rep(c("Overall", 2015:2021), 3),
  att = NA,
  min = NA,
  max = NA,
  pval = NA
)

# HH
dat_hh <- dat_merged2 %>% filter(trt_hh==2020 | ctr == 0)
temp_out <- att_gt(yname = "coverage",
                   gname = "trt_hh",
                   idname = "id_country_vac",
              
                  tname = "year",
                  xformla = ~ who_region + wb_income,
                         
                  data = dat_hh %>% filter(year>=2015) ,
                  est_method = "dr",
                  base_period = "universal")
  
temp_res <- summarize_did(temp_out)
res_didid[which(res_didid$trt == "High UHC - High GHS"),-1] <- temp_res

temp_res_full <- summarize_did_all(temp_out)
res_didid_full[which(res_didid_full$trt == "High UHC - High GHS"),-1] <- temp_res_full

# HL
dat_hl <- dat_merged2 %>% filter(trt_hl==2020 | ctr == 0)
temp_out <- att_gt(yname = "coverage",
                   gname = "trt_hl",
                   idname = "id_country_vac",
                   
                   tname = "year",
                   xformla = ~ who_region + wb_income,
                   
                   data = dat_hl %>% filter(year>=2015) ,
                   est_method = "dr",
                   base_period = "universal")

temp_res <- summarize_did(temp_out)
res_didid[which(res_didid$trt == "High UHC - Low GHS"),-1] <- temp_res

temp_res_full <- summarize_did_all(temp_out)
res_didid_full[which(res_didid_full$trt == "High UHC - Low GHS"),-1] <- temp_res_full

# LH
dat_lh <- dat_merged2 %>% filter(trt_lh==2020 | ctr == 0)
temp_out <- att_gt(yname = "coverage",
                   gname = "trt_lh",
                   idname = "id_country_vac",
                   
                   tname = "year",
                   xformla = ~ who_region + wb_income,
                   
                   data = dat_lh %>% filter(year>=2015) ,
                   est_method = "dr",
                   base_period = "universal")

temp_res <- summarize_did(temp_out)
res_didid[which(res_didid$trt == "Low UHC - High GHS"),-1] <- temp_res

temp_res_full <- summarize_did_all(temp_out)
res_didid_full[which(res_didid_full$trt == "Low UHC - High GHS"),-1] <- temp_res_full

### Plotting the results
p_didid <- ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  geom_errorbar(data = res_didid,
                aes(cat, ymin = min, ymax = max, color = cat), width = 0.2)+
  geom_point(data = res_didid,
             aes(cat, att, color = cat), size = 3)+
  scale_y_continuous(limits = c(-2, 6))+
  facet_wrap(.~trt, nrow = 2, scales = "free") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.line = element_line(),
        panel.border = element_rect(fill = NA, color = "black")) +
  xlab("") + ylab("Difference-in-difference (DID)\nCoefficient")

ggsave(here::here("results", "p_didid.png"),
       p_didid,
       width = 8,
       height = 7,
       dpi = 200)

res_didid_full %<>% mutate(
  post = ifelse(as.numeric(cat) %in% 2015:2019, 0, 1)
)

p_didid_full <- ggplot() + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_segment(data = res_didid_full %>% filter(cat == "overall"),
               aes(x = 2020, xend = 2021,
                   y = att, 
                   yend = att,
                   color = "ATT")) +
  geom_rect(data = res_didid_full %>% filter(cat == "overall"),
                aes(xmin = 2020, xmax = 2021,
                ymin = min, 
                ymax = max, 
                fill = "ATT"), alpha = 0.2)+
  geom_errorbar(data = res_didid_full %>% filter(cat!="overall"),
                aes(as.numeric(cat), ymin = min, ymax = max, color = as.character(post)), width = 0.5) +
  geom_point(data = res_didid_full %>% filter(cat!="overall"),
             aes(as.numeric(cat), (att), color = as.character(post))) +
  scale_x_continuous(breaks = seq(2015, 2021, by = 1))+
  scale_color_manual(
    values = c("0" = "#a6b7ed",
               "1" = "#213259",
               "ATT" = "#7391f0"),
    labels = c("Pre", "Post", "ATT")
  ) +
  scale_fill_manual(
    values = c("ATT" = "#7391f0")
  ) +
  guides(fill="none")+
  xlab("Year") + ylab("Difference-in-difference \nCoefficient") +
  facet_wrap(.~trt, scales = "free", nrow = 2)+
  scale_y_continuous(breaks = seq(-3, 5, by=1)) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.border = element_rect(fill = NA, color = "black"),
        strip.text = element_text(face = "bold")
        ) 
  

ggsave(here::here("results", "p_didid_full.png"),
       p_didid_full,
       width = 8,
       height = 7,
       dpi = 200)

rio::export(res_didid_full, here::here("results", "res_didid_full.xlsx"))
rio::export(res_didid, here::here("results", "res_didid.xlsx"))

