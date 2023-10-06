dat <- rio::import(here::here("data", "uhc_ghs_dataset.csv"))
ggthemr("fresh")

# Country id
id_n <- cbind(unique(dat$country), 1:length(unique(dat$country))) %>% data.frame()
colnames(id_n) <- c("country", "country_id")

id_n %<>% mutate(country_id = as.numeric(country_id))

# Country-vaccine id
id_n2 <- cbind(unique(dat[,c("country", "vaccine")]), 1:nrow(unique(dat[,c("country", "vaccine")])))
colnames(id_n2)[3] <- "id_country_vac"

dat_merged <- left_join(dat, id_n, by = "country")
dat_merged %<>% left_join(id_n2, by = c("country", "vaccine"))

cor(dat$coverage, dat$uhc_2019, use="na.or.complete")
cor(dat$ghsi, dat$uhc_2019, use="na.or.complete")
cor(dat$coverage, dat$gdp2019, use="na.or.complete")

# initialize list to save results
res_list <- list()

