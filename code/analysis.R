# Vessel power correction - R/V Solstice & R/V Resolution
# Large mesh trawl survey gear - Kodiak District
# prepared by:
# Ben Williams
# ben.williams@alaska.gov
# for
# Kally Spalinger
# kally.spalinger@alaska.gov
# 2019-01


# load ----
source('code/helper.r')
source('code/functions.r')

# data ----
# received on 2019-01-04 as "side_by_side_cpue_with_temps.xlsx"
# converted to "kodiak_vessel_power.csv" for easier import

read_csv('data/kodiak_vessel_power.csv') %>% 
  mutate(Vessel = factor(vessel_id),
         date = mdy(tow_date),
         year = year(date),
         Year = factor(year),
         Primary = factor(primary),
         Perf = factor(gear_perf),
         Station = factor(station),
         id = group_indices(., station)) %>% 
  dplyr::select(Vessel, juv_fem:pollock, tow) %>%
  gather(species, cpue, -tow, -Vessel) %>% 
  mutate(id = paste0(substring(tow, 1, 2), substring(tow, 7, 8))) %>% 
  dplyr::select(-tow) %>% 
  group_by(id, species) %>% 
  spread(Vessel, cpue) %>% 
  drop_na %>% 
  ungroup %>% 
  dplyr::select(species, res = `30`, sol = `32`) %>% 
  split(.$species) -> power

# Estimate FPC_r
# ratio estimator of mean cpue of a species from the R/V Resolution to the mean 
# cpue in the R/V Solstice

glimpse(power)

# change data for analysis FPC_ratio (log transformed CIs)
power %>%
  map(fpc_r) %>% 
  bind_rows (.id = 'species') %>% 
  mutate_at(2:8, funs(round(., 2))) %>% 
  ggplot(aes(species, FPCr)) + geom_point() +
  geom_errorbar(aes(ymin = lll, ymax = lul), width =0.1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 1, lty = 3) +
  expand_limits(y = 0) + 
  ggtitle("FPC ratio method")


# change data for analysis FPC_ratio (log transformed CIs)

power %>% 
  rep(., 1000) %>% 
  map(f_smpl) %>% 
  map(f_clean) %>% 
  map(f_model) %>% 
  map_df(~as.data.frame(.), .id = 'species') %>% 
  group_by(species)  %>% 
  summarise(ll = quantile(., 0.025),
            ul = quantile(., 0.975)) -> boot_ci

  power %>% 
    map(data.frame) %>% 
    map(f_clean) %>% 
    map(f_model) %>% 
    map_df(~data.frame(fpc_b = .x), .id = 'species') %>% 
    left_join(., boot_ci) %>% 
  ggplot(aes(species, fpc_b)) + geom_point() +
    geom_errorbar(aes(ymin = ll, ymax = ul), width =0.1) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_hline(yintercept = 1, lty = 3) +
    expand_limits(y = 0) + 
    ggtitle("FPC random block method")