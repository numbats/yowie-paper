## ---- sw_wages

# load the original data
sw <- brolgar::wages %>%
  group_by(id) %>%
  mutate(index = 1:n(),
         id = as.factor(id))

# load the dataset of hgc in 1979
hgc_1979 <- hgc_1979 %>%
  mutate(id = as.factor(id))

# XXX Re-do of the comparison section
# tabular hgc
sw_hgc <- sw %>%
  as_tibble() %>%
  select(id, high_grade) %>%
  distinct() %>%
  count(high_grade)

do_1994 <- wages_hs_do %>%
  filter(year <= 1994) %>%
  as_tibble() %>%
  group_by(id) %>%
  mutate(hgc_1994 = max(grade[between(year, 1979, 1994)], na.rm=TRUE)) %>%
  ungroup()

d94_hgc <- do_1994 %>%
  select(id, hgc_1994) %>%
  filter(between(hgc_1994, 6, 12)) %>%
  distinct() %>%
  count(hgc_1994)

# compare distributions of exp
exp_p1 <- sw %>% ggplot(aes(x=xp)) +
  geom_density(fill="black", alpha=0.7) +
  xlim(c(0, 13))
exp_p2 <- do_1994 %>% ggplot(aes(x=exp)) +
  geom_density(fill="black", alpha=0.7) +
  xlim(c(0, 13))

# tabulate race
sw_d <- sw %>%
  as_tibble() %>%
  select(id, black, hispanic) %>%
  distinct() %>%
  count(black, hispanic)

d94_d <- do_1994 %>%
  select(id, race) %>%
  distinct() %>%
  count(race)

# compare density of wages
wg_p1 <- sw %>% ggplot(aes(x=ln_wages)) +
  geom_density(fill="black", alpha=0.7) +
  xlim(c(0.5, 4.5))
wg_p2 <- do_1994 %>% ggplot(aes(x=log(wage))) +
  geom_density(fill="black", alpha=0.7) +
  xlim(c(0.5, 4.5))


# load the refreshed data, join it with the hgc_1979
do <- wages_hs_do %>%
  as_tibble() %>%
  left_join(hgc_1979, by = "id") %>%
  group_by(id) %>%
  mutate(lnwage = log(wage),
         index = 1:n())

# join original and refreshed data, only for the same individuals that appears in the two data
do_sw_join <- inner_join(sw, do, by = c("id", "index"))

sw_wages <- sw %>%
  ggplot(aes(x = xp,
             y = ln_wages)) +
  geom_line(aes(group = id), alpha = 0.1) +
  geom_smooth(se = FALSE, colour = "pink") +
  labs(tag = "(A)", title = "Original") +
  theme_bw() +
  ylab("Hourly wage ($, natural log)") +
  xlab("Experience (years)") +
  #theme(plot.title = element_text(size = 10)) +
  ylim(0, 5)

sw_wages_mod <- do_sw_join %>%
  as_tibble() %>%
  mutate(hgc = ifelse(high_grade < 9, "8TH", "12TH")) %>%
  mutate(race = case_when(black == 1 ~ "black",
                          hispanic == 1 ~ "hispanic",
                          TRUE ~ "white")) %>%
  mutate(race = factor(race)) %>%
  ggplot(aes(x = xp,
             y = ln_wages,
             linetype = hgc)) +
  geom_smooth(method = "lm", se = FALSE,
              colour = "black") +
  labs(tag = "(A)", title = "Original") +
  scale_linetype("") +
  theme_bw() +
  ylab("Hourly wage ($, natural log)") +
  xlab("Experience in years") +
  theme(legend.position = "bottom",
        legend.direction = "horizontal") +
  xlim(c(0,15))

## ---- do_refreshed

# singer-willet did not stated how they calculate experience
# in database, there is work experience topic
# (https://www.nlsinfo.org/content/cohorts/nlsy79/topical-guide/employment/work-experience)
# but there is no variable explicitly mention about experience.
# in this case, we approximate it with year-1979

# filter do data up to 1994 since the textbook data only covers that period.



do_ref <- do %>%
  ggplot(aes(x = year,
             y = lnwage)) +
  geom_line(aes(group = id), alpha = 0.1) +
  geom_smooth(se = FALSE, colour = "pink") +
  labs(tag = "(B)", title = "Refreshed data") +
  theme_bw() +
  ylab("Hourly wage ($, natural log)") +
  xlab("Year of data collection") +
  theme(plot.title = element_text(size = 10)) +
  ylim(0, 5)

do_ref_mod <- do_sw_join %>%
  mutate(hgc12 = ifelse(high_grade < 9, "8TH", "12TH")) %>%
  ggplot(aes(x = exp,
             y = lnwage,
             linetype = hgc12)) +
  geom_smooth(method = "lm", se = FALSE,
              colour = "black") +
  labs(title = "Refreshed data", tag = "(B)") +
  scale_linetype("") +
  theme_bw() +
  ylab("Hourly wage ($, natural log)") +
  xlab("Experience (years)") +
  theme(legend.position = "bottom",
        legend.direction = "horizontal") +
  xlim(c(0,15))

# sw_wages + do_ref

# the two plots are not comparable
# since the id who are in Singer-Willet and the id in do-refreshed might be different

# XXX Code to check that the hgc matches
do_check <- do %>%
  mutate(hgc12 = max(grade[between(do$year, 1979, 1994)])) %>% # ifelse(hgc_i < 9, "8TH", "12TH")) %>%
  select(id, hgc12) %>%
  distinct()
sw_check <- sw %>%
  as_tibble() %>%
  mutate(hgc = ifelse(high_grade < 9, "8TH", "12TH")) %>%
  select(id, hgc) %>%
  distinct()

#sw_check <- sw_check %>% left_join(do_check, by="id")
#sw_check %>% filter(hgc != hgc12) %>% count(hgc, hgc12)

## --- compare-sw-do
sw_id <- as_tibble(sw) %>%
  group_by(id) %>%
  count() %>%
  select(id)

do_id <- as_tibble(do) %>%
  group_by(id) %>%
  count() %>%
  select(id)

# Restrict do to 1994 and before
do_1994 <- do %>%
  filter(year <= 1994) %>%
  filter(!is.na(exp)) %>%
  select(id, year, wage, age_1979, gender, exp)
# duplicates(do_1994, key=id, index=exp)

# check common id in data set
sw_do <- inner_join(sw_id, do_id, by = "id")

sw_agree <- filter(sw, id %in% sw_do$id)
do_agree <- filter(do, id %in% sw_do$id) # 839 do_agree %>% count(id)

sw_do_sample <- sw_agree %>%
  mutate(id = as.integer(as.character(id))) %>%
  sample_n_keys(size = 12)

sw_agree_sample <- sw_agree %>%
  filter(id %in% sw_do_sample) %>%
  select(id, xp, ln_wages) %>%
  as_tibble()
do_agree_sample <- do_agree %>%
  mutate(id = as.integer(as.character(id))) %>%
  filter(id %in% sw_do_sample) %>%
  select(id, year, lnwage) %>%
  as_tibble()

agree_ref <- ggplot() +
  geom_line(data=do_agree_sample,
            aes(x = year-1979,
                y = lnwage), colour = "black") +
  geom_line(data=sw_agree_sample,
            aes(x = xp,
                y = ln_wages), colour = "grey70") +
  facet_wrap(~id)

## ---- plotting-sw-do

sw_wages_agree <- sw_agree %>%
  ggplot(aes(x = xp,
             y = ln_wages)) +
  geom_line(aes(group = id),alpha = 0.1) +
  geom_smooth(se = FALSE) +
  labs(tag = "(A)") +
  theme_bw() +
  ylab("ln(Hourly wage) ($)") +
  xlab("Experience (years)") +
  theme(plot.title = element_text(size = 10)) +
  ylim(-3, 5)


do_ref_agree <- do_agree %>%
  ggplot(aes(x = exp,
             y = lnwage)) +
  geom_line(aes(group = id), alpha = 0.1) +
  geom_smooth(se = FALSE) +
  labs(tag = "B") +
  theme_bw() +
  ylab("ln(Hourly wage) ($)") +
  xlab("Experience (years)") +
  theme(plot.title = element_text(size = 10)) +
  ylim(-3, 5)

#sw_wages_agree + do_ref_agree
# sw_wages + do_ref
sw_wages_mod + do_ref_mod

# ---- summaries
sw2 <- sw %>%
  as_tibble() %>%
  mutate(hgc = ifelse(high_grade < 9, "8TH", "12TH")) %>%
  mutate(race = case_when(black == 1 ~ "black",
                          hispanic == 1 ~ "hispanic",
                          TRUE ~ "white")) %>%
  mutate(race = factor(race))
sw2 %>% select(id, race) %>% distinct() %>% count(race)
sw2 %>% select(id, hgc) %>% distinct() %>% count(hgc)

do2 <- do %>%
  as_tibble() %>%
  mutate(hgc12 = ifelse(hgc_i < 12, "BELOW 12TH", "12TH"))
do2 %>% select(id, race) %>% distinct() %>% count(race)
do2 %>% select(id, hgc12) %>% distinct() %>% count(hgc12)

# ---- compare-xp-to-yrworkforce

ggplot(do, aes(x = yr_wforce,
               y = exp)) +
  geom_point(alpha = 0.1) +
  guides(color = "none") +
  geom_abline(intercept = 0, slope = 1)

# ---- compare-xp-sw

do_sw_join <- inner_join(sw, do, by = c("id", "index"))

wp_compare1 <- ggplot(do_sw_join, aes(x = xp,
               y = exp)) +
  geom_point(alpha = 0.1) +
  geom_abline(intercept = 0, slope = 1) +
  guides(color = "none") +
  #scale_x_sqrt() +
  #scale_y_sqrt() +
  xlab("Work experience (Singer & Willet, 2008)") +
  ylab("Work experience (refreshed)") +
  theme_bw() +
  theme(aspect.ratio=1)

wp_compare2 <- ggplot(do_sw_join, aes(x = xp,
                       y = exp)) +
  geom_density2d_filled(contour_var = "ndensity") +
  #geom_abline(intercept = 0, slope = 1) +
  guides(color = "none") +
  scale_x_sqrt(breaks = seq(0, 13, 1)) +
  scale_y_sqrt(breaks = seq(0, 13, 1)) +
  xlab("Work experience (Singer & Willet, 2008)") +
  ylab("Work experience (refreshed)") +
  theme_bw() +
  theme(aspect.ratio=1, legend.position="none")

wp_compare2

# Takeaways:

# we don't know how exactly the criteria of drop out,
# resulted view agreements of id being dropped out in the two dataset.

# with the same id, the ln(wages) didn't agree each other, probably due to:

# we don't have experience variable in the original data base.
# we want to calculate it, but we don't know how Singer-Willet compute the experience
# we want to compare it just with year, Singer and Willet dont have year variable
# hence, it is hard to create key-index pair.


# for people who work show data example (book, research, ets), it is very important to disclose how the data derived from the initial source, to make the analysis reproducible, especially for longitudinal data because other researcher often want to compare with the recent data.
# in this case, in Singer-Willet textbook, it important to state how the experience variable derived from the database.
# In this EDA, we found that it is difficult to make apple to apple comparison (and refresh the data in general) cause we don't know how exactly they calculate the variable (even the wages)


# This is why in our paper, we show the practice of reproducible and responsible data cleaning work flow to make sure the data can be refreshed.
# Especially with longitudinal data from a survey that is still being held (so it will be refreshed from year to year).





