## ---- sw_wages

# load the original data
sw <- brolgar::wages %>%
  group_by(id) %>%
  mutate(index = 1:n(),
         id = as.factor(id))


## ---- calculate-summaries-on-subsets
# Comparing the original and refreshed data
# tabulating hgc
sw_hgc <- sw %>%
  as_tibble() %>%
  select(id, high_grade) %>%
  distinct() %>%
  count(high_grade) %>%
  rename(hgc = high_grade, original = n)

do_1994 <- wages_hs_do %>%
  filter(year <= 1994) %>%
  as_tibble() %>%
  group_by(id) %>%
  mutate(hgc_1994 = max(grade[between(year, 1979, 1994)],
                        na.rm=TRUE)) %>%
  ungroup()

d94_hgc <- do_1994 %>%
  select(id, hgc_1979) %>%
  filter(between(hgc_1979, 6, 12)) %>%
  distinct() %>%
  count(hgc_1979) %>%
  rename(hgc = hgc_1979, refreshed = n)

hgc_join <- left_join(sw_hgc, d94_hgc) %>%
  mutate(original = -original, hgc = factor(hgc)) %>%
  pivot_longer(cols = c(original, refreshed),
               names_to = "subset",
               values_to = "count")

hgc_p <- ggplot(hgc_join, aes(x=hgc, y=count, fill=subset)) +
  geom_col(alpha=0.9) +
  facet_wrap(~subset, scales = "free_x") +
  scale_y_continuous("Count",
                     breaks = c(-300, -200, -100, 0, 100, 200, 300),
                     labels = c(300, 200, 100, 0, 100, 200, 300),
                     expand = c(0, 0)) +
  scale_fill_brewer("", palette="Dark2") +
  ggtitle("(A)") +
  coord_flip() +
  theme_bw() +
  theme(panel.spacing.x = unit(0, "mm"),
        legend.position = "none")

# compare distributions of exp
exp_sw <- sw %>%
  as_tibble() %>%
  select(xp) %>%
  rename(exp=xp) %>%
  mutate(subset = "original")

exp_d94 <- do_1994 %>%
  as_tibble() %>%
  select(exp) %>%
  mutate(subset = "refreshed")

exp_join <- bind_rows(exp_sw, exp_d94)

exp_p <- ggplot(exp_join) +
  geom_density(aes(x=exp, colour=subset, fill=subset), alpha=0.9) +
  facet_wrap(~subset, ncol=1) +
  xlim(c(0,13)) +
  xlab("Experience") + ylab("") +
  scale_fill_brewer("", palette="Dark2") +
  scale_colour_brewer("", palette="Dark2") +
  ggtitle("(B)") +
  theme_bw() +
  theme(legend.position = "none")

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
wg_sw <- sw %>%
  as_tibble() %>%
  select(ln_wages) %>%
  mutate(subset = "original")

wg_d94 <- do_1994 %>%
  as_tibble() %>%
  mutate(ln_wages = log(wage)) %>%
  select(ln_wages) %>%
  mutate(subset = "refreshed")

wg_join <- bind_rows(wg_sw, wg_d94)

wg_p <- ggplot(wg_join) +
  geom_density(aes(x=ln_wages, colour=subset, fill=subset), alpha=0.9) +
  facet_wrap(~subset, ncol=1) +
  xlim(c(0,4)) +
  xlab("Wages (natural log)") + ylab("") +
  scale_fill_brewer("", palette="Dark2") +
  scale_colour_brewer("", palette="Dark2") +
  ggtitle("(C)") +
  theme_bw() +
  theme(legend.position = "none")

##  ---- compare-subsets
hgc_p + exp_p + wg_p


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





