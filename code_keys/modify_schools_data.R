library(tidyverse)
library(here)
library(AER)

data(CASchools)

### Create a modified county column to introduce text changes and typos
set.seed(42)
sch_df <- CASchools %>%
  mutate(county_mod = as.character(county)) %>%
  ### randomly misspell Santa Barbara a few times
  mutate(county_mod = ifelse(runif(n()) > .80, str_replace(county_mod, 'Santa Barbara', 'Santa Barabara'), county_mod)) %>%
  ### randomly modify case and add some underscores
  mutate(r = runif(n = n()),
         county_mod = case_when(r < .25 ~ toupper(county_mod),
                                r > .85 ~ tolower(county_mod) %>% str_replace_all(' +', '_'),
                                r > .65 ~ tolower(county_mod),
                                TRUE ~ county_mod)) %>%
  select(-r) %>%
  rename(county_orig = county, county = county_mod)

### Loop over counties and write out new csvs for each
county_vec <- sch_df$county_orig %>% unique() %>% as.character()

for(cty in county_vec) {
  # cty <- county_vec[1]
  cty_df <- sch_df %>%
    filter(county_orig == cty) %>%
    select(district, school, county, everything(), -county_orig)
  cty_f <- here(sprintf('county_data/%s.csv', str_replace_all(tolower(cty), ' +', '_')))
  write_csv(cty_df, cty_f)
}
