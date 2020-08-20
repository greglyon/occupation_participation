# Occupation and participation APSA paper

# Packages -----
library(dplyr)
library(tidyr)
library(haven)
library(ggplot2)

# Functions ------

range01 <- function(x){(x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))}
means_and_bars_wt <- function(df, x, y, wt) {
  aa <- enquo(x)
  bb <- enquo(y)
  df %>%
    filter(!is.na(!! aa), !is.na(!! bb)) %>%
    group_by(!! aa) %>%
    summarise(avg = weighted.mean(!! bb, na.rm = T, w = wt),
              N = n(),
              sd = sd(!! bb, na.rm = T),
              se = sd/sqrt(N))
}
ind_names <- function(x) {
  ifelse(x > 23, NA,
         ifelse(x == 1, "Agriculture",
                ifelse(x == 2, "Forestry",
                       ifelse(x == 3, "Fishing and Hunting",
                              ifelse(x == 4, "Mining",
                                     ifelse(x == 5, "Utilities",
                                            ifelse(x == 6, "Construction",
                                                   ifelse(x == 7, "Manufacturing",
                                                          ifelse(x == 8, "Wholesale Trade",
                                                                 ifelse(x == 9, "Retail Trade",
                                                                        ifelse(x == 10, "Transportation and Warehousing",
                                                                               ifelse(x == 11, "Information",
                                                                                      ifelse(x == 12, "Finance and Insurance",
                                                                                             ifelse(x == 13, "Real Estate and Rental Leasing",
                                                                                                    ifelse(x == 14, "Professional, Scientific, Technical Services",
                                                                                                           ifelse(x == 15, "Management of Companies and Enterprises",
                                                                                                                  ifelse(x == 16, "Administrative Support",
                                                                                                                         ifelse(x == 17, "Waste Management and Remediation",
                                                                                                                                ifelse(x == 18, "Education Services",
                                                                                                                                       ifelse(x == 19, "Health Care and Social Assistance",
                                                                                                                                              ifelse(x == 20, "Arts, Entertainment, and Recreation",
                                                                                                                                                     ifelse(x == 21, "Hotel Accommodation and Food Services",
                                                                                                                                                            ifelse(x == 22, "Other Services except Public Administration",
                                                                                                                                                                   ifelse(x == 23, "Public Administration", x))))))))))))))))))))))))
}
occup_names <- function(x) {
  ifelse(x > 14, NA,
         ifelse(x == 1, "Management",
                ifelse(x == 2, "Independent Contractor",
                       ifelse(x == 3, "Business Owner",
                              ifelse(x == 4, "Owner-operator",
                                     ifelse(x == 5, "Office and Admin Support",
                                            ifelse(x == 6, "Healthcare Support",
                                                   ifelse(x == 7, "Protective Service",
                                                          ifelse(x == 8, "Food Preparation and Service",
                                                                 ifelse(x == 9, "Personal Care",
                                                                        ifelse(x == 10, "Install, Mainten., Repair",
                                                                               ifelse(x == 11, "Building Grounds Cleaning Mainten.",
                                                                                      ifelse(x == 12, "Other Service",
                                                                                             ifelse(x == 13, "Trade Worker or Laborer",
                                                                                                    ifelse(x == 14, "Professional", x)))))))))))))))
  
  
}

# Read in data and merge occupation data -----

# Read in data
c12 <- read_dta("Data/CCES12_Common_VV.dta")
c14 <- read_dta("Data/CCES14_Common_Content_Validated.dta")
c16 <- read_dta("Data/CCES16_Common_OUTPUT_vv_CLconfid_v2018.dta")

# Read in coded occupation data
occ14 <- readr::read_csv("Data/cces_occ_2014.csv")
occ16 <- readr::read_csv("Data/cces_occ_2016.csv")

# Merge occupation data to 2014 and 2016 by caseid
c14 <- left_join(c14, occ14, by = c("V101" = "caseid"))
c16 <- left_join(c16, occ16, by = c("V101" = "caseid"))

# Prepare Data: 2012 -----


cc12 <- c12 %>%
  mutate(
    year= 2012,
    state_fips = as.numeric(inputstate),
    vv_gen = ifelse(e2012g %in% c("Absentee", "Early", "Mail", "Polling", "UnknownMethod"), 1, 0),
    ownhome = as.numeric(ownhome == 1),
    child = as.numeric(child18 == 1),
    yrs_res = ifelse(CC351 %in% 1:3, 0,
                     ifelse(CC351 %in% 4:5, 1,
                            ifelse(CC351 > 5, 2, CC351))),
    income3 = ifelse(faminc %in% 1:4, -1,
                     ifelse(faminc %in% c(5:9, 97), 0, 1)),
    income = ifelse(faminc %in% c(16, 32), 15,
                    ifelse(faminc == 97, 7, faminc)),
    educ = ifelse(educ > 6, NA, educ),
    ba = ifelse(educ >= 5, 1, 0),
    age = as.numeric(2012-birthyr),
    married = as.numeric(marstat == 1), 
    female = as.numeric(gender == 2),
    black = as.numeric(race == 2),
    latino = as.numeric(race == 3),
    asian = as.numeric(race == 4),
    union_self = as.numeric(union == 1),
    rel_att = as.numeric(pew_churatd %in% 1:2),
    ideology = range01(ifelse(ideo5 > 6, NA,
                              ifelse(CC334A == 6, 3, CC334A))),
    pid7 = range01(ifelse(pid7 > 8, NA, 
                          ifelse(pid7 == 8, 4, pid7))),
    pol_int = range01(ifelse(newsint == 7, 4, 5-newsint)),
    unemp = as.numeric(employ == 4),
    occ = occup_names(occupationcat),
    ind = ind_names(industryclass),
    occ = as.numeric(occupationcat),
    occ_n = occup_names(occupationcat),
    p_run_off = as.numeric(CC418a == 1),
    p_att_mtg = as.numeric(CC417a_1 == 1),
    p_sign = as.numeric(CC417a_2 == 1),
    p_camp = as.numeric(CC417a_3 == 1),
    p_donate = as.numeric(CC417a_4 == 1),
    p_scale = (p_run_off + p_att_mtg + p_sign + p_camp + p_donate)/5,
    wt = as.numeric(V103)) %>%
  dplyr::select(year, state_fips, vv_gen, income3, income, educ, ba, age, married, female, ideology, pol_int, ind, unemp, rel_att,
                black, latino, asian, union_self, pid7, ownhome, yrs_res, child, wt,
                occ, occ_n, starts_with("p_"))


# Prepare Data: 2014 ------


cc14 <- c14 %>%
  mutate(
    year = 2014,
    state_fips = as.numeric(inputstate),
    vv_gen = ifelse(e2014gvm %in% c("absentee", "earlyVote", "mail", "polling", "unknown"), 1, 0),
    vote_gen = as.numeric(CC401 == 5),
    ownhome = as.numeric(ownhome == 1),
    child = as.numeric(child18 == 1),
    yrs_res = ifelse(CC351 %in% 1:3, 0,
                     ifelse(CC351 %in% 4:5, 1,
                            ifelse(CC351 > 5, 2, CC351))),
    income3 = ifelse(faminc %in% 1:4, -1,
                     ifelse(faminc %in% c(5:9, 97), 0, 1)),
    income = ifelse(faminc %in% c(16, 31, 32), 15,
                    ifelse(faminc == 97, 7, faminc)),
    educ = ifelse(educ > 6, NA, educ),
    ba = ifelse(educ >= 5, 1, 0),
    age = as.numeric(2014-birthyr),
    married = as.numeric(marstat == 1),
    female = as.numeric(gender == 2),
    black = as.numeric(race == 2),
    latino = as.numeric(race == 3),
    asian = as.numeric(race == 4),
    union_self = as.numeric(union == 1),
    rel_att = as.numeric(pew_churatd %in% 1:2),
    unemp = as.numeric(employ == 4),
    ideology = range01(ifelse(ideo5 > 6, NA,
                              ifelse(ideo5 == 6, 3, ideo5))),
    pid7 = range01(ifelse(pid7 > 8, NA,
                          ifelse(pid7 == 8, 4, pid7))),
    pol_int = range01(ifelse(newsint == 7, 4, 5-newsint)),
    ind = ind_names(industryclass),
    occ = as.numeric(occ),
    occ_n = occup_names(occ),
    p_run_off = as.numeric(CC418a == 1),
    p_att_mtg = as.numeric(CC417a_1 == 1),
    p_sign = as.numeric(CC417a_2 == 1),
    p_camp = as.numeric(CC417a_3 == 1),
    p_donate = as.numeric(CC417a_4 == 1),
    p_scale = (p_run_off + p_att_mtg + p_sign + p_camp + p_donate)/5,
    wt = as.numeric(weight)) %>%
  dplyr::select(year, state_fips, vv_gen, income3, income, educ, ba, age, married, female, ideology, pol_int, ind, unemp, rel_att,
                black, latino, asian, union_self, pid7, ownhome, yrs_res, child, wt,
                occ, occ_n, starts_with("p_"))


# Prepare Data: 2016 -----

cc16 <- c16 %>%
  mutate(
    year = 2016,
    state_fips = as.numeric(inputstate),
    vv_gen = ifelse(CL_E2016GVM %in% c("absentee", "earlyVote", "mail", "polling", "unknown"), 1, 0),
    ownhome = as.numeric(ownhome == 1),
    child = as.numeric(child18 == 1),
    yrs_res = ifelse(citylength_1 < 1, 0,
                     ifelse(citylength_1 > 1 & citylength_1 < 5, 1,
                            ifelse(citylength_1 >= 5, 2, citylength_1))),
    vote_gen = as.numeric(CC16_401 == 5),
    income3 = ifelse(faminc %in% 1:4, -1,
                     ifelse(faminc %in% c(5:9, 97), 0, 1)),
    income = ifelse(faminc %in% c(16, 31), 15,
                    ifelse(faminc == 97, 7, faminc)),
    educ = ifelse(educ < 0, NA, educ),
    ba = ifelse(educ >= 5, 1, 0),
    age = as.numeric(2016-birthyr),
    married = as.numeric(marstat == 1),
    female = ifelse(gender == 2, 1, 0),
    black = as.numeric(race == 2),
    latino = as.numeric(race == 3),
    asian = as.numeric(race == 4),
    union_self = as.numeric(union == 1),
    rel_att = as.numeric(pew_churatd %in% 1:2),
    unemp = as.numeric(employ == 4),
    ideology = range01(ifelse(ideo5 > 6, NA,
                              ifelse(ideo5 == 6, 3, ideo5))),
    pid7 = range01(ifelse(pid7 > 8, NA,
                          ifelse(pid7 == 8, 4, pid7))),
    pol_int = range01(ifelse(newsint == 7, 4, 5-newsint)),
    pi = ifelse(newsint == 7, 4, 5-newsint),
    ind = ind_names(industryclass),
    occ = as.numeric(occ),
    occ_n = occup_names(occ),
    p_run_off = as.numeric(CC16_418a == 1),
    p_att_mtg = as.numeric(CC16_417a_1 == 1),
    p_sign = as.numeric(CC16_417a_2 == 1),
    p_camp = as.numeric(CC16_417a_3 == 1),
    p_donate = as.numeric(CC16_417a_4 == 1),
    p_scale = (p_run_off + p_att_mtg + p_sign + p_camp + p_donate)/5,
    wt = as.numeric(commonweight)) %>%
  dplyr::select(year, state_fips, vv_gen, income3, income, educ, ba, age, married, female, ideology, pol_int, ind, unemp, rel_att,
                black, latino, asian, union_self, pid7, ownhome, yrs_res, child, wt,
                occ, occ_n, starts_with("p_"))

# Bind waves: call -----

call <- bind_rows(list(cc12, cc14, cc16))


# Analysis: participation by occupation -----

# Means
m <- means_and_bars_wt(call, occ_n, p_scale, wt)

ggplot(m, aes(x = reorder(occ_n, avg), y = avg)) +
  geom_point() + 
  geom_errorbar(aes(ymax = avg+se, ymin = avg-se), width = 0.1) +
  coord_flip() +
  theme_bw() +
  labs(title = "Means")

# Model, predicted values
m2 <- lm(p_scale ~ occ_n + union_self + ba + income3 + age + female + black + latino + asian +
           ideology + pid7 + married + ownhome + yrs_res + child + unemp + factor(year),
         weights = wt, data = call)

preds <- ggpredict(m2, c("occ_n"))

ggplot(preds, aes(x = reorder(x, predicted), y = predicted)) +
  geom_point() +
  geom_errorbar(aes(ymax = conf.high, ymin = conf.low), width = 0.1) +
  theme_bw() +
  coord_flip() +
  labs(y = "Participation Scale",
       x = "Occupation")

# Turnout
m3 <- glm(vv_gen ~ occ_n + union_self + ba + income3 + age + female + black + latino + asian +
          ideology + pid7 + married + ownhome + yrs_res + child + unemp + factor(year),
        weights = wt, 
        family = quasibinomial("logit"),
        data = call)

vote_preds <- ggpredict(m3, c("occ_n"))

ggplot(vote_preds, aes(x = reorder(x, predicted), y = predicted)) +
  geom_point() +
  geom_errorbar(aes(ymax = conf.high, ymin = conf.low), width = 0.1) +
  theme_bw() +
  coord_flip() +
  labs(y = "Turnout",
       x = "Occupation")
