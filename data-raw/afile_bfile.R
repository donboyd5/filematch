

# make pums testdata ----
library(tidyverse)
library(tidycensus)

lcnames <- function(df)
{
  vnames <- stringr::str_to_lower(names(df))
  stats::setNames(df, vnames)
}


pvars2021 <- pums_variables |>
  filter(year == 2021, survey == "acs1")

vars <- pvars2021 |>
  filter(level=="person") |>
  distinct(var_code, var_label, data_type)

testdata <- get_pums(
  variables = c("SEX", "MAR", "AGEP", "WKHP", "PINCP", "WAGP", "INTP", "SSP", "SSIP", "RETP", "SEMP"),
  state = "MA",
  survey = "acs1",
  year = 2021
)

count(testdata, SEX) # 1 male, 2 female
count(testdata, MAR) # 1=married

td2 <- testdata |>
  lcnames() |>
  filter(sex==1, mar==1, agep %in% 18:80) |>
  mutate(pid=paste0(serialno, sporder)) |>
  select(pid, weight=pwgtp, age=agep, hoursworked=wkhp,
         income=pincp, wages=wagp, interest=intp, socsec=ssp, pension=retp, selfemploy=semp)
quantile(td2$age)
anyDuplicated(td2$pid)

idvars <- c("pid", "weight")
xvars <- c("age", "hoursworked", "income")
yvars <- c("socsec", "selfemploy")
zvars <- c("interest", "pension", "wages")

set.seed(1234)
afile <- td2 |>
  sample_n(10000) |>
  select(all_of(c(idvars, xvars, yvars)))

bfile <- td2 |>
  filter(!pid %in% afile$pid) |>
  select(all_of(c(idvars, xvars, zvars))) |>
  mutate(weight=weight * sum(afile$weight) / sum(weight))

# sum(afile$weight)
# sum(bfile$weight)

# glimpse(afile)
# glimpse(bfile)

usethis::use_data(afile, overwrite = TRUE)
usethis::use_data(bfile, overwrite = TRUE)
