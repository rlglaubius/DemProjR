library(data.table)
library(ggplot2)
library(SpectrumUtils)

source("../R/upd-utils.R")
source("../R/ccmpp.R")

pjnz_name = "../testdata/lux.PJNZ"
upd_name  = "C:/Users/RobertGlaubius/AppData/Roaming/Avenir Health/Spectrum/UNPopDataByCountry/2019/Luxembourg_442_19.upd"

spec = read.raw.dp(pjnz_name)
spec_pop    = dp.output.bigpop(spec, direction="long")
spec_births = dp.output.births(spec, direction="long")
spec_deaths = dp.output.deaths.nonhiv(spec, direction="long")

upd = read_upd(upd_name)
proj = demproj(upd, ccmpp_migr_mid, spec.fert=TRUE)
proj_pop    = as.data.table(as.data.frame.table(proj$pop, responseName="Value"))
proj_births = as.data.table(as.data.frame.table(proj$births, responseName="Value"))
proj_deaths = as.data.table(as.data.frame.table(proj$deaths, responseName="Value"))

proj_births = proj_births[,.(Value=sum(Value)),by=.(Year)]

proj_pop$Year    = as.numeric(as.character(proj_pop$Year))
proj_births$Year = as.numeric(as.character(proj_births$Year))
proj_deaths$Year = as.numeric(as.character(proj_deaths$Year))

pop_both = dplyr::bind_rows(list(Spectrum=spec_pop, R=proj_pop), .id="Source")
pop_both$Age = factor(pop_both$Age, levels=c(0:79, "80+"))
ggplot(subset(pop_both, Sex=="Female"), aes(x=Year, y=Value, color=Source, linetype=Source)) +
  geom_line() +
  facet_wrap(~Age, scale="free_y") +
  theme_bw()
ggsave("popsize-female.tiff", compression="lzw", dpi=600, width=16, height=9)

ggplot(subset(pop_both, Sex=="Male"), aes(x=Year, y=Value, color=Source, linetype=Source)) +
  geom_line() +
  facet_wrap(~Age, scale="free_y") +
  theme_bw()
ggsave("popsize-male.tiff", compression="lzw", dpi=600, width=16, height=9)


deaths_both = dplyr::bind_rows(list(Spectrum=spec_deaths, R=proj_deaths), .id="Source")
deaths_both$Age = factor(deaths_both$Age, levels=c(0:79, "80+"))
ggplot(subset(deaths_both, Sex=="Female"), aes(x=Year, y=Value, color=Source, linetype=Source)) +
  geom_line() +
  facet_wrap(~Age, scale="free_y") +
  theme_bw()
ggsave("deaths-female.tiff", compression="lzw", dpi=600, width=16, height=9)

ggplot(subset(deaths_both, Sex=="Male"), aes(x=Year, y=Value, color=Source, linetype=Source)) +
  geom_line() +
  facet_wrap(~Age, scale="free_y") +
  theme_bw()
ggsave("deaths-male.tiff", compression="lzw", dpi=600, width=16, height=9)


births_both = dplyr::bind_rows(list(Spectrum=spec_births, R=proj_births), .id="Source")
ggplot(births_both, aes(x=Year, y=Value, color=Source, linetype=Source)) +
  geom_line() +
  theme_bw()
ggsave("births.tiff", compression="lzw", dpi=600, width=6.5, height=3.5)


