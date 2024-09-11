
#p-value under null hypothesis####
N_hadza = 80 #5-10 children
ASD_n = 4 - 2 #remove Retts and Downs cases
true_prop = 1.5 / 100 #proportion from UK
binom.test(ASD_n, N_hadza, true_prop, alternative="greater")

#effects on prevalence rates####

#open dataset
getwd()
data = read.csv("autism dataset Spectrum.csv")

df = data[,c("Age..years.","Year.published","Country","Diagnostic.tools",
             "Sample.size","Individuals.with.autism")]
colnames(df) = c("ages","year","country","measure","N","ASDcount")
df$N = as.numeric(gsub(",", "", df$N))
df$ASDcount = as.numeric(gsub(",", "", df$ASDcount))

#remove datasets with adults
ages = CIs = strsplit(df$ages, "-")
ages = (do.call(rbind, lapply(ages,as.numeric)))
df = df[!apply(ages, 1, function(x) any(x >17)),]
df = na.omit(df)

#remove data without diagnostic info
df = df[df$measure!="Unavailable",]

#account for overdispersion
df$obs = seq(1:nrow(df))

#estimate binomial model of prevalence
library(brms)
mod = bf(ASDcount | trials(N) ~ 1 + (1|country * measure) + (1|obs)) + binomial()
ma = brm(mod, data = df, iter = 4000, warmup = 1500, cores = 4, chains = 4,
         seed = 9, control = list(adapt_delta = 0.95, max_treedepth = 12))
saveRDS(ma, "ma.RDS")

#R2 for random effects
post = as_draws_df(ma)
vc = post$sd_country__Intercept^2
vm = post$sd_measure__Intercept^2
vi = post$`sd_country:measure__Intercept`^2
vo = post$sd_obs__Intercept^2
v_lres = pi^2/3
vcm = vc + vm + vi
vt = vc + vm + vi + vo + v_lres
r2_cm = vcm /vt
median(r2_cm); quantile(r2_cm, c(0.025,0.975))

