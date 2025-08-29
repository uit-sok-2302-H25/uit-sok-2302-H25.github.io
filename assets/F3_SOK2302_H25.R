#################################################################
##### Forelesning 3 Nytte-kostnadsanalyse
#################################################################


##### Start up #####
rm(list = ls()) # Empties all data

options(scipen=10) # writes 10 scipens before scientific script
options(digits=10) # writes up to 10 digits


# loading packages
library(tidyverse)
library(gt)

# Kode for a kunne bruke norske bokstaver
Sys.setlocale(locale="no_NO")


##### Graph over diskontering #####

# Parametre
belop <- 1000
tid <- 0:50                     # 00 år

df <- data.frame(tid)

df <- df %>% 
  mutate(PV_2 = belop / (1.02)^tid) %>%
  mutate(PV_5 = belop / (1.05)^tid) %>%
  mutate(PV_10 = belop / (1.1)^tid) 

# Plot med ggplot
ggplot(df, aes(x = tid, y = PV_2)) +
  geom_line(size = 1.2) +
  labs(
    title = "Naverdi av 1000 kr over tid",
    x = "Ar fram i tid",
    y = "Naverdi (kr)",
    color = "Diskonteringsrente"
  ) 



# Gjør data om til langt format
df_long <- df %>%
  pivot_longer(cols = starts_with("PV"),
               names_to = "rente",
               values_to = "PV") %>%
  mutate(rente = recode(rente,
                        "PV_2" = "2%",
                        "PV_5" = "5%",
                        "PV_10" = "10%"))

# Plot med ggplot
ggplot(df_long, aes(x = tid, y = PV, color = rente)) +
  geom_line(size = 1.2) +
  labs(
    title = "Naverdi av 1000 kr over tid",
    x = "Ar fram i tid",
    y = "Naverdi (kr)",
    color = "Diskonteringsrente"
  ) +
  theme_minimal(base_size = 14)


##### Tabel 3.1 diskontering ####
# Beløpene i hver periode
B <- c(3000, 5000, 6000, 10000, 12000)

# Tidsperioder (år 1 til 5)
t <- 1:5

# Sett diskonteringsrente (kan endres)
r <- 0.06   # 6 %

# Nåverdi av hvert beløp
PV_each <- B / (1 + r)^t

# Total nåverdi
NPV <- sum(PV_each)

# Skriv ut
data.frame(År = t, Beløp = B, Nåverdi = round(PV_each, 2))
cat("Total nåverdi:", round(NPV, 2), "kr")



##### Sum av uendelig rekke #####

n = 10000 # antall år
r = 0.05  # rente

# lager en datafram med n antall år med kostand 1 per år
df <- data.frame(tid = seq(1,n), kost =rep(1, n))

# Diskonterer kostnaden 
df <- df %>% 
  mutate(disk = kost/(1+r)^(tid) )

df %>% 
  summarise(sum(disk))

1/r

##### Forenklet SCC-beregning i R #####
## Antakelser:
## - Ett tonn CO2 gir 1 USD global skade per år (d_global)
## - … og 0.1 USD lokal skade per år (d_local)
## - Diskonteringsrenter kan varieres

# Parametre
d_global <- 1.0   # USD/år global skade pr. tonn
d_local  <- 0.1   # USD/år lokal skade pr. tonn
rates    <- c(0.025, 0.03, 0.05)  # 2.5%, 3%, 5%
T_horizon <- 300  # antall år i endelig horisont

# Hjelpefunksjoner
# Hvor stor endring er det hvis skaden varer tilnærmet uendelig?
pv_perpetuity <- function(d, r) d / r
# Hvor stor endring er det hvis skaden varer T år?
pv_stream     <- function(d, r, T) sum(d / (1 + r)^(1:T))

# Beregn SCC for hvert r
res <- lapply(rates, function(r){
  data.frame(
    r           = r,
    SCC_global_perpetuity = pv_perpetuity(d_global, r),
    SCC_local_perpetuity  = pv_perpetuity(d_local,  r),
    SCC_global_300y       = pv_stream(d_global, r, T_horizon),
    SCC_local_300y        = pv_stream(d_local,  r, T_horizon)
  )
})
res <- do.call(rbind, res)

# Pen utskrift
res_out <- within(res, {
  r_percent <- paste0(round(100*r, 1), "%")
})
res_out <- res_out[, c("r_percent",
                       "SCC_global_perpetuity", "SCC_local_perpetuity",
                       "SCC_global_300y", "SCC_local_300y")]

# print(round(res_out, 2), row.names = FALSE)

## Valgfritt: enkel visualisering (ggplot)
## Installer/last inn ggplot2 først om nødvendig
# install.packages("ggplot2")
library(ggplot2)
library(tidyr)
library(dplyr)

plot_df <- res %>%
  select(r,
         global_perp = SCC_global_perpetuity,
         local_perp  = SCC_local_perpetuity) %>%
  pivot_longer(cols = c(global_perp, local_perp),
               names_to = "scope", values_to = "SCC") %>%
  mutate(scope = ifelse(scope == "global_perp", "Global (perpetuitet)", "Lokal (perpetuitet)"),
         r_label = paste0(round(100*r,1), "%"))

ggplot(plot_df, aes(x = r, y = SCC, color = scope)) +
  geom_point(size = 3) +
  geom_line() +
  scale_x_continuous(breaks = rates, labels = paste0(100*rates, "%")) +
  labs(title = "Forenklet SCC: global vs. lokal og rente (perpetuitet)",
       x = "Diskonteringsrente",
       y = "SCC (USD per tonn CO2)",
       color = "Omfang") +
  theme_minimal(base_size = 14)


##### Oppgave #####

# Antakelser:
# La oss anta at et tonn med karbon gir $1 i skade hvert år i 20 år på globalt basis.
# obama brukte 2% diskonteringsrate 
# Hva er «hannes» SCC?

SCC_obama <- pv_stream( 1,  0.02,  20)
SCC_obama


# Trump brukte 10% diskonteringsrate
# I tillegg så han kun på lokal kostand av CO2 som er kun 30% av global skade
#Hva er hannes SCC?
  
SCC_trump <- pv_stream( 0.3,  0.1,  20)
SCC_trump
# Hvor mye høyere er obama sin SCC enn Trump sin SCC?
SCC_obama / SCC_trump
# Hvor mye høyere er obama sin SCC enn Trump sin SCC?
SCC_obama - SCC_trump

# Hvor stor endring er det hvis skaden varer tilnærmet uendelig?
SCC_obama_ue <- pv_perpetuity( 1,  0.02)
SCC_obama_ue
SCC_trump_ue <- pv_perpetuity( 0.3,  0.1)
SCC_trump_ue
SCC_obama_ue / SCC_trump_ue
SCC_obama_ue - SCC_trump_ue
