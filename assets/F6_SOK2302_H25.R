#################################################################
##### Forelesning 4 Verdsettning av miljo
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


##### Marginal bruker kostnad (MUC) over tid #####

# Parametre
a  <- 8
b  <- 0.4
MC <- 2
r <- 0.10

# Optimalt kvantum
q1 = (20+15*r)/(2+r)
q2 = (20+5*r)/(2+r)

# Shadow price eller MCU
lambda = 4/(2+r)

# Pris gitt fra kvantum
p1 = a - b*q1
p1
# Pris gitt fra MC og MUC
p1_MCU = MC + lambda
p1_MCU

# Pris gitt fra kvantum
p2 = a - b*q2
p2
# Pris gitt fra MC og MUC
p2_MCU = MC + lambda*(1+r)
p2_MCU




##### Depleterbar ressurs: Optimal allokering over tid #####

# Parametre (endre fritt) 
S  <- 40      # total ressurs (lager)
N  <- 10      # antall perioder
r  <- 0.1     # diskonteringsrente (10 %)
a  <- 8       # etterspørselsskjæring: P = a - b q
b  <- 0.40     # helning
c  <- 2       # (konstant) marginalkostnad MC

# Hjelpefunksjoner 
# q_t(mu) fra FOC: a - b q_t - c = mu*(1+r)^(t-1)  =>  q_t = (a - c - mu*(1+r)^(t-1))/b
# Denne funksjoner returnerer en vektor med q_t for t=1,...,N
# Negativt q_t settes til 0 (ingen negative mengder)
# pmax er elementvis maksimum av to vektorer
q_vec_from_mu <- function(mu, N, r, a, b, c) {
  # Lager en tidsvektor
  t <- 1:N
  # Beregner q_t for alle t gitt mu
  q <- (a - c - mu * (1 + r)^(t - 1)) / b
  pmax(0, q)  # ingen negative mengder
}

# summe q_t - S for gitt mu
sum_q_minus_S <- function(mu, S, N, r, a, b, c) {
  sum(q_vec_from_mu(mu, N, r, a, b, c)) - S
}

# Løs for skyggepris mu
# Sjekk om lagerbegrensningen er bindende: ved mu=0 får vi q_t0 = (a-c)/b pr periode (hvis positiv)
q_per_t_mu0 <- max(0, (a - c)/b)
sum_mu0     <- N * q_per_t_mu0

if (S >= sum_mu0) {
  # Lageret er så stort at FOC gir mu=0 (ingen knapphet)
  mu_star <- 0
} else {
  # Finn mu i [0, a-c] som tilfredsstiller sum q_t = S
  # Øvre grense a-c sikrer q1=0; det er strengt nok til å gjøre alle q_t <= 0 for større t også.
  upper <- max(1e-8, a - c)
  # Løs for mu_star
  # Finner roten av sum_q_minus_S = 0
  mu_star <- uniroot(sum_q_minus_S, interval = c(0, upper),
                     S = S, N = N, r = r, a = a, b = b, c = c)$root
}

# Optimal allokering og priser
t <- 1:N
q_t <- q_vec_from_mu(mu_star, N, r, a, b, c)
P_t <- pmax(0, a - b * q_t)          # pris i hver periode
MC  <- rep(c, N)                      # konstant MC
MUC_t <- P_t - MC                     # knapphetskostnad (current value)

# PV av MUC er lik i alle perioder ved optimum:
MUC_PV <- MUC_t / (1 + r)^(t - 1)

# --- Sjekk/utskrift ---
cat(sprintf("Skyggepris (PV av MUC): mu* = %.4f\n", mu_star))
cat(sprintf("Sum(q_t) = %.4f (mål: S = %.4f)\n\n", sum(q_t), S))

# --- Dataframe for plotting/tabell ---
df <- tibble(
  t, q_t, P_t, MC, MUC_t,
  MUC_PV = MUC_PV,
  disc   = 1/(1+r)^(t-1)
)

# Plot 1: Allokering over tid 
ggplot(df, aes(x = t, y = q_t)) +
  geom_col() +
  labs(title = "Optimal allokering av depleterbar ressurs over N perioder",
       subtitle = sprintf("S=%g, r=%.1f%%, P(q)=%.1f - %.2f q, MC=%.1f", S, 100*r, a, b, c),
       x = "Periode", y = expression(q[t])) +
  scale_x_continuous(breaks = 1:N) +
  theme_minimal(base_size = 14)

# Plot 2: Marginal kostand og MUC over tid
ggplot(df, aes(x = t)) +
  geom_line(aes(y = MC, color = "MC"), linewidth = 1) +
  geom_line(aes(y = MUC_t, color = "MUC"), linewidth = 1) +
  geom_line(aes(y = P_t, color = "Pris"), linewidth = 1) +
  scale_x_continuous(limits = c(1, N), breaks = 1:N) +
  scale_y_continuous(limits = c(0, a), breaks = seq(0, a, by = 1)) +
  labs(title = "Prisbanen og dekomponering: P_t = MC + MUC_t",
       x = "Periode", y = "Pris / kostnad",
       color = "Kurve") +
  theme_minimal(base_size = 14)

# --- (Valgfritt) Vis at PV(MUC) er (tilnærmet) konstant ---
round(df %>% select(t, MUC_t, MUC_PV, q_t), 4)


# ----- Parametre -----
S  <- 40      # total ressurs (lager)
N  <- 10      # antall perioder
r  <- 0.10    # diskonteringsrente (10 %)
a  <- 8       # etterspørselsskjæring: P = a - b q
b  <- 0.40    # helning
c  <- 2       # (konstant) marginalkostnad MC
c_sub <- 6    # kostnad (MC) for fornybar substitutt (uendelig tilbud)

# ----- Hjelpefunksjoner -----
q_vec_from_mu <- function(mu, N, r, a, b, c) {
  t <- 1:N
  q <- (a - c - mu * (1 + r)^(t - 1)) / b
  pmax(0, q)  # ingen negative mengder
}

sum_q_minus_S <- function(mu, S, N, r, a, b, c) {
  sum(q_vec_from_mu(mu, N, r, a, b, c)) - S
}

# ----- Løs for skyggepris mu (uten backstop) -----
q_per_t_mu0 <- max(0, (a - c)/b)
sum_mu0     <- N * q_per_t_mu0

if (S >= sum_mu0) {
  mu_star <- 0  # ingen knapphet
} else {
  upper <- max(1e-8, a - c)
  mu_star <- uniroot(sum_q_minus_S, interval = c(0, upper),
                     S = S, N = N, r = r, a = a, b = b, c = c)$root
}

# ----- Optimal allokering/priser (ressurs alene) -----
t    <- 1:N
q_t  <- q_vec_from_mu(mu_star, N, r, a, b, c)
P_t  <- pmax(0, a - b * q_t)
MC   <- rep(c, N)
MUC_t   <- P_t - MC
MUC_PV  <- MUC_t / (1 + r)^(t - 1)

# ----- Finn overgangstidspunkt t* når P_t >= c_sub -----
idx_hit <- which(P_t >= c_sub)
t_star  <- if (length(idx_hit)) min(idx_hit) else NA_integer_

# Uttak etter overgang settes til 0 (vi bytter til substitutt)
q_t_backstop <- q_t
if (!is.na(t_star)) q_t_backstop[t >= t_star] <- 0

# Hvor mye lager blir potensielt liggende igjen?
S_brukt <- sum(q_t_backstop)
S_igjen <- S - S_brukt

# (Valgfritt) Etter overgang kan etterspørselen dekkes av substitutt:
q_sub <- max(0, (a - c_sub)/b)  # etterspurt mengde ved pris = c_sub (per periode)

# ----- Dataframe -----
df <- tibble(
  t, q_t, q_t_backstop, P_t, MC, MUC_t, MUC_PV,
  c_sub = c_sub, S_brukt = S_brukt, S_igjen = S_igjen,
  t_star = t_star
)

# ===== Plot 1: Allokering over tid (med overgang) =====
p1 <- ggplot(df, aes(x = t, y = q_t_backstop)) +
  geom_col(fill = "grey70") +
  { if (!is.na(t_star)) geom_vline(xintercept = t_star, linetype = "dashed") } +
  labs(title = "Optimal allokering av depleterbar ressurs med fornybar substitutt",
       subtitle = sprintf("S=%g, r=%.1f%%, P(q)=%.1f - %.2f q, MC=%.1f, Backstop=%.1f",
                          S, 100*r, a, b, c, c_sub),
       x = "Periode", y = expression(q[t])) +
  scale_x_continuous(breaks = 1:N) +
  theme_minimal(base_size = 14)

print(p1)

# ===== Plot 2: Prisbanen (P = MC + MUC) og backstop-kostnad =====
p2 <- ggplot(df, aes(x = t)) +
  geom_line(aes(y = P_t, colour = "Pris (MC+MUC)"), linewidth = 1.2) +
  geom_hline(aes(yintercept = c_sub, colour = "Backstop-kostnad (=6)"), linewidth = 1, linetype = "dashed") +
  { if (!is.na(t_star)) geom_vline(xintercept = t_star, linetype = "dotted") } +
  labs(title = "Prisbanen og overgang til fornybar substitutt",
       x = "Periode", y = "Pris / kostnad",
       colour = "Kurve") +
  scale_x_continuous(breaks = 1:N) +
  theme_minimal(base_size = 14)

print(p2)

# (Valgfritt) Skriv ut litt nøkkelinformasjon
cat("\n--- Nøkkeltall ---\n")
cat(sprintf("mu* (PV av MUC) = %.4f\n", mu_star))
cat(sprintf("Overgangstidspunkt t* = %s\n", ifelse(is.na(t_star), "ingen (lager tømmes før backstop nås)", t_star)))
cat(sprintf("Brukt lager før/ved overgang = %.3f, Gjenstående lager = %.3f\n", S_brukt, S_igjen))
cat(sprintf("Etterspørsel ved backstop-pris (per periode) q_sub = %.3f\n", q_sub))



##### MUC: Konstant MC vs. Økende MC #####




# ---------------------------
# To perioder, stigende kostnad i periode 2
# Viser tilbud/etterspørsel og MUC i begge perioder
# ---------------------------

# Parametre (som tidligere)
S  <- 40       # total ressurs
r  <- 0.10     # diskonteringsrente
a  <- 8        # P = a - b q
b  <- 0.40
c0 <- 2        # grunnleggende MC (periode 1)
k  <- 0.10     # økning i MC2 per enhet brukt i p1: MC2 = c0 + k*q1

# FOC: a - b q1 - c0 = [(a - b(S - q1) - (c0 + k q1))]/(1+r)
num <- (a - c0) - (a - b*S - c0)/(1+r)
den <- b + (b - k)/(1+r)
q1  <- max(0, min(S, num/den))
q2  <- S - q1

# MC og priser i likevekt
MC1 <- c0
MC2 <- c0 + k*q1
P1  <- a - b*q1
P2  <- a - b*q2

# MUC i hver periode (current value)
MUC1 <- P1 - MC1
MUC2 <- P2 - MC2

# Data til plott
qmax <- max(q1, q2) * 1.25
grid <- data.frame(q = seq(0, qmax, length.out = 300))

d1 <- grid %>%
  mutate(Periode = "Periode 1",
         P = a - b*q,
         MC = MC1,
         q_star = q1, P_star = P1, MUC = MUC1)

d2 <- grid %>%
  mutate(Periode = "Periode 2",
         P = a - b*q,
         MC = MC2,               # stigende MC i p2
         q_star = q2, P_star = P2, MUC = MUC2)

df <- bind_rows(d1, d2)

# Plot: Etterspørsel, MC, prislinje i likevekt og MUC-markør
ggplot(df, aes(x = q)) +
  geom_line(aes(y = P), linewidth = 1.1) +                       # Etterspørsel
  geom_hline(aes(yintercept = MC), linewidth = 1) +              # Tilbud (MC)
  # Prislinje ved likevekt
  geom_segment(aes(x = 0, xend = q_star, y = P_star, yend = P_star),
               linewidth = 0.8, linetype = "dashed") +
  # MUC-markør (vertikal avstand ved q*)
  geom_segment(aes(x = q_star, xend = q_star, y = MC, yend = P_star),
               linewidth = 1.5, color = "steelblue") +
  # Punkt og etiketter
  geom_point(aes(x = q_star, y = P_star), size = 2) +
  geom_text(aes(x = q_star, y = (P_star + MC)/2,
                label = paste0("MUC = ", round(MUC, 2))),
            nudge_x = 0.03*max(df$q), vjust = -0.5, color = "steelblue") +
  geom_text(data = distinct(df, Periode, q_star, P_star),
            aes(x = q_star, y = P_star, label = paste0("P* = ", round(P_star, 2))),
            nudge_y = 0.35) +
  facet_wrap(~ Periode, scales = "free_x") +
  labs(
    title = "To perioder med stigende kostnad i periode 2",
    subtitle = sprintf("S=%g, r=%.1f%%, P(q)=%g - %.2fq, MC1=%g, MC2=c0+k·q1=%.2f (k=%.2f)",
                       S, 100*r, a, b, c0, MC2, k),
    x = "Kvantum (q)", y = "Pris / kostnad"
  )


# ---------------------------
# Konsument overskud med bruk av litt for mye resurser

# Parametre (som tidligere)
S  <- 9000       # total ressurs
r  <- 0.10     # diskonteringsrente
a  <- 10000        # P = a - b q
b  <- 1
MC <- 100        # grunnleggende MC (periode 1)



# Nøkkeltall
Q_max  <- a / b
Q_star <- (a - MC) / b
P_star <- MC
CS     <- (a - MC)*Q_star - 0.5*b*Q_star^2

cat("\n--- Én-periode ---\n")
cat(sprintf("Q* = %.2f, P* = %.2f, CS = %.2f\n", Q_star, P_star, CS))

# Data til plott
df <- tibble(q = seq(0, Q_max, 0.1),
             demand = a - b*q,
             mc = MC)

ggplot(df, aes(x = q)) +
  geom_line(aes(y = demand), linewidth = 1.1) +
  geom_line(aes(y = mc), linewidth = 1.1, linetype = "dashed") +
  geom_vline(xintercept = Q_star, linetype = "dotted") +
  geom_hline(yintercept = P_star, linetype = "dotted") +
  annotate("point", x = Q_star, y = P_star, linewidth = 3) +
  labs(title = "Én-periode: Etterspørsel og MC",
       subtitle = sprintf("Q* = %.1f, P* = %.1f", Q_star, P_star),
       x = "Mengde (q)", y = "Pris/kostnad") +
  theme_minimal(base_size = 14)



##############################
# 2) To-periodemodellen
##############################

# Parametre
S <- 9000    # total ressurs
r <- 0.10  # rente

P_demand <- function(q) pmax(0, a - b*q)
MNB <- function(q) P_demand(q) - MC

# Likevektsbetingelse: PV(MNB1) = PV(MNB2)
foc <- function(q1) MNB(q1) - MNB(S - q1)/(1 + r)
q1_star <- uniroot(foc, c(0, S))$root
q2_star <- S - q1_star

cat("\n--- To-periodemodellen ---\n")
cat(sprintf("q1* = %.2f, q2* = %.2f\n", q1_star, q2_star))

# Data til plott
grid <- tibble(q1 = seq(0, S, 0.1)) %>%
  mutate(`PV MNB (1)` = MNB(q1),
         `PV MNB (2)` = MNB(S - q1)/(1 + r)) %>%
  pivot_longer(-q1, names_to = "Kurve", values_to = "PV_MNB")

ggplot(grid, aes(x = q1, y = PV_MNB, colour = Kurve)) +
  geom_line(size = 1.1) +
  geom_vline(xintercept = q1_star, linetype = "dashed") +
  annotate("point", x = q1_star, y = MNB(q1_star), size = 3) +
  labs(title = "To-periodemodellen",
       subtitle = sprintf("q1* = %.1f, q2* = %.1f", q1_star, q2_star),
       x = expression(q[1]), y = "PV av MNB", colour = "") +
  theme_minimal(base_size = 14)

