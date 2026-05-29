# Bayesian Network replication — Sally Clark case
# Source: Fenton & Neil (2018), Chapter 15 Figure 15.12 and Table 15.2
library(gRain)   # Bayesian network inference
library(ggplot2) # plotting
setwd("c:/Users/alice/Documents/tesi R")

# PART 1: THE BAYESIAN NETWORK

# Conditional probabilities of symptoms given cause of death
# (from Table 15.1 in Fenton & Neil 2018)
# Format: P(no|SIDS), P(yes|SIDS), P(no|Murder), P(yes|Murder)

prob_bruising <- c(0.98988, 0.01012, 0.95177, 0.04823)
prob_disease  <- c(0.950, 0.050, 0.9984, 0.0016)

# Cause of death of child A (root node, no parents)
# Prior: 92.17% SIDS, 7.83% murder
cpt_cause_A <- cptable(~cause_A,
                        values = c(92.17, 7.83),
                        levels = c("SIDS", "Murder"))

# Cause of death of child B — depends on cause_A
# A genetic predisposition in the family raises the prior probability
# of SIDS for the second child if the first also died of SIDS
cpt_cause_B <- cptable(~cause_B | cause_A,
                        values = c(1 - 5.66e-4, 5.66e-4,   # if A = SIDS
                                         0.001,     0.999), # if A = Murder
                        levels = c("SIDS", "Murder"))

# The "findings" node summarises the combination of the two causes
cpt_findings <- cptable(~findings | cause_A:cause_B,
                         values = c(1, 0, 0,   # SIDS   + SIDS   -> neither murdered
                                    0, 1, 0,   # SIDS   + Murder -> either murdered
                                    0, 1, 0,   # Murder + SIDS   -> either murdered
                                    0, 0, 1),  # Murder + Murder -> both murdered
                         levels = c("Neither_murdered", "Either_murdered", "Both_murdered"))

cpt_guilty <- cptable(~clark_guilty | findings,
                       values = c(1, 0,   # neither murdered -> not guilty
                                  0, 1,   # either murdered  -> guilty
                                  0, 1),  # both murdered    -> guilty
                       levels = c("No", "Yes"))

# Evidence nodes for child A: bruising and signs of disease
# Both depend on cause_A
cpt_bruising_A <- cptable(~bruising_A | cause_A,
                           values = prob_bruising,
                           levels = c("no", "yes"))

cpt_disease_A <- cptable(~disease_A | cause_A,
                          values = prob_disease,
                          levels = c("no", "yes"))

# Evidence nodes for child B: bruising and signs of disease
# Both depend on cause_B
cpt_bruising_B <- cptable(~bruising_B | cause_B,
                           values = prob_bruising,
                           levels = c("no", "yes"))

cpt_disease_B <- cptable(~disease_B | cause_B,
                          values = prob_disease,
                          levels = c("no", "yes"))

bn <- grain(compileCPT(list(
  cpt_cause_A,
  cpt_cause_B,
  cpt_findings,
  cpt_guilty,
  cpt_bruising_A,
  cpt_disease_A,
  cpt_bruising_B,
  cpt_disease_B
)))

prior <- querygrain(bn, nodes = c(
  "cause_A", "cause_B", "findings", "clark_guilty",
  "bruising_A", "disease_A", "bruising_B", "disease_B"
))

# PART 2: TABLE 15.2 

# No evidence
p0 <- prior$clark_guilty["Yes"] * 100

# Evidence 1: bruising observed on child A
bn_1 <- setEvidence(bn, "bruising_A", "yes")
p1   <- querygrain(bn_1, "clark_guilty")$clark_guilty["Yes"] * 100

# Evidence 2: no signs of disease in child A
bn_2 <- setEvidence(bn_1, "disease_A", "no")
p2   <- querygrain(bn_2, "clark_guilty")$clark_guilty["Yes"] * 100

# Evidence 3: bruising observed on child B
bn_3 <- setEvidence(bn_2, "bruising_B", "yes")
p3   <- querygrain(bn_3, "clark_guilty")$clark_guilty["Yes"] * 100

# Evidence 4: no signs of disease in child B
bn_4 <- setEvidence(bn_3, "disease_B", "no")
p4   <- querygrain(bn_4, "clark_guilty")$clark_guilty["Yes"] * 100

# Print the table
cat("\n", strrep("=", 68), "\n")
cat("  Table 15.2 — Impact of evidence on P(Clark guilty)\n")
cat(strrep("=", 68), "\n")
cat(sprintf("  %-38s  %6s  %8s\n", "Evidence added", "Replica", "F&N(2018)"))
cat(strrep("-", 68), "\n")

rows <- list(
  c("No evidence",                          p0,  7.89),
  c("Child A bruising = True",              p1, 28.87),
  c("Child A signs of disease = False",     p2, 30.93),
  c("Child B bruising = True",              p3, 69.13),
  c("Child B signs of disease = False",     p4, 70.19)
)

for (row in rows) {
  cat(sprintf("  %-38s  %5.2f%%  %7.2f%%\n",
              row[[1]], as.numeric(row[[2]]), as.numeric(row[[3]])))
}
cat(strrep("=", 68), "\n\n")

# PART 3: FIGURE 15.12 — network visualisation

# Collect prior probabilities for each node in a ggplot-friendly format
node_probs <- list(
  bruising_A  = data.frame(state = c("No", "Yes"),
                            p = c(prior$bruising_A["no"], prior$bruising_A["yes"]) * 100),
  disease_A   = data.frame(state = c("No", "Yes"),
                            p = c(prior$disease_A["no"],  prior$disease_A["yes"])  * 100),
  cause_A     = data.frame(state = c("SIDS", "Murder"),
                            p = c(prior$cause_A["SIDS"],  prior$cause_A["Murder"]) * 100),
  cause_B     = data.frame(state = c("SIDS", "Murder"),
                            p = c(prior$cause_B["SIDS"],  prior$cause_B["Murder"]) * 100),
  bruising_B  = data.frame(state = c("No", "Yes"),
                            p = c(prior$bruising_B["no"], prior$bruising_B["yes"]) * 100),
  disease_B   = data.frame(state = c("No", "Yes"),
                            p = c(prior$disease_B["no"],  prior$disease_B["yes"])  * 100),
  findings    = data.frame(
    state = c("Both murdered", "Either murdered", "Neither murdered"),
    p = c(prior$findings["Both_murdered"],
          prior$findings["Either_murdered"],
          prior$findings["Neither_murdered"]) * 100),
  clark_guilty = data.frame(state = c("No", "Yes"),
                             p = c(prior$clark_guilty["No"],
                                   prior$clark_guilty["Yes"]) * 100)
)

node_info <- data.frame(
  id    = c("bruising_A", "disease_A", "cause_A", "cause_B",
            "bruising_B", "disease_B", "findings", "clark_guilty"),
  label = c("Child A bruising?",
            "Child A signs of disease?",
            "Child A cause of death",
            "Child B cause of death",
            "Child B bruising?",
            "Child B signs of disease?",
            "Findings",
            "Clark guilty?"),
  cx    = c(  2.8,   8.2,   5.5,  20.5,  17.8,  23.2,  13.0,  13.0),
  cy    = c( 15.0,  15.0,  10.0,  10.0,  15.0,  15.0,   5.5,   1.5),
  W     = c(  2.4,   2.7,   2.7,   2.7,   2.4,   2.7,   3.9,   2.4),
  H     = c(  1.30,  1.30,  1.30,  1.30,  1.30,  1.30,  1.80,  1.30),
  lbl_w = c(  0.90,  0.90,  0.90,  0.90,  0.90,  0.90,  2.10,  0.80),
  stringsAsFactors = FALSE
)

build_bars <- function(node_id) {
  nd      <- node_info[node_info$id == node_id, ]
  dat     <- node_probs[[node_id]]
  n_states <- nrow(dat)

  title_h  <- 0.40                          
  free_h   <- 2 * nd$H - title_h           
  step     <- free_h / (n_states + 0.30)   
  bar_hh   <- step * 0.37                  
  top_y    <- nd$cy + nd$H - title_h - step * 0.65
  bar_cy   <- top_y - (0:(n_states - 1)) * step

  margin_l  <- 0.10
  margin_r  <- 0.09
  lw        <- nd$lbl_w
  x_start   <- nd$cx - nd$W + margin_l + lw
  max_width <- 2 * nd$W - margin_l - margin_r - lw - 0.50
  x_end     <- x_start + (dat$p / 100) * max_width

  x_pct <- pmin(x_end + 0.06, nd$cx + nd$W - margin_r - 0.04)

  data.frame(
    id      = node_id,
    state   = dat$state,
    p       = dat$p,
    bar_x0  = x_start,
    bar_x1  = x_end,
    bg_x0   = x_start,
    bg_x1   = nd$cx + nd$W - margin_r,
    ymin    = bar_cy - bar_hh,
    ymax    = bar_cy + bar_hh,
    bar_cy  = bar_cy,
    lbl_x   = nd$cx - nd$W + margin_l,
    x_pct   = x_pct,
    stringsAsFactors = FALSE
  )
}

df_bars <- do.call(rbind, lapply(node_info$id, build_bars))

# Build rectangles grouping nodes into sections
make_section <- function(section_label, xmin, xmax, node_ids) {
  nds  <- node_info[node_info$id %in% node_ids, ]
  ymax <- max(nds$cy + nds$H) + 0.72
  ymin <- min(nds$cy - nds$H) - 0.30
  data.frame(section_label = section_label, xmin = xmin, xmax = xmax,
             ymin = ymin, ymax = ymax,
             lx = xmin + 0.22, ly = ymax - 0.10,
             stringsAsFactors = FALSE)
}

sections <- rbind(
  make_section("Child A - evidence",  0.2, 11.3, c("bruising_A", "disease_A")),
  make_section("Child B - evidence", 15.1, 26.1, c("bruising_B", "disease_B")),
  make_section("Judgements",          2.5, 23.5, c("cause_A", "cause_B")),
  make_section("Conclusions",         8.7, 17.3, c("findings", "clark_guilty"))
)

# Directed edges
edges_raw <- list(
  c("cause_A", "cause_B"),
  c("cause_A", "bruising_A"), c("cause_A", "disease_A"),
  c("cause_B", "bruising_B"), c("cause_B", "disease_B"),
  c("cause_A", "findings"),   c("cause_B", "findings"),
  c("findings", "clark_guilty")
)

border_point <- function(cx, cy, W, H, dx, dy) {
  n <- sqrt(dx^2 + dy^2)
  if (n < 1e-9) return(c(cx, cy))
  ux <- dx / n; uy <- dy / n
  tx <- if (abs(ux) > 1e-9) W / abs(ux) else 1e9
  ty <- if (abs(uy) > 1e-9) H / abs(uy) else 1e9
  t  <- min(tx, ty) * 0.91
  c(cx + t * ux, cy + t * uy)
}

df_edges <- do.call(rbind, lapply(edges_raw, function(e) {
  from <- node_info[node_info$id == e[1], ]
  to   <- node_info[node_info$id == e[2], ]
  dx   <- to$cx - from$cx
  dy   <- to$cy - from$cy
  p0   <- border_point(from$cx, from$cy, from$W, from$H,  dx,  dy)
  p1   <- border_point(to$cx,   to$cy,   to$W,   to$H,  -dx, -dy)
  data.frame(xs = p0[1], ys = p0[2], xe = p1[1], ye = p1[2])
}))

#the figure
fig <- ggplot() +

  geom_rect(data = sections,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "#F2F2F2", colour = "#AAAAAA", linewidth = 0.35) +
  geom_text(data = sections,
            aes(x = lx, y = ly, label = section_label),
            hjust = 0, vjust = 1, size = 5.5,
            fontface = "italic", colour = "#333333") +

  geom_segment(data = df_edges,
               aes(x = xs, y = ys, xend = xe, yend = ye),
               colour = "#777777", linewidth = 0.55,
               arrow = arrow(length = unit(0.17, "cm"), type = "closed")) +


  geom_rect(data = node_info,
            aes(xmin = cx - W, xmax = cx + W, ymin = cy - H, ymax = cy + H),
            fill = "white", colour = "black", linewidth = 0.45) +

  geom_segment(data = node_info,
               aes(x = cx - W, xend = cx + W,
                   y = cy + H - 0.40, yend = cy + H - 0.40),
               colour = "#CCCCCC", linewidth = 0.25) +

geom_text(data = node_info,
            aes(x = cx, y = cy + H - 0.20, label = label),
            hjust = 0.5, vjust = 0.5, size = 5.0,
            fontface = "bold", colour = "#111111") +

geom_rect(data = df_bars,
            aes(xmin = bg_x0, xmax = bg_x1, ymin = ymin, ymax = ymax),
            fill = "#D5D5D5", colour = NA) +

  geom_rect(data = df_bars,
            aes(xmin = bar_x0, xmax = bar_x1, ymin = ymin, ymax = ymax),
            fill = "#555555", colour = NA) +

  geom_text(data = df_bars,
            aes(x = lbl_x, y = bar_cy, label = state),
            hjust = 0, vjust = 0.5, size = 4.0, colour = "#111111") +

  geom_text(data = df_bars,
            aes(x = x_pct, y = bar_cy, label = sprintf("%.2f%%", p)),
            hjust = 0, vjust = 0.5, size = 4.0, colour = "#111111") +

  labs(
    title    = "Bayesian Network — Sally Clark case",
    subtitle = "Replication of Fenton & Neil (2018), Figure 15.12  |  Prior state"
  ) +

  coord_fixed(xlim = c(0, 26.5), ylim = c(-0.5, 18.0), expand = FALSE) +
  theme_void() +
  theme(
    plot.title    = element_text(hjust = 0.5, face = "bold",
                                 size = 18, margin = margin(b = 4)),
    plot.subtitle = element_text(hjust = 0.5, size = 13,
                                 colour = "#555555", margin = margin(b = 6)),
    plot.margin   = margin(12, 14, 12, 14)
  )

print(fig)

ggsave("figure_15_12_replication.pdf", fig, width = 17, height = 11, device = "pdf")
ggsave("figure_15_12_replication.png", fig, width = 17, height = 11, dpi = 600)
win.metafile("figure_15_12_replication.emf", width = 17, height = 11)
print(fig)
dev.off()

message("Saved: .pdf  .png  .emf")