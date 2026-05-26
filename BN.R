# Sally Clark Case: replication of Figure 15.12 e Table 15.2
# Fenton & Neil (2018), Capitolo 15

library(gRain)
library(ggplot2)

# PART 1 — BAYESIAN NETWORK

#Conditional probabilities from Fenton & Neil (2018), Table 15.1
#Values are identical for child A and B 
p_bruising <- c(0.997, 0.003, 0.868, 0.132)
p_disease  <- c(0.950, 0.050, 0.9984, 0.0016)

ev_cpt <- function(node, parent, vals)
  cptable(as.formula(paste0("~", node, "|", parent)), values = vals, levels = c("no","yes"))

# cause_B depends on cause_A to model sibling dependence:
# a family history of SIDS raises the prior probability of SIDS for the second child.
grain_net <- grain(compileCPT(list(
  cptable(~cause_A,
          values = c(92.17, 7.83), levels = c("SIDS","Murder")),
  cptable(~cause_B | cause_A,
          values = c(1-5.66e-4, 5.66e-4, 0.001, 0.999), levels = c("SIDS","Murder")),
  cptable(~findings | cause_A:cause_B,
          values = c(1,0,0, 0,1,0, 0,1,0, 0,0,1),
          levels = c("Neither_murdered","Either_murdered","Both_murdered")),
  cptable(~clark_guilty | findings,
          values = c(1,0, 0,1, 0,1), levels = c("No","Yes")),
  ev_cpt("bruising_A", "cause_A", p_bruising),
  ev_cpt("disease_A",  "cause_A", p_disease),
  ev_cpt("bruising_B", "cause_B", p_bruising),
  ev_cpt("disease_B",  "cause_B", p_disease)
)))

prior <- querygrain(grain_net, nodes = c(
  "cause_A","cause_B","findings","clark_guilty",
  "bruising_A","disease_A","bruising_B","disease_B"))

# PART 2 — TABLE 15.2
# Each step conditions the network on one additional piece of trial evidence,
# updating P(clark_guilty = Yes) cumulatively to replicate Table 15.2.

p0 <- prior$clark_guilty["Yes"] * 100
net1 <- setEvidence(grain_net, "bruising_A", "yes")
p1   <- querygrain(net1, "clark_guilty")$clark_guilty["Yes"] * 100
net2 <- setEvidence(net1, "disease_A", "no")
p2   <- querygrain(net2, "clark_guilty")$clark_guilty["Yes"] * 100
net3 <- setEvidence(net2, "bruising_B", "yes")
p3   <- querygrain(net3, "clark_guilty")$clark_guilty["Yes"] * 100
net4 <- setEvidence(net3, "disease_B", "no")
p4   <- querygrain(net4, "clark_guilty")$clark_guilty["Yes"] * 100

cat("\n", strrep("=",68), "\n")
cat("  Table 15.2 — Impact of evidence in original trial\n")
cat(strrep("=",68), "\n")
cat(sprintf("  %-36s  %8s  %8s\n","Evidence Added","R","F&N(2018)"))
cat(strrep("-",68), "\n")
tab <- list(
  c("None",                           p0, 7.89),
  c("Child A bruising True",          p1,28.87),
  c("Child A signs of disease False", p2,30.93),
  c("Child B bruising True",          p3,69.13),
  c("Child B signs of disease False", p4,70.19))
for(r in tab)
  cat(sprintf("  %-36s  %7.2f%%  %7.2f%%\n",
              r[[1]], as.numeric(r[[2]]), as.numeric(r[[3]])))
cat(strrep("=",68),"\n\n")

# PART 3 — FIGURE 15.12

node_probs <- list(
  bruising_A   = data.frame(state=c("No","Yes"),
                            p=c(prior$bruising_A["no"],prior$bruising_A["yes"])*100),
  disease_A    = data.frame(state=c("No","Yes"),
                            p=c(prior$disease_A["no"],prior$disease_A["yes"])*100),
  cause_A      = data.frame(state=c("SIDS","Murder"),
                            p=c(prior$cause_A["SIDS"],prior$cause_A["Murder"])*100),
  cause_B      = data.frame(state=c("SIDS","Murder"),
                            p=c(prior$cause_B["SIDS"],prior$cause_B["Murder"])*100),
  bruising_B   = data.frame(state=c("No","Yes"),
                            p=c(prior$bruising_B["no"],prior$bruising_B["yes"])*100),
  disease_B    = data.frame(state=c("No","Yes"),
                            p=c(prior$disease_B["no"],prior$disease_B["yes"])*100),
  findings     = data.frame(
    state=c("Both murdered","Either murdered","Neither murdered"),
    p=c(prior$findings["Both_murdered"],
        prior$findings["Either_murdered"],
        prior$findings["Neither_murdered"])*100),
  clark_guilty = data.frame(state=c("No","Yes"),
                            p=c(prior$clark_guilty["No"],prior$clark_guilty["Yes"])*100)
)

node_info <- data.frame(
  id    = c("bruising_A","disease_A","cause_A","cause_B",
            "bruising_B","disease_B","findings","clark_guilty"),
  title = c("Child A bruising?",
            "Child A signs of disease?",
            "Child A cause of death",
            "Child B cause of death",
            "Child B bruising?",
            "Child B signs of disease?",
            "Findings",
            "Clark guilty?"),
  cx    = c(  2.8,   8.2,   5.5,  20.5,  17.8,  23.2,  13.0,  13.0),
  cy    = c( 14.5,  14.5,  10.8,  10.8,  14.5,  14.5,   6.5,   2.8),
  W     = c(  2.4,   2.7,   2.7,   2.7,   2.4,   2.7,   3.9,   2.4),
  H     = c(  0.92,  0.92,  0.92,  0.92,  0.92,  0.92,  1.28,  0.92),
  lbl_w = c(  0.72,  0.72,  0.72,  0.72,  0.72,  0.72,  1.80,  0.62),
  stringsAsFactors = FALSE
)

build_bars <- function(nd_id) {
  nd  <- node_info[node_info$id == nd_id,]
  dat <- node_probs[[nd_id]]
  ns  <- nrow(dat)
  
  title_h   <- 0.28
  avail_h   <- 2*nd$H - title_h
  step      <- avail_h / (ns + 0.30)
  bar_hh    <- step * 0.37
  top_bar_y <- nd$cy + nd$H - title_h - step * 0.65
  bar_cy    <- top_bar_y - (0:(ns-1)) * step
  
  lm      <- 0.10
  rm      <- 0.09
  lbl_w   <- nd$lbl_w
  bar_x0  <- nd$cx - nd$W + lm + lbl_w
  max_bw  <- 2*nd$W - lm - rm - lbl_w - 0.50   # -0.50 riserva spazio per testo %
  bar_x1  <- bar_x0 + (dat$p/100) * max_bw
  
  pct_x   <- pmin(bar_x1 + 0.06,  nd$cx + nd$W - rm - 0.04)
  
  data.frame(
    id     = nd_id,
    state  = dat$state,
    p      = dat$p,
    bar_x0 = bar_x0,
    bar_x1 = bar_x1,
    bg_x0  = bar_x0,
    bg_x1  = nd$cx + nd$W - rm,
    ymin   = bar_cy - bar_hh,
    ymax   = bar_cy + bar_hh,
    bar_cy = bar_cy,
    lbl_x  = nd$cx - nd$W + lm,
    pct_x  = pct_x,
    stringsAsFactors = FALSE
  )
}

bar_df <- do.call(rbind, lapply(node_info$id, build_bars))

make_section <- function(label, xmin, xmax, node_ids) {
  nds     <- node_info[node_info$id %in% node_ids,]
  node_top    <- max(nds$cy + nds$H)
  node_bottom <- min(nds$cy - nds$H)
  ymax <- node_top + 0.72
  ymin <- node_bottom - 0.30
  data.frame(label=label, xmin=xmin, xmax=xmax,
             ymin=ymin, ymax=ymax,
             lx=xmin+0.22, ly=ymax-0.10,
             stringsAsFactors=FALSE)
}

sections <- rbind(
  make_section("Child A - evidence",  0.2, 11.3,
               c("bruising_A","disease_A")),
  make_section("Child B - evidence", 15.1, 26.1,
               c("bruising_B","disease_B")),
  make_section("Judgements",          2.5, 23.5,
               c("cause_A","cause_B")),
  make_section("Conclusions",         8.7, 17.3,
               c("findings","clark_guilty"))
)

edges_raw <- list(
  c("cause_A","cause_B"),
  c("cause_A","bruising_A"), c("cause_A","disease_A"),
  c("cause_B","bruising_B"), c("cause_B","disease_B"),
  c("cause_A","findings"),   c("cause_B","findings"),
  c("findings","clark_guilty")
)

border_pt <- function(cx, cy, W, H, dx, dy) {
  n <- sqrt(dx^2+dy^2)
  if (n<1e-9) return(c(cx,cy))
  ux <- dx/n; uy <- dy/n
  tx <- if(abs(ux)>1e-9) W/abs(ux) else 1e9
  ty <- if(abs(uy)>1e-9) H/abs(uy) else 1e9
  t  <- min(tx,ty)*0.91
  c(cx+t*ux, cy+t*uy)
}

edge_df <- do.call(rbind, lapply(edges_raw, function(e) {
  f <- node_info[node_info$id==e[1],]
  t <- node_info[node_info$id==e[2],]
  dx <- t$cx-f$cx; dy <- t$cy-f$cy
  p0 <- border_pt(f$cx,f$cy,f$W,f$H, dx, dy)
  p1 <- border_pt(t$cx,t$cy,t$W,t$H,-dx,-dy)
  data.frame(xs=p0[1],ys=p0[2],xe=p1[1],ye=p1[2])
}))

fig <- ggplot() +
  
  geom_rect(data=sections,
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
            fill="#F2F2F2", colour="#AAAAAA", linewidth=0.35) +
  geom_text(data=sections,
            aes(x=lx,y=ly,label=label),
            hjust=0, vjust=1, size=3.2,
            fontface="italic", colour="#333333") +
  
  geom_segment(data=edge_df,
               aes(x=xs,y=ys,xend=xe,yend=ye),
               colour="#777777", linewidth=0.55,
               arrow=arrow(length=unit(0.17,"cm"),type="closed",ends="last")) +
  
  geom_rect(data=node_info,
            aes(xmin=cx-W,xmax=cx+W,ymin=cy-H,ymax=cy+H),
            fill="white", colour="black", linewidth=0.45) +
  
  geom_segment(data=node_info,
               aes(x=cx-W,xend=cx+W,y=cy+H-0.28,yend=cy+H-0.28),
               colour="#CCCCCC", linewidth=0.25) +
  
  geom_text(data=node_info,
            aes(x=cx, y=cy+H-0.14, label=title),
            hjust=0.5, vjust=0.5, size=2.9,
            fontface="bold", colour="#111111") +
  
  geom_rect(data=bar_df,
            aes(xmin=bg_x0,xmax=bg_x1,ymin=ymin,ymax=ymax),
            fill="#D5D5D5", colour=NA) +
  
  geom_rect(data=bar_df,
            aes(xmin=bar_x0,xmax=bar_x1,ymin=ymin,ymax=ymax),
            fill="#555555", colour=NA) +
  
  geom_text(data=bar_df,
            aes(x=lbl_x,y=bar_cy,label=state),
            hjust=0, vjust=0.5, size=2.55, colour="#111111") +
  
  geom_text(data=bar_df,
            aes(x=pct_x,y=bar_cy,
                label=sprintf("%.2f%%",p)),
            hjust=0, vjust=0.5, size=2.55, colour="#111111") +
  
  labs(title    = "Simple BN model for Sally Clark case",
       subtitle = "Replica di Fenton & Neil (2018), Figura 15.12  |  Stato prior") +
  
  coord_fixed(xlim=c(0,26.5), ylim=c(1.2,17.2), expand=FALSE) +
  theme_void() +
  theme(
    plot.title    = element_text(hjust=0.5,face="bold",
                                 size=12, margin=margin(b=3)),
    plot.subtitle = element_text(hjust=0.5,size=9,
                                 colour="#555555",margin=margin(b=5)),
    plot.margin   = margin(12,14,12,14)
  )

print(fig)

ggsave("figure_15_12_replica.pdf", fig,
       width=17, height=11, device="pdf")
ggsave("figure_15_12_replica.png", fig,
       width=17, height=11, dpi=300)
message("Salvato: figure_15_12_replica.pdf  e  .png")