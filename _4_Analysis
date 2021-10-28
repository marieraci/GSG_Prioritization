# Create data frame ----
# this dataframe calculates a variety of metrics for
# solutions with masked out landowners

vars1 <- data.frame(
  "num_PUs" = numeric(), 
  "contiguity" = numeric(),
  "cost_threat" = numeric(),
  "irrep_sum" = numeric(),
  "irrep_1" = numeric(),
  "irrep_1_prop" = numeric(),
  "irrep_pu" = numeric(),
  "irrep_08" = numeric(),
  "irrep_08_prop" = numeric(),
  "roi_threat" = numeric(),
  "nesting" = numeric(),
  "brood" = numeric(),
  "winter" = numeric(),
  "LC" = numeric(),
  "overall_benefit" = numeric(),
  "PAC_overlap" = numeric(),
  "cost_per_PU_threat" = numeric()
)

# add values to dataframe
i <- 1
for (i in 1:nlayers(irrep_mask)) {
  vars1 <- vars1 %>%
    add_row(
      num_PUs = cellStats(mask(
        mask(
          cost1,
          irrep_mask[[i]],
          maskvalue = 0,
          updatevalue = NA
        ),
        irrep_mask[[i]],
        maskvalue = NA,
        updatevalue = NA
      ), sum),
      contiguity = mean(lsm_c_contig_mn(irrep_mask[[i]])$value),
      cost_threat = cellStats(mask(
        mask(
          cost3,
          irrep_mask[[i]],
          maskvalue = 0,
          updatevalue = NA
        ),
        irrep_mask[[i]],
        maskvalue = NA,
        updatevalue = NA
      ), sum),
      irrep_sum = cellStats(irrep_mask[[i]], 'sum'),
      irrep_1 = sum(getValues(irrep_mask[[i]]) == 1, na.rm=TRUE),
      irrep_1_prop = (irrep_1 / irrep_sum)*100,
      irrep_pu = irrep_sum/num_PUs,
      irrep_08 = sum(getValues(irrep_mask[[i]]) >= 0.8, na.rm=TRUE),
      irrep_08_prop = (irrep_08 / irrep_sum)*100,
      roi_threat = cellStats(irrep_mask[[i]], 'sum') /
        cost_threat,
      nesting = (cellStats(mask(
        mask(
          nesting_ddct,
          irrep_mask[[i]],
          maskvalue = 0,
          updatevalue = NA
        ),
        irrep_mask[[i]],
        maskvalue = NA,
        updatevalue = NA
      ), sum) / cellStats(nesting_ddct, sum))*100,
      brood = (cellStats(mask(
        mask(
          brood_ddct,
          irrep_mask[[i]],
          maskvalue = 0,
          updatevalue = NA
        ),
        irrep_mask[[i]],
        maskvalue = NA,
        updatevalue = NA
      ), sum) / cellStats(brood_ddct, sum))*100,
      winter = (cellStats(mask(
        mask(
          winter_ddct,
          irrep_mask[[i]],
          maskvalue = 0,
          updatevalue = NA
        ),
        irrep_mask[[i]],
        maskvalue = NA,
        updatevalue = NA
      ), sum) / cellStats(winter_ddct, sum))*100,
      LC = (cellStats(mask(
        mask(
          LC_rescale,
          irrep_mask[[i]],
          maskvalue = 0,
          updatevalue = NA
        ),
        irrep_mask[[i]],
        maskvalue = NA,
        updatevalue = NA
      ), sum) / cellStats(LC_rescale, sum))*100,
      overall_benefit = (nesting + brood + winter + LC),
      benefit_roi_threat = (overall_benefit / cost_threat)*1000,
      PAC_overlap = freq(sum(
        mask(
          mask(
            cost1,
            irrep_mask[[i]],
            maskvalue = 0,
            updatevalue = NA
          ),
          irrep_mask[[i]],
          maskvalue = NA,
          updatevalue = NA
        ), PACs
      ), value = 2),
      cost_per_PU_threat = cost_threat / num_PUs
    )
  i <- i + 1
}

# add groups to dataframe

# 2. feature weight group
vars1 <- vars1 %>%
  cbind(fw_group = c(rep("Equal", 20),
                     rep("B", 4),
                     rep("W&B", 4),
                     rep("LC", 4),
                     rep("Expert", 4),
                     rep("B", 4),
                     rep("W&B", 4),
                     rep("LC", 4),
                     rep("Expert", 4)))

# 3. conservation feature group
vars1 <- vars1 %>%
  cbind(features = c(
    rep("nesting", 4),
    rep("brood", 4),
    rep("winter", 4),
    rep("annual", 4),
    rep("multisp", 4),
    rep("annual", 16),
    rep("multisp", 16)
  ))

# 4. land owner group group
vars1 <- vars1 %>%
  cbind(land_owner = rep(c(rep("public", 1),
                           rep("private", 1)), 26))

# 5. PACs included?
vars1 <- vars1 %>%
  cbind(PACs_inc = c(rep(c(
    rep("no", 2), rep("yes", 2)
  ), 13)))


# create another dataframe for
# non-masked solutions (irrep1)

vars2 <- data.frame(
  "num_PUs" = numeric(),
  "contiguity" = numeric(),
  "cost_threat" = numeric(),
  "irrep_sum" = numeric(),
  "irrep_1" = numeric(),
  "irrep_1_prop" = numeric(),
  "irrep_pu" = numeric(),
  "irrep_08" = numeric(),
  "irrep_08_prop" = numeric(),
  "roi_threat" = numeric(),
  "nesting" = numeric(),
  "brood" = numeric(),
  "winter" = numeric(),
  "LC" = numeric(),
  "overall_benefit" = numeric(),
  "benefit_roi_threat" = numeric(),
  "PAC_overlap" = numeric(),
  "cost_per_PU_threat" = numeric()
)

# add values to dataframe
i <- 1
for (i in 1:nlayers(irrep1)) {
  vars2 <- vars2 %>%
    add_row(
      num_PUs = cellStats(mask(
        mask(
          cost1,
          irrep1[[i]],
          maskvalue = 0,
          updatevalue = NA
        ),
        irrep1[[i]],
        maskvalue = NA,
        updatevalue = NA
      ), sum),
      contiguity = mean(lsm_c_contig_mn(irrep1[[i]])$value),
      cost_threat = cellStats(mask(
        mask(
          cost3,
          irrep1[[i]],
          maskvalue = 0,
          updatevalue = NA
        ),
        irrep1[[i]],
        maskvalue = NA,
        updatevalue = NA
      ), sum),
      irrep_sum = cellStats(irrep1[[i]], 'sum'),
      irrep_1 = sum(getValues(irrep1[[i]]) == 1, na.rm=TRUE),
      irrep_1_prop = (irrep_1 / irrep_sum)*100,
      irrep_pu = irrep_sum/num_PUs,
      irrep_08 = sum(getValues(irrep1[[i]]) >= 0.8, na.rm=TRUE),
      irrep_08_prop = (irrep_08 / irrep_sum)*100,
      roi_threat = cellStats(irrep1[[i]], 'sum') /
        cost_threat,
      nesting = (cellStats(mask(
        mask(
          nesting_ddct,
          irrep1[[i]],
          maskvalue = 0,
          updatevalue = NA
        ),
        irrep1[[i]],
        maskvalue = NA,
        updatevalue = NA
      ), sum) / cellStats(nesting_ddct, sum))*100,
      brood = (cellStats(mask(
        mask(
          brood_ddct,
          irrep1[[i]],
          maskvalue = 0,
          updatevalue = NA
        ),
        irrep1[[i]],
        maskvalue = NA,
        updatevalue = NA
      ), sum) / cellStats(brood_ddct, sum))*100,
      winter = (cellStats(mask(
        mask(
          winter_ddct,
          irrep1[[i]],
          maskvalue = 0,
          updatevalue = NA
        ),
        irrep1[[i]],
        maskvalue = NA,
        updatevalue = NA
      ), sum) / cellStats(winter_ddct, sum))*100,
      LC = (cellStats(mask(
        mask(
          LC_rescale,
          irrep1[[i]],
          maskvalue = 0,
          updatevalue = NA
        ),
        irrep1[[i]],
        maskvalue = NA,
        updatevalue = NA
      ), sum) / cellStats(LC_rescale, sum))*100,
      overall_benefit = (nesting + brood + winter + LC),
      benefit_roi_threat = (overall_benefit / cost_threat)*1000,
      PAC_overlap = freq(sum(
        mask(
          mask(
            cost1,
            irrep1[[i]],
            maskvalue = 0,
            updatevalue = NA
          ),
          irrep1[[i]],
          maskvalue = NA,
          updatevalue = NA
        ), PACs
      ), value = 2),
      cost_per_PU_threat = cost_threat / num_PUs
    )
  i <- i + 1
}

# add groups to dataframe

# 2. feature weight group
vars2 <- vars2 %>%
  cbind(fw_group = c(rep("Equal", 10),
                     rep("B", 2),
                     rep("W&B", 2),
                     rep("LC", 2),
                     rep("Expert", 2),
                     rep("B", 2),
                     rep("W&B", 2),
                     rep("LC", 2),
                     rep("Expert", 2)))

# 3. conservation feature group
vars2 <- vars2 %>%
  cbind(features = c(
    rep("nesting", 2),
    rep("brood", 2),
    rep("winter", 2),
    rep("annual", 2),
    rep("multisp", 2),
    rep("annual", 8),
    rep("multisp", 8)
  ))


# 5. PACs included?
vars2 <- vars2 %>%
  cbind(PACs_inc = c(rep(c(
    rep("no", 1), rep("yes", 1)
  ), 13)))


# Analyze Solutions and Parameter Relationships

# RESULT 1: summary info ----

# This section contains calculations including:
# representation of HSMs in PACs
# land ownership and vulnerability/ development
# representation and land ownership
# representation and vulnerability
# overlap between conservation features and cost features

# threshold 1 : 0.5

# % focal habitat that is within a PAC
n_focal <- nesting
n_focal[n_focal>=0.5] <- 1
n_focal[n_focal!=1] <- 0
n_coverage <- n_focal + PACs
n_coverage[n_coverage!=2] <- 0
n_coverage[n_coverage==2] <- 1
cellStats(n_coverage,'sum')/cellStats(n_focal,'sum') * 100
# 80.2%
b_focal <- brood
b_focal[b_focal>=0.5] <- 1
b_focal[b_focal!=1] <- 0
b_coverage <- b_focal + PACs
b_coverage[b_coverage!=2] <- 0
b_coverage[b_coverage==2] <- 1
cellStats(b_coverage,'sum')/cellStats(b_focal,'sum') * 100
# 78.6%
w_focal <- winter
w_focal[w_focal>=0.5] <- 1
w_focal[w_focal!=1] <- 0
w_coverage <- w_focal + PACs
w_coverage[w_coverage!=2] <- 0
w_coverage[w_coverage==2] <- 1
cellStats(w_coverage,'sum')/cellStats(w_focal,'sum') * 100
# 84.7%

# threshold 2 : 0.75

# % focal habitat that is within a PAC
n_focal <- nesting
n_focal[n_focal>=0.75] <- 1
n_focal[n_focal!=1] <- 0
n_coverage <- n_focal + PACs
n_coverage[n_coverage!=2] <- 0
n_coverage[n_coverage==2] <- 1
cellStats(n_coverage,'sum')/cellStats(n_focal,'sum') * 100
# 90.95%
b_focal <- brood
b_focal[b_focal>=0.75] <- 1
b_focal[b_focal!=1] <- 0
b_coverage <- b_focal + PACs
b_coverage[b_coverage!=2] <- 0
b_coverage[b_coverage==2] <- 1
cellStats(b_coverage,'sum')/cellStats(b_focal,'sum') * 100
# 42.83%
w_focal <- winter
w_focal[w_focal>=0.75] <- 1
w_focal[w_focal!=1] <- 0
w_coverage <- w_focal + PACs
w_coverage[w_coverage!=2] <- 0
w_coverage[w_coverage==2] <- 1
cellStats(w_coverage,'sum')/cellStats(w_focal,'sum') * 100
# 37.28%

# threshold 3: all habitat > 0

# % focal habitat that is within a PAC
n_focal <- nesting
n_focal[n_focal>0] <- 1
n_focal[n_focal!=1] <- 0
n_coverage <- n_focal + PACs
n_coverage[n_coverage!=2] <- 0
n_coverage[n_coverage==2] <- 1
cellStats(n_coverage,'sum')/cellStats(n_focal,'sum') * 100
# 44.13%
b_focal <- brood
b_focal[b_focal>0] <- 1
b_focal[b_focal!=1] <- 0
b_coverage <- b_focal + PACs
b_coverage[b_coverage!=2] <- 0
b_coverage[b_coverage==2] <- 1
cellStats(b_coverage,'sum')/cellStats(b_focal,'sum') * 100
# 43.95%
w_focal <- winter
w_focal[w_focal>0] <- 1
w_focal[w_focal!=1] <- 0
w_coverage <- w_focal + PACs
w_coverage[w_coverage!=2] <- 0
w_coverage[w_coverage==2] <- 1
cellStats(w_coverage,'sum')/cellStats(w_focal,'sum') * 100
# 44.31%

# threshold 3: 0.75 quantile

# % focal habitat that is within a PAC
n_focal <- nesting
n_focal[n_focal>=0.2883271] <- 1
n_focal[n_focal!=1] <- 0
n_coverage <- n_focal + PACs
n_coverage[n_coverage!=2] <- 0
n_coverage[n_coverage==2] <- 1
cellStats(n_coverage,'sum')/cellStats(n_focal,'sum') * 100
# 69.18%
b_focal <- brood
b_focal[b_focal>=0.2944251] <- 1
b_focal[b_focal!=1] <- 0
b_coverage <- b_focal + PACs
b_coverage[b_coverage!=2] <- 0
b_coverage[b_coverage==2] <- 1
cellStats(b_coverage,'sum')/cellStats(b_focal,'sum') * 100
# 65.08%
w_focal <- winter
w_focal[w_focal>=0.12444113] <- 1
w_focal[w_focal!=1] <- 0
w_coverage <- w_focal + PACs
w_coverage[w_coverage!=2] <- 0
w_coverage[w_coverage==2] <- 1
cellStats(w_coverage,'sum')/cellStats(w_focal,'sum') * 100
# 72.74%

# Land ownership
# 1. BLM owned lands and HSMs

# get a raster of just BLM and USBR lands

BLM1 <- mang_type
BLM1[BLM1==1] <- 0 
BLM1[BLM1==2] <- 0
BLM1[BLM1==0] <- 1
BLM1[BLM1!=1] <- 0
is.na(BLM1) <- 0 
BLM1 <-  BLM1 + bckg

# total public lands: 
cellStats(BLM1,sum)/cellStats(cost1,sum)
# 71.46%

# assess coverage with each season

# a. any habitat over 0

# % focal habitat that is on BLM
n_focal <- nesting
n_focal[n_focal>0] <- 1
n_focal[n_focal!=1] <- 0
n_coverage <- n_focal + BLM1
n_coverage[n_coverage!=2] <- 0
n_coverage[n_coverage==2] <- 1
cellStats(n_coverage,'sum')/cellStats(n_focal,'sum') * 100
# 72.01%
b_focal <- brood
b_focal[b_focal>0] <- 1
b_focal[b_focal!=1] <- 0
b_coverage <- b_focal + BLM1
b_coverage[b_coverage!=2] <- 0
b_coverage[b_coverage==2] <- 1
cellStats(b_coverage,'sum')/cellStats(b_focal,'sum') * 100
# 71.90%
w_focal <- winter
w_focal[w_focal>0] <- 1
w_focal[w_focal!=1] <- 0
w_coverage <- w_focal + BLM1
w_coverage[w_coverage!=2] <- 0
w_coverage[w_coverage==2] <- 1
cellStats(w_coverage,'sum')/cellStats(w_focal,'sum') * 100
# 71.77%

# b. top habitat >= 0.75

# % focal habitat on BLM
n_focal <- nesting
n_focal[n_focal>=0.75] <- 1
n_focal[n_focal!=1] <- 0
n_coverage <- n_focal + BLM1
n_coverage[n_coverage!=2] <- 0
n_coverage[n_coverage==2] <- 1
cellStats(n_coverage,'sum')/cellStats(n_focal,'sum') * 100
# 87.71%
b_focal <- brood
b_focal[b_focal>=0.75] <- 1
b_focal[b_focal!=1] <- 0
b_coverage <- b_focal + BLM1
b_coverage[b_coverage!=2] <- 0
b_coverage[b_coverage==2] <- 1
cellStats(b_coverage,'sum')/cellStats(b_focal,'sum') * 100
# 92.67%
w_focal <- winter
w_focal[w_focal>=0.75] <- 1
w_focal[w_focal!=1] <- 0
w_coverage <- w_focal + BLM1
w_coverage[w_coverage!=2] <- 0
w_coverage[w_coverage==2] <- 1
cellStats(w_coverage,'sum')/cellStats(w_focal,'sum') * 100
# 83.93%

# c. upper quantile

# % focal habitat on BLM
n_focal <- nesting
n_focal[n_focal>=quantile(nesting, probs = c(0.75))] <- 1
n_focal[n_focal!=1] <- 0
n_coverage <- n_focal + BLM1
n_coverage[n_coverage!=2] <- 0
n_coverage[n_coverage==2] <- 1
cellStats(n_coverage,'sum')/cellStats(n_focal,'sum') * 100
# 77.76%
b_focal <- brood
b_focal[b_focal>=quantile(brood, probs = c(0.75))] <- 1
b_focal[b_focal!=1] <- 0
b_coverage <- b_focal + BLM1
b_coverage[b_coverage!=2] <- 0
b_coverage[b_coverage==2] <- 1
cellStats(b_coverage,'sum')/cellStats(b_focal,'sum') * 100
# 78.96
w_focal <- winter
w_focal[w_focal>=quantile(winter, probs = c(0.75))] <- 1
w_focal[w_focal!=1] <- 0
w_coverage <- w_focal + BLM1
w_coverage[w_coverage!=2] <- 0
w_coverage[w_coverage==2] <- 1
cellStats(w_coverage,'sum')/cellStats(w_focal,'sum') * 100
# 80.08%

# 2. private land and HSMs

# get a raster of just private lands

private1 <- mang_type
private1[private1!=3] <- 0
private1[private1==3] <- 1
private1 <-  private1 + bckg

# total private lands: 
cellStats(private1,sum)/cellStats(cost1,sum)
# 22.86

# assess coverage with each season

# a. any habitat over 0

# % focal habitat and land owner
n_focal <- nesting
n_focal[n_focal>0] <- 1
n_focal[n_focal!=1] <- 0
n_coverage <- n_focal + private1
n_coverage[n_coverage!=2] <- 0
n_coverage[n_coverage==2] <- 1
cellStats(n_coverage,'sum')/cellStats(n_focal,'sum') * 100
# 22.75%
b_focal <- brood
b_focal[b_focal>0] <- 1
b_focal[b_focal!=1] <- 0
b_coverage <- b_focal + private1
b_coverage[b_coverage!=2] <- 0
b_coverage[b_coverage==2] <- 1
cellStats(b_coverage,'sum')/cellStats(b_focal,'sum') * 100
# 22.86%
w_focal <- winter
w_focal[w_focal>0] <- 1
w_focal[w_focal!=1] <- 0
w_coverage <- w_focal + private1
w_coverage[w_coverage!=2] <- 0
w_coverage[w_coverage==2] <- 1
cellStats(w_coverage,'sum')/cellStats(w_focal,'sum') * 100
# 23.11%

# b. top habitat >= 0.75

# % focal habitat that is within a PAC
n_focal <- nesting
n_focal[n_focal>=0.75] <- 1
n_focal[n_focal!=1] <- 0
n_coverage <- n_focal + private1
n_coverage[n_coverage!=2] <- 0
n_coverage[n_coverage==2] <- 1
cellStats(n_coverage,'sum')/cellStats(n_focal,'sum') * 100
# 6.97%
b_focal <- brood
b_focal[b_focal>=0.75] <- 1
b_focal[b_focal!=1] <- 0
b_coverage <- b_focal + private1
b_coverage[b_coverage!=2] <- 0
b_coverage[b_coverage==2] <- 1
cellStats(b_coverage,'sum')/cellStats(b_focal,'sum') * 100
# 2.6%
w_focal <- winter
w_focal[w_focal>=0.75] <- 1
w_focal[w_focal!=1] <- 0
w_coverage <- w_focal + private1
w_coverage[w_coverage!=2] <- 0
w_coverage[w_coverage==2] <- 1
cellStats(w_coverage,'sum')/cellStats(w_focal,'sum') * 100
# 1.02%

# c. upper quantile

# % focal habitat on private
n_focal <- nesting
n_focal[n_focal>=quantile(nesting, probs = c(0.75))] <- 1
n_focal[n_focal!=1] <- 0
n_coverage <- n_focal + private1
n_coverage[n_coverage!=2] <- 0
n_coverage[n_coverage==2] <- 1
cellStats(n_coverage,'sum')/cellStats(n_focal,'sum') * 100
# 17.81%
b_focal <- brood
b_focal[b_focal>=quantile(brood, probs = c(0.75))] <- 1
b_focal[b_focal!=1] <- 0
b_coverage <- b_focal + private1
b_coverage[b_coverage!=2] <- 0
b_coverage[b_coverage==2] <- 1
cellStats(b_coverage,'sum')/cellStats(b_focal,'sum') * 100
# 15.62
w_focal <- winter
w_focal[w_focal>=quantile(winter, probs = c(0.75))] <- 1
w_focal[w_focal!=1] <- 0
w_coverage <- w_focal + private1
w_coverage[w_coverage!=2] <- 0
w_coverage[w_coverage==2] <- 1
cellStats(w_coverage,'sum')/cellStats(w_focal,'sum') * 100
# 15.87

# 3. Development potential and land ownership

# create raster of existing development

dev <- NLCD
dev[dev==81] <- 1 ## ag 1: pasture/hay
dev[dev==82] <- 1 ## ag 2: crops
dev[dev==21] <- 1 ## developed: open
dev[dev==22] <- 1 ## developed: low intensity
dev[dev==23] <- 1 ## developed: medium intensity
dev[dev==24] <- 1 ## developed: high intensity
dev[dev!=1] <- 0
# add padscars_buff, roads, topo, turbines, and bckg
dev <-  sum(dev,scar, na.rm = TRUE)
dev <-  sum(dev,roads, na.rm = TRUE)
dev <-  sum(dev,turbines, na.rm = TRUE)
dev[dev!=0] <- 1
# change the bckg to NA
dev <-  dev + bckg

# assess coverage of dev on each land type

dev_coverage1 <- dev + private1
dev_coverage2 <- dev + BLM1
dev_coverage1[dev_coverage1!=2] <- 0
dev_coverage1[dev_coverage1==2] <- 1
cellStats(dev_coverage1,'sum')/cellStats(private1,'sum') * 100
# 23.14
dev_coverage2[dev_coverage2!=2] <- 0
dev_coverage2[dev_coverage2==2] <- 1
cellStats(dev_coverage2,'sum')/cellStats(BLM1,'sum') * 100
# 20.69

# highly vulnerable areas on private and public lands

vul <- cost2
vul[vul>=1] <- 1
vul[vul!=1] <- 0
pr_vul <- vul + private1
pr_vul[pr_vul!=2] <- 0
pr_vul[pr_vul==2] <- 1
cellStats(pr_vul,'sum')/cellStats(vul,'sum') * 100
# 49.74%
pu_vul <- vul + BLM1
pu_vul[pu_vul!=2] <- 0
pu_vul[pu_vul==2] <- 1
cellStats(pu_vul,'sum')/cellStats(vul,'sum') * 100
# 45.19%

# total coverage of highly vulnerable areas:
cellStats(vul,sum)/cellStats(cost1,sum)
# 9.77

# vulnerable areas and HSMs

# any habitat > 0.5

# % focal habitat that is vulernable
n_focal <- nesting
n_focal[n_focal>=0.5] <- 1
n_focal[n_focal!=1] <- 0
n_coverage <- n_focal + vul
n_coverage[n_coverage!=2] <- 0
n_coverage[n_coverage==2] <- 1
cellStats(n_coverage,'sum')/cellStats(n_focal,'sum') * 100
# 6.97%
b_focal <- brood
b_focal[b_focal>=0.5] <- 1
b_focal[b_focal!=1] <- 0
b_coverage <- b_focal + vul
b_coverage[b_coverage!=2] <- 0
b_coverage[b_coverage==2] <- 1
cellStats(b_coverage,'sum')/cellStats(b_focal,'sum') * 100
# 2.6%
w_focal <- winter
w_focal[w_focal>=0.5] <- 1
w_focal[w_focal!=1] <- 0
w_coverage <- w_focal + vul
w_coverage[w_coverage!=2] <- 0
w_coverage[w_coverage==2] <- 1
cellStats(w_coverage,'sum')/cellStats(w_focal,'sum') * 100
# 1.02%

# quantile

# % focal habitat that is vulnerable
n_focal <- nesting
n_focal[n_focal>=quantile(nesting, probs = c(0.75))] <- 1
n_focal[n_focal!=1] <- 0
n_coverage <- n_focal + vul
n_coverage[n_coverage!=2] <- 0
n_coverage[n_coverage==2] <- 1
cellStats(n_coverage,'sum')/cellStats(n_focal,'sum') * 100
# 7.02%
b_focal <- brood
b_focal[b_focal>=quantile(nesting, probs = c(0.75))] <- 1
b_focal[b_focal!=1] <- 0
b_coverage <- b_focal + vul
b_coverage[b_coverage!=2] <- 0
b_coverage[b_coverage==2] <- 1
cellStats(b_coverage,'sum')/cellStats(b_focal,'sum') * 100
# 7.33%
w_focal <- winter
w_focal[w_focal>=quantile(nesting, probs = c(0.75))] <- 1
w_focal[w_focal!=1] <- 0
w_coverage <- w_focal + vul
w_coverage[w_coverage!=2] <- 0
w_coverage[w_coverage==2] <- 1
cellStats(w_coverage,'sum')/cellStats(w_focal,'sum') * 100
# 2.18%

# 4. Breaking down development by industry 
# in regards to each HSM
# oil and gas
# % habitat that is intersecting with padscars
n_focal <- nesting
n_focal[n_focal>=quantile(nesting, probs = c(0.75))] <- 1
n_focal[n_focal!=1] <- 0
n_coverage <- n_focal + scar_buff
n_coverage[n_coverage!=2] <- 0
n_coverage[n_coverage==2] <- 1
cellStats(n_coverage,'sum')/cellStats(n_focal,'sum') * 100
# 4.5
b_focal <- brood
b_focal[b_focal>=quantile(brood, probs = c(0.75))] <- 1
b_focal[b_focal!=1] <- 0
b_coverage <- b_focal + scar_buff
b_coverage[b_coverage!=2] <- 0
b_coverage[b_coverage==2] <- 1
cellStats(b_coverage,'sum')/cellStats(b_focal,'sum') * 100
# 4
w_focal <- winter
w_focal[w_focal>=quantile(winter, probs = c(0.75))] <- 1
w_focal[w_focal!=1] <- 0
w_coverage <- w_focal + scar_buff
w_coverage[w_coverage!=2] <- 0
w_coverage[w_coverage==2] <- 1
cellStats(w_coverage,'sum')/cellStats(w_focal,'sum') * 100
# 5

# wind
n_focal <- nesting
n_focal[n_focal>=quantile(nesting, probs = c(0.75))] <- 1
n_focal[n_focal!=1] <- 0
n_coverage <- n_focal + turbines
n_coverage[n_coverage!=2] <- 0
n_coverage[n_coverage==2] <- 1
cellStats(n_coverage,'sum')/cellStats(n_focal,'sum') * 100
# 0.53%
b_focal <- brood
b_focal[b_focal>=0.5] <- 1
b_focal[b_focal!=1] <- 0
b_coverage <- b_focal + turbines
b_coverage[b_coverage!=2] <- 0
b_coverage[b_coverage==2] <- 1
cellStats(b_coverage,'sum')/cellStats(b_focal,'sum') * 100
# 0.077%
w_focal <- winter
w_focal[w_focal>0] <- 1
w_focal[w_focal!=1] <- 0
w_coverage <- w_focal + turbines
w_coverage[w_coverage!=2] <- 0
w_coverage[w_coverage==2] <- 1
cellStats(w_coverage,'sum')/cellStats(w_focal,'sum') * 100
# 0.22%

# 5. development probabilities

windp2 <- windp2 + bckg
resp <- resp + bckg
ccr <- ccr + bckg 
og_rescale <- og_rescale + bckg

quantile(resp)

cellStats(windp2, mean) * 100 
cellStats(resp, mean) * 100 
cellStats(ccr, mean) * 100 
cellStats(og_rescale, mean) * 100 

windp2[windp2==0] <- NA
ccr[ccr==0] <- NA

cellStats(windp2, mean) * 100 
cellStats(ccr, mean) * 100 

windp2[windp2>0] <- 1
windp2[windp2!=1] <- 0
cellStats(windp2 , sum) /cellStats(cost1, sum) * 100
resp[resp>0] <- 1
resp[resp!=1] <- 0
cellStats(resp , sum) /cellStats(cost1, sum) * 100
ccr[ccr>0] <- 1
ccr[ccr!=1] <- 0
cellStats(ccr , sum) /cellStats(cost1, sum) * 100
og_rescale[og_rescale>0] <- 1
og_rescale[og_rescale!=1] <- 0
cellStats(og_rescale , sum) /cellStats(cost1, sum) * 100

# average probability of dev for private vs BLM land

cellStats(mask(cost2, private1, maskvalue = 0,
               updatevalue = NA), mean) - cellStats(mask(cost2, BLM1, maskvalue = 0,
                                                         updatevalue = NA), mean)

# overlap between features
stats <- c()
i <- 1
k <- 1
j <- 1
for (i in (1:nlayers(multisp))){
  for (j in (1:nlayers(multisp))){
    overlap1 <- multisp[[i]]
    overlap1[overlap1>=0.5] <- 1
    overlap1[overlap1!=1] <- 0
    overlap2 <- multisp[[j]]
    overlap2[overlap2>=0.5] <- 1
    overlap2[overlap2!=1] <- 0
    overlap <- overlap1 + overlap2
    overlap_potential <- cellStats(overlap1,sum)
    overlap[overlap!=2] <- 0
    overlap[overlap==2] <- 1
    stats[k] <-(cellStats(overlap,sum)/overlap_potential)
    k <- k + 1
    j <- j + 1
  }
  i <- i + 1
  j <- 1
}

# overlap between cons and cost features
stats <- c()
costs <- stack(cost2,cost3)
i <- 1
k <- 1
j <- 1
for (j in (1:nlayers(costs))){
  for (i in (1:nlayers(multisp))){
    overlap1 <- multisp[[i]]
    overlap1[overlap1>=quantile(multisp[[i]], probs = c(0.75))] <- 1
    overlap1[overlap1!=1] <- 0
    overlap2 <- costs[[j]]
    overlap2[overlap2>=quantile(costs[[j]], probs = c(0.75))] <- 1
    overlap2[overlap2!=1] <- 0
    overlap <- overlap1 + overlap2
    overlap_potential <- cellStats(overlap1,sum)
    overlap[overlap!=2] <- 0
    overlap[overlap==2] <- 1
    stats[k] <-(cellStats(overlap,sum)/overlap_potential)
    k <- k + 1
    i <- i + 1
  }
  i <- 1
  j <- j + 1
}


# assess overlap across features this time with quantiles
stats <- c()
i <- 1
k <- 1
j <- 1
for (i in (1:nlayers(multisp))){
  for (j in (1:nlayers(multisp))){
    overlap1 <- multisp[[i]]
    overlap1[overlap1>=0.5] <- 1
    overlap1[overlap1!=1] <- 0
    overlap2 <- multisp[[j]]
    overlap2[overlap2>=0.5] <- 1
    overlap2[overlap2!=1] <- 0
    overlap <- overlap1 + overlap2
    overlap_potential <- cellStats(overlap1,sum)
    overlap[overlap!=2] <- 0
    overlap[overlap==2] <- 1
    stats[k] <-(cellStats(overlap,sum)/overlap_potential)
    k <- k + 1
    j <- j + 1
  }
  i <- i + 1
  j <- 1
}

# Overlap between priority solutions and PACs
PACs_priority <- c()
i <- 1
for (i in (1:nlayers(irrep_mask))){
  temp1 <- sum(irrep_mask[[i]], PACs)
  temp1[temp1<=1] <- 0
  temp1[temp1!=0] <- 1
  temp2 <- irrep_mask[[i]]
  temp2[temp2!=0] <- 1
  temp <- (cellStats(temp1,"sum")/cellStats(temp2,"sum"))*100
  PACs_priority <- c(PACs_priority, temp)
  i <- i + 1
}

# RESULT 2: representation ----

# 1. maximum representation achieved 

max(vars2 %>% 
      select(nesting), na.rm=TRUE)
max(vars2 %>% 
      select(brood), na.rm=TRUE)
max(vars2 %>% 
      select(winter), na.rm=TRUE)
max(vars2 %>% 
      select(LC), na.rm=TRUE)

# 2. maximum representation achieved outside PACs

max(vars2 %>%
      filter(PACs_inc == "no") %>%
      select(nesting), na.rm=TRUE)
max(vars2 %>%
      filter(PACs_inc == "no") %>%
      select(brood), na.rm=TRUE)
max(vars2 %>%
      filter(PACs_inc == "no") %>%
      select(winter), na.rm=TRUE)
max(vars2 %>%
      filter(PACs_inc == "no") %>%
      select(LC), na.rm=TRUE)

# 3. representation for easement scenarios
max(vars1 %>%
      filter(PACs_inc == "no",
             land_owner == "private") %>%
      select(nesting), na.rm=TRUE)
max(vars1 %>%
      filter(PACs_inc == "no",
             land_owner == "private") %>%
      select(brood), na.rm=TRUE)
max(vars1 %>%
      filter(PACs_inc == "no",
             land_owner == "private") %>%
      select(winter), na.rm=TRUE)
max(vars1 %>%
      filter(PACs_inc == "no",
             land_owner == "private") %>%
      select(LC), na.rm=TRUE)


# RESULT 3: ROI ----

# priority areas

max(vars2 %>%
      filter(fw_group == "Equal")%>%
      select(roi_threat), na.rm=TRUE)
min(vars2 %>%
      filter(fw_group == "Equal")%>%
      select(roi_threat), na.rm=TRUE)

vars2 %>%
  filter(fw_group == "Equal", 
         PACs_inc == "yes") %>%
  group_by(features) %>%
  summarise(
    count = n(),
    mean = mean(roi_threat, na.rm = TRUE),
    sd = sd(roi_threat, na.rm = TRUE),
    median = median(roi_threat, na.rm = TRUE),
    IQR = IQR(roi_threat, na.rm = TRUE)
  )
  
 vars2 %>%
  filter(fw_group == "Equal", 
         PACs_inc == "no") %>%
  group_by(features) %>%
  summarise(
    count = n(),
    mean = mean(roi_threat, na.rm = TRUE),
    sd = sd(roi_threat, na.rm = TRUE),
    median = median(roi_threat, na.rm = TRUE),
    IQR = IQR(roi_threat, na.rm = TRUE)
  )

# plot ROI by feature weight group
vars2 %>%
  filter(PACs_inc != "yes") %>%
  group_by(fw_group) %>%
  ggboxplot(x = "fw_group", y = "roi_threat",
            color = "fw_group",
            palette = brewer.pal(5,"Set2"),
            order = c("B", "Equal", "LC", "W&B", "Expert"),
            ylab = "ROI",
            xlab = "Feature Weights")


# RESULT 4: irreplaceability ----

# average irreplaceable sites (%)
# no feature weights
vars2 %>%
  filter(fw_group == "Equal") %>%
  summarise(
    count = n(),
    mean = mean(irrep_1_prop, na.rm = TRUE),
    sd = sd(irrep_1_prop, na.rm = TRUE),
    median = median(irrep_1_prop, na.rm = TRUE),
    IQR = IQR(irrep_1_prop, na.rm = TRUE)
  )

min(vars2 %>% 
      filter(fw_group == "Equal") %>%
      select(irrep_1_prop), na.rm=TRUE)

max(vars2 %>% 
      filter(fw_group == "Equal") %>%
      select(irrep_1_prop), na.rm=TRUE)

vars2 %>%
  filter(fw_group == "Equal", PACs_inc == "yes") %>%
  group_by(features) %>%
  summarise(
    count = n(),
    mean = mean(irrep_08_prop, na.rm = TRUE),
    sd = sd(irrep_08_prop, na.rm = TRUE),
    median = median(irrep_08_prop, na.rm = TRUE),
    IQR = IQR(irrep_08_prop, na.rm = TRUE)
  )
  
vars2 %>%
  filter(fw_group == "Equal", PACs_inc == "no") %>%
  group_by(features) %>%
  summarise(
    count = n(),
    mean = mean(irrep_08_prop, na.rm = TRUE),
    sd = sd(irrep_08_prop, na.rm = TRUE),
    median = median(irrep_08_prop, na.rm = TRUE),
    IQR = IQR(irrep_08_prop, na.rm = TRUE)
  )

# RESULT 5: contiguity ----

vars2 %>%
  filter(fw_group == "Equal") %>%
  summarise(
    count = n(),
    mean = mean(contiguity, na.rm = TRUE),
    sd = sd(contiguity, na.rm = TRUE),
    median = median(contiguity, na.rm = TRUE),
    IQR = IQR(contiguity, na.rm = TRUE)
  )

vars1 %>%
  filter(fw_group == "Equal",
         PACs_inc == "no", 
         land_owner == "private") %>%
  group_by(features) %>%
  summarise(
    count = n(),
    mean = mean(contiguity, na.rm = TRUE),
    sd = sd(contiguity, na.rm = TRUE),
    median = median(contiguity, na.rm = TRUE),
    IQR = IQR(contiguity, na.rm = TRUE)
  )

# statistically assess contiguity

kt_cont <- kruskal.test(contiguity ~ land_owner, data = vars1 %>%
                          filter(PACs_inc == "yes"))

kt_cont2 <- kruskal.test(contiguity ~ PACs_inc, data = vars1 %>%
                          filter(land_owner == "public"))

wt1 <- wilcox.test(vars1 %>% filter(land_owner == "public") %>%
                                            pull(contiguity),
                                      vars1 %>% filter(land_owner == "private") %>%
                                           pull(contiguity))
wt2 <- wilcox.test(vars1 %>% filter(land_owner == "public") %>%
                                             pull(contiguity),
                                      vars2 %>% pull(contiguity))
                                      
# RESULT 6: feature weights ----

vars2 %>%
  filter(features == "multisp") %>%
  group_by(fw_group) %>%
  summarise(
    count = n(),
    mean = mean(irrep_08_prop, na.rm = TRUE),
    sd = sd(irrep_08_prop, na.rm = TRUE),
    median = median(irrep_08_prop, na.rm = TRUE),
    IQR = IQR(irrep_08_prop, na.rm = TRUE)
  )


kt1 <- kruskal.test(irrep_08_prop ~ fw_group, data = vars2 %>%
                      filter(features == "multisp"))

kt2 <- kruskal.test(contiguity ~ fw_group, data = vars2 %>%
                      filter(features == "multisp"))

kt3 <- kruskal.test(roi_threat ~ fw_group, data = vars2 %>%
                      filter(features == "multisp"))

# Restoration ----
# Create data frame 
vars3 <- data.frame(
  "num_PUs" = numeric(),
  "contiguity" = numeric(),
  "cost_threat" = numeric(),
  "irrep_sum" = numeric(),
  "irrep_1" = numeric(),
  "irrep_1_prop" = numeric(),
  "irrep_pu" = numeric(),
  "irrep_08" = numeric(),
  "roi_threat" = numeric(),
  "nesting" = numeric(),
  "brood" = numeric(),
  "`winter" = numeric(),
  "LC" = numeric(),
  "overall_benefit" = numeric(),
  "benefit_roi_threat" = numeric(),
  "PAC_overlap" = numeric(),
  "cost_per_PU_threat" = numeric()
)

# add values to dataframe
i <- 1
for (i in 1:nlayers(irrep_mask2)) {
  vars3 <- vars3 %>%
    add_row(
      num_PUs = cellStats(mask(
        mask(
          cost1,
          irrep_mask2[[i]],
          maskvalue = 0,
          updatevalue = NA
        ),
        irrep_mask2[[i]],
        maskvalue = NA,
        updatevalue = NA
      ), sum),
      contiguity = mean(lsm_c_contig_mn(irrep_mask2[[i]])$value),
      cost_threat = cellStats(mask(
        mask(
          recov,
          irrep_mask2[[i]],
          maskvalue = 0,
          updatevalue = NA
        ),
        irrep_mask2[[i]],
        maskvalue = NA,
        updatevalue = NA
      ), sum),
      irrep_sum = cellStats(irrep_mask2[[i]], 'sum'),
      irrep_1 = sum(getValues(irrep_mask2[[i]]) == 1, na.rm=TRUE),
      irrep_1_prop = (irrep_1 / irrep_sum)*100,
      irrep_pu = irrep_sum/num_PUs,
      irrep_08 = sum(getValues(irrep_mask2[[i]]) >= 0.8, na.rm=TRUE),
      roi_threat = cellStats(irrep_mask2[[i]], 'sum') /
        cost_threat,
      nesting = (cellStats(mask(
        mask(
          nesting_ddct,
          irrep_mask2[[i]],
          maskvalue = 0,
          updatevalue = NA
        ),
        irrep_mask2[[i]],
        maskvalue = NA,
        updatevalue = NA
      ), sum) / cellStats(nesting_ddct, sum))*100,
      brood = (cellStats(mask(
        mask(
          brood_ddct,
          irrep_mask2[[i]],
          maskvalue = 0,
          updatevalue = NA
        ),
        irrep_mask2[[i]],
        maskvalue = NA,
        updatevalue = NA
      ), sum) / cellStats(brood_ddct, sum))*100,
      winter = (cellStats(mask(
        mask(
          winter_ddct,
          irrep_mask2[[i]],
          maskvalue = 0,
          updatevalue = NA
        ),
        irrep_mask2[[i]],
        maskvalue = NA,
        updatevalue = NA
      ), sum) / cellStats(winter_ddct, sum))*100,
      LC = (cellStats(mask(
        mask(
          LC_rescale,
          irrep_mask2[[i]],
          maskvalue = 0,
          updatevalue = NA
        ),
        irrep_mask2[[i]],
        maskvalue = NA,
        updatevalue = NA
      ), sum) / cellStats(LC_rescale, sum))*100,
      overall_benefit = (nesting + brood + winter + LC),
      benefit_roi_threat = (overall_benefit / cost_threat)*1000,
      PAC_overlap = freq(sum(
        mask(
          mask(
            cost1,
            irrep_mask2[[i]],
            maskvalue = 0,
            updatevalue = NA
          ),
          irrep_mask2[[i]],
          maskvalue = NA,
          updatevalue = NA
        ), PACs
      ), value = 2),
      cost_per_PU_threat = cost_threat / num_PUs
    )
  i <- i + 1
}

# Sample boxplot ----

vars2 %>%
  filter(features == "annual") %>%
  group_by(fw_group) %>%
  ggboxplot(x = "fw_group", y = "contiguity",
            color = "fw_group",
            palette = brewer.pal(5,"Set2"),
            order = c("B", "Equal", "Expert", "LC", "W&B"),
            ylab = "Contiguity (0 - 1)",
            xlab = "Feature Weights")
