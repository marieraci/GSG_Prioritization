# Solution Plots ----

# load files for plotting
# study extent
# Rock Springs Field Office boundary
# source:
RSFO <- readOGR(dsn = ".", "RSFO_Boundary")
# core area boundaries 
# created in ArcMap
# source:
ca_bounds <- readOGR(dsn = ".", "ca_bounds")
# lek data
# clipped in Arcmap
# source: private
leks <- raster('OccLeks_1283_Clip2_PointToRa11.tif')
leks <- projectRaster(leks, bounds, crs=CRS_obj, method = 'ngb', res = 120)
leks_buff <- buffer(leks, 500)

# plotting irreplaceability without a background
# to better display the variation
irrep_bkg1 <- irrep_mask
irrep_bkg1[is.na(irrep_bkg1)] <- 0
irrep_bkg2 <- irrep_mask
irrep_bkg2[is.na(irrep_bkg2)] <- 0

tmap_mode("plot")
## tmap mode set to plotting

# remove bkg on PACs
PACs[PACs==0] <- NA

# get basemap to be saveable with read_osm
c_osm <- tmaptools::read_osm(boundaries(PACs), type = "esri-topo")

# Plots 1-4: nesting seasonal 
# Plots 5-8: brood seasonal 
# Plots 9-12: winter seasonal 
# Plots 13-16: annual equal weights 
# Plots 17-20: multi sp equal weights 
# Plots 21-36: annual weighted scenarios
# Plots 37-52: multi sp weighted scenarios

# save each plot as png
i <- 1
for (i in (1:nlayers(irrep_bkg1))) {
  tmap_save(
    tm_shape(c_osm) +
      tm_rgb() +
      tm_shape(PACs) +
      tm_raster(
        "CA_name",
        palette =  c("grey", "white"),
        alpha = 0.6,labels = "PACs",
        title = ""
      ) +
      tm_shape(leks_buff) +
      tm_raster(
        "layer",
        palette = "black",
        alpha = 0.6,labels = "Leks",
        title = ""
      ) +
      tm_shape(RSFO) +
      tm_borders(col = "black", lwd = 0.5) +
      tm_shape(ca_bounds) +
      tm_lines(col = "black", lwd = 0.5) +
      tm_shape(irrep_bkg1[[i]]) +
      tm_raster(
        palette = viridis(6),
        alpha = 0.8,
        title = "Irreplaceability"
      ),
    file =  paste("Priority_plot_", i, ".png", sep = ""),
    width = 1900,
    height = 1080,
    asp = 0
  )
  i <- i + 1
}

# restoration plot
# save each plot as png
i <- 1
for (i in (1:nlayers(irrep_bkg2))) {
  tmap_save(
    tm_shape(c_osm) +
      tm_rgb() +
      tm_shape(PACs) +
      tm_raster(
        "CA_name",
        palette =  c("grey", "white"),
        alpha = 0.6,labels = "PACs",
        title = ""
      ) +
      tm_shape(leks_buff) +
      tm_raster(
        "layer",
        palette = "black",
        alpha = 0.6,labels = "Leks",
        title = ""
      ) +
      tm_shape(RSFO) +
      tm_borders(col = "black", lwd = 0.5) +
      tm_shape(ca_bounds) +
      tm_lines(col = "black", lwd = 0.5) +
      tm_shape(irrep_bkg2[[i]]) +
      tm_raster(
        palette = viridis(6),
        alpha = 0.8,
        title = "Irreplaceability"
      ),
    file =  paste("Restore_plot_", i, ".png", sep = ""),
    width = 1900,
    height = 1080,
    asp = 0
  )
  i <- i + 1
}

# plot stacked feature weight scenarios / overlap
# i.e. multisp equal weights vs. expert weights
overlap1 <- irrep_bkg1[[19]]
overlap1[!is.na(overlap1)] <- 1
overlap2 <- irrep_bkg1[[51]]
overlap2[!is.na(overlap2)] <- 2
overlap <- sum(overlap1,overlap2,na.rm = TRUE)
overlap <- overlap + bckg
overlap[overlap==0] <- NA
# create plot
tmap_save(tm_shape(c_osm) +
            tm_rgb() +
            tm_shape(PACs) +
            tm_raster(
              "CA_name",
              palette =  c("grey", "white"),
              alpha = 0.6,
              title = "",
              labels = "PACs"
            ) +
            tm_shape(leks_buff) +
            tm_raster(
              "layer",
              palette = "black",
              alpha = 0.6, 
              title = "",
              labels = "Leks"
            ) +
            tm_shape(RSFO) +
            tm_borders(col = "black", lwd = 0.5) +
            tm_shape(ca_bounds) +
            tm_lines(col = "black", lwd = 0.5) +
            tm_shape(overlap) +
            tm_raster(palette = brewer.pal(5, 'Set2'), as.count = TRUE,
                      labels = c("Equal", "Expert", "Overlap"),
                      title = "Feature Weights"
            ),
          file = "plot_overlap_fw.png",
          width = 1920,
          height = 1080,
          asp = 0
)


# Delete Solutions - Free Memory for Portfolios ----
# rm("s_stack1")
# rm("irrep1")

# Generate Portfolios for Selected Problems ----
# all equal weighted problems are used for portfolios here

portfolios1 <-
  c(
    q1[[1]] %>% add_top_portfolio(10),
    q1[[2]] %>% add_top_portfolio(10),
    q1[[3]] %>% add_top_portfolio(10),
    q1[[4]] %>% add_top_portfolio(10),
    q1[[5]] %>% add_top_portfolio(10),
    q1[[6]] %>% add_top_portfolio(10),
    q1[[7]] %>% add_top_portfolio(10),
    q1[[8]] %>% add_top_portfolio(10),
    q1[[9]] %>% add_top_portfolio(10),
    q1[[10]] %>% add_top_portfolio(10)
  )
  
# one restoration problem also used for a portfolio
portfolios2 <- c(q2[[1]] %>% add_top_portfolio(10))

# solve porfolios
s_portfolios1 <- stack()
s_portfolios2 <- stack()
i <- 1
while (i <= length(portfolios1)) {
  s_portfolios1 <-
    addLayer(s_portfolios1, solve(portfolios1[[i]], force = TRUE)) # add solutions to stack
  i <- i + 1
}
i <- 1
while (i <= length(portfolios2)) {
  s_portfolios2 <-
    addLayer(s_portfolios2, solve(portfolios2[[i]], force = TRUE)) # add solutions to stack
  i <- i + 1
}

# irreplaceability
irrep_portfolios1 <- stack()
irrep_portfolios2 <- stack()
j <- 1
i <- 1
while (j <= nlayers(s_portfolios1)) {
  while (i <= nlayers(s_portfolios1)) {
    irrep_portfolios1 <- addLayer(
      s_portfolios1,
      eval_replacement_importance(portfolios1[[j]], s_portfolios1[[i]], force = TRUE)
    )
    i <- i + 1
  }
  j <- j + 10
}
i <- 1
j <- 1
while (j <= nlayers(s_portfolios2)) {
  while (i <= nlayers(s_portfolios2)) {
    irrep_portfolios2 <- addLayer(
      s_portfolios2,
      eval_replacement_importance(portfolios2[[j]], s_portfolios2[[i]], force = TRUE)
    )
    i <- i + 1
  }
  j <- j + 10
}

# remove background values
nlayers(irrep_portfolios1)
nlayers(irrep_portfolios2)
irrep_port_bkg1 <- stack()
i <- 1
for (i in (1:nlayers(irrep_portfolios1))) {
  temp1 <- irrep_portfolios1[[i]]
  temp1[Which(s_portfolios1[[i]] < 0.5)] <- NA
  irrep_port_bkg1 <- addLayer(irrep_port_bkg1, temp1)
  i <- i + 1
}

# Portfolio Plots ----

# plot portfolios
# plotting irreplaceability without a background
# to better display the variation
# summing multiple portfolios together
nesting_port <- sum(irrep_portfolios1[[1:20]], na.rm=TRUE)
nesting_port[nesting_port==0] <- NA
brood_port <- sum(irrep_port_bkg1[[21:40]], na.rm=TRUE)
brood_port[brood_port==0] <- NA
winter_port <- sum(irrep_port_bkg1[[41:60]], na.rm=TRUE)
winter_port[winter_port==0] <- NA
annual_port <- sum(irrep_port_bkg1[[61:80]], na.rm=TRUE)
annual_port[annual_port==0] <- NA
multisp_port <- sum(irrep_port_bkg1[[81:100]], na.rm=TRUE)
multisp_port[multisp_port==0] <- NA

restore_port <- sum(irrep_port_bkg2[[1:10]], na.rm=TRUE)
restore_port[restore_port==0] <- NA

portfolio_plot1 <- tm_basemap("Esri.WorldStreetMap") +
  tm_shape(PACs) +
  tm_raster("CA_name", palette =  c("grey","white"), alpha = 0.6, legend.show = FALSE) +
  tm_add_legend('fill', col = "grey", labels = 'PACs') +
  tm_shape(nesting_port) +
  tm_raster(
    palette = viridis(6),
    alpha = 0.8,
    title = "Selection Frequency"
  ) +
  tm_shape(brood_port) +
  tm_raster(
    palette = viridis(6),
    alpha = 0.8,
    title = "Selection Frequency"
  ) +
  tm_shape(winter_port) +
  tm_raster(
    palette = viridis(6),
    alpha = 0.8,
    title = "Selection Frequency"
  ) +
  tm_shape(annual_port) +
  tm_raster(
    palette = viridis(6),
    alpha = 0.8,
    title = "Selection Frequency"
  ) +
  tm_shape(multisp_port) +
  tm_raster(
    palette = viridis(6),
    alpha = 0.8,
    title = "Selection Frequency"
  ) +
  tm_shape(restore_port) +
  tm_raster(
    palette = viridis(6),
    alpha = 0.8,
    title = "Selection Frequency"
  ) +
  tm_facets(as.layers = TRUE, free.scales.raster =  TRUE) +
  tm_shape(ca_bounds) +
  tm_lines(col = "black", lwd = 0.1) +
  tm_shape(leks_buff) +
  tm_raster("layer", palette = "black", alpha = 0.6, legend.show = FALSE) +
  tm_shape(RSFO) +
  tm_borders(col = "black", lwd = 0.5)

portfolio_plot1

# save each plot as a png

portfolio_plots <- stack(nesting_port,brood_port,winter_port,annual_port,multisp_port,restore_port)

i <- 1
for (i in (1:nlayers(portfolio_plots))) {
  tmap_save(
    tm_shape(c_osm) +
      tm_rgb() +
      tm_shape(PACs) +
      tm_raster(
        "CA_name",
        palette =  c("grey", "white"),
        alpha = 0.6,labels = "PACs",
        title = ""
      ) +
      tm_shape(leks_buff) +
      tm_raster(
        "layer",
        palette = "black",
        alpha = 0.6,labels = "Leks",
        title = ""
      ) +
      tm_shape(RSFO) +
      tm_borders(col = "black", lwd = 0.5) +
      tm_shape(ca_bounds) +
      tm_lines(col = "black", lwd = 0.5) +
      tm_shape(portfolio_plots[[i]]) +
      tm_raster(
        palette = viridis(6),
        alpha = 0.8,
        title = "Selection frequency"
      ),
    file =  paste("plot", i, ".png", sep = ""),
    width = 1900,
    height = 1080,
    asp = 0
  )
  i <- i + 1
}
