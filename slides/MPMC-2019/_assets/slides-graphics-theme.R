#----------------------------------------

#   # custom GG theme
#   # Starts with theme_bw() and change some things
#   # Updates a number of geom defaults

#----------------------------------------

dgray <- "black"
mgray <- "gray60"
lgray <- "gray85"
whisp <- "gray98"

scale_colour_discrete <- ggthemes::scale_colour_solarized
scale_fill_discrete <- ggthemes::scale_fill_solarized


theme_mathcamp <- function(base_size = 12, base_family = "Myriad Pro") {

  # --- geom defaults -----------------------
  #   FYI not all of these work (e.g. smooth---why?)

  update_geom_defaults("point", list(color = dgray, size = 2))
  update_geom_defaults("text", list(color = dgray, size = 3.5, family = "Myriad Pro"))
  update_geom_defaults("line", list(color = dgray, size = 0.65))
  update_geom_defaults("bar", list(fill = mgray, color = dgray))
  update_geom_defaults("density", list(fill = mgray, 
                                       color = dgray, 
                                       alpha = 0.5))
  update_geom_defaults("pointrange", list(color = dgray))
  # update_geom_defaults("vline", list(color = lgray))
  # update_geom_defaults("hline", list(color = lgray))
  # update_geom_defaults("ribbon", list(fill = lgray, color = NULL, alpha = 0.5))

  margin_size <- (base_size / 2)
# --- start with bw -----------------------
  
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
  theme(

# --- general -----------------------

    title = element_text(size = rel(1), color = dgray),
    # plot.title = element_text(color = dgray),
    plot.caption = element_text(color = mgray,
                                size = rel(0.75), 
                                hjust = 1, 
                                vjust = 1, 
                                margin = margin(t = 1)),
    plot.margin = margin(t = margin_size, r = margin_size + 1.5, b = margin_size, l = margin_size),

            


# --- panel -----------------------
    panel.border = element_rect(size = 0.5, fill = NA, color = mgray),
    panel.grid = element_blank(),
    # panel.grid = element_line(color = whisp, size = 0.5), 
    # panel.grid.major = element_line(size = 0.25, color=lgray),
  #   panel.grid.minor = element_line(size = 0.15, color=whisp),

    
# --- strip -----------------------
    strip.background = element_blank(),
    strip.text = element_text(vjust = 1, size = rel(1.2)),



# --- axis -----------------------
    axis.title = element_text(size = rel(1.2), color = dgray),
    axis.title.x = element_text(margin = margin(t = 8, b = 4)),
    axis.title.y = element_text(angle = 90, vjust = 1, margin = margin(r = 8)),
    axis.text = element_text(size = rel(1.2), color = dgray),
    axis.ticks = element_line(size = rel(0.5), color = mgray),
    # axis.ticks = element_blank(),


# --- legend -----------------------

  #   legend.background = element_rect(color = "white"),
    legend.title = element_text(size = rel(0.9), color = dgray),
    legend.text = element_text(size = rel(0.8), color = dgray)


  )

}
