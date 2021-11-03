
summary.y <- function(x,probs=c(.25,.5,.75)){
  x <- stats::na.omit(x)
  v <- quantile(x, probs)
  data.frame(ymin = v[1],y = v[2],  ymax = v[3])}


# plot as time-varying R
plot.Rt <- function(t, Rjs =list("Adjusted"=NULL, "Non-adjusted"=NULL)){

  if(class(t)!="Date") t <- as.numeric(t)
  dat <- data.frame(t = t,
                    Rj1 = Rjs[[1]] )
  if (length(Rjs)==2) dat$Rj2 <- Rjs[[2]]
  dat <- dat[order(dat$t),]


  colset<- c("#D2691E","#267347")
  names(colset) <- names(Rjs)
  y_up <- quantile(aggregate(formula=Rj1~t,data = dat,FUN = median)$Rj1, 0.99)

  GG <- ggplot(data = dat,aes(x = t,y=Rj1))+
    stat_summary_bin(binwidth = 1,color=colset[1],alpha=0.2, size=1.5,
                     fun.data = summary.y ,geom = "linerange")+
    stat_summary_bin(aes(color=names(Rjs)[1]),binwidth = 1,size=1.1,alpha=.85,
                     fun = "median",geom = "line")+
    scale_color_manual(values = colset, name="Type")+
    coord_cartesian(ylim=c(0,y_up),expand = F)+
    annotate(geom="segment",x = extendrange(dat$t)[1],
             xend = extendrange(dat$t)[2], y = 1, yend = 1,
             size=1, alpha=.3)+
    labs(x = "Time", y="Time-varying reproductive number")+
    theme_bw(base_size = 16) %+replace%
    theme(panel.grid=element_blank(),
          legend.position = c(.99,.99),legend.justification = c(1,1) )


  if (length(Rjs)==2){
    GG <- GG+
      stat_summary_bin(aes(x = t,y=Rj2,color=names(Rjs)[2]),binwidth = 1,size=1.1,alpha=.85,
                       fun = "median",geom = "line")+
      stat_summary_bin(aes(x = t,y=Rj2),binwidth = 1,color=colset[2],alpha=0.2,size=1.5,
                       fun.data = summary.y ,geom = "linerange")}
  return(GG)
}


# plot epi curve
plot.epi <- function(t){
  if(class(t)!="Date") t <- as.numeric(t)
  dat <- data.frame(t = t)
  GG <- ggplot(data = dat)+
    geom_histogram(aes(x = t), color="grey80", fill="#e02c50",alpha=1, binwidth=7)+
    theme_bw(base_size = 16) %+replace%
    theme(legend.position = c(.99,.99),legend.justification = c(1,1),
          panel.grid=element_blank() )+
    labs(x = "Time", y="Weekly incident cases")
  return(GG)
}


# plot point pattern of individual R









animate.points <- function(t, x, y, Rj, dt=14, crs_pts=NULL, bnd=NULL,  base_map=NULL){

  # t <- df$date
  # x<-df$long
  # y<-df$lat
  # Rj <-NULL
  # dt<-15
  # crs_pts=NULL
  # base_map = base_tn
  # bnd = c(120.1,22.9,120.4,23.1)

  pts <-create.pts_sf(x, y, base_map = base_map )
  pts$t <- t
  pts$tg <- cut(t, breaks =  seq(min(t),max(t)+dt,dt),right = F)

  if(!is.null(Rj)){
    pts$Rj <- Rj
    qn <- 10
    colset <- colorRampPalette(c("#eac77b","#8c0d26"))(qn)
    psset <- seq(.5,6,length.out = qn)
    names(colset) <- levels(get.qRj(1,qn = qn))
    names(psset) <- levels(get.qRj(1,qn = qn))
    leg <- "point"
  } else {
    pts$qRj <- factor("i")
    colset<-c("i"="#A03638")
    psset<-c("i"=3)
    leg <- F
  }

  pts <- lapply(levels(pts$tg), FUN = function(m){
    sdf <- pts[as.character(pts$tg)==m,]
    if(!is.null(Rj)) sdf$qRj <- get.qRj(sdf$Rj,qn = qn)
    sdf
  })

  pts <- do.call("rbind",pts)
  pts$t.int <- as.integer(pts$tg)
  if(is.null(bnd)) bnd <- st_bbox(pts)
  bnd <- rescale.bnd(bnd)
  G0 <- create.basemap(base_map)
  bs <- G0$theme$text$size
  gui <- guide_legend(direction = "horizontal",
                      title = "Percentile of Rj",
                      title.theme = element_text(size=bs*.6),
                      label.theme = element_text(size=bs*.6),
                      title.position = "top")
  G <- G0 +
    geom_sf(data = pts,mapping = aes(size=qRj, color=qRj),
            alpha=.5, show.legend = leg)+
    scale_color_manual(values = colset,drop=F)+
    scale_size_manual(values = psset,drop=F)+
    guides(size = gui, col = gui)+
    coord_sf(xlim = c(bnd[1],bnd[3]),ylim =c(bnd[2],bnd[4]) ,crs = crs_pts)+
    theme(legend.background = element_rect(color="white",
                                           fill="#fbf6e9"),
          legend.key = element_blank(),
          legend.position = c(.98,.02))

  G <- G + transition_time(t.int) +
    labs(title = "Step: {frame_time}")

  animate(G, fps = 1,duration=max(pts$t.int))
}

animate.hex <- function(t, x, y, Rj, dt=14, nbin=30,
                        crs_pts=NULL, bnd=NULL,  base_map=NULL){

  # t <- df$date
  # x<-df$long
  # y<-df$lat
  # Rj <-df$Rj_adj
  # dt<-7
  # crs_pts=NULL
  # base_map = base_tn
  # nbin=30
  # bnd = c(120.1,22.9,120.4,23.1)

  pts <- create.pts_sf(x, y, crs_pts, base_map)
  pts <- cbind(pts,t , st_coordinates(pts$geometry) )
  pts$tg <- cut(t, breaks =  seq(min(t),max(t)+dt,dt),right = F)
  if(is.null(bnd)) bnd <- st_bbox(pts)

  if(!is.null(Rj)){
    z <- Rj
    zlab <- "Mean Rj"
    f.agg <- mean
    colset <- "YlGnBu"
  }else{
    z <- rep(1, length(x))
    zlab <- "Counts"
    f.agg <- sum
    colset <- "OrRd"
  }
  pts <- cbind(pts, z)
  pts <- cen.pts(pts, bnd)
  pts$tg <- droplevels(pts$tg)

  dum <- hexbin::hgridcent(xbins=nbin ,shape =1,
                           xbnds=c(bnd[1], bnd[3]),
                           ybnds=c(bnd[2], bnd[4]))
  dum <- data.frame (x = -dum$x[1] - c(0, dum$dx/2),
                     y = -dum$y[1] - c (0, dum$dy),
                     cell = c(0,0),
                     z = c(NA,NA))

  df.hex <- lapply(levels(pts$tg), FUN = function(m){
    sdf <- pts[as.character(pts$tg)==m,]
    sdf.hex <- hexbin::hexbin(sdf$X, sdf$Y, IDs=T, xbins=nbin ,
                              xbnds=c(bnd[1], bnd[3]),
                              ybnds=c(bnd[2], bnd[4]),shape =1)
    d <- dum; d$tg <- m
    gdf <- data.frame(hexbin::hcell2xy(sdf.hex),
                      cell = sdf.hex@cell,
                      z = hexbin::hexTapply(sdf.hex, sdf$z, f.agg),
                      tg = m)
    rbind(d,gdf)
  })

  df.hex <- do.call("rbind",df.hex)
  df.hex$t.int <- as.integer(factor(df.hex$tg, levels = levels(pts$tg)))

  if(is.null(bnd)) bnd <- st_bbox(pts)
  bnd <- rescale.bnd(bnd)
  G0 <- create.basemap(base_map)
  bs <- G0$theme$text$size
  G <- G0+
    geom_hex(data = df.hex, mapping = aes(x=x, y=y, fill=z),
             colour="white", alpha=.7, stat="identity")+
    scale_fill_distiller(palette = colset,
                         na.value = NA, direction = 1,
                         limits=quantile(df.hex$z,probs = c(.01,.99),na.rm = T),
                         oob=scales::squish,
                         guide =  guide_colorbar(barwidth=unit(.5,"npc"),
                                                 barheight=unit(.02,"npc"),
                                                 direction = "horizontal",
                                                 title = zlab,
                                                 title.theme = element_text(size=bs*.6),
                                                 label.theme = element_text(size=bs*.6),
                                                 title.position = "top")
    )+
    coord_sf(xlim = c(bnd[1], bnd[3]),ylim =c(bnd[2], bnd[4]), expand=F,
             crs = st_crs(pts))+
    labs(x="", y="")

  G <- G + transition_time(t.int) +
    labs(title = "Step: {frame_time}")

  animate(G, fps = 1,duration=max(df.hex$t.int))
}





plot.polygons=function(region,sf,...){
  sf=left_join(sf,data.frame(xtabs(~region)),by="region")
  ggplot(sf)+geom_sf(aes(fill=Freq))+
    scale_fill_gradientn(colours=rev(heat.colors(10)))
}

animate.polygons <- function(t,region,sf) {
  region=factor(region,levels=sf$region)
  df=melt(as.data.frame(xtabs(~region+t)))
  sf=left_join(sf,df,by="region")
  sf$t=as.numeric(sf$t)
  ggplot(sf)+geom_sf(aes(fill=value))+
    scale_fill_gradientn(colours=rev(heat.colors(10)))+
    transition_time(t)
}

plot.Tij <- function(Tij,sf) {
  OD=melt(Tij,c("from","to"),value.name = "Tij")
  OD=subset(OD,from!=to & Tij>1)
  OD=od2line(OD,sf,zone_code='region')
  OD$NAME=paste0(OD$from,"to",OD$to)
  OD=OD[,c("NAME",names(OD))]

  center=st_centroid(sf)
  center=inner_join(center,data.frame(Tij=diag(Tij),region=rownames(Tij)),by="region")

  tmap_mode("view")
  tm_shape(sf)+tm_polygons()+
    tm_shape(OD)+tm_lines(lwd="Tij",col ="Tij",legend.col.show = F,breaks = c(0:6)*10,scale = max(OD$Tij)/3)+
    tm_shape(center)+tm_symbols("Tij",col ="Tij",palette="Reds",legend.col.show = F)
}



