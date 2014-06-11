# get_aggregate_stats <- function(binning){
#    sql <- paste0("SELECT time, pop, avg(fsc_small) as fsc_small FROM (SELECT distinct cast(floor(ts) + floor((ts - floor(ts))*24*60/binsize)*binsize / (24*60) as datetime) as time, *  FROM (SELECT *, cast(file as float) as ts, ",
#     binning, " as binsize FROM opp INNER JOIN vct WHERE opp.file = vct.file) x) bins GROUP BY time, pop")
#   con <- dbConnect(SQLite(), dbname = db.name)
#   stats <- dbGetQuery(con, sql)
#   return(stats)
# }

# # , avg(fsc_perp) as fsc_perp,avg(fsc_big) as fsc_big, avg(pe) as pe, avg(chl_small) as chl_small, avg(chl_big) as chl_big, sd(fsc_small) as fsc_small_sd, sd(fsc_perp) as fsc_perp_sd, sd(fsc_big) as fsc_big_sd, sd(pe) as fsc_big_sd, sd(chl_small) as chl_small_sd, sd(chl_big) as chl_big_sd


# SELECT datetime(time) = datetime(julianday(time)), avg(fsc_small) as fsc_small 
# FROM (SELECT floor(ts) + floor((ts - floor(ts))*24*60/binsize)*binsize / (24*60) as time, *  
#   FROM (SELECT *, julianday(datetime(opp.file)) as ts, 3.0 as binsize
#     FROM opp
#       INNER JOIN vct 
#       WHERE opp.cruise = vct.cruise
#       AND opp.file = vct.file
#       AND opp.particle = vct.particle) x
#   ) bins 
# GROUP BY time, pop










# # testing
# evt.path <- system.file("extdata","seaflow_cruise","2011_001", "3.evt", package="flowPhyto")
# evt <- readSeaflow(opp.path, transform=T)

# plot.filter.cytogram(evt, width=1, notch=1)

# notch <- best.filter.notch(evt,notch=seq(0.4, 1.4, by=0.1),width =1, do.plot=TRUE)

# #opp.path <- system.file("extdata","seaflow_cruise","2011_001", "2.evt.opp", package="flowPhyto")
# #opp <- readSeaflow(opp.path, transform=T)
# par(mfrow=c(2,1))
# plot.cytogram(opp, "fsc_small", "chl_small")

# plot.vct.cytogram(opp, "fsc_small", "pe")