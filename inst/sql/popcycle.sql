CREATE TABLE IF NOT EXISTS metadata (
    cruise TEXT NOT NULL,
    inst TEXT NOT NULL,
    PRIMARY KEY (cruise, inst)
);

CREATE TABLE IF NOT EXISTS opp (
    file TEXT NOT NULL,
    all_count INTEGER NOT NULL,
    opp_count INTEGER NOT NULL,
    evt_count INTEGER NOT NULL,
    opp_evt_ratio REAL NOT NULL,
    filter_id TEXT NOT NULL,
    quantile REAL NOT NULL,
    PRIMARY KEY (file, filter_id, quantile)
);

CREATE TABLE IF NOT EXISTS vct (
    file TEXT NOT NULL,
    pop TEXT NOT NULL,
    count INTEGER NOT NULL,
    chl_1q REAL NOT NULL,
    chl_med REAL NOT NULL,
    chl_3q REAL NOT NULL,
    pe_1q REAL NOT NULL,
    pe_med REAL NOT NULL,
    pe_3q REAL NOT NULL,
    fsc_1q REAL NOT NULL,
    fsc_med REAL NOT NULL,
    fsc_3q REAL NOT NULL,
    diam_lwr_1q REAL NOT NULL,
    diam_lwr_med REAL NOT NULL,
    diam_lwr_3q REAL NOT NULL,
    diam_mid_1q REAL NOT NULL,
    diam_mid_med REAL NOT NULL,
    diam_mid_3q REAL NOT NULL,
    diam_upr_1q REAL NOT NULL,
    diam_upr_med REAL NOT NULL,
    diam_upr_3q REAL NOT NULL,
    Qc_lwr_1q REAL NOT NULL,
    Qc_lwr_med REAL NOT NULL,
    Qc_lwr_mean REAL NOT NULL,
    Qc_lwr_3q REAL NOT NULL,
    Qc_mid_1q REAL NOT NULL,
    Qc_mid_med REAL NOT NULL,
    Qc_mid_mean REAL NOT NULL,
    Qc_mid_3q REAL NOT NULL,
    Qc_upr_1q REAL NOT NULL,
    Qc_upr_med REAL NOT NULL,
    Qc_upr_mean REAL NOT NULL,
    Qc_upr_3q REAL NOT NULL,
    gating_id TEXT NOT NULL,
    filter_id TEXT NOT NULL,
    quantile REAL NOT NULL,
    PRIMARY KEY (file, pop, gating_id, quantile)
);

CREATE TABLE IF NOT EXISTS sfl (
  file TEXT NOT NULL,  -- in old files, File+Day. in new files, Timestamp.
  date TEXT,
  file_duration REAL,
  lat REAL,
  lon REAL,
  conductivity REAL,
  salinity REAL,
  ocean_tmp REAL,
  par REAL,
  bulk_red REAL,
  stream_pressure REAL,
  event_rate REAL,
  PRIMARY KEY (file)
);

CREATE INDEX IF NOT EXISTS sflDateIndex ON sfl (date);

CREATE TABLE IF NOT EXISTS filter (
  id TEXT NOT NULL,
  date TEXT NOT NULL,
  quantile REAL NOT NULL,
  beads_fsc_small REAL NOT NULL,
  beads_D1 REAL NOT NULL,
  beads_D2 REAL NOT NULL,
  width REAL NOT NULL,
  notch_small_D1 REAL NOT NULL,
  notch_small_D2 REAL NOT NULL,
  notch_large_D1 REAL NOT NULL,
  notch_large_D2 REAL NOT NULL,
  offset_small_D1 REAL NOT NULL,
  offset_small_D2 REAL NOT NULL,
  offset_large_D1 REAL NOT NULL,
  offset_large_D2 REAL NOT NULL,
  PRIMARY KEY (id, quantile)
);

CREATE TABLE IF NOT EXISTS gating (
  id TEXT NOT NULL,
  date TEXT NOT NULL,
  pop_order INTEGER NOT NULL,
  pop TEXT NOT NULL,
  method TEXT NOT NULL,
  channel1 TEXT,
  channel2 TEXT,
  gate1 REAL,
  gate2 REAL,
  position1 INTEGER,
  position2 INTEGER,
  scale REAL,
  minpe REAL,
  PRIMARY KEY (id, pop)
);

CREATE TABLE IF NOT EXISTS poly (
  pop TEXT NOT NULL,
  fsc_small REAL,
  fsc_perp REAL,
  fsc_big REAL,
  pe REAL,
  chl_small REAL,
  chl_big REAL,
  point_order INTEGER NOT NULL,
  gating_id TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS gating_plan (
  start_date TEXT NOT NULL,
  gating_id TEXT NOT NULL,
  PRIMARY KEY (start_date, gating_id)
);

CREATE TABLE IF NOT EXISTS filter_plan (
  start_date TEXT NOT NULL,
  filter_id TEXT NOT NULL,
  PRIMARY KEY (start_date, filter_id)
);

CREATE TABLE IF NOT EXISTS outlier (
  file TEXT NOT NULL,
  flag INTEGER,
  PRIMARY KEY (file)
);

CREATE VIEW IF NOT EXISTS stat AS
  SELECT
    opp.file as file,
    sfl.date as time,
    sfl.lat as lat,
    sfl.lon as lon,
    sfl.ocean_tmp as temp,
    sfl.salinity as salinity,
    sfl.par as par,
    vct.quantile as quantile,
    vct.pop as pop,
    sfl.stream_pressure as stream_pressure,
    sfl.file_duration as file_duration,
    sfl.event_rate as event_rate,
    opp.opp_evt_ratio as opp_evt_ratio,
    vct.count as n_count,
    vct.chl_1q as chl_1q,
    vct.chl_med as chl_med,
    vct.chl_3q as chl_3q,
    vct.pe_1q as pe_1q,
    vct.pe_med as pe_med,
    vct.pe_3q as pe_3q,
    vct.fsc_1q as fsc_1q,
    vct.fsc_med as fsc_med,
    vct.fsc_3q as fsc_3q,
    vct.diam_lwr_1q as diam_lwr_1q,
    vct.diam_lwr_med as diam_lwr_med,
    vct.diam_lwr_3q as diam_lwr_3q,
    vct.diam_mid_1q as diam_mid_1q,
    vct.diam_mid_med as diam_mid_med,
    vct.diam_mid_3q as diam_mid_3q,
    vct.diam_upr_1q as diam_upr_1q,
    vct.diam_upr_med as diam_upr_med,
    vct.diam_upr_3q as diam_upr_3q,
    vct.Qc_lwr_1q as Qc_lwr_1q,
    vct.Qc_lwr_med as Qc_lwr_med,
    vct.Qc_lwr_mean as Qc_lwr_mean,
    vct.Qc_lwr_3q as Qc_lwr_3q,
    vct.Qc_mid_1q as Qc_mid_1q,
    vct.Qc_mid_med as Qc_mid_med,
    vct.Qc_mid_mean as Qc_mid_mean,
    vct.Qc_mid_3q as Qc_mid_3q,
    vct.Qc_upr_1q as Qc_upr_1q,
    vct.Qc_upr_med as Qc_upr_med,
    vct.Qc_upr_mean as Qc_upr_mean,
    vct.Qc_upr_3q as Qc_upr_3q
  FROM
    opp, vct, sfl
  WHERE
    opp.quantile == vct.quantile
    AND
    opp.file == vct.file
    AND
    opp.file == sfl.file
  ORDER BY
    time, pop ASC;
