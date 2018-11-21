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
    chl_small REAL NOT NULL,
    pe REAL NOT NULL,
    fsc_small REAL NOT NULL,
    diam_lwr REAL NOT NULL,
    diam_mid REAL NOT NULL,
    diam_upr REAL NOT NULL,
    Qc_lwr REAL NOT NULL,
    Qc_mid REAL NOT NULL,
    Qc_upr REAL NOT NULL,
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
    sfl.conductivity as conductivity,
    sfl.par as par,
    sfl.stream_pressure as stream_pressure,
    sfl.file_duration as file_duration,
    sfl.event_rate as event_rate,
    opp.opp_evt_ratio as opp_evt_ratio,
    vct.pop as pop,
    vct.count as n_count,
    vct.chl_small as chl_small,
    vct.pe as pe,
    vct.fsc_small as fsc_small,
    vct.diam_lwr as diam_lwr,
    vct.diam_mid as diam_mid,
    vct.diam_upr as diam_upr,
    vct.Qc_lwr as Qc_lwr,
    vct.Qc_mid as Qc_mid,
    vct.Qc_upr as Qc_upr,
    vct.quantile as quantile
  FROM
    opp, vct, sfl
  WHERE
    opp.filter_id == (select id FROM filter ORDER BY date DESC limit 1)
    AND
    opp.quantile == vct.quantile
    AND
    opp.file == vct.file
    AND
    opp.file == sfl.file
  ORDER BY
    time, pop ASC;
