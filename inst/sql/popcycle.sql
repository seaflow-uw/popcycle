CREATE TABLE IF NOT EXISTS opp (
    cruise TEXT NOT NULL,
    file TEXT NOT NULL,
    all_count INTEGER NOT NULL,
    opp_count INTEGER NOT NULL,
    evt_count INTEGER NOT NULL,
    opp_evt_ratio REAL NOT NULL,
    filter_id TEXT NOT NULL,
    PRIMARY KEY (cruise, file, filter_id)
);

CREATE INDEX IF NOT EXISTS oppFileIndex ON opp (file);

CREATE TABLE IF NOT EXISTS vct (
    cruise TEXT NOT NULL,
    file TEXT NOT NULL,
    pop TEXT NOT NULL,
    count INTEGER NOT NULL,
    D1_mean REAL NOT NULL,
    D1_min REAL NOT NULL,
    D1_max REAL NOT NULL,
    D2_mean REAL NOT NULL,
    D2_min REAL NOT NULL,
    D2_max REAL NOT NULL,
    fsc_small_mean REAL NOT NULL,
    fsc_small_min REAL NOT NULL,
    fsc_small_max REAL NOT NULL,
    chl_small_mean REAL NOT NULL,
    chl_small_min REAL NOT NULL,
    chl_small_max REAL NOT NULL,
    pe_mean REAL NOT NULL,
    pe_min REAL NOT NULL,
    pe_max REAL NOT NULL,
    fsc_perp_mean REAL NOT NULL,
    fsc_perp_min REAL NOT NULL,
    fsc_perp_max REAL NOT NULL,
    gating_id TEXT NOT NULL,
    PRIMARY KEY (cruise, file, pop, gating_id)
);

CREATE INDEX IF NOT EXISTS vctFileIndex ON vct (file);

CREATE TABLE IF NOT EXISTS sfl (
  --First two columns are the SDS composite key
  cruise TEXT NOT NULL,
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
  flow_rate REAL,
  event_rate REAL,
  PRIMARY KEY (cruise, file)
);

CREATE INDEX IF NOT EXISTS sflDateIndex ON sfl (date);

CREATE TABLE IF NOT EXISTS filter (
  id TEXT NOT NULL,
  date TEXT NOT NULL,
  width REAL NOT NULL,
  notch_small_D1 REAL,
  notch_small_D2 REAL,
  notch_large_D1 REAL,
  notch_large_D2 REAL,
  offset_small_D1 REAL,
  offset_small_D2 REAL,
  offset_large_D1 REAL,
  offset_large_D2 REAL,
  PRIMARY KEY (id)
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
  cruise TEXT NOT NULL,
  file TEXT NOT NULL,
  flag INTEGER,
  PRIMARY KEY (cruise, file)
);

CREATE INDEX IF NOT EXISTS outlierFileIndex ON outlier (file);

CREATE VIEW IF NOT EXISTS stat AS
  SELECT
    opp.cruise as cruise,
    opp.file as file,
    sfl.date as time,
    sfl.lat as lat,
    sfl.lon as lon,
    sfl.ocean_tmp as temp,
    sfl.salinity as salinity,
    sfl.conductivity as conductivity,
    sfl.par as par,
    sfl.flow_rate as flow_rate,
    sfl.file_duration as file_duration,
    sfl.event_rate as event_rate,
    opp.opp_evt_ratio as opp_evt_ratio,
    vct.pop as pop,
    vct.count as n_count,
    vct.D1_mean as D1,
    vct.D2_mean as D2,
    vct.fsc_small_mean as fsc_small,
    vct.chl_small_mean as chl_small,
    vct.pe_mean as pe,
    vct.fsc_perp_mean as fsc_perp
  FROM
    opp, vct, sfl
  WHERE
    opp.filter_id == (select id FROM filter ORDER BY date DESC limit 1)
    AND
    opp.cruise == vct.cruise
    AND
    opp.file == vct.file
    AND
    opp.cruise == sfl.cruise
    AND
    opp.file == sfl.file
  ORDER BY
    cruise, time, pop ASC;
