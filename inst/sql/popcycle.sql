CREATE TABLE IF NOT EXISTS opp (
    cruise TEXT NOT NULL,
    file TEXT NOT NULL,
    all_count INTEGER NOT NULL,
    opp_count INTEGER NOT NULL,
    evt_count INTEGER NOT NULL,
    opp_evt_ratio REAL NOT NULL,
    notch1 REAL NOT NULL,
    notch2 REAL NOT NULL,
    offset REAL NOT NULL,
    origin REAL NOT NULL,
    width REAL NOT NULL,
    fsc_small_min REAL NOT NULL,
    fsc_small_max REAL NOT NULL,
    fsc_small_mean REAL NOT NULL,
    fsc_perp_min REAL NOT NULL,
    fsc_perp_max REAL NOT NULL,
    fsc_perp_mean REAL NOT NULL,
    fsc_big_min REAL NOT NULL,
    fsc_big_max REAL NOT NULL,
    fsc_big_mean REAL NOT NULL,
    pe_min REAL NOT NULL,
    pe_max REAL NOT NULL,
    pe_mean REAL NOT NULL,
    chl_small_min REAL NOT NULL,
    chl_small_max REAL NOT NULL,
    chl_small_mean REAL NOT NULL,
    chl_big_min REAL NOT NULL,
    chl_big_max REAL NOT NULL,
    chl_big_mean REAL NOT NULL,
    filter_id TEXT NOT NULL,
    PRIMARY KEY (cruise, file, filter_id)
);

CREATE INDEX IF NOT EXISTS oppFileIndex ON opp (file);

CREATE TABLE IF NOT EXISTS vct (
    cruise TEXT NOT NULL,
    file TEXT NOT NULL,
    pop TEXT NOT NULL,
    count INTEGER NOT NULL,
    fsc_small REAL NOT NULL,
    fsc_perp REAL NOT NULL,
    pe REAL NOT NULL,
    chl_small REAL NOT NULL,
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
  notch1 REAL,
  notch2 REAL,
  offset REAL NOT NULL,
  origin REAL,
  width REAL NOT NULL,
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

CREATE VIEW IF NOT EXISTS stat AS
  SELECT
    opp.cruise as cruise,
    opp.file as file,
    sfl.date as time,
    sfl.lat as lat,
    sfl.lon as lon,
    opp.opp_evt_ratio as opp_evt_ratio,
    sfl.flow_rate as flow_rate,
    sfl.file_duration as file_duration,
    vct.pop as pop,
    vct.count as n_count,
    vct.count / (sfl.flow_rate * (sfl.file_duration/60) * opp.opp_evt_ratio) as abundance,
    vct.fsc_small as fsc_small,
    vct.chl_small as chl_small,
    vct.pe as pe
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
