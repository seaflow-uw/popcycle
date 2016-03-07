-- NOTE: not creating evt table for now
--CREATE TABLE evt (
  -- First three columns are the EVT, OPP, VCT composite key
--  cruise TEXT NOT NULL,
--  file TEXT NOT NULL,  -- in old files, File+Day. in new files, Timestamp.
--  particle INTEGER NOT NULL,
  -- Next we have the measurements. For these, see
  -- https://github.com/fribalet/flowPhyto/blob/master/R/Globals.R and look
  -- at version 3 of the evt header
--  time INTEGER NOT NULL,
--  pulse_width INTEGER NOT NULL,
--  D1 REAL NOT NULL,
--  D2 REAL NOT NULL,
--  fsc_small REAL NOT NULL,
--  fsc_perp REAL NOT NULL,
--  fsc_big REAL NOT NULL,
--  pe REAL NOT NULL,
--  chl_small REAL NOT NULL,
--  chl_big REAL NOT NULL,
--  PRIMARY KEY (cruise, file, particle)
--);

CREATE TABLE IF NOT EXISTS opp (
    cruise TEXT NOT NULL,
    file TEXT NOT NULL,
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
    PRIMARY KEY (cruise, file)
);

CREATE INDEX IF NOT EXISTS oppFileIndex ON opp (file);

CREATE TABLE IF NOT EXISTS vct (
    cruise TEXT NOT NULL,
    file TEXT NOT NULL,
    pop TEXT NOT NULL,
    count INTEGER NOT NULL,
    method TEXT NOT NULL,
    fsc_small REAL NOT NULL,
    fsc_perp REAL NOT NULL,
    pe REAL NOT NULL,
    chl_small REAL NOT NULL,
    PRIMARY KEY (cruise, file, pop)
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

CREATE TABLE IF NOT EXISTS cytdiv (
  cruise TEXT NOT NULL,
  file TEXT NOT NULL,
  N0 INTEGER,
  N1 REAL,
  H REAL,
  J REAL,
  opp_red REAL,
  PRIMARY KEY (cruise, file)
);

CREATE TABLE IF NOT EXISTS filter (
  id INTEGER PRIMARY KEY,
  date TEXT NOT NULL,
  notch1 REAL,
  notch2 REAL,
  offset REAL NOT NULL,
  origin REAL,
  width REAL NOT NULL
);

CREATE TABLE IF NOT EXISTS gating (
  date TEXT NOT NULL,
  uuid TEXT NOT NULL,
  pop_order TEXT NOT NULL,
  PRIMARY KEY (uuid)
);

CREATE TABLE IF NOT EXISTS poly (
  gating_uuid TEXT NOT NULL,
  pop TEXT NOT NULL,
  fsc_small REAL,
  fsc_perp REAL,
  fsc_big REAL,
  pe REAL,
  chl_small REAL,
  chl_big REAL
);
