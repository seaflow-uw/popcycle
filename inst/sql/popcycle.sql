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
  -- First three columns are the EVT, OPP, VCT composite key
  cruise TEXT NOT NULL,
  file TEXT NOT NULL,  -- in old files, File+Day. in new files, Timestamp.
  particle INTEGER NOT NULL,
  -- Next we have the measurements. For these, see
  -- https://github.com/fribalet/flowPhyto/blob/master/R/Globals.R and look
  -- at version 3 of the evt header
  time INTEGER NOT NULL,
  pulse_width INTEGER NOT NULL,
  D1 REAL NOT NULL,
  D2 REAL NOT NULL,
  fsc_small REAL NOT NULL,
  fsc_perp REAL NOT NULL,
  fsc_big REAL NOT NULL,
  pe REAL NOT NULL,
  chl_small REAL NOT NULL,
  chl_big REAL NOT NULL,
  PRIMARY KEY (cruise, file, particle)
);

CREATE INDEX IF NOT EXISTS oppFileIndex ON opp (file);
CREATE INDEX IF NOT EXISTS oppFsc_smallIndex ON opp (fsc_small);
CREATE INDEX IF NOT EXISTS oppPeIndex ON opp (pe);
CREATE INDEX IF NOT EXISTS oppChl_smallIndex ON opp (chl_small);

CREATE TABLE IF NOT EXISTS vct (
  -- First three columns are the EVT, OPP, VCT, SDS composite key
  cruise TEXT NOT NULL,
  file TEXT NOT NULL,  -- in old files, File+Day. in new files, Timestamp.
  particle INTEGER NOT NULL,
  -- Next we have the classification
  pop TEXT NOT NULL,
  method TEXT NOT NULL,
  PRIMARY KEY (cruise, file, particle)
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


CREATE TABLE IF NOT EXISTS opp_evt_ratio (
  cruise TEXT NOT NULL,
  file TEXT NOT NULL,
  ratio REAL,
  PRIMARY KEY (cruise, file)
);


CREATE TABLE IF NOT EXISTS stats (
  cruise TEXT NOT NULL,
  file TEXT NOT NULL,
  time TEXT,
  lat REAL,
  lon REAL,
  opp_evt_ratio REAL,
  flow_rate REAL,
  file_duration REAL,
  pop TEXT NOT NULL,
  n_count INTEGER,
  abundance REAL,
  fsc_small REAL,
  chl_small REAL,
  pe REAL,
  PRIMARY KEY (cruise, file, pop)
);


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
