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
--  D1 INTEGER NOT NULL,
--  D2 INTEGER NOT NULL,
--  fsc_small INTEGER NOT NULL,
--  fsc_perp INTEGER NOT NULL,
--  fsc_big INTEGER NOT NULL,
--  pe INTEGER NOT NULL,
--  chl_small INTEGER NOT NULL,
--  chl_big INTEGER NOT NULL,
--  PRIMARY KEY (cruise, file, particle)
--);

CREATE TABLE opp (
  -- First three columns are the EVT, OPP, VCT composite key
  cruise TEXT NOT NULL,
  file TEXT NOT NULL,  -- in old files, File+Day. in new files, Timestamp.
  particle INTEGER NOT NULL,
  -- Next we have the measurements. For these, see
  -- https://github.com/fribalet/flowPhyto/blob/master/R/Globals.R and look
  -- at version 3 of the evt header
  time INTEGER NOT NULL,
  pulse_width INTEGER NOT NULL,
  D1 INTEGER NOT NULL,
  D2 INTEGER NOT NULL,
  fsc_small INTEGER NOT NULL,
  fsc_perp INTEGER NOT NULL,
  fsc_big INTEGER NOT NULL,
  pe INTEGER NOT NULL,
  chl_small INTEGER NOT NULL,
  chl_big INTEGER NOT NULL,
  PRIMARY KEY (cruise, file, particle)
);

CREATE TABLE vct (
  -- First three columns are the EVT, OPP, VCT, SDS composite key
  cruise TEXT NOT NULL,
  file TEXT NOT NULL,  -- in old files, File+Day. in new files, Timestamp.
  particle INTEGER NOT NULL,
  -- Next we have the classification
  pop TEXT NOT NULL,
  method TEXT NOT NULL,
  PRIMARY KEY (cruise, file, particle)
);

CREATE TABLE sfl (
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
  steam_pressure REAL,
  event_rate INTEGER,
  PRIMARY KEY (cruise, file)
);

CREATE TABLE evt_count (
  cruise TEXT NOT NULL,
  file TEST NOT NULL,
  count INTEGER,
  PRIMARY KEY (cruise, file)
);
