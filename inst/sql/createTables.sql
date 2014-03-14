create table activity(
  activity_id text primary key,
  sport text,
  timestamp text);

create table lap(
  activity_id text,
  lap_id integer,
  timestamp text,
  time real,
  distance real,
  avg_speed real,
  max_speed real,
  calories integer,
  avg_hr integer,
  max_hr integer,
  intensity text,
  trigger text,
  max_cadence integer,
  avg_cadence integer,
  steps integer,
  primary key (activity_id, lap_id));

create table trackpoint(
  activity_id text,
  lap_id integer,
  tp_id integer,
  timestamp text,
  latitude real,
  longitude real,
  altitude real,
  distance real,
  speed real,
  cadence integer,
  primary key(activity_id, lap_id, tp_id));
