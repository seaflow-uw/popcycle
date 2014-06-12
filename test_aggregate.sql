#SQL for aggregate on just one file
SELECT
  opp.cruise as cruise,
  opp.file as file,
  vct.pop as pop
  avg(opp.fsc_small) as fsc_small,
  avg(opp.chl_small) as chl_small,
  avg(pe) as pe,
  sfl.lat as lat,
  sfl.lon as lon,
  sfl.date as time,
  evt_count.count as evt_particles,
  count(vct.pop) as pop_count,
  sfl.flow_rate as flow_rate,
  sfl.file_duration as file_duration,
  pop_count / (flow_rate * file_duration * (pop_count / evt_particles)) as abundance
FROM
  opp, vct, sfl, evt_count
WHERE
  opp.cruise == vct.cruise
  AND
  opp.file == vct.file
  AND
  opp.particle == vct.particle
  AND
  opp.cruise == sfl.cruise
  AND
  opp.file == sfl.file
  AND
  opp.cruise == evt_count.cruise
  AND
  opp.file == evt_count.file
  AND
  opp.file == %s
GROUP BY
  opp.cruise, opp.file, vct.pop;
