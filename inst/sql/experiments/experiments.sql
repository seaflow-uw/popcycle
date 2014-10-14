.timer on
.echo on

-- Times with no indexes
SELECT * 
  FROM opp 
 WHERE file = '2015-06-11T22-54-07+00-00';


SELECT opp.cruise, opp.file as file, vct.pop as pop, avg(fsc_small) as fsc_small
FROM opp, vct
WHERE opp.cruise == vct.cruise
  AND opp.file == vct.file
  AND opp.particle == vct.particle
GROUP BY opp.cruise, opp.file, vct.pop
LIMIT 1;



