UNION
SELECT
   CHARACTER.player_name,
   COALESCE(SUM("Ming"), 0) AS "Ming",
   COALESCE(SUM("George"), 0) AS "George",
   COALESCE(SUM("Tyson"), 0) AS "Tyson",
   COALESCE(SUM("Ben"), 0) AS "Ben",
   {w} AS week 
FROM adventures_by_player_and_dm
   RIGHT JOIN
      CHARACTER 
      ON adventures_by_player_and_dm.name = CHARACTER.name 
WHERE
   adventures_by_player_and_dm.week <= {w} 
GROUP BY
   CHARACTER.player_name