SELECT
   CHARACTER.player_name AS "Player Name",
   CHARACTER.name AS "Name",
   MAX(total_xp) AS "XP",
   MAX(level) AS "Level",
   CHARACTER.race || ' ' || CHARACTER.class || ' (' || CHARACTER.class_option || ')' AS "Race / Class" 
FROM
   character_xp 
   JOIN
      CHARACTER 
      ON CHARACTER.id = character_id 
WHERE
   (
      final_adventuring_week >= {max_week} 
      OR final_adventuring_week IS NULL
   )
GROUP BY
   character_id,
   CHARACTER.player_name,
   CHARACTER.name,
   CHARACTER.race,
   CHARACTER.class,
   CHARACTER.class_option 
ORDER BY
   character_id;