SELECT
   magic_item.name AS "item",
   adventure.name AS "found",
   requires_attunement,
   description,
   rules,
   CHARACTER.name AS "Character",
   CHARACTER.player_name 
FROM
   magic_item 
   JOIN
      adventure 
      ON adventure.id = adventure_id 
   JOIN
      CHARACTER 
      ON CHARACTER.id = character_id 
WHERE
   final_adventuring_week >= {max_week} 
   OR final_adventuring_week IS NULL 
ORDER BY
   adventure.id DESC;