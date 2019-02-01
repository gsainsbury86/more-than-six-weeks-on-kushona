SELECT
   magic_item.name AS "Item",
   adventure.name AS "Found in Adventure",
   requires_attunement,
   description,
   rules,
   CHARACTER.name AS "Owned By",
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