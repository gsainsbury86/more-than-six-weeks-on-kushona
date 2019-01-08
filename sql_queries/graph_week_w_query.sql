SELECT
   c1.id AS c1id,
   c2.id AS c2id,
   c1.name,
   c2.name,
   string_agg(adventure.name, ',' ORDER BY week) AS adv_list,
   COUNT(adventure.name) AS num_adv 
FROM
   record t1 
   JOIN
      record t2 
      ON NOT t1.id = t2.id 
   JOIN
      adventure 
      ON t1.adventure_id = adventure.id 
   JOIN
      CHARACTER c1 
      ON t1.character_id = c1.id 
   JOIN
      CHARACTER c2 
      ON t2.character_id = c2.id 
WHERE
   t1.adventure_id = t2.adventure_id 
   AND c1.id < c2.id 
   AND week <= {w}
GROUP BY
   c1.id,
   c2.id 
ORDER BY
   c1.id,
   c2.id;