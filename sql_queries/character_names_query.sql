SELECT
   name,
   COALESCE(final_adventuring_week, 
   (
      SELECT
         MAX(week) 
      FROM
         adventure
   )
) AS final_adventuring_week 
FROM
   CHARACTER 
ORDER BY
   id;