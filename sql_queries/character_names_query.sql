SELECT
   name,
   COALESCE(final_adventuring_week, 
   (
      SELECT
         MAX(week) 
      FROM
         adventure
   )
) AS final_adventuring_week,
   COALESCE(week_of_death, 
   (
      SELECT
         MAX(week) 
      FROM
         adventure
   )
) AS week_of_death

FROM
   CHARACTER 
ORDER BY
   id;