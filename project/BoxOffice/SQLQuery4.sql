USE BoxOffice


---- correct release date
UPDATE movies 
SET release_date = CASE WHEN (YEAR(release_date) > 2017) THEN DATEADD(YEAR,-100,release_date) ELSE (release_date) END

-- select min(release_date),max(release_date), count(1) from movies

---------- GENRE

--- view
GO
CREATE VIEW movies_genres_v AS
SELECT movie_id
    , MAX(CASE WHEN (genre_id = 12) THEN (1) ELSE (0) END) AS genre_adventure
    , MAX(CASE WHEN (genre_id = 14) THEN (1) ELSE (0) END) AS genre_fantasy
    , MAX(CASE WHEN (genre_id = 16) THEN (1) ELSE (0) END) AS genre_animation
    , MAX(CASE WHEN (genre_id = 18) THEN (1) ELSE (0) END) AS genre_drama
    , MAX(CASE WHEN (genre_id = 27) THEN (1) ELSE (0) END) AS genre_horror
    , MAX(CASE WHEN (genre_id = 28) THEN (1) ELSE (0) END) AS genre_action
    , MAX(CASE WHEN (genre_id = 35) THEN (1) ELSE (0) END) AS genre_comedy
    , MAX(CASE WHEN (genre_id = 36) THEN (1) ELSE (0) END) AS genre_history
    , MAX(CASE WHEN (genre_id = 37) THEN (1) ELSE (0) END) AS genre_western
    , MAX(CASE WHEN (genre_id = 53) THEN (1) ELSE (0) END) AS genre_thriller
    , MAX(CASE WHEN (genre_id = 80) THEN (1) ELSE (0) END) AS genre_crime
    , MAX(CASE WHEN (genre_id = 99) THEN (1) ELSE (0) END) AS genre_documentary
    , MAX(CASE WHEN (genre_id = 878 OR genre_id = 10770) THEN (1) ELSE (0) END) AS genre_science_fiction
    , MAX(CASE WHEN (genre_id = 9648) THEN (1) ELSE (0) END) AS genre_mystery
    , MAX(CASE WHEN (genre_id = 10402) THEN (1) ELSE (0) END) AS genre_music
    , MAX(CASE WHEN (genre_id = 10749) THEN (1) ELSE (0) END) AS genre_romance
    , MAX(CASE WHEN (genre_id = 10751) THEN (1) ELSE (0) END) AS genre_family
    , MAX(CASE WHEN (genre_id = 10752) THEN (1) ELSE (0) END) AS genre_war
    , MAX(CASE WHEN (genre_id = 10769) THEN (1) ELSE (0) END) AS genre_foreign
FROM movies_genres
GROUP BY movie_id;

GO
CREATE VIEW movies_departments_v AS
SELECT movie_id
    , SUM(CASE WHEN (department = 'Writing') THEN (1) ELSE (0) END) AS depart_Writing
    , SUM(CASE WHEN (department = 'Directing') THEN (1) ELSE (0) END) AS depart_Directing
    , SUM(CASE WHEN (department = 'Art') THEN (1) ELSE (0) END) AS depart_Art
    , SUM(CASE WHEN (department = 'Sound') THEN (1) ELSE (0) END) AS depart_Sound
    , SUM(CASE WHEN (department = 'Crew') THEN (1) ELSE (0) END) AS depart_Crew
    , SUM(CASE WHEN (department = 'Costume & Make-Up') THEN (1) ELSE (0) END) AS depart_Custom_Mkup
    , SUM(CASE WHEN (department = 'Visual Effects') THEN (1) ELSE (0) END) AS depart_Visual_Effects
    , SUM(CASE WHEN (department = 'Production') THEN (1) ELSE (0) END) AS depart_Production
    , SUM(CASE WHEN (department = 'Camera') THEN (1) ELSE (0) END) AS depart_Camera
    , SUM(CASE WHEN (department = 'Lighting') THEN (1) ELSE (0) END) AS depart_Lighting
    , SUM(CASE WHEN (department = 'Editing') THEN (1) ELSE (0) END) AS depart_Editing
FROM movie_crew
GROUP BY movie_id;

GO
CREATE VIEW movies_departments_female_v AS
SELECT movie_id
    , SUM(CASE WHEN (department = 'Writing' AND b.gender = 1) THEN (1) ELSE (0) END) AS depart_Writing_female
    , SUM(CASE WHEN (department = 'Directing' AND b.gender = 1) THEN (1) ELSE (0) END) AS depart_Directing_female
    , SUM(CASE WHEN (department = 'Art' AND b.gender = 1) THEN (1) ELSE (0) END) AS depart_Art_female
    , SUM(CASE WHEN (department = 'Sound' AND b.gender = 1) THEN (1) ELSE (0) END) AS depart_Sound_female
    , SUM(CASE WHEN (department = 'Crew' AND b.gender = 1) THEN (1) ELSE (0) END) AS depart_Crew_female
    , SUM(CASE WHEN (department = 'Costume & Make-Up' AND b.gender = 1) THEN (1) ELSE (0) END) AS depart_Custom_Mkup_female
    , SUM(CASE WHEN (department = 'Visual Effects' AND b.gender = 1) THEN (1) ELSE (0) END) AS depart_Visual_Effects_female
    , SUM(CASE WHEN (department = 'Production' AND b.gender = 1) THEN (1) ELSE (0) END) AS depart_Production_female
    , SUM(CASE WHEN (department = 'Camera' AND b.gender = 1) THEN (1) ELSE (0) END) AS depart_Camera_female
    , SUM(CASE WHEN (department = 'Lighting' AND b.gender = 1) THEN (1) ELSE (0) END) AS depart_Lighting_female
    , SUM(CASE WHEN (department = 'Editing' AND b.gender = 1) THEN (1) ELSE (0) END) AS depart_Editing_female
FROM movie_crew a
LEFT OUTER JOIN crew_dim b
    ON a.crew_id = b.crew_id
GROUP BY movie_id;

GO
-- drop table dbo.movie_actor_date_revenue
----- movie_actor_date_revenue as view 
    SELECT a.actor_id, a.movie_id, a.[order], b.release_date, b.revenue
    INTO dbo.movie_actor_date_revenue
    FROM movie_cast a
    LEFT OUTER JOIN movies b
        ON a.movie_id = b.movie_id
;

GO

SELECT a.movie_id, crew_id AS director_id, c.release_date
INTO dbo.movie_director_date
FROM movie_crew a
LEFT OUTER JOIN movies c
    ON a.movie_id = c.movie_id
where job = 'Director'

GO


----- movie_actor_date_revenue as view 
SELECT a.productor_id, a.movie_id, b.release_date, b.revenue
INTO dbo.movie_producer_date_revenue
FROM movie_productors a
LEFT OUTER JOIN movies b
ON a.movie_id = b.movie_id
;

GO

/**************************/
/**      FLAT FILE       **/
/**************************/

CREATE VIEW dbo.movies_ff_v_DR AS
SELECT a.movie_id
      ,a.budget
      ,a.original_language
      ,a.popularity
      ,a.release_date
      ,a.runtime
      ,CASE 
            WHEN (a.runtime < 94) THEN ('Short')
            WHEN (a.runtime > 118) THEN ('Large')
            ELSE ('Medium')  
        END AS runtime_cat
      ,a.revenue
      , (CASE WHEN (original_language = 'en') THEN (1) ELSE (0) END) AS sw_lang_en
      , (CASE WHEN (homepage <> '') THEN (1) ELSE (0) END) AS sw_web_presence
      , (CASE WHEN (poster_path <> '') THEN (1) ELSE (0) END) AS sw_has_poster
      , (CASE WHEN (tagline <> '') THEN (1) ELSE (0) END) AS sw_tagline
      , (SELECT COUNT(1) FROM movie_keywords WHERE movie_id= a.movie_id) AS keyword_cnt
      , CASE WHEN (YEAR(a.release_date) > 2017) THEN (YEAR(a.release_date)-100) ELSE (YEAR(a.release_date)) END AS release_year
      , MONTH(a.release_date) AS release_month
      , CASE WHEN (MONTH(a.release_date) IN (8,9,10,12)) THEN (1) ELSE (0) END AS high_release_month
      , DAY(a.release_date) AS release_day
--- seasonality obtained from time-series decomposition
      , CASE 
            WHEN (MONTH(release_date) = 1) THEN (-50746591) 
            WHEN (MONTH(release_date) = 2) THEN (-15309317)
            WHEN (MONTH(release_date) = 3) THEN (-7149862)
            WHEN (MONTH(release_date) = 4) THEN (-28405871)
            WHEN (MONTH(release_date) = 5) THEN (21757736)
            WHEN (MONTH(release_date) = 6) THEN (74205073)
            WHEN (MONTH(release_date) = 7) THEN (40981593)
            WHEN (MONTH(release_date) = 8) THEN (-33273015)
            WHEN (MONTH(release_date) = 9) THEN (-37667937)
            WHEN (MONTH(release_date) = 10) THEN (-28659422)
            WHEN (MONTH(release_date) = 11) THEN (31944703)
            WHEN (MONTH(release_date) = 12) THEN (32322910)
        END AS seasonality
--- collections
    , (SELECT COUNT(1) FROM movie_collection WHERE movie_id = a.movie_id) AS sw_collection
--- producers
    , (SELECT count(DISTINCT productor_id) 
        FROM  dbo.movie_producer_date_revenue
        WHERE movie_id = a.movie_id 
        GROUP BY movie_id) AS producers_cnt
--- countries
    , (SELECT COUNT(1) FROM movie_countries WHERE movie_id = a.movie_id ) AS countries_cnt
--- languages
--- "en","fr","in","ru","es","ja"
    , CASE WHEN ((SELECT iso_639_1 FROM movie_languages WHERE movie_id=a.movie_id AND iso_639_1 = 'en') = 'en') THEN (1) ELSE (0) END AS lang_US
    , CASE WHEN ((SELECT iso_639_1 FROM movie_languages WHERE movie_id=a.movie_id AND iso_639_1 = 'fr') = 'fr') THEN (1) ELSE (0) END AS lang_FR
    , CASE WHEN ((SELECT iso_639_1 FROM movie_languages WHERE movie_id=a.movie_id AND iso_639_1 = 'ru') = 'ru') THEN (1) ELSE (0) END AS lang_RU
    , CASE WHEN ((SELECT iso_639_1 FROM movie_languages WHERE movie_id=a.movie_id AND iso_639_1 = 'es') = 'es') THEN (1) ELSE (0) END AS lang_ES
    , CASE WHEN ((SELECT iso_639_1 FROM movie_languages WHERE movie_id=a.movie_id AND iso_639_1 = 'ja') = 'ja') THEN (1) ELSE (0) END AS lang_JA
--- keywords 
    , (SELECT COUNT(1) FROM movie_keywords WHERE movie_id = a.movie_id ) AS keywords_cnt
--- main actors
    , (SELECT DISTINCT COUNT(1) -- as actor_movies_cnt
        FROM movie_actor_date_revenue
        WHERE release_date < a.release_date AND actor_id = (
            SELECT TOP 1 actor_id 
            FROM movie_actor_date_revenue
            WHERE movie_id = a.movie_id AND [order] = 0
      )) AS actor0_movies_cnt 
    , (SELECT DISTINCT COUNT(1) -- as actor_movies_cnt
        FROM movie_actor_date_revenue
        WHERE release_date BETWEEN DATEADD(YEAR, -5, a.release_date) AND DATEADD(DAY,-1,a.release_date) 
        AND actor_id = (
            SELECT TOP 1 actor_id 
            FROM movie_actor_date_revenue
            WHERE movie_id = a.movie_id AND [order] = 0
      )) AS actor0_movies_5y_cnt 
    , (SELECT DISTINCT COUNT(1) 
        FROM movie_actor_date_revenue
        WHERE release_date < a.release_date AND actor_id = (
            SELECT TOP 1 actor_id 
            FROM movie_actor_date_revenue
            WHERE movie_id = a.movie_id AND [order] = 1
      )) AS actor1_movies_cnt
    , (SELECT DISTINCT COUNT(1) -- as actor_movies_cnt
        FROM movie_actor_date_revenue
        WHERE release_date BETWEEN DATEADD(YEAR, -5, a.release_date) AND DATEADD(DAY,-1,a.release_date) 
        AND actor_id = (
            SELECT TOP 1 actor_id 
            FROM movie_actor_date_revenue
            WHERE movie_id = a.movie_id AND [order] = 1
      )) AS actor1_movies_5y_cnt 
    , (SELECT DISTINCT COUNT(1) -- as actor_movies_cnt
        FROM movie_actor_date_revenue
        WHERE release_date < a.release_date AND actor_id = (
            SELECT TOP 1 actor_id 
            FROM movie_actor_date_revenue
            WHERE movie_id = a.movie_id AND [order] = 2
      )) AS actor2_movies_cnt
    , (SELECT DISTINCT COUNT(1) -- as actor_movies_cnt
        FROM movie_actor_date_revenue
        WHERE release_date BETWEEN DATEADD(YEAR, -5, a.release_date) AND DATEADD(DAY,-1,a.release_date) 
        AND actor_id = (
            SELECT TOP 1 actor_id 
            FROM movie_actor_date_revenue
            WHERE movie_id = a.movie_id AND [order] = 2
      )) AS actor2_movies_5y_cnt 
  --- main actors female
    , (SELECT MAX(CASE WHEN (e.gender=1) THEN (1) ELSE (0) END) 
    FROM movie_cast d
    LEFT OUTER JOIN actors_dim e
        ON d.actor_id = e.actor_id
    WHERE d.movie_id = a.movie_id AND d.[order] = 0 ) AS sw_female_actor0
    , (SELECT MAX(CASE WHEN (e.gender=1) THEN (1) ELSE (0) END) 
    FROM movie_cast d
    LEFT OUTER JOIN actors_dim e
        ON d.actor_id = e.actor_id
    WHERE d.movie_id = a.movie_id AND d.[order] = 1 ) AS sw_female_actor1
    , (SELECT MAX(CASE WHEN (e.gender=1) THEN (1) ELSE (0) END) 
    FROM movie_cast d
    LEFT OUTER JOIN actors_dim e
        ON d.actor_id = e.actor_id
    WHERE d.movie_id = a.movie_id AND d.[order] = 2 ) AS sw_female_actor2
  --- main actors male
    , (SELECT MAX(CASE WHEN (e.gender=2) THEN (1) ELSE (0) END) 
    FROM movie_cast d
    LEFT OUTER JOIN actors_dim e
        ON d.actor_id = e.actor_id
    WHERE d.movie_id = a.movie_id AND d.[order] = 0 ) AS sw_male_actor0
    , (SELECT MAX(CASE WHEN (e.gender=2) THEN (1) ELSE (0) END) 
    FROM movie_cast d
    LEFT OUTER JOIN actors_dim e
        ON d.actor_id = e.actor_id
    WHERE d.movie_id = a.movie_id AND d.[order] = 1 ) AS sw_male_actor1
    , (SELECT MAX(CASE WHEN (e.gender=2) THEN (1) ELSE (0) END) 
    FROM movie_cast d
    LEFT OUTER JOIN actors_dim e
        ON d.actor_id = e.actor_id
    WHERE d.movie_id = a.movie_id AND d.[order] = 2 ) AS sw_male_actor2
--- main actors previous movie revenue
    ,  (SELECT MAX(revenue) -- as actor_movies_cnt
        FROM movie_actor_date_revenue 
        WHERE release_date < a.release_date AND actor_id = (
            SELECT TOP 1 actor_id 
            FROM movie_actor_date_revenue
            WHERE movie_id = a.movie_id AND [order] = 0
        ) AND revenue IS NOT NULL) AS actor0_prev_revenue
    ,    (SELECT MAX(revenue) -- as actor_movies_cnt
        FROM movie_actor_date_revenue 
        WHERE release_date < a.release_date AND actor_id = (
            SELECT TOP 1 actor_id 
            FROM movie_actor_date_revenue
            WHERE movie_id = a.movie_id AND [order] = 1
        ) AND revenue IS NOT NULL) AS actor1_prev_revenue
    ,    (SELECT MAX(revenue) -- as actor_movies_cnt
        FROM movie_actor_date_revenue 
        WHERE release_date < a.release_date AND actor_id = (
            SELECT TOP 1 actor_id 
            FROM movie_actor_date_revenue
            WHERE movie_id = a.movie_id AND [order] = 2
        ) AND revenue IS NOT NULL) AS actor2_prev_revenue

    --- directors previous movies
    ,  (SELECT DISTINCT COUNT(1) -- as actor_movies_cnt
        FROM movie_director_date
        WHERE release_date < a.release_date
        AND director_id = (
            SELECT TOP 1 director_id 
            FROM movie_director_date
            WHERE movie_id = a.movie_id 
        )) AS director_movies_cnt 

    ,  (SELECT DISTINCT COUNT(1) -- as actor_movies_cnt
        FROM movie_director_date
        WHERE release_date BETWEEN DATEADD(YEAR, -5, a.release_date) AND DATEADD(DAY,-1,a.release_date) 
        AND director_id = (
            SELECT TOP 1 director_id 
            FROM movie_director_date
            WHERE movie_id = a.movie_id 
        )) AS director_movies_5y_cnt 
	, 
    , b.genre_adventure
    , b.genre_fantasy
    , b.genre_animation
    , b.genre_drama
    , b.genre_horror
    , b.genre_action
    , b.genre_comedy
    , b.genre_history
    , b.genre_western
    , b.genre_thriller
    , b.genre_crime
    , b.genre_documentary
    , b.genre_science_fiction
    , b.genre_mystery
    , b.genre_music
    , b.genre_romance
    , b.genre_family
    , b.genre_war
    , b.genre_foreign
  --- movie crew departments
    , c.depart_Art
    , c.depart_Camera
    , c.depart_Crew
    , c.depart_Custom_Mkup
    , c.depart_Directing
    , c.depart_Editing
    , c.depart_Lighting
    , c.depart_Production
    , c.depart_Sound
    , c.depart_Visual_Effects
    , c.depart_Writing
  --- female movie crew departments
    , q.depart_Art_female
    , q.depart_Camera_female
    , q.depart_Crew_female
    , q.depart_Custom_Mkup_female
    , q.depart_Directing_female
    , q.depart_Editing_female
    , q.depart_Lighting_female
    , q.depart_Production_female
    , q.depart_Sound_female
    , q.depart_Visual_Effects_female
    , q.depart_Writing_female
  FROM [BoxOffice].[dbo].[movies] a
  INNER JOIN movies_genres_v b
    ON a.movie_id = b.movie_id
  LEFT OUTER JOIN movies_departments_v c 
    ON a.movie_id = c.movie_id
  LEFT OUTER JOIN movies_departments_female_v q
    ON a.movie_id = q.movie_id

/***************************/
