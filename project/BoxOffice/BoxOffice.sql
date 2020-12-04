USE BoxOffice

SELECT m.original_title AS movie_title, m. revenue, m.budget, m.release_date,
m.popularity, m.original_language, m.runtime, m.status,
gd.name AS genre,
cd.name AS collection_name,
cod.name AS country,
pd.name AS productor,
CASE WHEN m.runtime<80 THEN 'Short'
	WHEN m.runtime<120 THEN 'Medium'
	ELSE 'Long'
	END AS runtime_cat,
CASE WHEN m.original_language='en' THEN 'YES'
	ELSE 'NO'
	END AS sw_english_movie,
CASE WHEN month(m.release_date)>4 AND month(m.release_date)<10 THEN 'Summer'
	ELSE 'Winter'
	END AS sw_season,
CASE WHEN month(m.release_date) IN (4,7,8,12) THEN 'Yes'
	ELSE 'No'
	END AS sw_holiday_season
FROM movies AS m
LEFT JOIN movies_genres AS mg ON m.movie_id=mg.movie_id
LEFT JOIN genres_dim AS gd ON mg.genre_id=gd.genre_id
LEFT JOIN movie_collection AS mc ON m.movie_id=mc.movie_id
LEFT JOIN collection_dim AS cd ON mc.collection_id=cd.collection_id
LEFT JOIN movie_countries AS mco ON m.movie_id=mco.movie_id
LEFT JOIN countries_dim AS cod ON mco.iso_3166_1=cod.iso_3166_1
LEFT JOIN movie_productors AS mp ON m.movie_id=mp.movie_id
LEFT JOIN productors_dim AS pd ON mp.productor_id=pd.productor_id


