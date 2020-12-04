USE BoxOffice

select a.movie_id, a.release_date, b.job, b.crew_id into #movie_date_crew
from [BoxOffice].[dbo].[movies] as a
inner join [BoxOffice].[dbo].[movie_crew] as b
on a.movie_id = b.movie_id
order by movie_id
--select * from #movie_date_crew



select * 
 from (select [movie_id], [job], [crew_id], [release_date] 
	from (select a.movie_id, a.release_date, b.job, b.crew_id 
		from [BoxOffice].[dbo].[movies] as a
		inner join [BoxOffice].[dbo].[movie_crew] as b
		on a.movie_id = b.movie_id)) as t 
	pivot (
		avg(crew_id)
		for job in (
		[Director],
		[Producer])) 
	 AS pivot_table 
	 order by movie_id desc
	 --concate crew_id
SELECT [movie_id],[release_date], [Director],[Producer],
	 CONCAT(Director,'', Producer) AS DP
	 into a2
	 FROM (select * 
 from (select [movie_id], [job], [crew_id], [release_date] 
	from (select a.movie_id, a.release_date, b.job, b.crew_id 
		from [BoxOffice].[dbo].[movies] as a
		inner join [BoxOffice].[dbo].[movie_crew] as b
		on a.movie_id = b.movie_id)) as t 
	pivot (
		avg(crew_id)
		for job in (
		[Director],
		[Producer])) 
	 AS pivot_table 
	 order by movie_id desc)
	  where Director | Producer is not null
	 
select * from a2




select movie_ID, release_date, DP ,
(select count (DP) 
from (SELECT [movie_id],[release_date], [Director],[Producer],
	 CONCAT(Director,'', Producer) AS DP
	 
	 FROM (select * 
 from (select [movie_id], [job], [crew_id], [release_date] 
	from (select a.movie_id, a.release_date, b.job, b.crew_id 
		from [BoxOffice].[dbo].[movies] as a
		inner join [BoxOffice].[dbo].[movie_crew] as b
		on a.movie_id = b.movie_id) as t 
	pivot (
		avg(crew_id)
		for job in (
		[Director],
		[Producer])) 
	 AS pivot_table 
	 order by movie_id desc)
	  where Director | Producer is not null) 
where convert(DATE,release_date) <= convert(date,DP_count.release_date)
and DP = DP_count.DP
) sum_of into DP_count
from a2 as (SELECT [movie_id],[release_date], [Director],[Producer],
	 CONCAT(Director,'', Producer) AS DP
	 FROM (select * 
 from (select [movie_id], [job], [crew_id], [release_date] 
	from (select a.movie_id, a.release_date, b.job, b.crew_id 
		from [BoxOffice].[dbo].[movies] as a
		inner join [BoxOffice].[dbo].[movie_crew] as b
		on a.movie_id = b.movie_id) as t 
	pivot (
		avg(crew_id)
		for job in (
		[Director],
		[Producer])) 
	 AS pivot_table 
	 order by movie_id desc)
	  where Director | Producer is not null)
	  DP_count
order by dp

select year(release_date) from a2




--into #movie_dp_data_count1 
from a2
group by dp
order by count_movie desc

select * from #movie_dp_data_count1
order by count_movie desc


select a.movie_id, a.Director, a.Producer, a.DP, 
b.count_movie into DP
from a2 as a
inner join #movie_dp_data_count as b on a.dp=b.dp

select * from dp

select * into DP3 from DP1 
where director | Producer is not null

select * from DP3

select count(distinct count_movie) from DP3



select a.movie_id, a.Director, a.Producer, a.DP, a.count_movie, b.original_title, b. revenue into DP2
from DP1 as a
inner join movies as b on a.movie_id=b.movie_id
order by count_movie desc

select * from DP2
where revenue is null




select a.movie_id, a.Director, a.Producer, a.DP, a.count_movie, b.original_title, b. revenue into DP2
from DP1 as a
inner join movies as b on a.movie_id=b.movie_id
order by count_movie desc

select * from DP2
where revenue is null

