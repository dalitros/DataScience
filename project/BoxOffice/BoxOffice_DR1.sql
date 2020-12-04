USE BoxOffice

select movie_id, 
count(distinct job) as crew_number into #crew_num_movie
from movie_crew
group by movie_id 
order by crew_number desc

select * from #crew_num_movie

select a.movie_id, a.crew_number,
		 b. original_title, revenue 
from #crew_num_movie as a
inner join movies as b
on a.movie_id = b.movie_id
order by movie_id 


select [revenue]
from movies

select * from (select [movie_id], [job], [crew_id] from movie_crew)t
pivot (
	count(crew_id)
	for job in (
	[Director],
	[Producer]))
 AS pivot_table 
 order by movie_id

 select * from pivot_table

 select * into  #crew_num_movie3 from (select [movie_id], [job], [crew_id] from movie_crew) t 
pivot (
	count(movie_id)
	for job in (
	[Director],
	[Producer])) 
 AS pivot_table 
 order by crew_id desc


 
ALTER TABLE #crew_num_movie3 ADD crew_id_sum1 AS Director+Producer;

select * from #crew_num_movie3
order by crew_id_sum1 desc

select a. crew_id, a. Director, a. Producer, a. crew_id_sum1, b.movie_id, c. revenue into #crew_num_movie4
from #crew_num_movie3 as a
inner join movie_crew as b on a. crew_id = b. crew_id
inner join movies as c on b.movie_id = c.movie_id
order by movie_id 

select * 
from #crew_num_movie4

-----------------------

select a.movie_id, a.release_date, b.job, b.crew_id into #movie_date_crew
from [BoxOffice].[dbo].[movies] as a
inner join [BoxOffice].[dbo].[movie_crew] as b
on a.movie_id = b.movie_id
order by movie_id
--select * from #movie_date_crew
select * into  crew_num_movie1 from (select [movie_id], [job], [crew_id], [release_date] 
	from #movie_date_crew) as t 
pivot (
	avg(crew_id)
	for job in (
	[Director],
	[Producer])) 
 AS pivot_table 
 order by movie_id desc

--select * from crew_num_movie1

SELECT [movie_id],[release_date], [Director],[Producer],
	 CONCAT(Director,'', Producer) AS DP
	 into a2
	 FROM crew_num_movie1 where Director | Producer is not null
	 
select * from a2




select movie_ID, release_date, DP ,
(select count (DP) 
from a2 
where convert(DATE,release_date) <= convert(date,DP_count.release_date)
and DP = DP_count.DP
) sum_of into DP_count
from a2 as DP_count
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

