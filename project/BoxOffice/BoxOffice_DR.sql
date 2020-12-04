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


select * into  crew_num_movie from (select [movie_id], [job], [crew_id] from movie_crew) as t 
pivot (
	avg(crew_id)
	for job in (
	[Director],
	[Producer])) 
 AS pivot_table 
 order by movie_id desc

select * from crew_num_movie

SELECT [movie_id],[Director],[Producer],
	 CONCAT(Director,'', Producer) AS DP
	 into a
	 FROM crew_num_movie where Director | Producer is not null
	 
select * from a

select dp, count(DP) as count_movie into b
from a
group by dp
order by count_movie desc

select * from b
order by count_movie desc


select a.movie_id, a.Director, a.Producer, a.DP, b.count_movie into DP1
from a as a
inner join b as b on a.dp=b.dp

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

