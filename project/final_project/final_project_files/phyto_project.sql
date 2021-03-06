/****** Script for SelectTopNRows command from SSMS  ******/
use phyto_project
SELECT TOP 1000 
		[d_stamp]
      ,[t_stamp]
      ,[chlorophyll_concentration]
      ,[turbidity_units]
      ,[threshold]
      ,[const_err]
  FROM [phyto_project].[dbo].[flntu1]

  SELECT [d_stamp],[t_stamp],[chlorophyll_concentration],[turbidity_units],
	 CONCAT(d_stamp,' ', t_stamp) AS dt_stamp 
	 into flntu2
	 FROM flntu1 
--DROP TABLE flntu2
select * from flntu2

--flntu1 agg of chlA and turbidity according to hour and dates
 select *, 
	SUBSTRING (t_stamp,0,3) as t_hour 
	into #temp
	from [phyto_project].[dbo].[flntu1] 

select * from #temp 

select d_stamp, t_hour, 
avg (cast(chlorophyll_concentration as float)) as avg_chl_conc,
avg (cast(turbidity_units as float)) as avg_turbidity
from #temp
group by d_stamp, t_hour
order by d_stamp, t_hour asc

select d_stamp,  
avg (cast(chlorophyll_concentration as float)) as avg_chl_conc,
avg (cast(turbidity_units as float)) as avg_turbidity
from #temp
group by d_stamp 
order by d_stamp  asc

--microcat agg of salinity and conductivity and temperature according to hour and dates
 select *, 
	SUBSTRING (t_stamp,0,3) as t_hour 
	into #microcat
	from [phyto_project].[dbo].[microcat] 

select * from #microcat 
--DROP TABLE #microcat;

select d_stamp, t_hour, 
avg (cast(Salinity as float)) as avg_salinity,
avg (cast(Conductivity as float)) as avg_conduc,
avg (cast(Temperature as float)) as avg_temp
from #microcat 
group by d_stamp, t_hour
order by d_stamp, t_hour asc

select d_stamp,  
avg (cast(Salinity as float)) as avg_salinity,
avg (cast(Conductivity as float)) as avg_conduc,
avg (cast(Temperature as float)) as avg_temp
from #microcat 
group by d_stamp
order by d_stamp asc

--s9 agg of temp according to hour and dates, and depth
 select *, 
	SUBSTRING (t_stamp,0,3) as t_hour 
	into #s9
	from [phyto_project].[dbo].[s9] 

select * from #s9 

select d_stamp, t_hour,depth, 
avg (cast(temperature as float)) as avg_temp
from #s9
group by d_stamp, t_hour,depth
order by d_stamp, t_hour, depth asc

select d_stamp,depth, 
avg (cast(temperature as float)) as avg_temp
from #s9
group by d_stamp, depth
order by d_stamp,  depth asc

--s9 agg of temp according to hour and dates, and depth
 select *, 
	SUBSTRING (t_stamp,0,3) as t_hour 
	into #s9
	from [phyto_project].[dbo].[s9] 

select * from #s9 

select d_stamp, t_hour,depth, 
avg (cast(temperature as float)) as avg_temp
from #s9
group by d_stamp, t_hour,depth
order by d_stamp, t_hour, depth asc

select d_stamp,depth, 
avg (cast(temperature as float)) as avg_temp
from #s9
group by d_stamp, depth
order by d_stamp,  depth asc