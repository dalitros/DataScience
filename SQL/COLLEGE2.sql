use COLLEGE
--uploading tables--
select * 
from classrooms

delete from Classrooms where CourseId is null

select * 
from Courses
delete from Courses where CourseId is null

select * 
from Departments
delete from Departments where DepartmentId is null

select * 
from Students
delete from Students where StudentId is null

--2a---list how many students for each department?

select a.CourseId, a.studentId, 
		 b. DepartmentID, c.DepartmentName into sum_stud_college
from Classrooms as a
inner join Courses as b
on a.CourseId = b.CourseId
inner join Departments as c
on b.DepartmentID = c.DepartmentID

select * from sum_stud_college

--2a--answer
select DepartmentName, 
count(distinct studentId) as sum_stud
from sum_stud_college
group by DepartmentName 
order by DepartmentName

--2b- How many students for each course and total under English teacher's.

select a.CourseId, a.studentId,  
		 b. DepartmentID, b. TeacherId, b. CourseName, c. FirstName, c. LastName into sum_stud_college2
from Classrooms as a
inner join Courses as b
on a.CourseId = b.CourseId
inner join Teachers as c
on b.TeacherId = c.TeacherId

select * from sum_stud_college2

--answer. The number of students at every English course.
select CourseName, 
count(studentId) as stud_English_courses
from sum_stud_college2 where CourseName like '%english%'
group by CourseName


--answer.Total number of students under all English courses
--CourseName, FirstName 
select 
count(studentId) as Total_stud_English_teacher
from sum_stud_college2 where CourseName like '%english%'



--2c- How many small (<22 students) and Large (>22 students) classes are needed for sciences department?


select CourseId,
count(studentId) as count_stud into class_stud3
from sum_stud_college where departmentName = 'Science'
group by CourseId 

select * from class_stud3

--answer--10 large and 6 small classes in science department. 
select summ.size,count(summ.size) as class_size_sciences
from (
	select courseId, 
	case when (count_stud<22) then 'small' else 'large'
	end as size
	from class_stud3
) as summ
group by summ.size;


--2d--Are there more male than female in the college? Answer- No- (165 female and 115 male)

select Gender,
count(studentId) as Gender_total 
from Students
group by Gender

--2e--Which course hold more than 70% male or female?

select * from #course_gend1

select a.CourseId, CourseName, a.studentId, b.Gender into #course_gend1
from sum_stud_college2 as a
inner join Students as b
on a.studentId = b.studentId

--number of female in the course
select CourseId,
count (Gender) as F_total into #course_gendF
from #course_gend1 where Gender = 'F'
group by CourseId

--number of male in the course
select CourseId,
count (Gender) as M_total into #course_gendM
from #course_gend1 where Gender = 'M'
group by CourseId

--total number of students in the course
select CourseId,
count (Gender) as G_total into #course_gendG
from #course_gend1
group by CourseId

--table joining the columns 
select a.CourseId, a.F_total, b.M_total, c.G_total into #course_gend2
from #course_gendF as a
inner join #course_gendM as b on b.CourseId = a.CourseId
inner join #course_gendG as c on c.CourseId = a.CourseId


select * from #course_gend2

ALTER TABLE #course_gend2 ADD female_percentage AS (CAST(F_total AS float) / cast(G_total  AS float));
ALTER TABLE #course_gend2 ADD male_percentage AS (CAST(M_total AS float) / cast(G_total  AS float));

--Answer, the courses that have equal or more than 70% stuents from the same gender are 22,23,29 for females and 5 for males 

select a.CourseId,b.CourseName, a.female_percentage,a.male_percentage 
from #course_gend2 as a
inner join courses as b
on a.CourseId = b.CourseId
where a.female_percentage >= 0.7 or a.male_percentage >= 0.7


 
--2f--what percentage of the students and how many of them passed by more than 80 at each department?

 select a.CourseId, a.studentId, a.degree,  
		b.DepartmentID, b. TeacherId, b. CourseName into sum_college4
from Classrooms as a
inner join Courses as b
on a.CourseId = b.CourseId

select * from sum_college4


select DepartmentID, 
count(studentId) as num_stud into #num1
from sum_college4 where degree >= 80
group by DepartmentID;

select DepartmentID,
count(studentId) as total_num_stud into #total_num1
from sum_college4
group by DepartmentID;

 
select a.DepartmentId, a.num_stud,   
		b.total_num_stud into #sum_stud
from #num1 as a
inner join #total_num1 as b
on a.DepartmentId = b.DepartmentId

select * from #sum_stud

select DepartmentId, num_stud, 
cast(num_stud as float) * 100/ cast(total_num_stud as float) as perc_over80 into #sum3
from #sum_stud

--2f--answer
select  a.DepartmentId, b.DepartmentName, a.num_stud, a.perc_over80 --into #sum
from #sum3 as a
inner join Departments as b
on a.DepartmentId = b.DepartmentId


--2g--what percentage of the students and how many of them didnt passed (less than 60) at each department?

select DepartmentID, 
count(studentId) as num_stud_less60 into #less60
from sum_college4 where degree < 60
group by DepartmentID;

select * from #less60
 
select a.DepartmentId, a.num_stud_less60,   
		b.total_num_stud into #sum_stud_less60
from #less60 as a
inner join #total_num1 as b
on a.DepartmentId = b.DepartmentId

select * from #sum_stud_less60

select DepartmentId, num_stud_less60, 
(cast(num_stud_less60 as float) * 100/ cast(total_num_stud as float)) as perc_less60 into #sum_less60_a
from #sum_stud_less60


--2g--answer
select  a.DepartmentId, b.DepartmentName, a.num_stud_less60, a.perc_less60 
from #sum_less60_a as a
inner join Departments as b
on a.DepartmentId = b.DepartmentId

--2h--order teachers according to students' average.
--2h--answer
select * from sum_college4

select a.TeacherID, b.firstName, b.LastName, avg(a.degree) as degree_avg 
from sum_college4 as a
inner join Teachers as b
on a.TeacherID=b.TeacherId
group by a.TeacherID,b.firstName, b.LastName
order by degree_avg desc


--3a--create view of courses- departments, teachers and students 

create view college_sum_view1 as 
select a.CourseName, b.DepartmentName, c. FirstName, C. LastName, d. sum_stud
from courses as a
inner Join departments as b
on a.DepartmentID= b. DepartmentId
inner Join Teachers as c
on a. TeacherId = c.TeacherId
inner Join 
	(select CourseId, 
	count (StudentId) as sum_stud
	from Classrooms
	group by courseID) as d 
on a. CourseId = d. CourseId

select * from college_sum_view1

--3b--create view of students-courses, grades averages (by department), grades averages (all)
create view stud_avg_view as 
select stud_count.StudentId, stud_count.FirstName, stud_count.LastName, stud_count.num_course, dept_avg.dep_grade_avg, stud_avg.total_grade_avg
from 
	(select a.StudentId,a.FirstName, a.LastName, b.num_course
	from students as a
	inner Join (
		select studentID,
		count(courseID) as num_course
		from classrooms
		group by studentID) as b
		on a.StudentId = b.StudentId) as stud_count
inner join 
	(select a.StudentId, avg(a.degree) as dep_grade_avg, c. DepartmentName  
	from Classrooms as a 
	inner join courses as b
	on a.CourseId = b.CourseId 
	right join Departments as c
	on b.DepartmentID=c.DepartmentId
	group by StudentId, DepartmentName
	) as dept_avg
on stud_count.studentID= dept_avg.studentID
right join
	(select StudentId, 
	avg(degree) as total_grade_avg 
	from Classrooms
	group by StudentId
	) as stud_avg
on 	dept_avg.StudentId=stud_avg.StudentId


select * from stud_avg_view