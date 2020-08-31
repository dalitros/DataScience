
con <- dbConnect(odbc::odbc(), 
                 Driver = "SQL Server", 
                 Server = "localhost\\SQLEXPRESS", 
                 Database = "COLLEGE", 
                 Trusted_Connection = "True")

#######################################################
#Q1. Count the number of students on each department
######################################################

courses <- tbl(con, "Courses")                   
courses

classrooms <- tbl(con, "Classrooms")                   
classrooms

departments <- tbl(con, "Departments")                   
departments

students <- tbl(con, "Students")                   
students

teachers <- tbl(con, "Teachers")                   
teachers


##numstud <- courses %>% inner_join(classrooms, by = "CourseId") %>% inner_join(departments, by.x = "DepartmentID", by.y = "DepartmentId")
                                      
##numstud

###inner join courses and classrooms by CoureId and departments by departmentID on the left and departmentId on the right

numstud <- merge((merge(classrooms, courses, by = "CourseId")), departments, by.x = "DepartmentID", by.y = "DepartmentId")
numstud

###count distinct the number of students, groupby deprtmentName.

numstud1 <- numstud %>% group_by(DepartmentName) %>% summarise(num_student = n_distinct(StudentId))
numstud1

####################################################################################################################
#Q2. How many students have each course of the English department and the total number of students in the department
####################################################################################################################

#we aggregate the number of students, in all courses belong to deprtmentName == English. 

numstud2 <- subset(numstud, numstud$DepartmentName == 'English')
numstud2 <- numstud2[!duplicated(numstud2$StudentId), ]
numstud3 <- numstud2 %>% group_by(CourseName) %>% summarise(number_stud=(n_distinct(StudentId))) 
sum <- c('Sum',sum(numstud3$number_stud))
numstud5 <- rbind(numstud3,sum)
numstud5


#############################################################################################################
#Q3. How many small (<22 students) and large (22+ students) classrooms are needed for the Science department?
#############################################################################################################

#we count the number of students at each course that belong to the science department (departmentID==2)
df1 <- subset(numstud, numstud$DepartmentID==2)
df2 <- df1 %>% group_by(CourseName) %>% summarise(Students=(n_distinct(StudentId)))
df2      

#we defined big when the value at the students column >= 22 and small when it is less than 22                                                                                
big <- subset(df2, df2$Students>=22)
small <- subset(df2, df2$Students<22)

#we create a dataFrame with the names of columns and rows and the sum of big and small classes
class_size <- data.frame("classroom_size" = c("Big Classrooms","Small classrooms"),"num_classrooms"=c(count(big)[[1]],count(small)[[1]]))
class_size

######################################################################################################################
#Q4. A feminist student claims that there are more male than female in the College. Justify if the argument is correct
######################################################################################################################

#we counted the number of students at each Gender
stud_gender <- students %>% group_by(Gender) %>% summarise(n_distinct(StudentId))
stud_gender

##########################################################################
#Q5. For which courses the percentage of male/female students is over 70%?
##########################################################################

numstud6 <- merge(numstud, students, by = "StudentId")

#aggregartion total number of students at each course

total<- numstud6 %>% group_by(CourseName) %>% summarise(total_stud = n_distinct(StudentId))
male <- subset(numstud6, numstud6$Gender== 'M') %>% group_by(CourseName) %>% summarise(male = n_distinct(StudentId))
female <- subset(numstud6, numstud6$Gender== 'F') %>% group_by(CourseName) %>% summarise(female = n_distinct(StudentId))


#merging total with male and female
f_m <- merge(merge(total, male, by = "CourseName"),female)

#create a new column by Dividing the number of female(female)*100 to total number of students (total) to get the percentage
f_m$percF <- f_m$female*100/f_m$total
f_m$percM <- f_m$male*100/f_m$total
f_m
f_m %>% filter(percF>70 | percM>70)

#########################################################################
#Q6. For each department, how many students passed with a grades over 80?
#########################################################################

#we counted the number of students with grade>80, grouped by departments 

deg80 <- numstud6 %>% subset(degree>80)
deg80 %>% group_by(DepartmentName) %>% summarise(PassedOver_80 = n_distinct(StudentId))


##########################################################################
#Q7. For each department, how many students passed with a grades under 60?
##########################################################################

##we counted the number of students with grade<60, grouped by departments 


deg60 <- numstud6 %>% subset(degree<60)
deg60 %>% group_by(DepartmentName) %>% summarise(Passedunder_60 = n_distinct(StudentId))


###############################################################################
#Q8. Rate the teachers by their average student's grades (in descending order).
###############################################################################

#getting the averages of students' degrees for each teacher. 
numstud7 <- merge(numstud6, teachers, by = "TeacherId")
teachersRate <- numstud7 %>% group_by(FirstName.y, LastName.y) %>% summarise(Avg_degree = mean(degree))
#sorting the table by the 'Avg_degree' column
arrange(teachersRate,desc(Avg_degree))


##########################################################################################################################################################################################################################
#Q9. Create a dataframe showing the courses, departments they are associated with, the teacher in each course, and the number of students enrolled in the course (for each course, department and teacher show the names).
##########################################################################################################################################################################################################################


#counting the number of student for each course 
numstudclass <- numstud7 %>% group_by(CourseId, CourseName, DepartmentName, FirstName.y, LastName.y) %>% summarise(num_students = n_distinct(StudentId))


#######################################################################################################################################################################################
#Q10. Create a dataframe showing the students, the number of courses they take, the average of the grades per class, and their overall average (for each student show the student name).
#######################################################################################################################################################################################

#calculate the total grades' average (mean) and the total number of courses (nunique) for each students 
numclass <- numstud7 %>% group_by(StudentId) %>% summarise(mean_general=(mean(degree)), num_course=(n_distinct(degree)))
numclass


#calculate grades' average for each students grouped by departments
studentdegree <- numstud7 %>% group_by(StudentId,FirstName.x,LastName.x,DepartmentName) %>% summarise(mean=(mean(degree)))
studentdegree

a <- studentdegree %>% inner_join(numclass, by = "StudentId")

#unstaking the departmentName

b <- studentdegree %>% spread(DepartmentName, mean)

cbind(b,mean_gen=numclass$mean_general,num_course=numclass$num_course)

