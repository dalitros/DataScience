
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
numstud2
numstud3 <- numstud2 %>% group_by(CourseName) %>% summarise(num_student = n_distinct(StudentId))
numstud3
