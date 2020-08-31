
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
numstud2 <- numstud2[!duplicated(numstud2$StudentId), ]
numstud3 <- numstud2 %>% group_by(CourseName) %>% summarise(n_distinct(StudentId)) 
numstud4 <- colSums(Filter(is.numeric,numstud3))
bind_rows(numstud3,numstud4)
#######need to add the word sum!!!!!!!
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

##smallsize <- count(Filter(is.numeric,small))

#we create a dataFrame with the names of columns and rows and the sum of big and small classes
df3 <- c(sum(big),(small))
df3
class_size <- data.frame("classroom_size" = c("Big Classrooms","Small classrooms"),"num_classrooms"=c(count(big)[[1]],count(small)[[1]]))
class_size
big
