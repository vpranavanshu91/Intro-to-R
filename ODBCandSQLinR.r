require(RODBC)
db<-odbcConnect("localhost",uid = "pv_ynwa",pwd = "Complicated123#")

Query1 <- "SELECT * FROM EMP,DEPT where EMP.DEPTNO=DEPT.DEPTNO"

EmpData <- sqlQuery(db,query = Query1,sqlGetResults(stringsAsFactors = FALSE))
