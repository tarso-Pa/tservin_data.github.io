use employees;

/* EJERCICIOS EN LA TABLA titles */

-- Número de registros / Muestra todos los registros
Select count(*) From titles;
Select * from titles;

-- Muestra todos los campos ordenado ascendentemente por número de empleado, limitado a 10 registros
Select * From titles Order by emp_no asc Limit 10;

-- Muestra todos los campos ordenado ascendentemente por número de empleado, comenzando desde el 5° registro hasta el 14°
Select * From titles Order by emp_no asc Limit 4, 10; -- En este caso: Limit # registro límite a recortar, # registros a mostrar

-- Muestra el número de nombramientos en toda la historia
Select Distinct title As 'Nombramientos', Count(title) as Cantidad From titles
Group by title Order by count(title);

-- Nombramientos nuevos por año
Select Year(from_date) As 'Año', Count(emp_no) As 'Nombramientos nuevos'
From titles
Group by Year(from_date);

-- Nombramientos finalizados por año
Select Year(to_date) As 'Año' , Count(emp_no) As 'Nombramientos finalizados'
From titles
Group by Year(to_date);

-- Número de nombramientos nuevos por puesto en el año 2000
Select title As 'Puesto', Count(emp_no) As 'Nombramientos año 2000' 
From titles 
Where Year(from_date)=2000
Group by title
Order by Count(emp_no);

-- Empleados con nuevo nombramiento de Assistant Engineer el año 2000
Select emp_no As 'Empleado'
From titles 
Where title like 'As%' and Year(from_date)=2000;

-- Nuevos puestos de Senior Engineer en el año 2000
Select title As 'Puesto', Count(emp_no) As 'Nombramientos nuevos año 2000'
From titles
Where title = 'Senior Engineer' 
and Year(from_date)=2000;

-- Número de ascensos por empleado entre 1990 y 2000
Select emp_no AS 'No. de empleado', Count(title)-1 As 'Ascensos 1990-2000' 
From titles 
Where Year(from_date) Between 1990 and 2000
Group by emp_no
Order by count(title) desc;

-- Número de puestos del empleado 420769 entre 1990 y 2000
Select emp_no AS 'No. de empleado', Count(title) As 'Puestos 1990-2000' 
From titles 
Where emp_no=420769 and Year(from_date) Between 1990 and 2000;

-- Número de ascensos por empleado 2000
Select emp_no AS 'No. de empleado', Count(title)-1 As 'Ascensos 2000' 
From titles 
Where Year(from_date) = 2000
Group by emp_no
Order by count(title) desc;




