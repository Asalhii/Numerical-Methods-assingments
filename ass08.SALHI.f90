program ass08
 USE operations     !using a module for the determinats 
 USE lup_solver     !using the lup solver module to solve the system 
 implicit none 
  real , dimension(3,3) :: matrix,m
  real, dimension(3) :: v, x_lup, x_cramer
  real, dimension(3,3) :: A1, A2, A3
  integer :: ios, i,j
  !reading the matrix and the vector from the file 
  OPEN(10, iostat=ios, FILE='matrix.dat', STATUS='old', ACTION='read')
   IF (ios==0) THEN
    DO i=1,3 
     READ(10,*) matrix(:,i)                       
    END DO
     READ(10,*) v
   else 
    print*,'error'
   End if
  CLOSE(10)
 !defining the matrix A1
 DO i=1,3
 DO j=1,3
  A1(i,j)=matrix(i,j)
 END DO
 END Do
 !replacing the first column of A1 with b values  
 DO i=1,3
  A1(1,i)=v(i)
 END DO
 !defining the matrix A2
 DO i=1,3
 DO j=1,3
  A2(i,j)=matrix(i,j)
 END DO
 END Do
 !replacing the second colmun of A2 with the values of b
 DO i=1,3
  A2(2,i)=v(i)
 END DO
 !defing the matrix A3 
 DO i=1,3
 DO j=1,3
  A3(i,j)=matrix(i,j)
 END DO
 END Do
 !replacing the third colmun of A3 with the values of b
 DO i=1,3
  A3(3,i)=v(i)
 END DO
 !caculating x by calling the determinat module 
 x_cramer(1)=Determinat(A1)/Determinat(matrix)
 x_cramer(2)=Determinat(A2)/Determinat(matrix)
 x_cramer(3)=Determinat(A3)/Determinat(matrix)

 !defining a matrix to be used by the lup solver, since the order of input matrix a is columnwise 
 do i=1,3
  m(i,:)=matrix(:,i)  
 end do
 !calling the lup solver module 
 CALL solve_lup(m,v,x_lup)
 
 !writing the results in a file 
  OPEN(10, iostat=ios, FILE='the solution of the linear system.txt', STATUS='new', ACTION='WRITE')  
  IF (ios==0) THEN
   WRITE(10,'(3f9.1)') matrix
   WRITE(10,*)
   WRITE(10,'(3f9.1)') v
   WRITE(10,*)
   WRITE(10,*) 'the solution of this linear system using Cramer rule is :'
   WRITE(10,*)
   WRITE(10,'(a,f5.1)')'x1=',x_cramer(1)
   WRITE(10,'(a,f5.1)')'x2=',x_cramer(2)
   WRITE(10,'(a,f5.1)')'x3=',x_cramer(3)
   WRITE(10,*)
   WRITE(10,*) 'the solution of this linear system using the lup solver module is :'
   WRITE(10,*)
   !the format here approximates the values given by lup solver (in the standard precision format)
   WRITE(10,'(a,f5.1)')'x1=',x_lup(1)
   WRITE(10,'(a,f5.1)')'x2=',x_lup(2)
   WRITE(10,'(a,f5.1)')'x3=',x_lup(3) 
  ELSE 
   PRINT*,'error, couldnt write into the file'
 END IF
End program ass08