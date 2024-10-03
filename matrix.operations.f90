PROGRAM matrice
 USE operations
 IMPLICIT NONE

 REAL, allocatable :: v(:,:)  !declaring an array as allocatble ie unknown size array
 INTEGER :: i, j, k, l    
 REAL :: A, B
PRINT*,'Enter the number of columns' 
READ*,k                                
PRINT*,'Enter the number of rows'
READ*,l
allocate(v(k,l))
PRINT*,'Enter the elements row by row'
do i=1,l
 do j=1,k
  READ*,v(i,j)
 end do
end do
PRINT*,V
A= Trace(v)
B=Determinat(v)
END PROGRAM matrice