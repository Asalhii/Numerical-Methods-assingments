MODULE operations 
IMPLICIT NONE 
CONTAINS

 REAL FUNCTION Trace(x) RESULT(R)
 REAL, DIMENSION(:,:), INTENT(IN) :: x      !declaring the variable to be a matrix without specific dimensions
 if (size(x)==9 .or. size(x)==4) then       ! putting conditions over the size of the matrice    
 if (size(x)==9) then                       
 R=x(1,1)+x(2,2)+x(3,3)
 PRINT*,'the trace is T=',R
 else                                        
 R=x(1,1)+x(2,2)
 PRINT*,'the trace is T=',R
 END IF
 else 
 PRINT*,'trace cant be caculated, you entered a wrong sized matrice'
 END IF
 END FUNCTION 
 
 REAL FUNCTION Determinat(y) RESULT(S)
 REAL, DIMENSION(:,:), INTENT(IN) :: y
 if (size(y)==9 .or. size(y)==4) then
 if (size(y)==9) then
 S=y(1,1)*(y(2,2)*y(3,3)-y(2,3)*y(3,2))-y(1,2)*(y(2,1)*y(3,3)-y(2,3)*y(3,1))+y(1,3)*(y(2,1)*y(3,2)-y(2,2)*y(3,1))
 !PRINT*,'the determinat is D=',S
 else
 S=y(1,1)*y(2,2)-y(2,1)*y(1,2)
 PRINT*,'the determinat is D=',S
 END IF
 else 
 PRINT*,'determinat cant be caculated, you entered a wrong sized matrice'
 END IF
 END FUNCTION 
 
END MODULE 