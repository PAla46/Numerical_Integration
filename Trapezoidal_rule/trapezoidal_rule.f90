Program trapezoidal_rule
    IMPLICIT NONE 

    Real*8 :: func,a,b,integral,actual,error  
    
    a=0
    b=3 
    actual = exp(3.0d0) - 1 
    integral = (b-a) * ((func(a) + func(b))/2)
    error = actual - integral 

    print*, 'The value of the integral using Trapezoidal rule is ',integral 
    print*, 'The actual value of the integral is ',actual 
    print*, 'The error in the value is ',error 


End Program trapezoidal_rule 

Real*8 function func(x) 
    IMPLICIT NONE 
    real*8 :: x
    func = exp(x)

End function 