Program composite_trapezoidal_rule
    IMPLICIT NONE 

    Real*8 :: func,a,b,h,x,integral,actual,error 
    Real*8 :: fa,fb,sum
    Integer :: n,i 

    print*, 'Give the value of n'
    read*,n 
    
    a=0
    b=3 
    actual = exp(3.0d0) - 1 

    do 
        n=n*10 
        h=(b-a)/real(n) 
        sum=0.d0

        do i=1,n-1 
            x=a+(h*i)
            sum=sum+func(x)

        end do 

        fa = func(a)/2.0d0
        fa = func(b)/2.0d0

        integral = h*(fa + fb + sum)


        error = actual - integral 

        print*, n,' ',integral,' ',error 

        if(n .ge. 1000000000) exit 

    end do 

End Program composite_trapezoidal_rule 

Real*8 function func(x) 
    IMPLICIT NONE 
    real*8 :: x
    func = exp(x)

End function 