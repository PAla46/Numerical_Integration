Program brute_mc
    IMPLICIT NONE 

    Real*8 :: func,x,p,actual,error 
    Real*8 :: length,sum
    Integer*8 :: n,i 

    print*, 'Give the value of n'
    read*,n 
    
    length = 3.0d0
    actual = exp(3.0d0) - 1 

    do 
        n=n*10 
        sum=0.d0

        do i=1,n
            call random_number(p) 
            x=3.0d0 * p 
            sum=sum+func(x)

        end do 

        sum = sum/real(n)
        sum = length*sum 


        error = actual - sum  

        print*, n,' ',sum,' ',error 

        if(n .ge. 1000000000) exit 

    end do 

End Program brute_mc 

Real*8 function func(x) 
    IMPLICIT NONE 
    real*8 :: x
    func = exp(x)

End function 