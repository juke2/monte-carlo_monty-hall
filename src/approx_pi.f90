PROGRAM approx_pi
    IMPLICIT NONE
    INTEGER :: i,j,iterations,step,inside_circle
    REAL :: rand_x, rand_y, pi_val, elapsed_time, time_ns
    CHARACTER(len=32) :: arg
    INTEGER iteration, beginning, end, rate

    CALL RANDOM_SEED()

    DO i = 1, iargc()
        CALL getarg(i, arg)
        IF (i == 1) THEN
            READ(arg,"(I20.1)") iterations
        END IF
        IF (i == 2) THEN
            READ(arg,"(I20.1)") step
        END IF
    END DO

    OPEN(1, file = './src/output/fortran_out.txt', status = 'replace')  
    
    DO i = step,iterations,step
        inside_circle = 0
        WRITE(*,*) "Iterations: ", i
        CALL system_clock(beginning, rate)
        DO j=0,i,1
            CALL RANDOM_NUMBER(rand_x)
            CALL RANDOM_NUMBER(rand_y)
            IF (rand_x**2 + rand_y**2 .LT. 1) THEN
                inside_circle = inside_circle + 1
            END IF

        END DO
        pi_val = 4 * (REAL(inside_circle)/REAL(i))
        CALL system_clock(end)
        elapsed_time =  real(end - beginning) / real(rate)
        time_ns = (elapsed_time) * 1000000000.0
        WRITE(1,"(F12.10,A,I29.1,A,F20.10)") pi_val,",", i, ",",time_ns

    END DO
    
END PROGRAM