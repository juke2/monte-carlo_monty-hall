PROGRAM approx_pi_multithread
    USE OMP_LIB
    IMPLICIT NONE
    INTEGER :: i,j,iterations,step,inside_circle, thread_num, iteration, beginning, end, rate
    REAL :: rand_x, rand_y, pi_val, elapsed_time, time_ns
    CHARACTER(len=32) :: arg
    INTEGER, SAVE :: thread_id
    INTEGER, DIMENSION(:), ALLOCATABLE :: sum_vals 
    !$OMP THREADPRIVATE(thread_id)

    thread_num = 4
    CALL RANDOM_SEED()
    DO i = 1, iargc()
        CALL getarg(i, arg)
        IF (i == 1) THEN
            READ(arg,"(I20.1)") iterations
        END IF
        IF (i == 2) THEN
            READ(arg,"(I20.1)") step
        END IF
        IF (i == 3) THEN
            READ(arg,"(I20.1)") thread_num
        END IF
    END DO


    CALL OMP_set_num_threads(thread_num)

    ALLOCATE(sum_vals(0:thread_num-1))

    OPEN(1, file = './src/output/fortran_out_multithread.txt', status = 'replace')  
    
    !$OMP PARALLEL
    thread_id = OMP_get_thread_num()
    WRITE(*,*) thread_id
    !$OMP END PARALLEL
    DO i = step,iterations,step
        inside_circle = 0
        DO j = 0,thread_num-1,1
            sum_vals(j) = 0
        END DO
        WRITE(*,*) "Iterations: ", i
        CALL system_clock(beginning, rate)
        !$OMP PARALLEL DO PRIVATE(j, rand_x, rand_y)
        DO j=0,i,1
            ! WRITE(*,*) thread_id
            CALL RANDOM_NUMBER(rand_x)
            CALL RANDOM_NUMBER(rand_y)
            IF (rand_x**2 + rand_y**2 .LT. 1) THEN
                sum_vals(thread_id) = sum_vals(thread_id) + 1
            END IF

        END DO
        !$OMP END PARALLEL DO
        DO j = 0,thread_num-1,1
            inside_circle = inside_circle + sum_vals(j)
        END DO
        pi_val = 4 * (REAL(inside_circle)/REAL(i))
        CALL system_clock(end)
        elapsed_time =  real(end - beginning) / real(rate)
        time_ns = (elapsed_time) * 1000000000.0
        WRITE(*,"(F12.10,A,I29.1,A,F20.10)") pi_val,",", i, ",",time_ns
        WRITE(1,"(F12.10,A,I29.1,A,F20.10)") pi_val,",", i, ",",time_ns

    END DO

    
END PROGRAM