#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <pthread.h>
#include <time.h>
#include <math.h>

#define INT_MAX 0x7fffffff

int DEFAULT_ITER_COUNT = 100;
int nanosecond_conversion = 1000000000;
bool output_points = false;
bool multi_thread = false;
bool deterministic = false;
int core_count = 1;
long *iterations_input;
pthread_mutex_t allowed_access;
pthread_t *thread_id;
pthread_attr_t attr;
void *status;

// constants for mersenne twister
long w_t = sizeof(int);
long n_t = 624;
long m_t = 397;
long r_t = 31;
long a_t = 0x9090B0DF;
long u_t = 11;
long s_t = 7;
long b_t = 0x9D2C5680;
long t_t = 15;
long c_t = 0xEFC60000;
long l_t = 18;
long f_t = 1812433253;

typedef struct
{
    int in_radius;
    int *seed_list;
    int *iter_count;
    bool twist;
} point_container;

point_container point_cont;
int my_rand(int last)
{
    last = 1103515245 * last + 12345;
    return (unsigned int)(last % RAND_MAX);
}

int *mersenne_twister(int seed, int amount)
{
    unsigned int *series = (int *)malloc(amount * sizeof(int)); // only a "small" amount of memory... oh no
    series[0] = seed;
    for (int i = 1; i < amount; i++)
    {
        series[i] = f_t * (series[i - 1] ^ (series[i - 1] >> w_t - 2)) + i;
    }
    for (int i = 0; i < amount; i++)
    {
        series[i] = series[i] ^ ((series[i] >> u_t));
        series[i] = series[i] ^ ((series[i] >> s_t) & b_t);
        series[i] = series[i] ^ ((series[i] >> t_t) & c_t);
        series[i] = series[i] ^ ((series[i] >> l_t));
    }
    return series;
}

void *
calc_random_point(void *void_iterations)
{

    long index = (long)void_iterations;
    int local_in_radius_count = 0;
    int iterations = point_cont.iter_count[index];
    if (!point_cont.twist)
    {
        int random_x_last, random_y_last;
        random_y_last = point_cont.seed_list[index];
        for (int i = 0; i < iterations; i++)
        {
            random_x_last = my_rand(random_y_last);
            random_y_last = my_rand(random_x_last);
            double random_x = (double)random_x_last / (double)RAND_MAX;
            double random_y = (double)random_y_last / (double)RAND_MAX;
            if (random_x * random_x + random_y * random_y < 1.0)
            {
                local_in_radius_count++;
            }
        }
    }
    else
    {
        int *series = mersenne_twister(point_cont.seed_list[index], iterations * 2);
        for (int i = iterations * 2 - 1; i >= 0; i -= 2)
        {
            double random_x = (double)series[i - 1] / (double)INT_MAX;
            double random_y = (double)series[i] / (double)INT_MAX;
            if (random_x * random_x + random_y * random_y < 1.0)
            {
                local_in_radius_count++;
            }
        }
        free(series);
        series = NULL;
    }
    pthread_mutex_lock(&allowed_access);
    point_cont.in_radius += local_in_radius_count;
    pthread_mutex_unlock(&allowed_access);

    pthread_exit(NULL);
}

double monte_carlo_multithread(int iterations)
{

    thread_id = (pthread_t *)malloc(core_count * sizeof(pthread_t));

    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
    pthread_mutex_init(&allowed_access, NULL);

    long iteration_per_thread = (long)iterations / (long)core_count;
    long remaining_iterations = (long)iterations % (long)core_count;

    for (long i = 0; i < core_count; i++)
    {
        point_cont.iter_count[i] = iteration_per_thread;
        point_cont.seed_list[i] = rand();
        if (i == 0)
        {
            point_cont.iter_count[i] += remaining_iterations;
        }
        pthread_create(&thread_id[i], &attr, calc_random_point, (void *)i);
    }
    printf("\nCORE COUNT:%d\n", core_count);
    for (int i = 0; i < core_count; i++)
    {

        pthread_join(thread_id[i], &status);
    }
    pthread_attr_destroy(&attr);
    double r_val = ((double)point_cont.in_radius / (double)iterations) * 4.0;
    pthread_mutex_destroy(&allowed_access);
    return r_val;
}

double monte_carlo(int iterations)
{
    double *x_array = (double *)malloc(iterations * sizeof(double));
    double *y_array = (double *)malloc(iterations * sizeof(double));
    int in_radius = 0;
    if (!point_cont.twist)
    {
        for (int i = 0; i < iterations; i++)
        {
            double random_x = (double)rand() / (double)RAND_MAX;
            double random_y = (double)rand() / (double)RAND_MAX;
            if (random_x * random_x + random_y * random_y < 1.0)
            {
                in_radius++;
            }
            if (output_points)
            {
                x_array[i] = random_x;
                y_array[i] = random_y;
            }
        }
    }
    else
    {
        int *series = mersenne_twister(rand(), iterations * 2);
        for (int i = iterations * 2 - 1; i >= 0; i -= 2)
        {

            double random_x = (double)series[i - 1] / (double)INT_MAX;
            double random_y = (double)series[i] / (double)INT_MAX;
            if (random_x * random_x + random_y * random_y < 1.0)
            {
                in_radius++;
            }
            if (output_points)
            {
                x_array[i] = random_x;
                y_array[i] = random_y;
            }
        }
        free(series);
        series = NULL;
    }
    if (output_points)
    {
        FILE *file_pointer;
        file_pointer = fopen("src/output/output_monte_carlo_generated_points.txt", "w");
        for (int point = 0; point < iterations; point++)
        {
            fprintf(file_pointer, "(%.30f,%.30f)", x_array[point], y_array[point]);
            if (point != iterations - 1)
            {
                fprintf(file_pointer, ",");
            }
        }
        fclose(file_pointer);
    }
    free(x_array);
    free(y_array);
    x_array = NULL;
    y_array = NULL;
    return ((double)in_radius / (double)iterations) * 4.0;
}

double monte_carlo_deterministic(int iterations)
{
    // assume input is perfect square, if it isn't round down
    double real_x, real_y;
    int in_circle = 0;
    int iter_per_side = (int)(sqrt((double)iterations));
    for (double x = 0; x <= iter_per_side; x++)
    {
        for (double y = 0; y <= iter_per_side; y++)
        {
            real_x = (double)x / (double)iter_per_side;
            real_y = (double)y / (double)iter_per_side;
            if (real_x * real_x + real_y * real_y < 1)
            {
                in_circle++;
            }
        }
    }

    return 4.0 * ((double)in_circle / (double)(iter_per_side * iter_per_side));
}
int main(int argc, char **argv)
{
    int iter_count = DEFAULT_ITER_COUNT;
    point_cont.twist = false;
    struct timespec start, end;
    point_cont.in_radius = 0;
    if (argc >= 3)
    {
        iter_count = atoi(argv[1]);
        double pi_val;
        srand(atoi(argv[2]));
        if (argc >= 4 && atoi(argv[3]) == 1)
        {
            multi_thread = true;
        } // Use Multi-Threading
        if (argc >= 5 && atoi(argv[4]) == 1)
        {
            output_points = true;
        } // Output generated random points
        if (argc >= 6)
        {
            core_count = atoi(argv[5]);
        } // Number of cores
        if (argc >= 7 && atoi(argv[6]) == 1)
        {
            point_cont.twist = true;
        } // use Mersenne Twister
        if (argc >= 8 && atoi(argv[7]) == 1)
        {
            deterministic = true;
        }
        if (argc)
            if (deterministic)
            {
                clock_gettime(CLOCK_MONOTONIC, &start);
                pi_val = monte_carlo_deterministic(iter_count);
                clock_gettime(CLOCK_MONOTONIC, &end);
            }
            else if (multi_thread)
            {
                point_cont.seed_list = (int *)malloc(core_count * sizeof(int));
                point_cont.iter_count = (int *)malloc(core_count * sizeof(int));
                clock_gettime(CLOCK_MONOTONIC, &start);
                pi_val = monte_carlo_multithread(iter_count);
                clock_gettime(CLOCK_MONOTONIC, &end);
            }

            else
            {
                clock_gettime(CLOCK_MONOTONIC, &start);
                pi_val = monte_carlo(iter_count);
                clock_gettime(CLOCK_MONOTONIC, &end);
            }
        double time_diff = (end.tv_sec - start.tv_sec) * nanosecond_conversion;
        time_diff += (end.tv_nsec - start.tv_nsec);
        printf("Value of pi calculated with Monte-Carlo integration: %f\nNumber of iterations: %d\nRandom Seed: %s\nMultithread? %d Twister? %d Deterministic? %d\n", pi_val, iter_count, argv[2], multi_thread, point_cont.twist, deterministic);
        FILE *file_pointer;
        file_pointer = fopen("src/output/output_monte_carlo_val.txt", "w");
        fprintf(file_pointer, "(%.30f,%.30f)", pi_val, time_diff);
        fclose(file_pointer);
        file_pointer = NULL;
    }
    else
    {
        printf("Number of arguments does not match expected. Expected at least 3 arguments, recieved %d", argc);
    }
    pthread_exit(NULL);
}
