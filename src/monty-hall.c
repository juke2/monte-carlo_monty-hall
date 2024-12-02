#include <stdio.h>
#include <stdlib.h>

int DEFAULT_ITER = 1000;

double *monty_hall(int iter_count)
{
    double *results_array = (double *)malloc(2 * sizeof(double));
    int success_switch_total = 0;
    int success_no_switch_total = 0;
    int prize, guess;
    for (int i = 0; i < iter_count * 2; i++)
    {
        prize = rand() % 3;
        guess = rand() % 3;
        // printf("Guess: %d. Prize: %d. Iter %%2: %d\n", guess, prize, i % 2);

        if (i % 2 == 0 && prize == guess)
        {
            // printf("No Switch Success!\n");
            success_no_switch_total++;
        }
        else if (i % 2 == 1)
        {
            if (prize != guess)
            {
                success_switch_total++;
            }

            // printf("New guess: %d\n", new_guess);
        }
    }
    // printf("Switch: %d. No Switch: %d.\n", success_switch_total, success_no_switch_total);
    results_array[0] = (double)(success_no_switch_total) / (double)(iter_count);
    results_array[1] = (double)(success_switch_total) / (double)(iter_count);
    // printf("Probability when switching: %f. Probability without switching: %f.", results_array[0], results_array[1]);
    return results_array;
}

int main(int argc, char **argv)
{
    double *results_array;
    double switch_prob, no_switch_prob;
    unsigned int rand_seed = 1;
    int iter_count = DEFAULT_ITER;
    if (argc >= 3)
    {
        iter_count = atoi(argv[1]);
        rand_seed = atoi(argv[2]);
    }
    srand(rand_seed);
    results_array = monty_hall(iter_count);
    no_switch_prob = results_array[0];
    switch_prob = results_array[1];
    printf("Probability when switching: %f. Probability without switching: %f. Number of iterations: %d\n", switch_prob, no_switch_prob, iter_count);
    FILE *file_pointer;
    file_pointer = fopen("src/output/output_monty_hall_val.txt", "w");
    fprintf(file_pointer, "(%.30f,%.30f)", switch_prob, no_switch_prob);
    fclose(file_pointer);
    file_pointer = NULL;
    free(results_array);
    results_array = NULL;
}
