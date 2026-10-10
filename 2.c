#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#define E_VALUE 2.718281828459045
#define PI_VALUE 3.141592653589793
#define LN2_VALUE 0.693147180559945
#define SQRT2_VALUE 1.414213562373095
#define GAMMA_VALUE 0.5772156649015328

#define MAX_ITERATIONS 10000000
#define NEWTON_MAX_ITERATIONS 1000
#define GAMMA_EPS_LIMIT 0.01

typedef enum state {
    OK,
    ERROR_NULL,
    ERROR_RANGE,
    ERROR_INPUT,
    ERROR_CONVERGENCE
} state;

state NumberD(const char *n, double *res) {
    if (n == NULL || res == NULL) return ERROR_NULL;

    int sign = 1;
    size_t i = 0;

    if (n[0] == '-') {
        i = 1;
        sign = -1;
    } else if (n[0] == '+') {
        i = 1;
    }

    if (n[i] == '\0') return ERROR_INPUT;

    double x = 0.0;
    double f = 0.0;
    double c = 1.0;
    int digit = 0;
    int point = 0;

    while (n[i] != '\0') {
        if (n[i] >= '0' && n[i] <= '9') {
            if (!point) {
                x = x * 10.0 + (n[i] - '0');
            } else {
                c *= 0.1;
                f = f + (n[i] - '0') * c;
            }
            digit = 1;
        } else if (n[i] == '.') {
            if (point) return ERROR_INPUT;
            point = 1;
        } else {
            return ERROR_INPUT;
        }
        ++i;
    }

    if (!digit) return ERROR_INPUT;

    *res = (x + f) * sign;
    if (!isfinite(*res)) return ERROR_INPUT;
    if (*res <= 0.0 || *res >= 1.0) return ERROR_RANGE;

    return OK;
}

state ELimit(const double eps, double *res) {
    if (res == NULL) return ERROR_NULL;
    if (eps <= 0.0) return ERROR_RANGE;

    double cur;
    double prev = 0.0;
    for (long long n = 1; n <= MAX_ITERATIONS; ++n) {
        cur = pow(1.0 + 1.0 / n, (double)n);
        if (!isfinite(cur)) return ERROR_RANGE;
        if (fabs(cur - prev) < eps) {
            *res = cur;
            return OK;
        }
        prev = cur;
    }
    return ERROR_CONVERGENCE;
}

state ESum(const double eps, double *res) {
    if (res == NULL) return ERROR_NULL;
    if (eps <= 0.0) return ERROR_RANGE;

    double sum = 1.0;
    double cur = 1.0;

    for (long long n = 1; n <= MAX_ITERATIONS; ++n) {
        cur /= (double)n;
        double next = sum + cur;
        if (!isfinite(next)) return ERROR_RANGE;
        if (fabs(sum - next) < eps) {
            *res = next;
            return OK;
        }
        sum = next;
    }
    return ERROR_CONVERGENCE;
}

state EEqua(const double eps, double *res) {
    if (res == NULL) return ERROR_NULL;
    if (eps <= 0.0) return ERROR_RANGE;

    double x = 2.0;
    double prev = 0.0;
    int iterations = 0;

    while (fabs(x - prev) >= eps && iterations < NEWTON_MAX_ITERATIONS) {
        prev = x;
        x = x - (log(x) - 1.0) / (1.0 / x);
        if (!isfinite(x)) return ERROR_RANGE;
        iterations++;
    }

    if (iterations >= NEWTON_MAX_ITERATIONS) return ERROR_CONVERGENCE;

    *res = x;
    return OK;
}

state PiLimit(const double eps, double *res) {
    if (res == NULL) return ERROR_NULL;
    if (eps <= 0.0) return ERROR_RANGE;

    double a = 4.0;
    double pi_prev = 0.0;

    for (long long n = 1; n <= MAX_ITERATIONS; ++n) {
        double pi_n = a;

        if (!isfinite(pi_n)) return ERROR_RANGE;

        if (fabs(pi_n - pi_prev) < eps) {
            *res = pi_n;
            return OK;
        }

        pi_prev = pi_n;

        double numerator = 16.0 * (double)n * (double)(n + 1) * (double)(n + 1) * (double)(n + 1);
        double denominator = (2.0 * n + 1.0) * (2.0 * n + 1.0) * (2.0 * n + 2.0) * (2.0 * n + 2.0);
        a *= numerator / denominator;
    }

    return ERROR_CONVERGENCE;
}

state PiSum(const double eps, double *res) {
    if (res == NULL) return ERROR_NULL;
    if (eps <= 0.0) return ERROR_RANGE;

    double sum = 0.0;
    double sign = 1.0;

    for (long long n = 1; n <= MAX_ITERATIONS; ++n) {
        double next = sum + sign / (2.0 * n - 1.0);
        if (!isfinite(next)) return ERROR_RANGE;
        if (fabs(sum - next) < eps / 4.0) {
            *res = 4.0 * next;
            return OK;
        }
        sum = next;
        sign *= -1.0;
    }

    return ERROR_CONVERGENCE;
}

state PiEqua(const double eps, double *res) {
    if (res == NULL) return ERROR_NULL;
    if (eps <= 0.0) return ERROR_RANGE;

    double x = 3.0;
    double prev = 0.0;
    int iterations = 0;

    while (fabs(x - prev) >= eps && iterations < NEWTON_MAX_ITERATIONS) {
        prev = x;
        double sin_x = sin(x);

        if (fabs(sin_x) < eps) break;

        x = x - (cos(x) + 1.0) / (-sin_x);
        if (!isfinite(x)) return ERROR_RANGE;
        iterations++;
    }

    if (iterations >= NEWTON_MAX_ITERATIONS) return ERROR_CONVERGENCE;

    *res = x;
    return OK;
}

state Ln2Limit(const double eps, double *res) {
    if (res == NULL) return ERROR_NULL;
    if (eps <= 0.0) return ERROR_RANGE;

    double cur = 0.0;
    double prev = 0.0;

    for (long long n = 1; n <= MAX_ITERATIONS; ++n) {
        cur = (double)n * (pow(2.0, 1.0 / (double)n) - 1.0);
        if (!isfinite(cur)) return ERROR_RANGE;
        if (fabs(cur - prev) < eps) {
            *res = cur;
            return OK;
        }
        prev = cur;
    }

    return ERROR_CONVERGENCE;
}

state Ln2Sum(const double eps, double *res) {
    if (res == NULL) return ERROR_NULL;
    if (eps <= 0.0) return ERROR_RANGE;

    double sum = 0.0;
    double sign = 1.0;

    for (long long n = 1; n <= MAX_ITERATIONS; ++n) {
        double next = sum + sign / (double)n;
        if (!isfinite(next)) return ERROR_RANGE;
        if (fabs(sum - next) < eps) {
            *res = next;
            return OK;
        }
        sum = next;
        sign *= -1.0;
    }

    return ERROR_CONVERGENCE;
}

state Ln2Equa(const double eps, double *res) {
    if (res == NULL) return ERROR_NULL;
    if (eps <= 0.0) return ERROR_RANGE;

    double x = 0.5;
    double prev = 0.0;
    int iterations = 0;

    while (fabs(x - prev) >= eps && iterations < NEWTON_MAX_ITERATIONS) {
        prev = x;
        double exp_x = exp(x);
        x = x - (exp_x - 2.0) / exp_x;
        if (!isfinite(x)) return ERROR_RANGE;
        iterations++;
    }

    if (iterations >= NEWTON_MAX_ITERATIONS) return ERROR_CONVERGENCE;

    *res = x;
    return OK;
}

state Sqrt2Limit(const double eps, double *res) {
    if (res == NULL) return ERROR_NULL;
    if (eps <= 0.0) return ERROR_RANGE;

    double cur;
    double prev = -0.5;

    for (long long n = 1; n <= MAX_ITERATIONS; ++n) {
        cur = prev - (prev * prev) / 2.0 + 1.0;
        if (!isfinite(cur)) return ERROR_RANGE;
        if (fabs(cur - prev) < eps) {
            *res = cur;
            return OK;
        }
        prev = cur;
    }

    return ERROR_CONVERGENCE;
}

state Sqrt2Mult(const double eps, double *res) {
    if (res == NULL) return ERROR_NULL;
    if (eps <= 0.0) return ERROR_RANGE;

    double p = 1.0;

    for (long long n = 2; n <= 63; ++n) {
        double power = exp2(ldexp(1.0, -(int)n));
        double next = p * power;
        if (!isfinite(next)) return ERROR_RANGE;
        if (fabs(p - next) < eps) {
            *res = next;
            return OK;
        }
        p = next;
    }

    return ERROR_CONVERGENCE;
}

state Sqrt2Equa(const double eps, double *res) {
    if (res == NULL) return ERROR_NULL;
    if (eps <= 0.0) return ERROR_RANGE;

    double x = 1.0;
    double prev = 0.0;
    int iterations = 0;

    while (fabs(x - prev) >= eps && iterations < NEWTON_MAX_ITERATIONS) {
        prev = x;
        x = x - (x * x - 2.0) / (2.0 * x);
        if (!isfinite(x)) return ERROR_RANGE;
        iterations++;
    }

    if (iterations >= NEWTON_MAX_ITERATIONS) return ERROR_CONVERGENCE;

    *res = x;
    return OK;
}

state YLimit(const double eps, double *res) {
    if (res == NULL) return ERROR_NULL;
    if (eps <= 0.0) return ERROR_RANGE;
    (void)eps;
    return ERROR_CONVERGENCE;
}

state YSum(const double eps, double *res) {
    if (res == NULL) return ERROR_NULL;
    if (eps <= 0.0) return ERROR_RANGE;
    (void)eps;
    return ERROR_CONVERGENCE;
}

void sieve_of_eratosthenes(int limit, int *primes, int *prime_count) {
    if (primes == NULL || prime_count == NULL) return;

    unsigned char *is_prime = (unsigned char*)malloc((size_t)limit + 1);
    if (is_prime == NULL) {
        *prime_count = 0;
        return;
    }

    for (int i = 0; i <= limit; ++i) is_prime[i] = 1;
    if (limit >= 0) is_prime[0] = 0;
    if (limit >= 1) is_prime[1] = 0;

    for (int i = 2; i <= limit / i; ++i) {
        if (is_prime[i]) {
            for (int j = i * i; j <= limit; j += i) {
                is_prime[j] = 0;
            }
        }
    }

    *prime_count = 0;
    for (int i = 2; i <= limit; ++i) {
        if (is_prime[i]) {
            primes[(*prime_count)++] = i;
        }
    }

    free(is_prime);
}

state YEqua(const double eps, double *res) {
    if (res == NULL) return ERROR_NULL;
    if (eps <= 0.0) return ERROR_RANGE;
    if (eps < GAMMA_EPS_LIMIT) return ERROR_CONVERGENCE;

    int limit = 200000;
    int *primes = (int*)malloc((size_t)limit * sizeof(int));
    if (primes == NULL) return ERROR_NULL;

    int prime_count = 0;
    sieve_of_eratosthenes(limit, primes, &prime_count);

    double prev_gamma = NAN;

    for (int t = 2000; t <= limit; t += 2000) {
        long double ln_t = logl((long double)t);
        long double s = 0.0L;

        for (int i = 0; i < prime_count && primes[i] <= t; ++i) {
            long double p = (long double)primes[i];
            s += logl((p - 1.0L) / p);
        }

        long double gamma_ld = -logl(ln_t) - s;
        double gamma = (double)gamma_ld;

        if (!isfinite(gamma)) {
            free(primes);
            return ERROR_RANGE;
        }

        if (!isnan(prev_gamma) && fabs(gamma - prev_gamma) < eps) {
            *res = gamma;
            free(primes);
            return OK;
        }

        prev_gamma = gamma;
    }

    free(primes);
    return ERROR_CONVERGENCE;
}

void print_result(const char *name, state st_limit, double limit_val,
                  state st_series, double series_val,
                  state st_eq, double eq_val, int is_gamma, double eps) {
    int gamma_eq_na = is_gamma && (eps < GAMMA_EPS_LIMIT);

    printf("%-10s | Limit: ", name);
    if (is_gamma) {
        printf("N/A");
    } else if (st_limit == OK) {
        printf("%.15f", limit_val);
    } else if (st_limit == ERROR_CONVERGENCE) {
        printf("NO_CONVERGENCE");
    } else {
        printf("ERROR");
    }

    printf(" | Series: ");
    if (is_gamma) {
        printf("N/A");
    } else if (st_series == OK) {
        printf("%.15f", series_val);
    } else if (st_series == ERROR_CONVERGENCE) {
        printf("NO_CONVERGENCE");
    } else {
        printf("ERROR");
    }

    printf(" | Equation: ");
    if (gamma_eq_na) {
        printf("N/A (eps < %.2f)", GAMMA_EPS_LIMIT);
    } else if (st_eq == OK) {
        printf("%.15f", eq_val);
    } else if (st_eq == ERROR_CONVERGENCE) {
        printf("NO_CONVERGENCE");
    } else {
        printf("ERROR");
    }

    printf("\n");
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Использование: %s <epsilon>\n", argv[0]);
        fprintf(stderr, "Пример: %s 0.00001\n", argv[0]);
        fprintf(stderr, "Примечание: epsilon должна быть в диапазоне (0, 1)\n");
        return EXIT_FAILURE;
    }

    double eps;
    state st = NumberD(argv[1], &eps);
    if (st != OK) {
        switch (st) {
            case ERROR_RANGE:
                fprintf(stderr, "Ошибка: epsilon должна быть в диапазоне (0, 1)\n");
                break;
            case ERROR_INPUT:
                fprintf(stderr, "Ошибка: некорректное значение epsilon (не число или бесконечность)\n");
                break;
            default:
                fprintf(stderr, "Ошибка: некорректное значение epsilon\n");
        }
        return EXIT_FAILURE;
    }

    printf("Вычисление математических констант с точностью epsilon = %.10f\n\n", eps);

    double eLim, eSum, eEqua;
    state st_e_lim = ELimit(eps, &eLim);
    state st_e_sum = ESum(eps, &eSum);
    state st_e_equ = EEqua(eps, &eEqua);

    printf("Вычисление числа e:\n");
    print_result("e", st_e_lim, eLim, st_e_sum, eSum, st_e_equ, eEqua, 0, eps);
    printf("Стандартное значение e: %.15f\n\n", E_VALUE);

    double piLim, piSum, piEqua;
    state st_pi_lim = PiLimit(eps, &piLim);
    state st_pi_sum = PiSum(eps, &piSum);
    state st_pi_equ = PiEqua(eps, &piEqua);

    printf("Вычисление числа π:\n");
    print_result("π", st_pi_lim, piLim, st_pi_sum, piSum, st_pi_equ, piEqua, 0, eps);
    printf("Стандартное значение π: %.15f\n\n", PI_VALUE);

    double ln2Lim, ln2Sum, ln2Equa;
    state st_ln2_lim = Ln2Limit(eps, &ln2Lim);
    state st_ln2_sum = Ln2Sum(eps, &ln2Sum);
    state st_ln2_equ = Ln2Equa(eps, &ln2Equa);

    printf("Вычисление числа ln(2):\n");
    print_result("ln(2)", st_ln2_lim, ln2Lim, st_ln2_sum, ln2Sum, st_ln2_equ, ln2Equa, 0, eps);
    printf("Стандартное значение ln(2): %.15f\n\n", LN2_VALUE);

    double sqrt2Lim, sqrt2Mult, sqrt2Equa;
    state st_sqrt2_lim = Sqrt2Limit(eps, &sqrt2Lim);
    state st_sqrt2_mult = Sqrt2Mult(eps, &sqrt2Mult);
    state st_sqrt2_equ = Sqrt2Equa(eps, &sqrt2Equa);

    printf("Вычисление числа √2:\n");
    print_result("√2", st_sqrt2_lim, sqrt2Lim, st_sqrt2_mult, sqrt2Mult, st_sqrt2_equ, sqrt2Equa, 0, eps);
    printf("Стандартное значение √2: %.15f\n\n", SQRT2_VALUE);

    double yLim = 0.0, ySum = 0.0, yEqua = 0.0;
    state st_y_lim = YLimit(eps, &yLim);
    state st_y_sum = YSum(eps, &ySum);
    state st_y_equ = YEqua(eps, &yEqua);

    printf("Вычисление числа γ (постоянная Эйлера-Маскерони):\n");
    print_result("γ", st_y_lim, yLim, st_y_sum, ySum, st_y_equ, yEqua, 1, eps);
    printf("Приблизительное значение γ: %.15f\n", GAMMA_VALUE);

    return EXIT_SUCCESS;
}