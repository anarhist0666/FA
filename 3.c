#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

typedef enum state {
    OK,
    ERROR_NULL,
    ERROR_RANGE,
    ERROR_OVERFLOW,
    ERROR_INPUT,
    INF,
    NO,
    ONE,
    TWO
} state;

state Flag(const char* f, char* res) {
    if (f == NULL || res == NULL) return ERROR_NULL;
    if (f[2] != '\0' || (f[0] != '/' && f[0] != '-')) return ERROR_INPUT;
    if (f[1] != 'q' && f[1] != 'm' && f[1] != 't') return ERROR_INPUT;
    *res = f[1];
    return OK;
}

state NumberLL(const char* n, long long* res) {
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

    unsigned long long x = 0;
    unsigned long long limit = (unsigned long long)LLONG_MAX;

    while (n[i] != '\0') {
        if (n[i] < '0' || n[i] > '9') return ERROR_INPUT;

        unsigned int digit = n[i] - '0';

        if (sign > 0) {
            if (x > (limit - digit) / 10) return ERROR_OVERFLOW;
        } else {
            unsigned long long abs_min = (unsigned long long)LLONG_MAX + 1;
            if (x > (abs_min - digit) / 10) return ERROR_OVERFLOW;
        }

        x = x * 10 + digit;
        ++i;
    }

    if (sign > 0) {
        *res = (long long)x;
    } else {
        if (x == (unsigned long long)LLONG_MAX + 1) {
            *res = LLONG_MIN;
        } else {
            *res = -(long long)x;
        }
    }

    return OK;
}

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

    return OK;
}

state SolveQuadratic(const double eps, double a, double b, double c, double* x1, double* x2) {
    if (x1 == NULL || x2 == NULL) return ERROR_NULL;

    double scale = fabs(a) + fabs(b) + fabs(c) + 1.0;
    double a_threshold = eps * scale;
    double b_threshold = eps * scale;
    double c_threshold = eps * scale;

    if (fabs(a) < a_threshold) {
        if (fabs(b) < b_threshold) {
            if (fabs(c) < c_threshold) {
                return INF;
            } else {
                return NO;
            }
        }
        *x1 = -c / b;
        return ONE;
    }

    double D = b * b - 4.0 * a * c;
    double D_threshold = eps * scale * scale;

    if (fabs(D) < D_threshold) {
        *x1 = -b / (2.0 * a);
        return ONE;
    } else if (D > D_threshold) {
        *x1 = (-b - sqrt(D)) / (2.0 * a);
        *x2 = (-b + sqrt(D)) / (2.0 * a);
        return TWO;
    } else {
        return NO;
    }
}

void PrintQuadraticResult(const state s, const double a, const double b, const double c,
                         const double x1, const double x2) {
    printf("    a = %.10g, b = %.10g, c = %.10g: ", a, b, c);

    switch (s) {
        case NO:
            printf("Корней нет\n");
            break;
        case INF:
            printf("Бесконечно много решений\n");
            break;
        case ONE:
            printf("x = %.10g\n", x1);
            break;
        case TWO:
            printf("x1 = %.10g, x2 = %.10g\n", x1, x2);
            break;
        default:
            printf("Ошибка\n");
            break;
    }
}

state Q(const int argc, char *argv[]) {
    if (argv == NULL) return ERROR_NULL;
    if (argc != 6) {
        fprintf(stderr, "Для флага -q требуется 5 параметров (epsilon и три коэффициента)\n");
        return ERROR_INPUT;
    }

    double eps, a, b, c;
    state st;

    st = NumberD(argv[2], &eps);
    if (st != OK) return st;

    if (eps <= 0.0 || eps >= 1.0) return ERROR_RANGE;

    st = NumberD(argv[3], &a);
    if (st != OK) return st;

    st = NumberD(argv[4], &b);
    if (st != OK) return st;

    st = NumberD(argv[5], &c);
    if (st != OK) return st;

    double x1 = 0.0;
    double x2 = 0.0;

    printf("Решения уравнения при всех уникальных перестановках коэффициентов:\n\n");

    st = SolveQuadratic(eps, a, b, c, &x1, &x2);
    PrintQuadraticResult(st, a, b, c, x1, x2);

    if (fabs(b - c) >= eps) {
        st = SolveQuadratic(eps, a, c, b, &x1, &x2);
        PrintQuadraticResult(st, a, c, b, x1, x2);
    }

    if (fabs(a - b) >= eps) {
        st = SolveQuadratic(eps, b, a, c, &x1, &x2);
        PrintQuadraticResult(st, b, a, c, x1, x2);
    }

    if (fabs(a - b) >= eps && fabs(a - c) >= eps) {
        st = SolveQuadratic(eps, b, c, a, &x1, &x2);
        PrintQuadraticResult(st, b, c, a, x1, x2);
    }

    if (fabs(a - c) >= eps && fabs(b - c) >= eps) {
        st = SolveQuadratic(eps, c, a, b, &x1, &x2);
        PrintQuadraticResult(st, c, a, b, x1, x2);
    }

    if (fabs(a - b) >= eps && fabs(a - c) >= eps && fabs(b - c) >= eps) {
        st = SolveQuadratic(eps, c, b, a, &x1, &x2);
        PrintQuadraticResult(st, c, b, a, x1, x2);
    }

    return OK;
}

state M(const int argc, char *argv[], long long* res) {
    if (argv == NULL || res == NULL) return ERROR_NULL;
    if (argc != 4) {
        fprintf(stderr, "Для флага -m требуется 2 целых числа\n");
        return ERROR_INPUT;
    }

    long long x1, x2;
    state st;

    *res = 0;

    st = NumberLL(argv[2], &x1);
    if (st != OK) return st;

    st = NumberLL(argv[3], &x2);
    if (st != OK) return st;

    if (x1 == 0 || x2 == 0) {
        fprintf(stderr, "Ошибка: оба числа должны быть ненулевыми\n");
        return ERROR_RANGE;
    }

    if (x1 == LLONG_MIN && x2 == -1) {
        *res = 1;
    } else if (x1 % x2 == 0) {
        *res = 1;
    }

    return OK;
}

state T(const int argc, char *argv[], long long *res, double *out_a, double *out_b, double *out_c) {
    if (argv == NULL || res == NULL || out_a == NULL || out_b == NULL || out_c == NULL) {
        return ERROR_NULL;
    }
    if (argc != 6) {
        fprintf(stderr, "Для флага -t требуется 4 параметра (epsilon и три стороны)\n");
        return ERROR_INPUT;
    }

    double eps, a, b, c;
    state st;

    *res = 0;

    st = NumberD(argv[2], &eps);
    if (st != OK) return st;

    if (eps <= 0.0 || eps >= 1.0) return ERROR_RANGE;

    st = NumberD(argv[3], &a);
    if (st != OK) return st;

    st = NumberD(argv[4], &b);
    if (st != OK) return st;

    st = NumberD(argv[5], &c);
    if (st != OK) return st;

    *out_a = a;
    *out_b = b;
    *out_c = c;

    if (a <= 0.0 || b <= 0.0 || c <= 0.0) {
        fprintf(stderr, "Ошибка: длины сторон должны быть положительными\n");
        return ERROR_RANGE;
    }

    double scale = a * a + b * b + c * c;
    double threshold = eps * scale;

    if (fabs(a * a + b * b - c * c) < threshold ||
        fabs(c * c + b * b - a * a) < threshold ||
        fabs(a * a + c * c - b * b) < threshold) {
        *res = 1;
    }

    return OK;
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Использование:\n");
        fprintf(stderr, "  %s -q <epsilon> <a> <b> <c>\n", argv[0]);
        fprintf(stderr, "  %s -m <x1> <x2>\n", argv[0]);
        fprintf(stderr, "  %s -t <epsilon> <a> <b> <c>\n", argv[0]);
        return EXIT_FAILURE;
    }

    char flag = '\0';
    state st = Flag(argv[1], &flag);

    if (st != OK) {
        fprintf(stderr, "Ошибка: некорректный флаг '%s'\n", argv[1]);
        fprintf(stderr, "Допустимые флаги: -q, -m, -t\n");
        return EXIT_FAILURE;
    }

    long long res = 0;

    if (flag == 'q') {
        st = Q(argc, argv);
        if (st != OK) {
            switch (st) {
                case ERROR_NULL:
                    fprintf(stderr, "Ошибка: NULL-указатель\n");
                    break;
                case ERROR_INPUT:
                    fprintf(stderr, "Ошибка: некорректный формат входных данных\n");
                    break;
                case ERROR_OVERFLOW:
                    fprintf(stderr, "Ошибка: переполнение при парсинге числа\n");
                    break;
                case ERROR_RANGE:
                    fprintf(stderr, "Ошибка: epsilon должен быть в диапазоне (0, 1)\n");
                    break;
                default:
                    fprintf(stderr, "Неизвестная ошибка\n");
                    break;
            }
            return EXIT_FAILURE;
        }
    } else if (flag == 'm') {
        st = M(argc, argv, &res);
        if (st != OK) {
            switch (st) {
                case ERROR_NULL:
                    fprintf(stderr, "Ошибка: NULL-указатель\n");
                    break;
                case ERROR_INPUT:
                    fprintf(stderr, "Ошибка: некорректный формат входных данных\n");
                    break;
                case ERROR_OVERFLOW:
                    fprintf(stderr, "Ошибка: переполнение при парсинге числа\n");
                    break;
                case ERROR_RANGE:
                    fprintf(stderr, "Ошибка: оба числа должны быть ненулевыми\n");
                    break;
                default:
                    fprintf(stderr, "Неизвестная ошибка\n");
                    break;
            }
            return EXIT_FAILURE;
        }

        printf("Число %s ", argv[2]);
        if (res) {
            printf("кратно числу %s\n", argv[3]);
        } else {
            printf("не кратно числу %s\n", argv[3]);
        }
    } else if (flag == 't') {
        double out_a = 0.0, out_b = 0.0, out_c = 0.0;
        st = T(argc, argv, &res, &out_a, &out_b, &out_c);
        if (st != OK) {
            switch (st) {
                case ERROR_NULL:
                    fprintf(stderr, "Ошибка: NULL-указатель\n");
                    break;
                case ERROR_INPUT:
                    fprintf(stderr, "Ошибка: некорректный формат входных данных\n");
                    break;
                case ERROR_OVERFLOW:
                    fprintf(stderr, "Ошибка: переполнение при парсинге числа\n");
                    break;
                case ERROR_RANGE:
                    fprintf(stderr, "Ошибка: epsilon должен быть в диапазоне (0, 1) или стороны не положительны\n");
                    break;
                default:
                    fprintf(stderr, "Неизвестная ошибка\n");
                    break;
            }
            return EXIT_FAILURE;
        }

        if (res) {
            printf("Стороны (%.10g, %.10g, %.10g) могут быть сторонами прямоугольного треугольника\n",
                   out_a, out_b, out_c);
        } else {
            printf("Стороны (%.10g, %.10g, %.10g) не могут быть сторонами прямоугольного треугольника\n",
                   out_a, out_b, out_c);
        }
    }

    return EXIT_SUCCESS;
}