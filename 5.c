#include <math.h>
#include <stdio.h>
#include <stdlib.h>

typedef enum state {
    OK,
    ERROR_NULL,
    ERROR_RANGE,
    ERROR_INPUT
} state;

state NumberD(const char *n, double *res) {
    if (n == NULL || res == NULL) return ERROR_NULL;
    
    long long sign = 1;
    long long i = 0;
    
    if (n[0] == '-') {
        i = 1;
        sign = -1;
    } else if (n[0] == '+') {
        i = 1;
    }
    
    if (n[i] == '\0') {
        return ERROR_INPUT;
    }
    
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
            if (point) {
                return ERROR_INPUT;
            }
            point = 1;
        } else {
            return ERROR_INPUT;
        }
        ++i;
    }
    
    if (!digit) return ERROR_INPUT;
    *res = (x + f) * sign;
    return OK;
}

state SeriesA1(const double eps, const double x, double *res) {
    if (res == NULL) return ERROR_NULL;
    
    double sum = 1.0;
    double cur = 1.0;
    long long n = 1;
    
    for (; n <= 1000000; ++n) {
        cur = cur * x / (double)n;
        sum += cur;
        if (fabs(cur) < eps) {
            break;
        }
    }
    
    *res = sum;
    return OK;
}

state SeriesB1(const double eps, const double x, double *res) {
    if (res == NULL) return ERROR_NULL;
    
    double sum = 1.0;
    double cur = 1.0;
    long long n = 1;
    
    for (; n <= 1000000; ++n) {
        cur = cur * (-1.0) * x * x / ((2.0 * n - 1.0) * (2.0 * n));
        sum += cur;
        if (fabs(cur) < eps) {
            break;
        }
    }
    
    *res = sum;
    return OK;
}

state SeriesC1(const double eps, const double x, double *res) {
    if (res == NULL) return ERROR_NULL;
    if (fabs(x) >= 1.0) {
        return ERROR_RANGE;
    }
    
    double sum = 1.0;
    double cur = 1.0;
    long long n = 1;
    
    for (; n <= 1000000; ++n) {
        cur = cur * 9.0 * (double)n * (double)n * x * x / 
              ((3.0 * n - 2.0) * (3.0 * n - 1.0));
        sum += cur;
        if (fabs(cur) < eps) {
            break;
        }
    }
    
    *res = sum;
    return OK;
}

state SeriesD1(const double eps, const double x, double *res) {
    if (res == NULL) return ERROR_NULL;
    if (fabs(x) >= 1.0) {
        return ERROR_RANGE;
    }
    
    double sum = 0.0;
    double cur = 1.0;
    long long n = 1;
    
    for (; n <= 1000000; ++n) {
        cur = cur * (-1.0) * x * x * (2.0 * n - 1.0) / (2.0 * n);
        sum += cur;
        if (fabs(cur) < eps) {
            break;
        }
    }
    
    *res = sum;
    return OK;
}

state IntegralA2(const double eps, double *res) {
    if (res == NULL) return ERROR_NULL;
    
    double sum = 0.0;
    long long n = 1;
    
    for (; n <= 1000000; ++n) {
        double cur = pow(-1.0, (double)(n + 1)) / ((double)n * (double)n);
        sum += cur;
        if (fabs(cur) < eps) {
            break;
        }
    }
    
    *res = sum;
    return OK;
}

state IntegralB2(const double eps, double *res) {
    if (res == NULL) return ERROR_NULL;
    
    double sum = 0.0;
    double cur = 1.0;
    long long n = 0;
    
    for (; n <= 1000000; ++n) {
        sum += cur;
        if (fabs(cur) < eps) {
            break;
        }
        cur = cur * (-1.0) * (2.0 * n + 1.0) / 
              (2.0 * (double)(n + 1) * (2.0 * n + 3.0));
    }
    
    *res = sum;
    return OK;
}

state IntegralC2(const double eps, double *res) {
    if (res == NULL) return ERROR_NULL;
    
    double sum = 0.0;
    double cur = 0.5;
    long long n = 1;
    
    for (; n <= 1000000; ++n) {
        sum += cur;
        if (fabs(cur) < eps) {
            break;
        }
        cur = cur * (double)n / (double)(n + 2);
    }
    
    *res = sum;
    return OK;
}

state IntegralD2(const double eps, double *res) {
    if (res == NULL) return ERROR_NULL;
    
    double sum = 0.0;
    long long n = 1;
    
    for (; n <= 1000000; ++n) {
        double cur = pow(-1.0, (double)(n + 1)) * pow((double)n, -(double)n);
        sum += cur;
        if (fabs(cur) < eps) {
            break;
        }
    }
    
    *res = sum;
    return OK;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Использование: %s <epsilon>\n", argv[0]);
        fprintf(stderr, "Пример: %s 0.00001\n", argv[0]);
        return EXIT_FAILURE;
    }
    
    double eps;
    state st = NumberD(argv[1], &eps);
    
    if (st != OK) {
        fprintf(stderr, "Ошибка: некорректное значение эпсилон\n");
        return EXIT_FAILURE;
    }
    
    if (eps <= 0.0 || eps >= 1.0) {
        fprintf(stderr, "Ошибка: эпсилон должен быть в диапазоне (0, 1)\n");
        return EXIT_FAILURE;
    }
    
    printf("Вычисление рядов и интегралов с точностью epsilon = %.10f\n\n", eps);
    printf("РЯДЫ\n\n");
    
    printf("1. Суммы рядов:\n\n");
    
    printf("a) sum(x^n / n!, n=0..inf) - e^x\n");
    printf("   Введите значение x: ");
    
    double x;
    if (scanf("%lf", &x) != 1) {
        fprintf(stderr, "Ошибка: не удалось прочитать значение x\n");
        return EXIT_FAILURE;
    }
    
    double res_a1 = 0.0;
    st = SeriesA1(eps, x, &res_a1);
    if (st == OK) {
        printf("   Результат: %.15f\n", res_a1);
        printf("   Проверка (e^x): %.15f\n\n", exp(x));
    } else {
        fprintf(stderr, "   Ошибка при вычислении\n\n");
    }
    
    printf("b) sum((-1)^n * x^(2n) / (2n)!, n=0..inf) - cos(x)\n");
    printf("   Результат: %.15f\n", cos(x));
    double res_b1 = 0.0;
    st = SeriesB1(eps, x, &res_b1);
    if (st == OK) {
        printf("   Вычисленное значение: %.15f\n\n", res_b1);
    } else {
        fprintf(stderr, "   Ошибка при вычислении\n\n");
    }
    
    printf("c) sum(3^(3n) * (n!)^3 * x^(2n) / (3n)!, n=0..inf)\n");
    printf("   (требует |x| < 1)\n");
    if (fabs(x) < 1.0) {
        double res_c1 = 0.0;
        st = SeriesC1(eps, x, &res_c1);
        if (st == OK) {
            printf("   Результат: %.15f\n\n", res_c1);
        } else {
            fprintf(stderr, "   Ошибка при вычислении\n\n");
        }
    } else {
        printf("   x вне диапазона |x| < 1\n\n");
    }
    
    printf("d) sum((-1)^n * (2n-1)!! * x^(2n) / (2n)!!, n=1..inf)\n");
    printf("   (требует |x| < 1)\n");
    if (fabs(x) < 1.0) {
        double res_d1 = 0.0;
        st = SeriesD1(eps, x, &res_d1);
        if (st == OK) {
            printf("   Результат: %.15f\n\n", res_d1);
        } else {
            fprintf(stderr, "   Ошибка при вычислении\n\n");
        }
    } else {
        printf("   x вне диапазона |x| < 1\n\n");
    }
    
    printf("ИНТЕГРАЛЫ\n\n");
    printf("2. Значения определённых интегралов:\n\n");
    
    printf("a) Integral from 0 to 1: ln(1+x)/x dx\n");
    double res_a2 = 0.0;
    st = IntegralA2(eps, &res_a2);
    if (st == OK) {
        printf("   Результат: %.15f\n\n", res_a2);
    } else {
        fprintf(stderr, "   Ошибка при вычислении\n\n");
    }
    
    printf("b) Integral from 0 to 1: e^(-x^2/2) dx\n");
    double res_b2 = 0.0;
    st = IntegralB2(eps, &res_b2);
    if (st == OK) {
        printf("   Результат: %.15f\n\n", res_b2);
    } else {
        fprintf(stderr, "   Ошибка при вычислении\n\n");
    }
    
    printf("c) Integral from 0 to 1: ln(1/(1-x)) dx\n");
    double res_c2 = 0.0;
    st = IntegralC2(eps, &res_c2);
    if (st == OK) {
        printf("   Результат: %.15f\n\n", res_c2);
    } else {
        fprintf(stderr, "   Ошибка при вычислении\n\n");
    }
    
    printf("d) Integral from 0 to 1: x^x dx\n");
    double res_d2 = 0.0;
    st = IntegralD2(eps, &res_d2);
    if (st == OK) {
        printf("   Результат: %.15f\n\n", res_d2);
    } else {
        fprintf(stderr, "   Ошибка при вычислении\n\n");
    }
        
    return EXIT_SUCCESS;
}