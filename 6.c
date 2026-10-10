#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <math.h>
#include <limits.h>
#include <errno.h>

typedef enum state {
    OK,
    ERROR_NULL,
    ERROR_RANGE,
    ERROR_INPUT,
    ERROR_OVERFLOW,
    ERROR_CONVERGENCE
} state;

state NumberInt(int* n) {
    if (n == NULL) return ERROR_NULL;
    if (scanf("%d", n) != 1) {
        int c;
        while ((c = getchar()) != '\n' && c != EOF);
        return ERROR_INPUT;
    }
    return OK;
}

state NumberD(double *n) {
    if (n == NULL) return ERROR_NULL;
    if (scanf("%lf", n) != 1) {
        int c;
        while ((c = getchar()) != '\n' && c != EOF);
        return ERROR_INPUT;
    }
    return OK;
}

state NumberLL(long long* n) {
    if (n == NULL) return ERROR_NULL;
    if (scanf("%lld", n) != 1) {
        int c;
        while ((c = getchar()) != '\n' && c != EOF);
        return ERROR_INPUT;
    }
    return OK;
}

state IsConvex(double eps, int n, int* res, ...) {
    if (res == NULL) return ERROR_NULL;
    if (eps <= 0.0) return ERROR_RANGE;
    if (n < 3 || n > 100) return ERROR_RANGE;

    va_list param;
    va_start(param, res);

    double* X = (double*)malloc(n * sizeof(double));
    double* Y = (double*)malloc(n * sizeof(double));

    if (X == NULL || Y == NULL) {
        free(X);
        free(Y);
        va_end(param);
        return ERROR_NULL;
    }

    for (int i = 0; i < n; ++i) {
        X[i] = va_arg(param, double);
        Y[i] = va_arg(param, double);
    }
    va_end(param);

    int sign = 0;
    *res = 1;

    for (int i = 0; i < n; ++i) {
        int i1 = (i + 1) % n;
        int i2 = (i + 2) % n;

        double ABx = X[i1] - X[i];
        double ABy = Y[i1] - Y[i];
        double BCx = X[i2] - X[i1];
        double BCy = Y[i2] - Y[i1];

        double cross = ABx * BCy - ABy * BCx;
        double scale = fabs(ABx * BCy) + fabs(ABy * BCx) + 1.0;
        double threshold = eps * scale;

        if (cross > threshold) {
            if (sign == 0) {
                sign = 1;
            } else if (sign == -1) {
                *res = 0;
                free(X);
                free(Y);
                return OK;
            }
        } else if (cross < -threshold) {
            if (sign == 0) {
                sign = -1;
            } else if (sign == 1) {
                *res = 0;
                free(X);
                free(Y);
                return OK;
            }
        }
    }

    free(X);
    free(Y);
    return OK;
}

state PolyValue(double x, int n, double* res, ...) {
    if (res == NULL) return ERROR_NULL;
    if (n < 0) return ERROR_RANGE;

    va_list param;
    va_start(param, res);

    *res = 0.0;
    for (int i = n; i >= 0; --i) {
        double coef = va_arg(param, double);
        *res = *res * x + coef;
    }

    va_end(param);
    return OK;
}

static void ToBase(long long value, int base, char* buf, size_t buf_size) {
    static const char digits[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    char tmp[128];
    int len = 0;

    if (buf_size == 0) return;

    if (value == 0) {
        if (buf_size >= 2) {
            buf[0] = '0';
            buf[1] = '\0';
        } else {
            buf[0] = '\0';
        }
        return;
    }

    while (value > 0 && len < (int)sizeof(tmp)) {
        tmp[len++] = digits[value % base];
        value /= base;
    }

    int out = 0;
    for (int i = len - 1; i >= 0 && (size_t)out + 1 < buf_size; --i) {
        buf[out++] = tmp[i];
    }
    buf[out] = '\0';
}

state FindKaprekar(int base, int count, int* found_count, char** results, ...) {
    if (results == NULL || found_count == NULL) return ERROR_NULL;
    if (base < 2 || base > 36) return ERROR_RANGE;
    if (count <= 0) return ERROR_RANGE;

    va_list param;
    va_start(param, results);

    *found_count = 0;

    for (int i = 0; i < count; ++i) {
        char* num_str = va_arg(param, char*);
        if (num_str == NULL) continue;

        errno = 0;
        char* end = NULL;
        long long num = strtoll(num_str, &end, base);
        if (errno == ERANGE) continue;
        if (end == num_str || *end != '\0') continue;
        if (num < 0) continue;

        if (num > 0 && num > LLONG_MAX / num) continue;
        long long squared = num * num;

        char sq_base[256];
        ToBase(squared, base, sq_base, sizeof(sq_base));

        size_t len = strlen(sq_base);

        for (size_t split = 1; split < len; ++split) {
            char left[256] = {0};
            char right[256] = {0};

            memcpy(left, sq_base, split);
            left[split] = '\0';
            memcpy(right, sq_base + split, len - split);
            right[len - split] = '\0';

            errno = 0;
            long long left_val = (left[0] == '\0') ? 0 : strtoll(left, NULL, base);
            if (errno == ERANGE) continue;

            errno = 0;
            long long right_val = (right[0] == '\0') ? 0 : strtoll(right, NULL, base);
            if (errno == ERANGE) continue;

            if (left_val + right_val == num) {
                results[*found_count] = num_str;
                (*found_count)++;
                break;
            }
        }
    }

    va_end(param);
    return OK;
}

state GeoMean(double* res, int count, ...) {
    if (res == NULL) return ERROR_NULL;
    if (count <= 0) return ERROR_RANGE;

    va_list param;
    va_start(param, count);

    double log_sum = 0.0;
    int has_zero = 0;

    for (int i = 0; i < count; ++i) {
        double val = va_arg(param, double);
        if (val < 0.0) {
            va_end(param);
            return ERROR_RANGE;
        }
        if (val == 0.0) {
            has_zero = 1;
        } else {
            log_sum += log(val);
        }
    }

    va_end(param);

    if (has_zero) {
        *res = 0.0;
    } else {
        *res = exp(log_sum / (double)count);
    }
    return OK;
}

static state FastPowU(double x, unsigned long long n, double* res) {
    if (res == NULL) return ERROR_NULL;

    if (n == 0) {
        *res = 1.0;
        return OK;
    }

    double half = 0.0;
    state st = FastPowU(x, n / 2ULL, &half);
    if (st != OK) return st;

    if (n % 2ULL == 0) {
        *res = half * half;
    } else {
        *res = half * half * x;
    }
    return OK;
}

state FastPow(double x, long long n, double* res) {
    if (res == NULL) return ERROR_NULL;

    if (n == 0) {
        *res = 1.0;
        return OK;
    }

    if (n < 0) {
        if (x == 0.0) return ERROR_RANGE;
        unsigned long long un = (unsigned long long)(-(n + 1)) + 1ULL;
        double temp = 0.0;
        state st = FastPowU(x, un, &temp);
        if (st != OK) return st;
        *res = 1.0 / temp;
        return OK;
    }

    return FastPowU(x, (unsigned long long)n, res);
}

typedef double (*FunctionPtr)(double);

state Bisection(double a, double b, double eps, FunctionPtr func, double* root) {
    if (root == NULL || func == NULL) return ERROR_NULL;
    if (eps <= 0.0) return ERROR_RANGE;
    if (a >= b) return ERROR_RANGE;

    double fa = func(a);
    double fb = func(b);

    if (!isfinite(fa) || !isfinite(fb)) return ERROR_RANGE;

    if ((fa > 0.0 && fb > 0.0) || (fa < 0.0 && fb < 0.0)) {
        return ERROR_RANGE;
    }

    if (fa == 0.0) {
        *root = a;
        return OK;
    }
    if (fb == 0.0) {
        *root = b;
        return OK;
    }

    double c = 0.0;
    int max_iter = 1000000;
    int iter = 0;

    while ((b - a) > eps && iter < max_iter) {
        c = (a + b) / 2.0;
        double fc = func(c);

        if (!isfinite(fc)) return ERROR_RANGE;

        if (fabs(fc) < eps) {
            *root = c;
            return OK;
        }

        if ((fa > 0.0 && fc < 0.0) || (fa < 0.0 && fc > 0.0)) {
            b = c;
            fb = fc;
        } else {
            a = c;
            fa = fc;
        }

        iter++;
    }

    if (iter >= max_iter) {
        return ERROR_CONVERGENCE;
    }

    *root = (a + b) / 2.0;
    return OK;
}

void DemoIsConvex(void) {
    printf("\nПроверка выпуклости многоугольника\n");

    int n;
    printf("Введите количество вершин (3-10): ");
    if (NumberInt(&n) != OK) {
        fprintf(stderr, "Ошибка: некорректный ввод\n");
        return;
    }

    if (n < 3 || n > 10) {
        fprintf(stderr, "Ошибка: количество вершин должно быть от 3 до 10\n");
        return;
    }

    double eps;
    printf("Введите точность epsilon: ");
    if (NumberD(&eps) != OK || eps <= 0.0) {
        fprintf(stderr, "Ошибка: некорректная точность\n");
        return;
    }

    printf("Введите координаты %d вершин (x y):\n", n);

    double coords[20];
    for (int i = 0; i < n; ++i) {
        printf("Вершина %d: ", i + 1);
        if (NumberD(&coords[2 * i]) != OK || NumberD(&coords[2 * i + 1]) != OK) {
            fprintf(stderr, "Ошибка: некорректный ввод\n");
            return;
        }
    }

    int res;
    state st;

    switch (n) {
        case 3:
            st = IsConvex(eps, 3, &res, coords[0], coords[1], coords[2], coords[3], coords[4], coords[5]);
            break;
        case 4:
            st = IsConvex(eps, 4, &res, coords[0], coords[1], coords[2], coords[3], coords[4], coords[5], coords[6], coords[7]);
            break;
        case 5:
            st = IsConvex(eps, 5, &res, coords[0], coords[1], coords[2], coords[3], coords[4], coords[5], coords[6], coords[7], coords[8], coords[9]);
            break;
        case 6:
            st = IsConvex(eps, 6, &res, coords[0], coords[1], coords[2], coords[3], coords[4], coords[5], coords[6], coords[7], coords[8], coords[9], coords[10], coords[11]);
            break;
        case 7:
            st = IsConvex(eps, 7, &res, coords[0], coords[1], coords[2], coords[3], coords[4], coords[5], coords[6], coords[7], coords[8], coords[9], coords[10], coords[11], coords[12], coords[13]);
            break;
        case 8:
            st = IsConvex(eps, 8, &res, coords[0], coords[1], coords[2], coords[3], coords[4], coords[5], coords[6], coords[7], coords[8], coords[9], coords[10], coords[11], coords[12], coords[13], coords[14], coords[15]);
            break;
        case 9:
            st = IsConvex(eps, 9, &res, coords[0], coords[1], coords[2], coords[3], coords[4], coords[5], coords[6], coords[7], coords[8], coords[9], coords[10], coords[11], coords[12], coords[13], coords[14], coords[15], coords[16], coords[17]);
            break;
        case 10:
            st = IsConvex(eps, 10, &res, coords[0], coords[1], coords[2], coords[3], coords[4], coords[5], coords[6], coords[7], coords[8], coords[9], coords[10], coords[11], coords[12], coords[13], coords[14], coords[15], coords[16], coords[17], coords[18], coords[19]);
            break;
        default:
            st = ERROR_RANGE;
    }

    if (st == OK) {
        printf("Результат: многоугольник %s\n", res ? "выпуклый" : "не выпуклый");
    } else {
        fprintf(stderr, "Ошибка при вычислении\n");
    }
}

void DemoPolyValue(void) {
    printf("\nВычисление значения многочлена\n");

    double x;
    printf("Введите точку x: ");
    if (NumberD(&x) != OK) {
        fprintf(stderr, "Ошибка: некорректный ввод\n");
        return;
    }

    int n;
    printf("Введите степень многочлена (0-5): ");
    if (NumberInt(&n) != OK) {
        fprintf(stderr, "Ошибка: некорректный ввод\n");
        return;
    }

    if (n < 0 || n > 5) {
        fprintf(stderr, "Ошибка: степень должна быть от 0 до 5\n");
        return;
    }

    double coef[6] = {0};
    printf("Введите %d коэффициентов от старшей степени к младшей:\n", n + 1);
    for (int i = 0; i <= n; ++i) {
        printf("Коэффициент %d: ", i);
        if (NumberD(&coef[i]) != OK) {
            fprintf(stderr, "Ошибка: некорректный ввод\n");
            return;
        }
    }

    double res = 0.0;
    state st;

    switch (n) {
        case 0: st = PolyValue(x, 0, &res, coef[0]); break;
        case 1: st = PolyValue(x, 1, &res, coef[0], coef[1]); break;
        case 2: st = PolyValue(x, 2, &res, coef[0], coef[1], coef[2]); break;
        case 3: st = PolyValue(x, 3, &res, coef[0], coef[1], coef[2], coef[3]); break;
        case 4: st = PolyValue(x, 4, &res, coef[0], coef[1], coef[2], coef[3], coef[4]); break;
        case 5: st = PolyValue(x, 5, &res, coef[0], coef[1], coef[2], coef[3], coef[4], coef[5]); break;
        default: st = ERROR_RANGE;
    }

    if (st == OK) {
        printf("Значение многочлена P(%.10g) = %.15f\n", x, res);
    } else {
        fprintf(stderr, "Ошибка при вычислении\n");
    }
}

void DemoFindKaprekar(void) {
    printf("\nЧисла Капрекара\n");

    int base;
    printf("Введите основание системы счисления (2-36): ");
    if (NumberInt(&base) != OK || base < 2 || base > 36) {
        fprintf(stderr, "Ошибка: некорректное основание\n");
        return;
    }

    int count;
    printf("Введите количество чисел (1-10): ");
    if (NumberInt(&count) != OK || count <= 0 || count > 10) {
        fprintf(stderr, "Ошибка: некорректное количество\n");
        return;
    }

    char buffers[10][64];
    char* ptrs[10];

    printf("Введите %d чисел в системе с основанием %d:\n", count, base);
    for (int i = 0; i < count; ++i) {
        printf("Число %d: ", i + 1);
        if (scanf("%63s", buffers[i]) != 1) {
            fprintf(stderr, "Ошибка: некорректный ввод\n");
            int c;
            while ((c = getchar()) != '\n' && c != EOF);
            return;
        }
        ptrs[i] = buffers[i];
    }

    char* results[10];
    int found = 0;
    state st;

    switch (count) {
        case 1: st = FindKaprekar(base, 1, &found, results, ptrs[0]); break;
        case 2: st = FindKaprekar(base, 2, &found, results, ptrs[0], ptrs[1]); break;
        case 3: st = FindKaprekar(base, 3, &found, results, ptrs[0], ptrs[1], ptrs[2]); break;
        case 4: st = FindKaprekar(base, 4, &found, results, ptrs[0], ptrs[1], ptrs[2], ptrs[3]); break;
        case 5: st = FindKaprekar(base, 5, &found, results, ptrs[0], ptrs[1], ptrs[2], ptrs[3], ptrs[4]); break;
        case 6: st = FindKaprekar(base, 6, &found, results, ptrs[0], ptrs[1], ptrs[2], ptrs[3], ptrs[4], ptrs[5]); break;
        case 7: st = FindKaprekar(base, 7, &found, results, ptrs[0], ptrs[1], ptrs[2], ptrs[3], ptrs[4], ptrs[5], ptrs[6]); break;
        case 8: st = FindKaprekar(base, 8, &found, results, ptrs[0], ptrs[1], ptrs[2], ptrs[3], ptrs[4], ptrs[5], ptrs[6], ptrs[7]); break;
        case 9: st = FindKaprekar(base, 9, &found, results, ptrs[0], ptrs[1], ptrs[2], ptrs[3], ptrs[4], ptrs[5], ptrs[6], ptrs[7], ptrs[8]); break;
        case 10: st = FindKaprekar(base, 10, &found, results, ptrs[0], ptrs[1], ptrs[2], ptrs[3], ptrs[4], ptrs[5], ptrs[6], ptrs[7], ptrs[8], ptrs[9]); break;
        default: st = ERROR_RANGE;
    }

    if (st != OK) {
        fprintf(stderr, "Ошибка при вычислении\n");
        return;
    }

    if (found == 0) {
        printf("Чисел Капрекара не найдено\n");
    } else {
        printf("Найдено чисел Капрекара: %d\n", found);
        for (int i = 0; i < found; ++i) {
            printf("  %s\n", results[i]);
        }
    }
}

void DemoGeoMean(void) {
    printf("\nСреднее геометрическое\n");

    int n;
    printf("Введите количество чисел (1-10): ");
    if (NumberInt(&n) != OK) {
        fprintf(stderr, "Ошибка: некорректный ввод\n");
        return;
    }

    if (n <= 0 || n > 10) {
        fprintf(stderr, "Ошибка: количество чисел должно быть от 1 до 10\n");
        return;
    }

    double vals[10] = {0};
    printf("Введите %d чисел:\n", n);
    for (int i = 0; i < n; ++i) {
        printf("Число %d: ", i + 1);
        if (NumberD(&vals[i]) != OK) {
            fprintf(stderr, "Ошибка: некорректный ввод\n");
            return;
        }
        if (vals[i] < 0.0) {
            fprintf(stderr, "Ошибка: все числа должны быть неотрицательными\n");
            return;
        }
    }

    double res = 0.0;
    state st;

    switch (n) {
        case 1: st = GeoMean(&res, 1, vals[0]); break;
        case 2: st = GeoMean(&res, 2, vals[0], vals[1]); break;
        case 3: st = GeoMean(&res, 3, vals[0], vals[1], vals[2]); break;
        case 4: st = GeoMean(&res, 4, vals[0], vals[1], vals[2], vals[3]); break;
        case 5: st = GeoMean(&res, 5, vals[0], vals[1], vals[2], vals[3], vals[4]); break;
        case 6: st = GeoMean(&res, 6, vals[0], vals[1], vals[2], vals[3], vals[4], vals[5]); break;
        case 7: st = GeoMean(&res, 7, vals[0], vals[1], vals[2], vals[3], vals[4], vals[5], vals[6]); break;
        case 8: st = GeoMean(&res, 8, vals[0], vals[1], vals[2], vals[3], vals[4], vals[5], vals[6], vals[7]); break;
        case 9: st = GeoMean(&res, 9, vals[0], vals[1], vals[2], vals[3], vals[4], vals[5], vals[6], vals[7], vals[8]); break;
        case 10: st = GeoMean(&res, 10, vals[0], vals[1], vals[2], vals[3], vals[4], vals[5], vals[6], vals[7], vals[8], vals[9]); break;
        default: st = ERROR_RANGE;
    }

    if (st == OK) {
        printf("Среднее геометрическое: %.15f\n", res);
    } else {
        fprintf(stderr, "Ошибка при вычислении\n");
    }
}

void DemoFastPow(void) {
    printf("\nБыстрое возведение в степень\n");

    double x;
    long long n;

    printf("Введите основание x: ");
    if (NumberD(&x) != OK) {
        fprintf(stderr, "Ошибка: некорректный ввод\n");
        return;
    }

    printf("Введите показатель степени n: ");
    if (NumberLL(&n) != OK) {
        fprintf(stderr, "Ошибка: некорректный ввод\n");
        return;
    }

    double res;
    state st = FastPow(x, n, &res);

    if (st == OK) {
        printf("%.15g ^ %lld = %.15f\n", x, n, res);
        printf("Проверка (стандартная функция pow): %.15f\n", pow(x, (double)n));
    } else if (st == ERROR_RANGE) {
        fprintf(stderr, "Ошибка: деление на нуль или некорректные параметры\n");
    } else {
        fprintf(stderr, "Ошибка при вычислении\n");
    }
}

double func1(double x) { return x * x - 2.0; }
double func2(double x) { return x * x * x - 1.0; }
double func3(double x) { return sin(x) - 0.5; }
double func4(double x) { return exp(x) - 3.0; }

void DemoBisection(void) {
    printf("\nМетод дихотомии\n");

    int choice;
    printf("Выберите уравнение:\n");
    printf("1. x^2 - 2 = 0 (корень sqrt(2))\n");
    printf("2. x^3 - 1 = 0 (корень 1)\n");
    printf("3. sin(x) - 0.5 = 0\n");
    printf("4. e^x - 3 = 0\n");
    printf("Выбор: ");

    if (NumberInt(&choice) != OK) {
        fprintf(stderr, "Ошибка: некорректный ввод\n");
        return;
    }

    FunctionPtr func = NULL;
    double a, b;

    switch (choice) {
        case 1:
            func = func1;
            a = 1.0; b = 2.0;
            printf("Уравнение: x^2 - 2 = 0, диапазон [%.1f, %.1f]\n", a, b);
            break;
        case 2:
            func = func2;
            a = 0.5; b = 1.5;
            printf("Уравнение: x^3 - 1 = 0, диапазон [%.1f, %.1f]\n", a, b);
            break;
        case 3:
            func = func3;
            a = 0.0; b = 1.57;
            printf("Уравнение: sin(x) - 0.5 = 0, диапазон [%.1f, %.1f]\n", a, b);
            break;
        case 4:
            func = func4;
            a = 0.0; b = 2.0;
            printf("Уравнение: e^x - 3 = 0, диапазон [%.1f, %.1f]\n", a, b);
            break;
        default:
            fprintf(stderr, "Ошибка: некорректный выбор\n");
            return;
    }

    double eps;
    printf("Введите точность epsilon: ");
    if (NumberD(&eps) != OK) {
        fprintf(stderr, "Ошибка: некорректный ввод\n");
        return;
    }

    if (eps <= 0.0) {
        fprintf(stderr, "Ошибка: точность должна быть положительной\n");
        return;
    }

    double root;
    state st = Bisection(a, b, eps, func, &root);

    if (st == OK) {
        printf("Корень: x = %.15f\n", root);
        printf("Проверка f(x) = %.2e\n", func(root));
    } else if (st == ERROR_CONVERGENCE) {
        fprintf(stderr, "Ошибка: не удалось достичь точности за отведённое число итераций\n");
    } else if (st == ERROR_RANGE) {
        fprintf(stderr, "Ошибка: функция не меняет знак на диапазоне [%f, %f]\n", a, b);
    } else {
        fprintf(stderr, "Ошибка при вычислении\n");
    }
}

int main(void) {
    int choice;

    while (1) {
        printf("     Выберите функцию для демонстрации    \n");
        printf(" 1. Проверка выпуклости многоугольника    \n");
        printf(" 2. Значение многочлена в точке           \n");
        printf(" 3. Числа Капрекара                       \n");
        printf(" 4. Среднее геометрическое                \n");
        printf(" 5. Быстрое возведение в степень          \n");
        printf(" 6. Метод дихотомии                       \n");
        printf(" 0. Выход                                 \n");
        printf("Выбор: ");

        if (NumberInt(&choice) != OK) {
            fprintf(stderr, "Ошибка: некорректный ввод\n");
            continue;
        }

        switch (choice) {
            case 1: DemoIsConvex(); break;
            case 2: DemoPolyValue(); break;
            case 3: DemoFindKaprekar(); break;
            case 4: DemoGeoMean(); break;
            case 5: DemoFastPow(); break;
            case 6: DemoBisection(); break;
            case 0:
                printf("Конец\n");
                return EXIT_SUCCESS;
            default:
                fprintf(stderr, "Ошибка: неверный номер функции\n");
        }
    }

    return EXIT_SUCCESS;
}