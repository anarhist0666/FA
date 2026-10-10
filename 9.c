#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <math.h>
#include <limits.h>

#define FIXED_ARRAY_SIZE 13

typedef enum state {
    OK,
    ERROR_NULL,
    ERROR_RANGE,
    ERROR_OVERFLOW,
    ERROR_INPUT
} state;

state Number(const char* n, int* res) {
    if (n == NULL || res == NULL) return ERROR_NULL;

    int sign = 1;
    size_t i = 0;

    if (n[0] == '-') {
        sign = -1;
        i = 1;
    } else if (n[0] == '+') {
        i = 1;
    }

    if (n[i] == '\0') return ERROR_INPUT;

    unsigned long long value = 0;
    unsigned long long limit = (sign > 0)
        ? (unsigned long long)INT_MAX
        : (unsigned long long)INT_MAX + 1ULL;

    while (n[i] != '\0') {
        if (n[i] < '0' || n[i] > '9') return ERROR_INPUT;

        unsigned int digit = (unsigned int)(n[i] - '0');

        if (value > (limit - digit) / 10ULL) {
            return ERROR_OVERFLOW;
        }

        value = value * 10ULL + digit;
        ++i;
    }

    if (sign > 0) {
        *res = (int)value;
    } else if (value == (unsigned long long)INT_MAX + 1ULL) {
        *res = INT_MIN;
    } else {
        *res = -(int)value;
    }

    return OK;
}

int GetRandomInRange(int a, int b) {
    if (a == b) return a;
    if (a > b) {
        int temp = a;
        a = b;
        b = temp;
    }

    unsigned long long range = (unsigned long long)((long long)b - (long long)a) + 1ULL;

    unsigned long long random_val = ((unsigned long long)rand() << 30) |
                                    ((unsigned long long)rand() << 15) |
                                    (unsigned long long)rand();

    return (int)((long long)a + (long long)(random_val % range));
}

state FillArray(int a, int b, int* array, int size) {
    if (array == NULL) return ERROR_NULL;
    if (size <= 0) return ERROR_RANGE;
    if (a > b) return ERROR_RANGE;

    for (int i = 0; i < size; ++i) {
        array[i] = GetRandomInRange(a, b);
    }

    return OK;
}

state SwapMinMax(int* array, int size, int* min_val, int* max_val, int* min_idx, int* max_idx) {
    if (array == NULL) return ERROR_NULL;
    if (size <= 0) return ERROR_RANGE;

    int index_max = 0;
    int index_min = 0;

    for (int i = 0; i < size; ++i) {
        if (array[i] < array[index_min]) {
            index_min = i;
        }
        if (array[i] > array[index_max]) {
            index_max = i;
        }
    }

    *min_val = array[index_min];
    *max_val = array[index_max];
    *min_idx = index_min;
    *max_idx = index_max;

    if (index_max != index_min) {
        int temp = array[index_min];
        array[index_min] = array[index_max];
        array[index_max] = temp;
    }

    return OK;
}

void PrintArray(const char* name, const int* array, int size) {
    if (array == NULL) return;

    printf("%s: ", name);
    for (int i = 0; i < size; ++i) {
        printf("%d", array[i]);
        if (i < size - 1) printf(" ");
    }
    printf("\n");
}

void Task1Demo(int a, int b) {
    printf("\nЗАДАНИЕ 1: SWAP MIN/MAX\n\n");

    printf("Параметры: a = %d, b = %d\n", a, b);
    printf("Размер массива: %d\n\n", FIXED_ARRAY_SIZE);

    int array[FIXED_ARRAY_SIZE];

    state st = FillArray(a, b, array, FIXED_ARRAY_SIZE);
    if (st != OK) {
        fprintf(stderr, "Ошибка: не удалось заполнить массив\n");
        return;
    }

    PrintArray("Исходный массив", array, FIXED_ARRAY_SIZE);

    int min_val, max_val, min_idx, max_idx;
    st = SwapMinMax(array, FIXED_ARRAY_SIZE, &min_val, &max_val, &min_idx, &max_idx);

    if (st != OK) {
        fprintf(stderr, "Ошибка: не удалось выполнить обмен\n");
        return;
    }

    printf("\nНайденные значения:\n");
    printf("  Минимум: %d (индекс: %d)\n", min_val, min_idx);
    printf("  Максимум: %d (индекс: %d)\n", max_val, max_idx);

    PrintArray("\nМассив после обмена", array, FIXED_ARRAY_SIZE);
}

int GetRandomSize() {
    return 10 + rand() % (10000 - 10 + 1);
}

int FindNearestInB(const int* B, int size_B, int value) {
    if (B == NULL || size_B <= 0) return 0;

    int index = 0;
    int min_dist = abs(B[0] - value);

    for (int j = 1; j < size_B; ++j) {
        int dist = abs(B[j] - value);
        if (dist < min_dist) {
            min_dist = dist;
            index = j;
        }
    }

    return index;
}

state BuildArrayC(const int* A, const int* B, int size_A, int size_B, int* C) {
    if (A == NULL || B == NULL || C == NULL) return ERROR_NULL;
    if (size_A <= 0 || size_B <= 0) return ERROR_RANGE;

    for (int i = 0; i < size_A; ++i) {
        int nearest_idx = FindNearestInB(B, size_B, A[i]);
        C[i] = A[i] + B[nearest_idx];
    }

    return OK;
}

void PrintArrayPartial(const char* name, const int* array, int size, int max_elements) {
    if (array == NULL) return;

    printf("%s (размер: %d): ", name, size);

    int elements_to_print = (size < max_elements) ? size : max_elements;

    for (int i = 0; i < elements_to_print; ++i) {
        printf("%d", array[i]);
        if (i < elements_to_print - 1) printf(" ");
    }

    if (size > max_elements) {
        printf(" ... [всего %d элементов]", size);
    }

    printf("\n");
}

void Task2Demo() {
    printf("\nЗАДАНИЕ 2: ДИНАМИЧЕСКИЕ МАССИВЫ A, B, C\n\n");

    int size_A = GetRandomSize();
    int* A = (int*)malloc(size_A * sizeof(int));
    if (A == NULL) {
        fprintf(stderr, "Ошибка: не удалось выделить память под массив A\n");
        return;
    }

    state st = FillArray(-1000, 1000, A, size_A);
    if (st != OK) {
        fprintf(stderr, "Ошибка: не удалось заполнить массив A\n");
        free(A);
        return;
    }

    int size_B = GetRandomSize();
    int* B = (int*)malloc(size_B * sizeof(int));
    if (B == NULL) {
        fprintf(stderr, "Ошибка: не удалось выделить память под массив B\n");
        free(A);
        return;
    }

    st = FillArray(-1000, 1000, B, size_B);
    if (st != OK) {
        fprintf(stderr, "Ошибка: не удалось заполнить массив B\n");
        free(A);
        free(B);
        return;
    }

    int* C = (int*)malloc(size_A * sizeof(int));
    if (C == NULL) {
        fprintf(stderr, "Ошибка: не удалось выделить память под массив C\n");
        free(A);
        free(B);
        return;
    }

    st = BuildArrayC(A, B, size_A, size_B, C);
    if (st != OK) {
        fprintf(stderr, "Ошибка: не удалось построить массив C\n");
        free(A);
        free(B);
        free(C);
        return;
    }

    printf("Размеры массивов:\n");
    printf("  A: %d элементов\n", size_A);
    printf("  B: %d элементов\n", size_B);
    printf("  C: %d элементов\n\n", size_A);

    int max_display = 20;

    PrintArrayPartial("A", A, size_A, max_display);
    PrintArrayPartial("B", B, size_B, max_display);
    PrintArrayPartial("C", C, size_A, max_display);

    printf("\nОписание массива C:\n");
    printf("  C[i] = A[i] + B[j], где B[j] - элемент из B, ближайший к A[i]\n");

    if (size_A > 0) {
        printf("\nПример расчёта для первого элемента:\n");
        printf("  A[0] = %d\n", A[0]);

        int nearest_idx = FindNearestInB(B, size_B, A[0]);
        printf("  B[%d] = %d (ближайший к A[0])\n", nearest_idx, B[nearest_idx]);
        printf("  C[0] = %d + %d = %d\n", A[0], B[nearest_idx], C[0]);
    }

    free(A);
    free(B);
    free(C);
}

void ShowMenu() {
    printf("\nВЫБОР ЗАДАНИЯ\n");
    printf("1. Задание 1: Поиск и обмен Min/Max\n");
    printf("2. Задание 2: Динамические массивы A,B,C\n");
    printf("0. Выход\n");
}

int main(int argc, char *argv[]) {
    srand((unsigned int)time(NULL));

    if (argc > 3) {
        fprintf(stderr, "Использование:\n");
        fprintf(stderr, "  %s              (интерактивный режим)\n", argv[0]);
        fprintf(stderr, "  %s <a> <b>     (задание 1 с параметрами)\n", argv[0]);
        return EXIT_FAILURE;
    }

    if (argc == 2) {
        fprintf(stderr, "Ошибка: требуется два аргумента или ноль\n");
        fprintf(stderr, "Использование:\n");
        fprintf(stderr, "  %s              (интерактивный режим)\n", argv[0]);
        fprintf(stderr, "  %s <a> <b>     (задание 1 с параметрами)\n", argv[0]);
        return EXIT_FAILURE;
    }

    if (argc == 3) {
        int a, b;
        state st = Number(argv[1], &a);
        if (st != OK) {
            fprintf(stderr, "Ошибка: некорректное значение a\n");
            return EXIT_FAILURE;
        }

        st = Number(argv[2], &b);
        if (st != OK) {
            fprintf(stderr, "Ошибка: некорректное значение b\n");
            return EXIT_FAILURE;
        }

        if (a > b) {
            fprintf(stderr, "Ошибка: должно быть a <= b\n");
            return EXIT_FAILURE;
        }

        Task1Demo(a, b);
        return EXIT_SUCCESS;
    }

    int choice;
    while (1) {
        ShowMenu();
        printf("Выберите задание: ");

        if (scanf("%d", &choice) != 1) {
            fprintf(stderr, "Ошибка: некорректный ввод\n");
            int c;
            while ((c = getchar()) != '\n' && c != EOF);
            continue;
        }
        int c;
        while ((c = getchar()) != '\n' && c != EOF);

        switch (choice) {
            case 1: {
                printf("\nВведите границы диапазона (a b): ");
                int a, b;
                if (scanf("%d %d", &a, &b) != 2) {
                    fprintf(stderr, "Ошибка: некорректный ввод\n");
                    int ch;
                    while ((ch = getchar()) != '\n' && ch != EOF);
                    break;
                }
                int ch;
                while ((ch = getchar()) != '\n' && ch != EOF);

                if (a > b) {
                    fprintf(stderr, "Ошибка: должно быть a <= b\n");
                    break;
                }

                Task1Demo(a, b);
                break;
            }
            case 2:
                Task2Demo();
                break;
            case 0:
                printf("До свидания!\n");
                return EXIT_SUCCESS;
            default:
                fprintf(stderr, "Ошибка: неверный номер задания\n");
        }
    }

    return EXIT_SUCCESS;
}