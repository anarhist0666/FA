#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>

#define BUFFER_SIZE 1024

typedef enum state {
    OK,
    ERROR_RANGE,
    ERROR_NULL,
    ERROR_INPUT,
    ERROR_OVERFLOW,
    ERROR_FORMAT
} state;

void StripNewline(char* s) {
    if (s == NULL) return;
    int len = (int)strlen(s);
    if (len > 0 && s[len - 1] == '\n') {
        s[--len] = '\0';
    }
    if (len > 0 && s[len - 1] == '\r') {
        s[--len] = '\0';
    }
}

state ReadLine(char* buffer, int size) {
    if (buffer == NULL || size <= 1) return ERROR_NULL;

    if (fgets(buffer, size, stdin) == NULL) {
        return ERROR_INPUT;
    }

    size_t len = strlen(buffer);
    if (len > 0 && buffer[len - 1] == '\n') {
        buffer[len - 1] = '\0';
        return OK;
    }

    int c = getchar();

    if (c == EOF) {
        return OK;
    }
    if (c == '\n') {
        return OK;
    }

    while ((c = getchar()) != '\n' && c != EOF) { }
    return ERROR_OVERFLOW;
}

state ReadBase(int* base) {
    if (base == NULL) return ERROR_NULL;

    printf("Введите основание системы счисления (2-36): ");
    fflush(stdout);

    char buffer[BUFFER_SIZE];
    state rst = ReadLine(buffer, sizeof(buffer));

    if (rst == ERROR_INPUT) return ERROR_INPUT;
    if (rst == ERROR_OVERFLOW) return ERROR_OVERFLOW;
    if (buffer[0] == '\0') return ERROR_INPUT;

    int b = 0;
    for (int i = 0; buffer[i] != '\0'; ++i) {
        if (buffer[i] < '0' || buffer[i] > '9') return ERROR_INPUT;
        int digit = buffer[i] - '0';
        if (b > (36 - digit) / 10) return ERROR_RANGE;
        b = b * 10 + digit;
        if (b > 36) return ERROR_RANGE;
    }

    if (b < 2) return ERROR_RANGE;

    *base = b;
    return OK;
}

int CharToDigit(char c) {
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'A' && c <= 'Z') return c - 'A' + 10;
    return -1;
}

char DigitToChar(int d) {
    if (d >= 0 && d <= 9) return (char)('0' + d);
    if (d >= 10 && d <= 35) return (char)('A' + d - 10);
    return '\0';
}

state ConvertToDecimal(const char* str, int base, long long* result) {
    if (str == NULL || result == NULL) return ERROR_NULL;
    if (base < 2 || base > 36) return ERROR_RANGE;
    if (str[0] == '\0') return ERROR_INPUT;

    int sign = 1;
    int i = 0;

    if (str[0] == '-') {
        i = 1;
        sign = -1;
    } else if (str[0] == '+') {
        i = 1;
    }

    if (str[i] == '\0') return ERROR_INPUT;

    unsigned long long uresult = 0;
    unsigned long long limit = sign == -1
        ? (unsigned long long)LLONG_MAX + 1ULL
        : (unsigned long long)LLONG_MAX;

    for (; str[i] != '\0'; ++i) {
        int digit = CharToDigit(str[i]);
        if (digit < 0 || digit >= base) return ERROR_FORMAT;

        if (uresult > (limit - (unsigned long long)digit) / (unsigned long long)base) {
            return ERROR_OVERFLOW;
        }
        uresult = uresult * (unsigned long long)base + (unsigned long long)digit;
    }

    if (sign == -1) {
        if (uresult == (unsigned long long)LLONG_MAX + 1ULL) {
            *result = LLONG_MIN;
        } else {
            *result = -(long long)uresult;
        }
    } else {
        *result = (long long)uresult;
    }

    return OK;
}

void ConvertFromDecimal(long long number, int base, char* buffer, int buffer_size) {
    if (buffer == NULL || buffer_size <= 0) return;

    if (number == 0) {
        buffer[0] = '0';
        buffer[1] = '\0';
        return;
    }

    unsigned long long un;
    int is_negative = 0;

    if (number < 0) {
        is_negative = 1;
        un = (unsigned long long)(-(number + 1)) + 1ULL;
    } else {
        un = (unsigned long long)number;
    }

    char digits[128];
    int len = 0;

    while (un > 0 && len < (int)sizeof(digits)) {
        digits[len++] = DigitToChar((int)(un % (unsigned long long)base));
        un /= (unsigned long long)base;
    }

    int pos = 0;
    if (is_negative) buffer[pos++] = '-';

    for (int i = len - 1; i >= 0 && pos < buffer_size - 1; --i) {
        buffer[pos++] = digits[i];
    }
    buffer[pos] = '\0';
}

void PrintInBase(long long number, int base) {
    char buffer[BUFFER_SIZE];
    ConvertFromDecimal(number, base, buffer, sizeof(buffer));
    printf("%s", buffer);
}

void RemoveLeadingZeros(char* str) {
    if (str == NULL) return;

    int is_negative = 0;
    int has_sign = 0;
    int start = 0;

    if (str[0] == '-') {
        is_negative = 1;
        has_sign = 1;
        start = 1;
    } else if (str[0] == '+') {
        has_sign = 1;
        start = 1;
    }

    int i = start;
    while (str[i] == '0' && str[i + 1] != '\0') {
        i++;
    }

    int is_zero = (str[i] == '0' && str[i + 1] == '\0');

    if (is_zero) {
        str[0] = '0';
        str[1] = '\0';
        return;
    }

    if (i > start) {
        if (is_negative) {
            str[0] = '-';
            memmove(&str[1], &str[i], strlen(&str[i]) + 1);
        } else {
            memmove(str, &str[i], strlen(&str[i]) + 1);
        }
    } else if (has_sign && !is_negative) {
        memmove(str, &str[start], strlen(&str[start]) + 1);
    }
}

int main(void) {
    int base = 0;
    state st = ReadBase(&base);

    if (st != OK) {
        fprintf(stderr, "Ошибка при чтении основания: ");
        switch (st) {
            case ERROR_NULL:      fprintf(stderr, "NULL-указатель\n"); break;
            case ERROR_INPUT:     fprintf(stderr, "некорректный ввод\n"); break;
            case ERROR_RANGE:     fprintf(stderr, "основание должно быть в диапазоне [2..36]\n"); break;
            case ERROR_OVERFLOW:  fprintf(stderr, "слишком длинная строка\n"); break;
            default:              fprintf(stderr, "неизвестная ошибка\n"); break;
        }
        return EXIT_FAILURE;
    }

    printf("Основание: %d\n", base);
    printf("Вводите числа в системе счисления с основанием %d\n", base);
    printf("(цифры > 9 вводите как прописные буквы A-Z)\n");
    printf("Для завершения введите 'Stop'\n\n");

    char buffer[BUFFER_SIZE];
    long long sum = 0;
    long long max_abs_number = 0;
    char max_number_str[BUFFER_SIZE] = {0};
    int count = 0;
    int sum_overflowed = 0;

    while (1) {
        printf("Число %d: ", count + 1);
        fflush(stdout);

        state rst = ReadLine(buffer, sizeof(buffer));

        if (rst == ERROR_INPUT) {
            fprintf(stderr, "Ошибка при чтении ввода\n");
            break;
        }
        if (rst == ERROR_OVERFLOW) {
            fprintf(stderr, "Ошибка: строка слишком длинная, ввод пропущен\n");
            continue;
        }

        if (buffer[0] == '\0') {
            printf("(пустая строка пропущена)\n");
            continue;
        }

        if (strcmp(buffer, "Stop") == 0) {
            printf("Ввод завершён.\n\n");
            break;
        }

        long long number = 0;
        st = ConvertToDecimal(buffer, base, &number);

        if (st != OK) {
            fprintf(stderr, "Ошибка: ");
            switch (st) {
                case ERROR_NULL:     fprintf(stderr, "NULL-указатель\n"); break;
                case ERROR_INPUT:    fprintf(stderr, "некорректный формат числа\n"); break;
                case ERROR_FORMAT:   fprintf(stderr, "некорректный символ для базы %d\n", base); break;
                case ERROR_OVERFLOW: fprintf(stderr, "число слишком большое\n"); break;
                default:             fprintf(stderr, "неизвестная ошибка\n"); break;
            }
            continue;
        }

        count++;

        int overflow = 0;
        if (number > 0 && sum > LLONG_MAX - number) {
            overflow = 1;
        } else if (number < 0 && sum < LLONG_MIN - number) {
            overflow = 1;
        }

        if (!overflow && !sum_overflowed) {
            sum += number;
        } else {
            sum_overflowed = 1;
        }

        unsigned long long abs_number = (number < 0)
            ? (unsigned long long)(-(number + 1)) + 1ULL
            : (unsigned long long)number;
        unsigned long long abs_max = (max_abs_number < 0)
            ? (unsigned long long)(-(max_abs_number + 1)) + 1ULL
            : (unsigned long long)max_abs_number;

        if (count == 1 || abs_number > abs_max) {
            max_abs_number = number;
            strncpy(max_number_str, buffer, sizeof(max_number_str) - 1);
            max_number_str[sizeof(max_number_str) - 1] = '\0';
        }

        printf("  В десятичной системе: %lld\n", number);
        printf("  В базах (9, 18, 27, 36): ");
        PrintInBase(number, 9);   printf(", ");
        PrintInBase(number, 18);  printf(", ");
        PrintInBase(number, 27);  printf(", ");
        PrintInBase(number, 36);
        printf("\n\n");
    }

    if (count == 0) {
        printf("Ошибка: числа не были введены\n");
        return EXIT_FAILURE;
    }

    printf("РЕЗУЛЬТАТЫ\n\n");

    printf("Всего введено чисел: %d\n\n", count);

    printf("СУММА:\n");
    if (sum_overflowed) {
        printf("  ВНИМАНИЕ: сумма не помещается в long long.\n");
        printf("  Точное значение не может быть представлено.\n");
    } else {
        printf("  В десятичной системе: %lld\n", sum);
        printf("  В базах (9, 18, 27, 36): ");
        PrintInBase(sum, 9);   printf(", ");
        PrintInBase(sum, 18);  printf(", ");
        PrintInBase(sum, 27);  printf(", ");
        PrintInBase(sum, 36);
        printf("\n");
    }
    printf("\n");

    printf("МАКСИМАЛЬНОЕ ПО МОДУЛЮ:\n");
    RemoveLeadingZeros(max_number_str);
    printf("  Исходное представление (база %d): %s\n", base, max_number_str);
    printf("  В десятичной системе: %lld\n", max_abs_number);
    printf("  В базах (9, 18, 27, 36): ");
    PrintInBase(max_abs_number, 9);   printf(", ");
    PrintInBase(max_abs_number, 18);  printf(", ");
    PrintInBase(max_abs_number, 27);  printf(", ");
    PrintInBase(max_abs_number, 36);
    printf("\n\n");

    return EXIT_SUCCESS;
}