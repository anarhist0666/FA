#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>

typedef enum state {
    OK,
    END,
    ERROR_NULL,
    ERROR_INPUT,
    ERROR_FILE,
    ERROR_OVERFLOW,
    ERROR_FORMAT
} state;

int IsSpace(char c) {
    return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

state ReadLexeme(FILE* file, char* buffer, int size, int* overflow) {
    if (file == NULL || buffer == NULL || overflow == NULL) return ERROR_NULL;
    if (size <= 1) return ERROR_INPUT;

    *overflow = 0;

    int c;
    while ((c = fgetc(file)) != EOF) {
        if (!IsSpace(c)) break;
    }

    if (c == EOF) return END;

    int i = 0;
    while (c != EOF && !IsSpace(c)) {
        if (i + 1 < size) {
            buffer[i++] = (char)c;
        } else {
            *overflow = 1;
        }
        c = fgetc(file);
    }

    buffer[i] = '\0';
    return OK;
}

state CharToDigit(char c, int* digit) {
    if (digit == NULL) return ERROR_NULL;

    if (c >= '0' && c <= '9') {
        *digit = c - '0';
        return OK;
    }

    char lower = (char)tolower((unsigned char)c);
    if (lower >= 'a' && lower <= 'z') {
        *digit = lower - 'a' + 10;
        return OK;
    }

    return ERROR_FORMAT;
}

state FindMinBase(const char* str, int* min_base) {
    if (str == NULL || min_base == NULL) return ERROR_NULL;
    if (str[0] == '\0') return ERROR_INPUT;

    int max_digit = 0;

    for (int i = 0; str[i] != '\0'; ++i) {
        int digit;
        state st = CharToDigit(str[i], &digit);
        if (st != OK) return ERROR_FORMAT;

        if (digit > max_digit) max_digit = digit;
    }

    *min_base = max_digit + 1;
    if (*min_base < 2) *min_base = 2;

    if (*min_base > 36) return ERROR_INPUT;

    return OK;
}

state ConvertToDecimal(const char* str, int base, long long* result) {
    if (str == NULL || result == NULL) return ERROR_NULL;
    if (base < 2 || base > 36) return ERROR_INPUT;
    if (str[0] == '\0') return ERROR_INPUT;

    *result = 0;

    for (int i = 0; str[i] != '\0'; ++i) {
        int digit;
        state st = CharToDigit(str[i], &digit);
        if (st != OK) return ERROR_FORMAT;

        if (digit >= base) return ERROR_INPUT;

        if (*result > (LLONG_MAX - digit) / base) {
            return ERROR_OVERFLOW;
        }

        *result = *result * base + digit;
    }

    return OK;
}

const char* RemoveLeadingZeros(const char* str) {
    if (str == NULL) return str;

    int i = 0;
    while (str[i] == '0' && str[i + 1] != '\0') {
        ++i;
    }

    return str + i;
}

state ProcessNumbers(const char* inFile, const char* outFile, int* processed_count, int* error_count) {
    if (inFile == NULL || outFile == NULL || processed_count == NULL || error_count == NULL) {
        return ERROR_NULL;
    }

    *processed_count = 0;
    *error_count = 0;

    if (strcmp(inFile, outFile) == 0) {
        return ERROR_INPUT;
    }

    FILE* in = fopen(inFile, "r");
    if (in == NULL) return ERROR_FILE;

    FILE* out = fopen(outFile, "w");
    if (out == NULL) {
        fclose(in);
        return ERROR_FILE;
    }

    char buffer[4096];
    int base;
    long long value;
    state st;
    int overflow = 0;

    while (1) {
        st = ReadLexeme(in, buffer, sizeof(buffer), &overflow);

        if (st == END) break;

        if (st != OK) {
            fclose(in);
            fclose(out);
            return st;
        }

        if (overflow) {
            (*error_count)++;
            continue;
        }

        st = FindMinBase(buffer, &base);
        if (st != OK) {
            (*error_count)++;
            continue;
        }

        st = ConvertToDecimal(buffer, base, &value);
        if (st != OK) {
            (*error_count)++;
            continue;
        }

        const char* cleaned = RemoveLeadingZeros(buffer);

        if (fprintf(out, "%s %d %lld\n", cleaned, base, value) < 0) {
            fclose(in);
            fclose(out);
            return ERROR_FILE;
        }

        (*processed_count)++;
    }

    if (ferror(in)) {
        fclose(in);
        fclose(out);
        return ERROR_FILE;
    }

    fclose(in);
    fclose(out);
    return OK;
}

int main(int argc, char *argv[]) {
    if (argc != 3) {
        fprintf(stderr, "Использование: %s <входной_файл> <выходной_файл>\n", argv[0]);
        fprintf(stderr, "Пример: %s input.txt output.txt\n", argv[0]);
        return EXIT_FAILURE;
    }

    const char* inFile = argv[1];
    const char* outFile = argv[2];

    if (strcmp(inFile, outFile) == 0) {
        fprintf(stderr, "Ошибка: входной и выходной файлы не должны совпадать\n");
        return EXIT_FAILURE;
    }

    int processed = 0;
    int errors = 0;

    state st = ProcessNumbers(inFile, outFile, &processed, &errors);

    if (st != OK) {
        switch (st) {
            case ERROR_NULL:
                fprintf(stderr, "Ошибка: NULL-указатель\n");
                break;
            case ERROR_FILE:
                fprintf(stderr, "Ошибка: проблема с файлом\n");
                break;
            case ERROR_INPUT:
                fprintf(stderr, "Ошибка: некорректный ввод (входной и выходной файлы совпадают)\n");
                break;
            case ERROR_OVERFLOW:
                fprintf(stderr, "Ошибка: переполнение при обработке числа\n");
                break;
            case ERROR_FORMAT:
                fprintf(stderr, "Ошибка: некорректный формат числа\n");
                break;
            default:
                fprintf(stderr, "Неизвестная ошибка\n");
                break;
        }
        return EXIT_FAILURE;
    }

    fprintf(stderr, "Обработка завершена успешно!\n");
    fprintf(stderr, "  Обработано чисел: %d\n", processed);
    fprintf(stderr, "  Пропущено (ошибки): %d\n", errors);
    fprintf(stderr, "  Результаты записаны в файл: %s\n", outFile);

    return EXIT_SUCCESS;
}