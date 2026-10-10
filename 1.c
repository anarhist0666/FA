#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define MAX LLONG_MAX

typedef enum state {
    OK,
    ERROR_NULL,
    ERROR_RANGE,
    ERROR_OVERFLOW,
    ERROR_INPUT,
    ERROR_MEMORY
} state;

state Flag(const char* f, char* res) {
    if (f == NULL || res == NULL) return ERROR_NULL;
    if (f[2] != '\0') return ERROR_INPUT;
    if (f[0] != '/' && f[0] != '-') return ERROR_INPUT;
    if (f[1] != 'h' && f[1] != 'p' && f[1] != 's' && 
        f[1] != 'e' && f[1] != 'a' && f[1] != 'f') return ERROR_INPUT;
    *res = f[1];
    return OK;
}

state Number(const char* n, long long* res) {
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
    unsigned long long max_limit = (sign > 0) ? (unsigned long long)MAX : (unsigned long long)LLONG_MIN;
    
    while (n[i] != '\0') {
        if (n[i] < '0' || n[i] > '9') return ERROR_INPUT;
        
        unsigned int digit = n[i] - '0';
        
        if (x > (max_limit - digit) / 10) return ERROR_OVERFLOW;
        
        x = x * 10 + digit;
        ++i;
    }
    
    if (sign > 0) {
        *res = (long long)x;
    } else {
        if (x == (unsigned long long)LLONG_MIN) {
            *res = LLONG_MIN;
        } else {
            *res = -(long long)x;
        }
    }
    
    return OK;
}

state H(long long x, long long** arr, long long* have) {
    if (arr == NULL || have == NULL) return ERROR_NULL;
    if (x <= 0) return ERROR_RANGE;
    
    *have = 100 / x;
    
    if (*have == 0) {
        *arr = NULL;
        return OK;
    }
    
    *arr = (long long*)malloc((*have) * sizeof(long long));
    if (*arr == NULL) return ERROR_MEMORY;
    
    for (long long i = 0; i < *have; ++i) {
        (*arr)[i] = (i + 1) * x;
    }
    
    return OK;
}

state P(long long x, long long *prime) {
    if (prime == NULL) return ERROR_NULL;
    
    if (x <= 1) {
        *prime = 0;
        return OK;
    }
    
    if (x == 2) {
        *prime = 2;
        return OK;
    }
    
    if (x % 2 == 0) {
        *prime = 1;
        return OK;
    }
    
    for (long long i = 3; i <= x / i; i += 2) {
        if (x % i == 0) {
            *prime = 1;
            return OK;
        }
    }
    
    *prime = 2;
    return OK;
}

state S(long long x, char* r, long long* len) {
    if (r == NULL || len == NULL) return ERROR_NULL;
    
    *len = 0;
    
    if (x == 0) {
        r[0] = '0';
        r[1] = '\0';
        *len = 1;
        return OK;
    }
    
    unsigned long long ux = (unsigned long long)x;
    
    char temp[64];
    long long temp_len = 0;
    
    while (ux > 0) {
        unsigned long long digit = ux % 16;
        if (digit <= 9) {
            temp[temp_len] = '0' + digit;
        } else {
            temp[temp_len] = 'A' + (digit - 10);
        }
        temp_len++;
        ux /= 16;
    }
    
    for (long long i = temp_len - 1; i >= 0; --i) {
        r[*len] = temp[i];
        (*len)++;
    }
    
    r[*len] = '\0';
    return OK;
}

typedef struct {
    long long base;
    long long exp;
    long long result;
} PowerEntry;

state ComputePowers(long long x, PowerEntry** entries, long long* count) {
    if (entries == NULL || count == NULL) return ERROR_NULL;
    if (x < 1 || x > 10) return ERROR_RANGE;
    
    *count = 10 * x;
    *entries = (PowerEntry*)malloc((*count) * sizeof(PowerEntry));
    if (*entries == NULL) {
        *count = 0;
        return ERROR_MEMORY;
    }
    
    long long idx = 0;
    for (long long base = 1; base <= 10; ++base) {
        long long power = 1;
        for (long long exp = 1; exp <= x; ++exp) {
            if (base > 1 && power > MAX / base) {
                free(*entries);
                *entries = NULL;
                *count = 0;
                return ERROR_OVERFLOW;
            }
            power *= base;
            (*entries)[idx].base = base;
            (*entries)[idx].exp = exp;
            (*entries)[idx].result = power;
            idx++;
        }
    }
    
    return OK;
}

void PrintPowers(const PowerEntry* entries, long long count) {
    if (entries == NULL || count == 0) return;
    
    printf("Основание | Показатель | Результат\n");
    for (long long i = 0; i < count; ++i) {
        printf("%9lld | %10lld | %9lld\n", 
               entries[i].base, entries[i].exp, entries[i].result);
    }
}

state E(long long x, PowerEntry** entries, long long* count) {
    return ComputePowers(x, entries, count);
}

state A(long long x, long long* res) {
    if (res == NULL) return ERROR_NULL;
    if (x < 0) return ERROR_RANGE;
    
    if (x == 0) {
        *res = 0;
        return OK;
    }
    
    if (x & 1) {
        long long half = x / 2 + 1;
        if (x > MAX / half) return ERROR_OVERFLOW;
        *res = x * half;
    } else {
        long long half = x / 2;
        if (half > MAX / (x + 1)) return ERROR_OVERFLOW;
        *res = half * (x + 1);
    }
    
    return OK;
}

state F(long long x, long long* res) {
    if (res == NULL) return ERROR_NULL;
    if (x < 0) return ERROR_RANGE;
    
    if (x > 20) return ERROR_OVERFLOW;
    
    *res = 1;
    for (long long i = 2; i <= x; ++i) {
        if (*res > MAX / i) return ERROR_OVERFLOW;
        *res *= i;
    }
    
    return OK;
}

void PrintHResults(const long long* arr, long long count, long long x) {
    if (count == 0) {
        printf("Нет натуральных чисел <= 100, кратных %lld\n", x);
    } else {
        printf("Натуральные числа <= 100, кратные %lld:\n", x);
        for (long long i = 0; i < count; ++i) {
            printf("%lld", arr[i]);
            if (i < count - 1) printf(" ");
        }
        printf("\n");
    }
}

void PrintPrimality(long long x, long long prime) {
    switch (prime) {
        case 0:
            printf("%lld: ни простое, ни составное\n", x);
            break;
        case 1:
            printf("%lld: составное\n", x);
            break;
        case 2:
            printf("%lld: простое\n", x);
            break;
    }
}

void PrintHex(const char* hex_str, long long len) {
    if (hex_str == NULL) return;
    
    for (long long i = 0; i < len; ++i) {
        printf("%c", hex_str[i]);
        if (i < len - 1) printf(" ");
    }
    printf("\n");
}

int main(int argc, char *argv[]) {
    if (argc != 3) {
        fprintf(stderr, "Использование: %s <флаг> <число>\n", argv[0]);
        fprintf(stderr, "Флаги: -h, -p, -s, -e, -a, -f\n");
        return EXIT_FAILURE;
    }
    
    char flag = '\0';
    state st = Flag(argv[1], &flag);
    if (st != OK) {
        fprintf(stderr, "Ошибка: некорректный флаг '%s'\n", argv[1]);
        return EXIT_FAILURE;
    }
    
    long long number = 0;
    st = Number(argv[2], &number);
    if (st != OK) {
        switch (st) {
            case ERROR_INPUT:
                fprintf(stderr, "Ошибка: некорректное число '%s'\n", argv[2]);
                break;
            case ERROR_OVERFLOW:
                fprintf(stderr, "Ошибка: число вне диапазона long long\n");
                break;
            default:
                fprintf(stderr, "Ошибка при парсинге числа\n");
        }
        return EXIT_FAILURE;
    }
    
    switch (flag) {
        case 'h': {
            long long* arr = NULL;
            long long have = 0;
            st = H(number, &arr, &have);
            if (st != OK) {
                switch (st) {
                    case ERROR_RANGE:
                        fprintf(stderr, "Ошибка: число должно быть натуральным\n");
                        break;
                    case ERROR_MEMORY:
                        fprintf(stderr, "Ошибка: невозможно выделить память\n");
                        break;
                    default:
                        fprintf(stderr, "Ошибка в функции H\n");
                }
                return EXIT_FAILURE;
            }
            
            PrintHResults(arr, have, number);
            free(arr);
            break;
        }
        
        case 'p': {
            long long prime = 0;
            st = P(number, &prime);
            if (st != OK) {
                fprintf(stderr, "Ошибка в функции P\n");
                return EXIT_FAILURE;
            }
            
            PrintPrimality(number, prime);
            break;
        }
        
        case 's': {
            char hex_str[64];
            long long len = 0;
            st = S(number, hex_str, &len);
            if (st != OK) {
                fprintf(stderr, "Ошибка в функции S\n");
                return EXIT_FAILURE;
            }
            
            printf("Представление числа %lld в hex: ", number);
            PrintHex(hex_str, len);
            break;
        }
        
        case 'e': {
            PowerEntry* entries = NULL;
            long long count = 0;
            
            st = E(number, &entries, &count);
            if (st != OK) {
                switch (st) {
                    case ERROR_RANGE:
                        fprintf(stderr, "Ошибка: число должно быть в диапазоне [1, 10]\n");
                        break;
                    case ERROR_OVERFLOW:
                        fprintf(stderr, "Ошибка: переполнение при вычислении степеней\n");
                        break;
                    case ERROR_MEMORY:
                        fprintf(stderr, "Ошибка: невозможно выделить память\n");
                        break;
                    default:
                        fprintf(stderr, "Ошибка в функции E\n");
                }
                return EXIT_FAILURE;
            }
            
            PrintPowers(entries, count);
            free(entries);
            break;
        }
        
        case 'a': {
            long long sum = 0;
            st = A(number, &sum);
            if (st != OK) {
                switch (st) {
                    case ERROR_RANGE:
                        fprintf(stderr, "Ошибка: число должно быть неотрицательным\n");
                        break;
                    case ERROR_OVERFLOW:
                        fprintf(stderr, "Ошибка: переполнение при вычислении суммы\n");
                        break;
                    default:
                        fprintf(stderr, "Ошибка в функции A\n");
                }
                return EXIT_FAILURE;
            }
            printf("Сумма чисел от 1 до %lld = %lld\n", number, sum);
            break;
        }
        
        case 'f': {
            long long factorial = 0;
            st = F(number, &factorial);
            if (st != OK) {
                switch (st) {
                    case ERROR_RANGE:
                        fprintf(stderr, "Ошибка: факториал отрицательного числа не определён\n");
                        break;
                    case ERROR_OVERFLOW:
                        fprintf(stderr, "Ошибка: %lld! переполняет long long (максимум 20!)\n", number);
                        break;
                    default:
                        fprintf(stderr, "Ошибка в функции F\n");
                }
                return EXIT_FAILURE;
            }
            printf("%lld! = %lld\n", number, factorial);
            break;
        }
    }
    
    return EXIT_SUCCESS;
}