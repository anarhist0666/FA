#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

typedef enum state {
    OK,
    ERROR_NULL,
    ERROR_INPUT,
    ERROR_FILE,
    ERROR_MEMORY
} state;

state Flag(const char* f, char* res, int* hasn) {
    if (f == NULL || res == NULL || hasn == NULL) return ERROR_NULL;
    if (f[0] != '/' && f[0] != '-') return ERROR_INPUT;
    
    size_t len = strlen(f);
    if (len != 2 && len != 3) return ERROR_INPUT;
    
    int i = 1;
    *hasn = 0;
    
    if (f[i] == 'n') {
        ++i;
        *hasn = 1;
    }
    
    if (f[i] != 'd' && f[i] != 'i' && f[i] != 's' && f[i] != 'a') return ERROR_INPUT;
    
    *res = f[i];
    return OK;
}

state GenerateOutputFileName(const char* inFile, char** outFile) {
    if (inFile == NULL || outFile == NULL) return ERROR_NULL;
    
    size_t len = strlen(inFile);
    size_t prefix_len = strlen("out_");
    size_t total_len = len + prefix_len + 1;
    
    char* buf = (char*)malloc(total_len);
    if (buf == NULL) return ERROR_MEMORY;
    
    snprintf(buf, total_len, "out_%s", inFile);
    *outFile = buf;
    return OK;
}

state D(const char* inFile, const char* outFile) {
    if (inFile == NULL || outFile == NULL) return ERROR_NULL;
    
    FILE* in = fopen(inFile, "r");
    if (in == NULL) {
        return ERROR_FILE;
    }
    
    FILE* out = fopen(outFile, "w");
    if (out == NULL) {
        fclose(in);
        return ERROR_FILE;
    }
    
    int c;
    while ((c = fgetc(in)) != EOF) {
        if (!(c >= '0' && c <= '9')) {
            if (fputc(c, out) == EOF) {
                fclose(in);
                fclose(out);
                return ERROR_FILE;
            }
        }
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

state I(const char* inFile, const char* outFile) {
    if (inFile == NULL || outFile == NULL) return ERROR_NULL;
    
    FILE* in = fopen(inFile, "r");
    if (in == NULL) {
        return ERROR_FILE;
    }
    
    FILE* out = fopen(outFile, "w");
    if (out == NULL) {
        fclose(in);
        return ERROR_FILE;
    }
    
    long long count = 0;
    int c;
    int last_was_newline = 1;
    
    while ((c = fgetc(in)) != EOF) {
        if (c == '\n') {
            if (fprintf(out, "%lld\n", count) < 0) {
                fclose(in);
                fclose(out);
                return ERROR_FILE;
            }
            count = 0;
            last_was_newline = 1;
        } else {
            if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) {
                ++count;
            }
            last_was_newline = 0;
        }
    }
    
    if (ferror(in)) {
        fclose(in);
        fclose(out);
        return ERROR_FILE;
    }
    
    if (!last_was_newline) {
        if (fprintf(out, "%lld\n", count) < 0) {
            fclose(in);
            fclose(out);
            return ERROR_FILE;
        }
    }
    
    fclose(in);
    fclose(out);
    return OK;
}

state S(const char* inFile, const char* outFile) {
    if (inFile == NULL || outFile == NULL) return ERROR_NULL;
    
    FILE* in = fopen(inFile, "r");
    if (in == NULL) {
        return ERROR_FILE;
    }
    
    FILE* out = fopen(outFile, "w");
    if (out == NULL) {
        fclose(in);
        return ERROR_FILE;
    }
    
    long long count = 0;
    int c;
    int last_was_newline = 1;
    
    while ((c = fgetc(in)) != EOF) {
        if (c == '\n') {
            if (fprintf(out, "%lld\n", count) < 0) {
                fclose(in);
                fclose(out);
                return ERROR_FILE;
            }
            count = 0;
            last_was_newline = 1;
        } else {
            if (c != '\r') {
                int is_latin = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
                int is_digit = (c >= '0' && c <= '9');
                int is_space = (c == ' ');
                
                if (!(is_latin || is_digit || is_space)) {
                    ++count;
                }
            }
            last_was_newline = 0;
        }
    }
    
    if (ferror(in)) {
        fclose(in);
        fclose(out);
        return ERROR_FILE;
    }
    
    if (!last_was_newline) {
        if (fprintf(out, "%lld\n", count) < 0) {
            fclose(in);
            fclose(out);
            return ERROR_FILE;
        }
    }
    
    fclose(in);
    fclose(out);
    return OK;
}

state A(const char* inFile, const char* outFile) {
    if (inFile == NULL || outFile == NULL) return ERROR_NULL;
    
    FILE* in = fopen(inFile, "r");
    if (in == NULL) {
        return ERROR_FILE;
    }
    
    FILE* out = fopen(outFile, "w");
    if (out == NULL) {
        fclose(in);
        return ERROR_FILE;
    }
    
    int c;
    while ((c = fgetc(in)) != EOF) {
        if (c >= '0' && c <= '9') {
            if (fputc(c, out) == EOF) {
                fclose(in);
                fclose(out);
                return ERROR_FILE;
            }
        } else {
            if (fprintf(out, "%02X", (unsigned char)c) < 0) {
                fclose(in);
                fclose(out);
                return ERROR_FILE;
            }
        }
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
    if (argc < 3 || argc > 4) {
        fprintf(stderr, "Использование:\n");
        fprintf(stderr, "  %s <флаг> <входной_файл> [<выходной_файл>]\n", argv[0]);
        fprintf(stderr, "Флаги: -d, -i, -s, -a, -nd, -ni, -ns, -na\n");
        return EXIT_FAILURE;
    }
    
    char flag = '\0';
    int hasn = 0;
    state st = Flag(argv[1], &flag, &hasn);
    
    if (st != OK) {
        fprintf(stderr, "Ошибка: некорректный флаг '%s'\n", argv[1]);
        fprintf(stderr, "Допустимые флаги: -d, -i, -s, -a, -nd, -ni, -ns, -na\n");
        return EXIT_FAILURE;
    }
    
    const char* inFile = argv[2];
    char* outFile = NULL;
    int free_outFile = 0;
    
    if (hasn) {
        if (argc != 4) {
            fprintf(stderr, "Ошибка: для флагов с 'n' требуется явно указать выходной файл\n");
            return EXIT_FAILURE;
        }
        outFile = argv[3];
    } else {
        if (argc != 3) {
            fprintf(stderr, "Ошибка: для флагов без 'n' выходной файл не должен быть указан\n");
            return EXIT_FAILURE;
        }
        st = GenerateOutputFileName(inFile, &outFile);
        if (st != OK) {
            switch (st) {
                case ERROR_NULL:
                    fprintf(stderr, "Ошибка: NULL-указатель\n");
                    break;
                case ERROR_MEMORY:
                    fprintf(stderr, "Ошибка: не удалось выделить память для имени выходного файла\n");
                    break;
                default:
                    fprintf(stderr, "Неизвестная ошибка\n");
                    break;
            }
            return EXIT_FAILURE;
        }
        free_outFile = 1;
    }
    
    if (strcmp(inFile, outFile) == 0) {
        fprintf(stderr, "Ошибка: входной и выходной файлы не должны совпадать\n");
        if (free_outFile) free(outFile);
        return EXIT_FAILURE;
    }
    
    switch (flag) {
        case 'd':
            st = D(inFile, outFile);
            break;
        case 'i':
            st = I(inFile, outFile);
            break;
        case 's':
            st = S(inFile, outFile);
            break;
        case 'a':
            st = A(inFile, outFile);
            break;
        default:
            fprintf(stderr, "Ошибка: неизвестный флаг\n");
            if (free_outFile) free(outFile);
            return EXIT_FAILURE;
    }
    
    if (st != OK) {
        switch (st) {
            case ERROR_NULL:
                fprintf(stderr, "Ошибка: NULL-указатель\n");
                break;
            case ERROR_FILE:
                fprintf(stderr, "Ошибка: проблема с файлом\n");
                break;
            case ERROR_INPUT:
                fprintf(stderr, "Ошибка: некорректный ввод\n");
                break;
            case ERROR_MEMORY:
                fprintf(stderr, "Ошибка: не удалось выделить память\n");
                break;
            default:
                fprintf(stderr, "Неизвестная ошибка\n");
                break;
        }
        if (free_outFile) free(outFile);
        return EXIT_FAILURE;
    }
    
    fprintf(stderr, "Обработка завершена успешно\n");
    
    if (free_outFile) free(outFile);
    
    return EXIT_SUCCESS;
}