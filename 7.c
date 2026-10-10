#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

typedef enum state {
    OK,
    END,
    ERROR_NULL,
    ERROR_INPUT,
    ERROR_FILE,
    ERROR_MEMORY
} state;

state Flag(const char* f, char* res) {
    if (f == NULL || res == NULL) return ERROR_NULL;
    if (f[0] != '/' && f[0] != '-') return ERROR_INPUT;

    int i = 1;
    if (f[i] == '\0' || f[i + 1] != '\0') return ERROR_INPUT;
    if (f[i] != 'r' && f[i] != 'a') return ERROR_INPUT;

    *res = f[i];
    return OK;
}

int IsSpace(char c) {
    return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

state ReadLexeme(FILE* file, char* buffer, int size, int* overflow) {
    if (file == NULL || buffer == NULL || overflow == NULL) return ERROR_NULL;
    if (size <= 0) return ERROR_INPUT;

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

void ToLower(char* s) {
    if (s == NULL) return;
    for (int i = 0; s[i] != '\0'; ++i) {
        if (s[i] >= 'A' && s[i] <= 'Z') {
            s[i] = s[i] - 'A' + 'a';
        }
    }
}

void PrintASCIIInBase(FILE* out, unsigned char c, int base) {
    if (out == NULL) return;

    char digits[64] = {0};
    int len = 0;

    if (c == 0) {
        fputc('0', out);
        return;
    }

    while (c > 0) {
        int digit = c % base;
        digits[len++] = (digit < 10) ? ('0' + digit) : ('A' + digit - 10);
        c /= base;
    }

    for (int i = len - 1; i >= 0; --i) {
        fputc(digits[i], out);
    }
}

state R(const char* file1, const char* file2, const char* outFile) {
    if (file1 == NULL || file2 == NULL || outFile == NULL) return ERROR_NULL;

    if (strcmp(file1, file2) == 0) {
        return ERROR_INPUT;
    }

    if (strcmp(file1, outFile) == 0 || strcmp(file2, outFile) == 0) {
        return ERROR_INPUT;
    }

    FILE* f1 = fopen(file1, "r");
    if (f1 == NULL) {
        return ERROR_FILE;
    }

    FILE* f2 = fopen(file2, "r");
    if (f2 == NULL) {
        fclose(f1);
        return ERROR_FILE;
    }

    FILE* out = fopen(outFile, "w");
    if (out == NULL) {
        fclose(f1);
        fclose(f2);
        return ERROR_FILE;
    }

    char buffer1[4096];
    char buffer2[4096];
    int first_lex = 1;
    int end_f1 = 0;
    int end_f2 = 0;
    int overflow = 0;

    state st1 = ReadLexeme(f1, buffer1, sizeof(buffer1), &overflow);
    if (st1 == ERROR_NULL || st1 == ERROR_INPUT) {
        fclose(f1); fclose(f2); fclose(out);
        return st1;
    }
    if (st1 == ERROR_MEMORY) {
        fclose(f1); fclose(f2); fclose(out);
        return st1;
    }
    if (st1 == END) end_f1 = 1;

    state st2 = ReadLexeme(f2, buffer2, sizeof(buffer2), &overflow);
    if (st2 == ERROR_NULL || st2 == ERROR_INPUT) {
        fclose(f1); fclose(f2); fclose(out);
        return st2;
    }
    if (st2 == ERROR_MEMORY) {
        fclose(f1); fclose(f2); fclose(out);
        return st2;
    }
    if (st2 == END) end_f2 = 1;

    while (!end_f1 || !end_f2) {
        if (!end_f1 && st1 == OK) {
            if (!first_lex) fprintf(out, " ");
            fprintf(out, "%s", buffer1);
            first_lex = 0;

            st1 = ReadLexeme(f1, buffer1, sizeof(buffer1), &overflow);
            if (st1 == ERROR_NULL || st1 == ERROR_INPUT || st1 == ERROR_MEMORY) {
                fclose(f1); fclose(f2); fclose(out);
                return st1;
            }
            if (st1 == END) end_f1 = 1;
        } else if (!end_f1 && st1 == END) {
            end_f1 = 1;
        }

        if (!end_f2 && st2 == OK) {
            if (!first_lex) fprintf(out, " ");
            fprintf(out, "%s", buffer2);
            first_lex = 0;

            st2 = ReadLexeme(f2, buffer2, sizeof(buffer2), &overflow);
            if (st2 == ERROR_NULL || st2 == ERROR_INPUT || st2 == ERROR_MEMORY) {
                fclose(f1); fclose(f2); fclose(out);
                return st2;
            }
            if (st2 == END) end_f2 = 1;
        } else if (!end_f2 && st2 == END) {
            end_f2 = 1;
        }
    }

    if (ferror(f1)) {
        fclose(f1); fclose(f2); fclose(out);
        return ERROR_FILE;
    }
    if (ferror(f2)) {
        fclose(f1); fclose(f2); fclose(out);
        return ERROR_FILE;
    }

    fclose(f1);
    fclose(f2);
    fclose(out);
    return OK;
}

state A(const char* inFile, const char* outFile) {
    if (inFile == NULL || outFile == NULL) return ERROR_NULL;

    if (strcmp(inFile, outFile) == 0) {
        return ERROR_INPUT;
    }

    FILE* in = fopen(inFile, "r");
    if (in == NULL) {
        return ERROR_FILE;
    }

    FILE* out = fopen(outFile, "w");
    if (out == NULL) {
        fclose(in);
        return ERROR_FILE;
    }

    char buffer[4096];
    int lex_count = 0;
    int first_lex = 1;
    int overflow = 0;
    state st;

    while ((st = ReadLexeme(in, buffer, sizeof(buffer), &overflow)) == OK) {
        if (overflow) {
            fclose(in);
            fclose(out);
            return ERROR_MEMORY;
        }

        lex_count++;

        if (!first_lex) {
            fprintf(out, " ");
        }
        first_lex = 0;

        if (lex_count % 10 == 0) {
            ToLower(buffer);
            for (int i = 0; buffer[i] != '\0'; ++i) {
                PrintASCIIInBase(out, (unsigned char)buffer[i], 4);
            }
        } else if (lex_count % 2 == 0 && lex_count % 5 != 0) {
            ToLower(buffer);
            fprintf(out, "%s", buffer);
        } else if (lex_count % 5 == 0 && lex_count % 10 != 0) {
            for (int i = 0; buffer[i] != '\0'; ++i) {
                PrintASCIIInBase(out, (unsigned char)buffer[i], 8);
            }
        } else {
            fprintf(out, "%s", buffer);
        }
    }

    if (st == ERROR_NULL || st == ERROR_INPUT || st == ERROR_MEMORY) {
        fclose(in);
        fclose(out);
        return st;
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
    if (argc < 3) {
        fprintf(stderr, "Использование:\n");
        fprintf(stderr, "  %s -r <file1> <file2> <output_file>\n", argv[0]);
        fprintf(stderr, "  %s -a <input_file> <output_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    char flag = '\0';
    state st = Flag(argv[1], &flag);

    if (st != OK) {
        fprintf(stderr, "Ошибка: некорректный флаг '%s'\n", argv[1]);
        fprintf(stderr, "Допустимые флаги: -r, -a, /r, /a\n");
        return EXIT_FAILURE;
    }

    if (flag == 'r') {
        if (argc != 5) {
            fprintf(stderr, "Ошибка: для флага -r требуется 4 аргумента (флаг, файл1, файл2, выходной_файл)\n");
            return EXIT_FAILURE;
        }

        st = R(argv[2], argv[3], argv[4]);

        if (st != OK) {
            switch (st) {
                case ERROR_NULL:
                    fprintf(stderr, "Ошибка: NULL-указатель\n");
                    break;
                case ERROR_INPUT:
                    fprintf(stderr, "Ошибка: входные файлы совпадают или некорректный ввод\n");
                    break;
                case ERROR_FILE:
                    fprintf(stderr, "Ошибка: проблема с файлом\n");
                    break;
                case ERROR_MEMORY:
                    fprintf(stderr, "Ошибка: буфер переполнен (лексема слишком длинная)\n");
                    break;
                default:
                    fprintf(stderr, "Неизвестная ошибка\n");
                    break;
            }
            return EXIT_FAILURE;
        }

        printf("Флаг -r: лексемы успешно переплетены и записаны в '%s'\n", argv[4]);
    } else if (flag == 'a') {
        if (argc != 4) {
            fprintf(stderr, "Ошибка: для флага -a требуется 3 аргумента (флаг, входной_файл, выходной_файл)\n");
            return EXIT_FAILURE;
        }

        st = A(argv[2], argv[3]);

        if (st != OK) {
            switch (st) {
                case ERROR_NULL:
                    fprintf(stderr, "Ошибка: NULL-указатель\n");
                    break;
                case ERROR_INPUT:
                    fprintf(stderr, "Ошибка: входной и выходной файлы совпадают или некорректный ввод\n");
                    break;
                case ERROR_FILE:
                    fprintf(stderr, "Ошибка: проблема с файлом\n");
                    break;
                case ERROR_MEMORY:
                    fprintf(stderr, "Ошибка: буфер переполнен (лексема слишком длинная)\n");
                    break;
                default:
                    fprintf(stderr, "Неизвестная ошибка\n");
                    break;
            }
            return EXIT_FAILURE;
        }

        printf("Флаг -a: лексемы успешно обработаны и записаны в '%s'\n", argv[3]);
    }

    return EXIT_SUCCESS;
}