#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <stdbool.h>
#include <string.h>
#include <unistd.h>
#include <math.h>
#include "prim_ops.h"


#define BUFFER_SIZE 256

/*
NOTES:

 * In error cases we just simple exit as this part is still under active development.
 * _prim_ffi_file_eof is a placeholder implementation.

*/


struct string* create_string_len(int64_t l) {
    struct string* r = (struct string*)malloc(sizeof(struct string));
    r->data = (char*)calloc(sizeof(char), l * sizeof(char));
    r->length = l;
#ifdef DEBUG
    printf("create_string_len(%ld) = %d\n", l, (int)r);
#endif
    return r;
}

struct string* create_string_copy(char* str) {
    struct string* r = (struct string*)malloc(sizeof(struct string));
    int64_t l = strlen(str);
    r->data = (char*)malloc(l * sizeof(char));
    strncpy(r->data, str, l);
    r->length = l;
#ifdef DEBUG
    printf("create_string_copy(\"%s\") = %d\n", str, (int)r);
#endif
    return r;
}

void cstring(char* buffer, struct string* s){
    memcpy(buffer, s->data, s->length);
    buffer[s->length] = 0;
#ifdef DEBUG
    printf("cstring(%s, %d) = %d\n", buffer, (int)s, (int)buffer);
#endif
}

void _prim_string_print(struct string* p1){
#ifdef DEBUG
    printf("_prim_string_print(%d)\n", (int)p1);
#endif
    for(int i = 0; i < p1->length; i++) {
        putchar(p1->data[i]);
    }
}

void _prim_int_print(int64_t p1) {
#ifdef DEBUG
    printf("_prim_int_print(%d)\n", (int)p1);
#endif
    printf("%ld", p1);
}

struct string* _prim_read_string() {
    char *buffer = NULL;
    size_t len = 0;
    size_t read;
    read = getline(&buffer, &len, stdin);
    if (read == -1) {
        return create_string_len(0);
    } else {
        struct string* r = create_string_copy(buffer);
        free(buffer);
#ifdef DEBUG
        printf("_prim_string_read() = %d\n", (int)r);
#endif
        return r;
    }
}

void _prim_usleep(int64_t p1) {
#ifdef DEBUG
    printf("_prim_usleep(%ld)\n", p1);
#endif
    usleep(p1); // p1 microseconds
}

void _prim_error(struct string* p1) {
#ifdef DEBUG
    printf("_prim_error(%d)\n", (int)p1);
#endif
    _prim_string_print(p1);
    exit(-1);
}

int64_t _prim_ffi_file_eof(int64_t p1) {
    // Currently this is a placeholder implementation for the idris frontend.
    // In the idris examples only the stdin gets tested for feof so p1 is ignored by now.
    // Appropiate file handling will be implemented later on.
#ifdef DEBUG
    printf("_prim_ffi_file_eof(%ld)\n", p1);
#endif
    return feof(stdin);
}

struct string* _prim_string_concat(struct string* p1, struct string* p2) {
    struct string* r = create_string_len(p1->length + p2->length);
    memcpy(r->data, p1->data, p1->length);
    memcpy(r->data + p1->length, p2->data, p2-> length);
#ifdef DEBUG
    printf("_prim_string_concat(%d,%d) = %d\n", (int)p1, (int)p2, (int)r);
#endif
    return r;
}

struct string* _prim_string_reverse(struct string* src){
    struct string* dst = create_string_len(src->length);
    for(size_t i = 0; i < src->length; i++) {
        dst->data[i] = src->data[src->length - i - 1];
    }
#ifdef DEBUG
    printf("_prim_string_reverse(%d)\n", (int)src);
#endif
    return dst;
}

int64_t _prim_string_eq(struct string* p1, struct string* p2){
#ifdef DEBUG
    printf("_prim_string_eq(%d,%d)\n", (int)p1, (int)p2);
#endif
    if(p1->length != p2->length) {
        return 0;
    }
    return memcmp(p1->data, p2->data, p1->length) == 0;
}

int64_t _prim_string_lt(struct string* p1, struct string* p2) {
#ifdef DEBUG
    printf("_prim_string_lt(%d,%d)\n", (int)p1, (int)p2);
#endif
    int len = (p1->length < p2->length)?(p1->length):(p2->length);
    int cmp = memcmp(p1->data,p2->data, len);
    if (p1->length < p2->length) {
        return (int64_t)(cmp <= 0);
    } else {
        return (int64_t)(cmp < 0);
    }
}

int64_t _prim_string_head(struct string* p1) {
#ifdef DEBUG
    printf("_prim_string_head(%d)\n", (int)p1);
#endif
    if (p1->length == 0) {
        printf("_prim_string_head\n");
        exit(-1);
    }
    return (int64_t)p1->data[0];
}

int64_t _prim_string_len(struct string* p1) {
#ifdef DEBUG
    printf("_prim_string_len(%d) = %ld\n", (int)p1, p1 -> length);
#endif
    return p1->length;
}

struct string* _prim_string_tail(struct string* p1){
    if(p1->length == 0) {
        printf("_prim_string_tail\n");
        exit(-1);
    }
    struct string* r = create_string_len(p1->length - 1);
    memcpy(r->data, p1->data + 1, r->length);
#ifdef DEBUG
    printf("_prim_string_tail(%d) = %d\n", (int)p1, (int)r);
#endif
    return r;
}

struct string* _prim_string_cons(int64_t p1, struct string* p2){
    struct string* r = create_string_len(p2->length + 1);
    r->data[0] = (char)p1;
    memcpy(r->data+1,p2->data,p2->length);
#ifdef DEBUG
    printf("_prim_string_cons(%ld, %d) = %d\n", p1, (int)p2, (int)r);
#endif
    return r;
}

struct string* _prim_int_str(int64_t p1){
#ifdef DEBUG
    printf("_prim_int_str(%ld)\n", p1);
#endif
    char buffer[BUFFER_SIZE];
    int len = snprintf(buffer, BUFFER_SIZE, "%ld", p1);
    if (len >= 0 && len < BUFFER_SIZE) {
        return create_string_copy(buffer);
    } else {
        printf("_prim_int_str\n");
        exit(-1);
    }
}

int64_t _prim_str_int(struct string* p1) {
#ifdef DEBUG
    printf("_prim_str_int(%d)\n", (int)p1);
#endif
    char buffer[p1->length+1];
    cstring(buffer, p1);
    int64_t r = strtoll(buffer, NULL, 10);
    return r;
}

float _prim_int_float(int64_t p1) {
#ifdef DEBUG
    printf("_prim_int_float(%ld)\n", p1);
#endif
    return (float)p1;
}

struct string* _prim_float_string(float p1) {
#ifdef DEBUG
    printf("_prim_float_string(%f)\n", p1);
#endif
    char buffer[BUFFER_SIZE];
    int len = snprintf(buffer, BUFFER_SIZE, "%.13g", p1);
    if (len >= 0 && len < BUFFER_SIZE) {
        return create_string_copy(buffer);
    } else {
        printf("_prim_float_string\n");
        exit(-1);
    }
}

double _prim_int_double(int64_t p1) {
#ifdef DEBUG
    printf("_prim_int_double(%ld)\n", p1);
#endif
    return (double)p1;
}

int64_t _prim_double_int(double p1){
#ifdef DEBUG
    printf("_prim_double_int(%ld)\n", p1);
#endif
    return (int64_t)p1;
}

struct string* _prim_double_string(double p1) {
#ifdef DEBUG
    printf("_prim_double_string(%f)\n", p1);
#endif
    char buffer[BUFFER_SIZE];
    int len = snprintf(buffer, BUFFER_SIZE, "%.13g", p1);
    if (len >= 0 && len < BUFFER_SIZE) {
        return create_string_copy(buffer);
    } else {
        printf("_prim_double_string\n");
        exit(-1);
    }
}

double _prim_double_exp(double p1){
#ifdef DEBUG
    printf("_prim_double_exp(%f)\n", p1);
#endif
    return exp(p1);
}

double _prim_double_log(double p1){
#ifdef DEBUG
    printf("_prim_double_log(%f)\n", p1);
#endif
    return log(p1);
}

double _prim_double_sin(double p1){
#ifdef DEBUG
    printf("_prim_double_sin(%f)\n", p1);
#endif
    return sin(p1);
}

double _prim_double_cos(double p1){
#ifdef DEBUG
    printf("_prim_double_cos(%f)\n", p1);
#endif
    return cos(p1);
}

double _prim_double_tan(double p1){
#ifdef DEBUG
    printf("_prim_double_tan(%f)\n", p1);
#endif
    return tan(p1);
}

double _prim_double_asin(double p1){
#ifdef DEBUG
    printf("_prim_double_asin(%f)\n", p1);
#endif
    return asin(p1);
}

double _prim_double_acos(double p1){
#ifdef DEBUG
    printf("_prim_double_acos(%f)\n", p1);
#endif
    return acos(p1);
}

double _prim_double_atan(double p1){
#ifdef DEBUG
    printf("_prim_double_atan(%f)\n", p1);
#endif
    return atan(p1);
}

double _prim_double_sqrt(double p1){
#ifdef DEBUG
    printf("_prim_double_sqrt(%f)\n", p1);
#endif
    return sqrt(p1);
}

double _prim_double_floor(double p1){
#ifdef DEBUG
    printf("_prim_double_floor(%f)\n", p1);
#endif
    return floor(p1);
}

double _prim_double_ceil(double p1){
#ifdef DEBUG
    printf("_prim_double_ceil(%f)\n", p1);
#endif
    return ceil(p1);
}

double _prim_double_negate(double p1){
#ifdef DEBUG
    printf("_prim_double_negate(%f)\n", p1);
#endif
    return ((-1.0) * p1);
}

double _prim_double_atan2(double p1, double p2){
#ifdef DEBUG
    printf("_prim_double_atan2(%f, %f)\n", p1, p2);
#endif
    return atan2(p1,p2);
}

int64_t _prim_char_int(char p1) {
#ifdef DEBUG
    printf("_prim_char_int(%c)\n", p1);
#endif
    return (int64_t)p1;
}
