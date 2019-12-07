#include <stdio.h>
#include <stdlib.h>
#include "prim_ops.h"

// Compile with
// clang-7 prim_ops.c test_prim_ops.c -o test_prim_ops
// This is a simple test file, the expected results are printed after the computed value.

int main() {

    struct string* r = create_string_len(0);
    struct string* s1;
    struct string* s2;
    struct string* s3;
    struct string* s4;

    s1 = create_string_copy("Hello.");
    s2 = create_string_copy("World");
    s3 = create_string_len(0);
    s4 = create_string_copy("");

    _prim_string_print(_prim_string_concat(s1,s2));
    printf(" == Hello.World\n");
    _prim_string_print(_prim_string_concat(s1,s3));
    printf(" == Hello.\n");
    _prim_string_print(_prim_string_concat(s3,s2));
    printf(" == World\n");
    _prim_string_print(_prim_string_concat(s1,s4));
    printf(" == Hello.\n");
    _prim_string_print(_prim_string_concat(s4,s2));
    printf(" == World\n");
    _prim_string_print(_prim_string_concat(s3,s4));
    printf(" == \n");
    _prim_string_print(_prim_string_concat(s3,s4));
    printf(" == \n");

    _prim_string_print(_prim_string_reverse(create_string_copy("")));
    printf(" == \n");
    _prim_string_print(_prim_string_reverse(create_string_copy("a")));
    printf(" == a\n");
    _prim_string_print(_prim_string_reverse(create_string_copy("ab")));
    printf(" == ba \n");
    _prim_string_print(_prim_string_reverse(create_string_copy("abc")));
    printf(" == cba\n");

    printf("%ld == 1\n", _prim_string_eq(s1,s1));
    printf("%ld == 1\n", _prim_string_eq(s2,s2));
    printf("%ld == 1\n", _prim_string_eq(s3,s3));
    printf("%ld == 1\n", _prim_string_eq(s4,s4));
    printf("%ld == 1\n", _prim_string_eq(s3,s4));
    printf("%ld == 0\n", _prim_string_eq(s1,s2));
    printf("%ld == 0\n", _prim_string_eq(s3,s1));

    printf("%c == H\n", (char)_prim_string_head(s1));

    _prim_string_print(_prim_string_tail(create_string_copy("a")));
    printf(" == \n");
    _prim_string_print(_prim_string_tail(create_string_copy("ab")));
    printf(" == b\n");
    _prim_string_print(_prim_string_tail(create_string_copy("abc")));
    printf(" == bc\n");

    _prim_string_print(_prim_string_cons(65, s3));
    printf(" == A\n");
    _prim_string_print(_prim_string_cons(65, create_string_copy("b")));
    printf(" == Ab\n");
    _prim_string_print(_prim_string_cons(65, create_string_copy("bc")));
    printf(" == Abc\n");

    printf("%ld == 0\n", _prim_string_lt(create_string_copy(""), create_string_copy("")));
    printf("%ld == 1\n", _prim_string_lt(create_string_copy(""), create_string_copy("a")));
    printf("%ld == 0\n", _prim_string_lt(create_string_copy("a"), create_string_copy("a")));
    printf("%ld == 1\n", _prim_string_lt(create_string_copy("a"), create_string_copy("aa")));
    printf("%ld == 1\n", _prim_string_lt(create_string_copy("aa"), create_string_copy("ab")));
    printf("%ld == 1\n", _prim_string_lt(create_string_copy("aa"), create_string_copy("ab")));
    printf("%ld == 1\n", _prim_string_lt(create_string_copy("aaa"), create_string_copy("ab")));
    printf("%ld == 0\n", _prim_string_lt(create_string_copy("aaa"), create_string_copy("")));
    printf("%ld == 0\n", _prim_string_lt(create_string_copy("bbb"), create_string_copy("aaa")));

    _prim_string_print(_prim_int_str(0));
    printf(" == 0\n");
    _prim_string_print(_prim_int_str(10));
    printf(" == 10\n");
    _prim_string_print(_prim_int_str(-10));
    printf(" == -10\n");

    printf("%ld == 0\n", _prim_str_int(create_string_copy("0")));
    printf("%ld == 10\n", _prim_str_int(create_string_copy("10")));
    printf("%ld == +10\n", _prim_str_int(create_string_copy("+10")));
    printf("%ld == -10\n", _prim_str_int(create_string_copy("-10")));

    _prim_string_print(_prim_float_string(0.0));
    printf(" == 0.0\n");
    _prim_string_print(_prim_float_string(10.123));
    printf(" == 10.123\n");
    _prim_string_print(_prim_float_string(-10.34));
    printf(" = -10.34\n");

    printf("%d == 0\n", feof(stdin));

    return 0;
}
