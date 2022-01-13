#include <stdio.h>
#include <stdlib.h>
#include "BitArray/bit_array.h"

typedef struct seafloor {
    BIT_ARRAY** easters;
    BIT_ARRAY** southers;
    int rows;
    int cols;
} Seafloor;

void die(char* message) {
    fprintf(stderr, "%s\n", message);
    exit (1);
}

Seafloor* load_floor(char* filename, int rows, int cols) {
    FILE *in;
    Seafloor* floor = malloc(sizeof(Seafloor));
    if (! floor) die("malloc");
    int c, row, col;
    BIT_ARRAY* tmp;

    floor->easters  = malloc(rows * sizeof(BIT_ARRAY*));
    floor->southers = malloc(rows * sizeof(BIT_ARRAY*));
    for (int i = 0; i < rows; i++) floor->easters[i]  = bit_array_create(cols);
    for (int i = 0; i < rows; i++) floor->southers[i] = bit_array_create(cols);
    floor->cols = cols;
    floor->rows = rows;

    if (! (in = fopen(filename, "r"))) die("fopen");

    row = col = 0;
    while ((c = fgetc(in)) != EOF) {
        if (c == '\n') {
            row++;
            col = 0;
            continue;
        }
        if (c == '.') {
            col++;
            continue;
        }
        if (c == '>') tmp = floor->easters[row];
        else if (c == 'v') tmp = floor->southers[row];
        else die("bad input char");

        bit_array_set_bit(tmp, col);
        col++;
    }

    return floor;
}

void print_row(int row, Seafloor* floor) {
    for (int col = 0; col < floor->cols; col++) {
        char e = bit_array_get_bit(floor->easters[row], col);
        if (e) {
            putchar('>');
        }
        else {
            char s = bit_array_get_bit(floor->southers[row], col);
            if (s) putchar('v');
            else putchar('.');
        }
    }
    putchar('\n');
}

void print_floor(Seafloor* floor) {
    for (int row = 0; row < floor->rows; row++) {
        print_row(row, floor);
    }
    putchar('\n');
}

int east_shift_floor(Seafloor* floor) {
    int changed = 0;
    for (int row = 0; row < floor->rows; row++) {
        int first_set = 0;
        for (int col = 0; col < floor->cols; col++) {
            if (bit_array_get(floor->easters[row], col)) {
                if (col == 0) first_set = 1;
                int nxt_col = col + 1;
                if (nxt_col == floor->cols) nxt_col = 0; // wrap to beginning of row
                if (bit_array_get(floor->easters[row], nxt_col) == 0
                    && bit_array_get(floor->southers[row], nxt_col) == 0) {
                    if (! (nxt_col == 0 && first_set)) {
                        bit_array_clear_bit(floor->easters[row], col);
                        bit_array_set(floor->easters[row], nxt_col);
                        col++; // skip next check since we just moved there
                        changed++;
                    }
                }
            }
        }
    }
    return changed;
}

int south_shift_floor(Seafloor* floor) {
    int changed = 0;

    for (int col = 0; col < floor->cols; col++) {
        int first_set = 0;
        for (int row = 0; row < floor->rows; row++) {
            if (bit_array_get(floor->southers[row], col)) {
                if (row == 0) first_set = 1;
                int nxt_row = row + 1;
                if (nxt_row == floor->rows) nxt_row = 0; // wrap to top
                if (bit_array_get(floor->southers[nxt_row], col) == 0
                    && bit_array_get(floor->easters[nxt_row], col) == 0) {
                    if (! (nxt_row == 0 && first_set)) {
                        bit_array_clear_bit(floor->southers[row], col);
                        bit_array_set(floor->southers[nxt_row], col);
                        row++; // skip next check since we just moved there
                        changed++;
                    }
                }
            }
        }
    }

    return changed;
}

int main() {
    Seafloor* seafloor = load_floor("input.txt", 137, 139);
    // Seafloor* seafloor = load_floor("test_input.txt", 9, 10);

    int steps = 0;
    int changes = -1;
    while (changes != 0) {
        changes = east_shift_floor(seafloor);
        changes += south_shift_floor(seafloor);
        steps++;
    }
    print_floor(seafloor);
    printf("Num steps to gridlock: %d\n", steps);

    return 0;
}
