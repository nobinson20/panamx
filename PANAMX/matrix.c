#include <stdlib.h>
#include <stdio.h>

typedef struct Matrix {
    int row;
    int col;
    double **mat;
} *matrix;

matrix initMatrix(int row, int col) {
    matrix m = (matrix)malloc(sizeof(struct Matrix));
    m->row = row;
    m->col = col;
    double **mat = (double **)malloc(sizeof(double) * row);
    for (int i = 0; i < row; i++) {
        mat[i] = (double *)malloc(sizeof(double) * col);
    }
    m->mat = mat;
    return m;
}

matrix buildMatrix(double **mat, int row, int col) {
    matrix m = initMatrix(row, col);
    for (int i = 0; i < row; i++) {
        for (int j = 0; j < col; j++) {
            m->mat[i][j] = mat[i][j];
        }
    }
    return m;
}

matrix buildMatrixEmpty(int row, int col) {
    matrix m = initMatrix(row, col);
    for (int i = 0; i < row; i++) {
        for (int j = 0; j < col; j++) {
            m->mat[i][j] = 0;
        }
    }
    return m;
}