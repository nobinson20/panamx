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
    m->mat = (double **)malloc(sizeof(double *) * row);
    for (int i = 0; i < row; i++) {
        m->mat[i] = (double *)malloc(sizeof(double) * col);
    }
    return m;
}

matrix buildMatrix(int row, int col, double arr[][col]) {
    // char str[20];
    // sprintf(str, "%d %d\n", row, col);
    // perror(str);
    matrix m = initMatrix(row, col);
    // char str[20];
    // sprintf(str, "%f\n", m->mat[0][0]);
    // perror(str);
    for (int i = 0; i < row; i++) {
        for (int j = 0; j < col; j++) {
            // char str[20];
            // sprintf(str, "%d %d %f\n", i, j, arr[i][j]);
            // perror(str);
            m->mat[i][j] = arr[i][j];
            // char str[20];
            // sprintf(str, "%d %d %f\n", i, j, arr[i][j]);
            // perror(str);
        }
    }
    // char str[20];
    // sprintf(str, "%f\n", m->mat[0][0]);
    // perror(str);
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

double matrixAccess(matrix m, int row, int col) {
    if (m->row <= row || m->col <= col) {
        perror("matrix index out of bound");
        exit(1);
    }
    return m->mat[row][col];
}

double matrixAssign(matrix m, int i, int j, double val) {
    if (m->row <= i || m->col <= j) {
        perror("matrix index out of bound");
        exit(1);
    }
    m->mat[i][j] = val;
    return val;
}
