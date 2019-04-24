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

matrix buildMatrix(int row, int col, double *arr) {
    matrix m = initMatrix(row, col);
    for (int i = 0; i < row; i++) {
        for (int j = 0; j < col; j++) {
            m->mat[i][j] = *(arr + i * col + j);
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

void printMatrix(matrix m) {
    if (m == NULL || m->mat == NULL) {
        printf("Empty Matrix\n");
    }
    else {
        printf("[\n");
        for (int i = 0; i < m->row; i++) {
            printf("\t");
            for (int j = 0; j < m->col; j++) {
                printf("%.3f   ", m->mat[i][j]);
            }
            printf("\n");
        }
        printf("]\n");
    }
}
/*
void freeMatrix(matrix m) {
    free(m);
}
*/
int getHeight(matrix m) {
    if (m == NULL || m->mat == NULL)
        return 0;
    else
        return m->row;
}

int getWidth(matrix m) {
    if (m == NULL || m->mat == NULL)
        return 0;
    else
        return m->col;
}
