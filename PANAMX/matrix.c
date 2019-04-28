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

void freeMatrix(matrix m) {
    free(m);
}

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

// returns the sum of all values in the matrix m; for now, only works for
// double type
double sum(matrix m) {
    // total starts at zero; handles error cases
    double s = 0;
    if(m == NULL || m->mat == NULL) {
        perror("Empty Matrix");
        exit(1);
    }
    else {
	// iterate through all entries
        for (int i = 0; i < m->row; i++) {
            for (int j = 0; j < m->col; j++) {
		        // add each entry to running total
                s = s + m->mat[i][j];
              }
 	    }
    }
    return s;
}

// returns the average of all entries in a matrix m; for now, only works for 
// double type
double mean(matrix m) {
    double avg = 0;
    double tot = 0;
    if(m == NULL || m->mat == NULL) {
        perror("Empty Matrix");
        exit(1);
    }
    else {
        // iterate through all entries
	for (int i = 0; i < m->row; i++) {
            for (int j = 0; j < m->col; j++) {
		// avg keeps track of the sum; tot keeps track of the size
                tot++;
                avg = avg + m->mat[i][j];
	        }
	    }
    }
    // average = total / size
    avg = avg / tot;
    return avg;
}

matrix trans(matrix m) {
    // if the matrix m does not exist, return NULL
    if(m == NULL || m->mat == NULL) {
        printf("Empty Matrix\n");
	return NULL;
    }
    // all other cases, transpose by swapping elements accordingly
    else {
	// create new matrix with all entries initialized to zero
        matrix new = buildMatrixEmpty(m->col, m->row);
	// iterate through all entries to initialize correct values
        for (int i = 0; i < new->row; i++) {
            for (int j = 0; j < new->col; j++) {
                // transpose each value
                new->mat[i][j] = m->mat[j][i];
            }
        }
	return new;
    }    
}

double* eig(matrix m) {
    return NULL;
}

double det(matrix m) {
    return 0;
}

double rank(matrix m) {
    return 0;
}

matrix rref(matrix m) {
    return NULL;
}
