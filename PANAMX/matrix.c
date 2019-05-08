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
        perror("Matrix Index Out of Bounds");
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

// helper function for rref() function, swaps rows of given matrix by reference
static void swap(matrix *m, int a, int b) {
	double temp;
	if (m == NULL | (*m)->mat == NULL) {
		perror("Empty Matrix");
		exit(1);	
	}
	else if ((*m)->row <= a || (*m)->col <= b) {
		perror("Matrix Index Out of Bounds");
		exit(1);
	}
	else {
		for (int i = 0; i < (*m)->col; i++) {
			temp = (*m)->mat[i][a];
			(*m)->mat[i][a] = (*m)->mat[i][b];
			(*m)->mat[i][b] = temp;
		}	
	}
}

// helper function for rref() function, divides specified row in matrix by a non-zero scalar
static void divideRow(matrix *m, int row, double scalar) {
	if (m == NULL || (*m)->mat == NULL) {
		perror("Empty Matrix");
		exit(1);
	}
	else if (scalar == 0) {
		perror("Divide By Zero Error");
		exit(1);
	}
	else if ((*m)->row <= row) {
		perror("Matrix Index Out of Bounds");
		exit(1);
	}
	else {
		for (int i = 0; i < (*m)->col; i++) {
			(*m)->mat[i][row] *= scalar;
		}
	}
}

// helper function for rref() function, subtracts a multiple of one row from another
// (e.g., row a = row a - row b * (scalar)
static void subtractRow(matrix *m, int a, int b, double scalar) {
	if (m == NULL || (*m)->mat == NULL) {
        perror("Empty Matrix");
        exit(1);
    }
    else if ((*m)->row < a || (*m)->row < b) { // note: should this be <= 
        perror("Matrix Index Out of Bounds");
        exit(1);
    }
	else {
		for (int i = 0; (*m)->col; i++) {
			(*m)->mat[i][a] -= (*m)->mat[i][b]*scalar;
		}
	}	
}

// swaps rows a and b from the given matrix m
matrix rowSwap(matrix m, int a, int b) {
	// should not work if matrix is empty
	if (m == NULL | m->mat == NULL) {
		perror("Empty Matrix");
		exit(1);
    }
    // should not work for rows out of bounds
    else if (m->row <= a || m->row <= b) {
		perror("Matrix Index Out of Bounds");
		exit(1);
    }
    // should work for all other cases
    else { 
		// create new matrix with all entries initialized to zero
        matrix new = buildMatrixEmpty(m->row, m->col);
        // iterate through all entries to initialize correct values
        for (int i = 0; i < new->row; i++) {
			for (int j = 0; j < new->col; j++) {
				if (i == a) {
					new->mat[i][j] = m->mat[b][j];
				}
				else if (i == b) {
					new->mat[i][j] = m->mat[a][j];
				}
				else { 
					new->mat[i][j] = m->mat[i][j];
				}
			}
		}
        return new;
	}
}

// swaps columns a and b from the given matrix m
matrix colSwap(matrix m, int a, int b) {
	// should not work if matrix is empty
    if (m == NULL | m->mat == NULL) {
        perror("Empty Matrix");
        exit(1);
    }
    // should not work for columns out of bounds
    else if (m->col <= a || m->col <= b) {
        perror("Matrix Index Out of Bounds");
        exit(1);
    }
    // should work for all other cases
    else {
		// create new matrix with all entries initialized to zero
		matrix new = buildMatrixEmpty(m->row, m->col);
        // iterate through all entries to initialize correct values
		for (int i = 0; i < new->row; i++) {
			for (int j = 0; j < new->col; j++) {
				if (j == a) {
					new->mat[i][j] = m->mat[i][b];
				}
				else if (i == b) {
					new->mat[i][j] = m->mat[i][b];
				}
				else {
					new->mat[i][j] = m->mat[i][j];
				}
			}
		}
        return new;
	}
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

// returns an "array" (e.g., 1 x N matrix) of eigenvalues for given matrix
matrix eig(matrix m) {
    // TODO	
    return NULL;
}

// returns the determinant of given matrix
double det(matrix m) {
    // TODO
    return 0;
}

// returns inverse matrix if invertible 
matrix inv(matrix m) {
    // TODO
    return NULL;
}

// returns matrix in reduced row echelon form
matrix rref(matrix m) {
    if (m == NULL | m->mat == NULL) {
        perror("Empty Matrix");
        exit(1);
    }
	// create new matrix with all entries initialized to zero
	matrix new = buildMatrixEmpty(m->col, m->row);
	// iterate through all entries to initialize correct values
	for (int i = 0; i < new->row; i++) {
		for (int j = 0; j < new->col; j++) {
			// transpose each value
			new->mat[i][j] = m->mat[i][j];
		}
	}
	int i = 0;
	int j = 0;
	while (i < new->row && j < new->col) {
		int srow;
		// if the entry new[i][j] is zero, iterate through the column to find a non-zero value
		if (new->mat[i][j] == 0) {
			srow = -1;
			int k = i + 1;
			while (k < new->row && srow == -1)
			{
				if (new->mat[k][j] != 0) {
					srow = k;
				}
				k++;
			} 
			// move to next column if all zero
			if (srow == -1) {
				j++;
			}
			// ... and swap the row containing that non-zero value
			else {
				swap(&new, i, srow);
				divideRow(&new, i, new->mat[i][j]);
					k = i + 1;
				while (k < new->row) {
					if (new->mat[k][j] != 0) {
						subtractRow(&new, k, i, new->mat[k][j]);
					}
					k++;
				}
				j++;
				i++;
			}
		}
		// entry new[i][j] is non-zero, normalize
		else {
			divideRow(&new, i, new->mat[i][j]);
			int k = i + 1;
			while(k < new->row) {
				if(new->mat[k][j] != 0) {
					subtractRow(&new, k, i, new->mat[k][j]);
				}
				k++;
			}
			j++;
			i++;
		}
	}
	return new;
}

// returns rank of a given matrix
double rank(matrix m) {
        // get the reduced row echelon form of given matrix
    matrix new = rref(m);
    double rnk = 0;
        // count the number of non-zero rows
        for (int i = 0; i < new->row; i++) {
                int zero = 1;
                for (int j = 0; j < new->col; j++) {
                        if (new->mat[i][j] != 0) {
                                zero = 0;
                        }
                }
                if (zero == 0) {
                        rnk++;
                }
        }
        return rnk;
}


