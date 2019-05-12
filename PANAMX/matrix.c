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

matrix matrixSlice(matrix m, int s1, int e1, int s2, int e2) {
  if (m == NULL || m->mat == NULL) {
    perror("Empty Matrix");
    exit(1);
  }
  if (m->row < e1 || m->col < e2) {
    perror("matrix index out of bound");
    exit(1);
  }
  int row = e1 - s1;
  int col = e2 - s2;
  if (row <= 0 || col <= 0) {
    perror("Matrix dimensions should be greater than zero");
    exit(1);
  }
  matrix tmp = initMatrix(row, col);
  for (int i = s1, k = 0; i < e1; i++, k++) {
    for (int j = s2, l = 0; j < e2; j++, l++) {
      tmp->mat[k][l] = m->mat[i][j];
    }
  }
  return tmp;
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

matrix addMatrixDouble(matrix m, double n, int minus) {
  if (m == NULL || m->mat == NULL) {
    perror("Empty Matrix");
    exit(1);
  }
  if (minus) {
    n = -n;
  }
  int h = m->row, w = m->col;
  matrix result = initMatrix(h, w);
  for (int i = 0; i < h; i++) {
    for (int j = 0; j < w; j++) {
      result->mat[i][j] = m->mat[i][j] + n;
    }
  }
  return result;
}

matrix subDoubleMatrix(double n, matrix m) {
  if (m == NULL || m->mat == NULL) {
    perror("Empty Matrix");
    exit(1);
  }
  int h = m->row, w = m->col;
  matrix result = initMatrix(h, w);
  for (int i = 0; i < h; i++) {
    for (int j = 0; j < w; j++) {
      result->mat[i][j] = n - m->mat[i][j];
    }
  }
  return result;
}

matrix addMatrixMatrix(matrix m, matrix n, int minus) {
  if (m == NULL || m->mat == NULL || n == NULL || n->mat == NULL) {
    perror("Empty Matrix");
    exit(1);
  }
  if (m->row != n->row || m->col != n->col) {
    perror("Error in matrix addition: dimension mismatched");
    exit(1);
  }
  int h = m->row, w = m->col;
  matrix result = initMatrix(h, w);
  if (minus) {
    for (int i = 0; i < h; i++)
    for (int j = 0; j < w; j++)
    result->mat[i][j] = m->mat[i][j] - n->mat[i][j];
  }
  else {
    for (int i = 0; i < h; i++)
    for (int j = 0; j < w; j++)
    result->mat[i][j] = m->mat[i][j] + n->mat[i][j];
  }
  return result;
}

matrix mulMatrixDouble(matrix m, double n, int div) {
  if (m == NULL || m->mat == NULL) {
    perror("Empty Matrix");
    exit(1);
  }
  int h = m->row, w = m->col;
  matrix result = initMatrix(h, w);
  if (div) {
    n = 1 / n;
  }
  for (int i = 0; i < h; i++) {
    for (int j = 0; j < w; j++) {
      result->mat[i][j] = m->mat[i][j] * n;
    }
  }
  return result;
}

matrix mulMatrixMatrix(matrix m, matrix n) {
  if (m == NULL || m->mat == NULL || n == NULL || n->mat == NULL) {
    perror("Empty Matrix");
    exit(1);
  }
  if (m->col != n->row) {
    perror("Error in matrix multiplication: dimension mismatched");
    exit(1);
  }
  int h = m->row, w = n->col, p = m->col;
  matrix result = initMatrix(h, w);
  for (int i = 0; i < h; i++) {
    for (int j = 0; j < w; j++) {
      double tmp = 0;
      for (int k = 0; k < p; k++) {
        tmp += m->mat[i][k] * n->mat[k][j];
      }
      result->mat[i][j] = tmp;
    }
  }
  return result;
}

matrix mulElementWiseMatrix(matrix m, matrix n, int div) {
  if (m == NULL || m->mat == NULL || n == NULL || n->mat == NULL) {
    perror("Empty Matrix");
    exit(1);
  }
  if (m->row != n->row || m->col != n->col) {
    perror("Error in element wise operation: dimension mismatched");
    exit(1);
  }
  int h = m->row, w = m->col;
  matrix result = initMatrix(h, w);
  if (div) {
    for (int i = 0; i < h; i++)
    for (int j = 0; j < w; j++)
    result->mat[i][j] = m->mat[i][j] / n->mat[i][j];
  }
  else {
    for (int i = 0; i < h; i++)
    for (int j = 0; j < w; j++)
    result->mat[i][j] = m->mat[i][j] * n->mat[i][j];
  }
  return result;
}

// helper function for rref() function, swaps rows of given matrix by reference
static void swap(matrix m, int a, int b) {
  double temp;
  if (m == NULL || m->mat == NULL) {
    perror("Empty Matrix");
    exit(1);
  }
  else if (m->row <= a || m->col <= b) {
    perror("Matrix Index Out of Bounds");
    exit(1);
  }
  else {
    for (int i = 0; i < m->col; i++) {
      temp = m->mat[i][a];
      m->mat[i][a] = m->mat[i][b];
      m->mat[i][b] = temp;
    }
  }
}

// helper function for rref() function, divides specified row in matrix by a non-zero scalar
static void divideRow(matrix m, int row, double scalar) {
  if (m == NULL || m->mat == NULL) {
    perror("Empty Matrix");
    exit(1);
  }
  else if (scalar == 0) {
    perror("Divide By Zero Error");
    exit(1);
  }
  else if (m->row <= row) {
    perror("Matrix Index Out of Bounds");
    exit(1);
  }
  else {
    for (int i = 0; i < m->col; i++) {
      m->mat[i][row] *= scalar;
    }
  }
}

// helper function for rref() function, subtracts a multiple of one row from another
// (e.g., row a = row a - row b * (scalar)
static void subtractRow(matrix m, int a, int b, double scalar) {
  if (m == NULL || m->mat == NULL) {
    perror("Empty Matrix");
    exit(1);
  }
  else if (m->row <= a || m->row <= b) { // note: should this be <=
    perror("Matrix Index Out of Bounds");
    exit(1);
  }
  else {
    for (int i = 0; m->col; i++) {
      m->mat[i][a] -= m->mat[i][b]*scalar;
    }
  }
}

// swaps rows a and b from the given matrix m
matrix rowSwap(matrix m, int a, int b) {
  // should not work if matrix is empty
  if (m == NULL || m->mat == NULL) {
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
  if (m == NULL || m->mat == NULL) {
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

// returns nxn identity matrix given n
matrix iden(int n) {
  matrix empty = buildMatrixEmpty(n, n);
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      if (i == j) {
        empty->mat[i][j] = 1;
      }
    }
  }
  return empty;
}

// returns an "array" (e.g., 1 x N matrix) of eigenvalues for given matrix
matrix eig(matrix m) {
  if (m == NULL | m->mat == NULL) {
    perror("Empty Matrix");
    exit(1);
  }
  else if (m->row != m->col) {
    perror("Cannot find eigenvalues of non-square matrices.");
    exit(1);
  }
  double lambda;
  int n = m->row;
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      if (i == j) {
        m->mat[i][j] = m->mat[i][j] - lambda;
      }
    }
  }
  return m;
}

void getCofactor(matrix m, matrix tmp, int p, int q, int n) {
  int i = 0, j = 0;
  for (int row = 0; row < n; row++) {
    for (int col = 0; col < n; col++) {
      if (row != p && col != q) {
        tmp->mat[i][j++] = m->mat[row][col];
        if (j == n - 1) {
          j = 0;
          i++;
        }
      }
    }
  }
}

double detOfMatrix(matrix m, int n) {
  double D = 0;
  if (n == 1) {
    return m->mat[0][0];
  }
  int sign = 1;
  matrix tmp = initMatrix(m->row, m->col);
  for (int f = 0; f < n; f++) {
    getCofactor(m, tmp, 0, f, n);
    D += sign * m->mat[0][f] * detOfMatrix(tmp, n-1);
    sign = -sign;
  }
  return D;
}

// returns the determinant of given matrix
double det(matrix m) {
  if (m == NULL | m->mat == NULL) {
    perror("Empty Matrix");
    exit(1);
  }
  else if (m->row != m->col) {
    perror("Cannot find determinant of non-square matrices.");
    exit(1);
  }
  int n = m->col;
  double D = detOfMatrix(m, n);
  return D;
}

// Function to get adjoint of A[N][N] in adj[N][N].
void adjoint(matrix a, matrix adj) {
  if (a->row == 1) {
    adj->mat[0][0] = 1;
    return;
  }
  // temp is used to store cofactors of A[][]
  int sign = 1;
  matrix tmp = initMatrix(a->row, a->col);
  int n = a->row;
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      // Get cofactor of A[i][j]
      getCofactor(a, tmp, i, j, n);

      // sign of adj[j][i] positive if sum of row
      // and column indexes is even.
      sign = ((i+j)%2==0)? 1: -1;

      // Interchanging rows and columns to get the
      // transpose of the cofactor matrix
      adj->mat[j][i] = (sign)*(detOfMatrix(tmp, n-1));
    }
  }
}

// returns inverse matrix if invertible
matrix inv(matrix m) {
  double deter = det(m);
  if (deter == 0) {
    perror("Cannot find inverse of Singular Matrix.");
    exit(1);
  }
  matrix adj = initMatrix(m->row, m->col);
  adjoint(m, adj);

  matrix inv = initMatrix(m->row, m->col);
  for (int i = 0; i < inv->row; i++) {
    for (int j = 0; j < inv->col; j++) {
      inv->mat[i][j] = adj->mat[i][j]/deter;
    }
  }
  return inv;
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
        swap(new, i, srow);
        divideRow(new, i, new->mat[i][j]);
        k = i + 1;
        while (k < new->row) {
          if (new->mat[k][j] != 0) {
            subtractRow(new, k, i, new->mat[k][j]);
          }
          k++;
        }
        j++;
        i++;
      }
    }
    // entry new[i][j] is non-zero, normalize
    else {
      divideRow(new, i, new->mat[i][j]);
      int k = i + 1;
      while(k < new->row) {
        if(new->mat[k][j] != 0) {
          subtractRow(new, k, i, new->mat[k][j]);
        }
        k++;
      }
      j++;
      i++;
    }
  }
  return new;
}


// Working RREF implementation
void mulandaddRows(matrix m, int dest, int src, double mplr)
{
  double *drow, *srow;
  drow = m->mat[dest];
  srow = m->mat[src];
  for (int i = 0; i < m->col; i++)
  drow[i] += mplr * srow[i];
}

void swapRows(matrix m, int a, int b) {
  double *r1, *r2, temp;
  if (a == b) return;
  r1 = m->mat[a];
  r2 = m->mat[b];
  for (int i = 0; i < m->col; i++) {
    temp = r1[i];
    r1[i] = r2[i];
    r2[i] = temp;
  }
}

void normalizeRow(matrix m, int row, int lead)
{
  double *drow = m->mat[row];
  double lv = drow[lead];
  for (int i = 0; i < m->col; i++)
  drow[i] /= lv;
}

// returns rref of given matrix m
matrix rrref(matrix m) {
  int i;
  double lv;
  int rowCount = m->row;
  int lead = 0;
  for (int r = 0; r < rowCount; r++) {
    if (lead >= m->col)
    break;
    i = r;
    while (0 == m->mat[i][lead]) {
      i++;
      if (i == rowCount) {
        i = r;
        lead++;
        if (lead == m->col)
        break;
      }
    }
    swapRows(m, i, r);
    normalizeRow(m, r, lead );
    for (i = 0; i < rowCount; i++) {
      if (i != r) {
        lv = m->mat[i][lead];
        mulandaddRows(m, i, r, -lv) ;
      }
    }
    lead++;
  }
  return m;
}


// returns rank of a given matrix
double rank(matrix m) {
  // get the reduced row echelon form of given matrix
  matrix new = rrref(m);
  double rnk = 0;
  // count the number of non-zero rows
  for (int i = 0; i < new->row; i++) {
    int zero = 0;
    for (int j = 0; j < new->col; j++) {
      if (new->mat[i][j] != 0) {
        zero = 1;
        break;
      }
    }
    rnk += zero;
  }
  return rnk;
}
