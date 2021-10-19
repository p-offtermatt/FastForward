/****************************************************************************
  This file is part of LoLA.

  LoLA is free software: you can redistribute it and/or modify it under the
  terms of the GNU Affero General Public License as published by the Free
  Software Foundation, either version 3 of the License, or (at your option)
  any later version.

  LoLA is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
  more details.

  You should have received a copy of the GNU Affero General Public License
  along with LoLA. If not, see <http://www.gnu.org/licenses/>.
****************************************************************************/

/*!
\file
\author Andreas
\status approved 11.04.2012

\todo finalize overflow handling
*/

#include <Core/Dimensions.h>
#include <Core/Runtime.h>
#include <Net/LinearAlgebra.h>

// LCOV_EXCL_START

/// writes current row on cout
void Matrix::Row::DEBUG__printRow() const
{
    for (arrayindex_t i = 0; i < varCount; ++i)
    {
        std::cout << coefficients[i] << "*" << variables[i] << " ";
    }
    std::cout << "[" << Net::Name[PL][reference] << "]";
    std::cout << std::endl;
    /*
    for (arrayindex_t i = 0; i < varCount; ++i) {
       //std::cout << coefficients[i] << "*" << variables[i] << " ";
       std::cout << coefficients[i] << "*" << Net::Name[PL][variables[i]] << " ";
    }
    std::cout << std::endl;
    */
}

/// writes current matrix on cout
void Matrix::DEBUG__printMatrix() const
{
    for (arrayindex_t c = 0; c < colCount; ++c)
    {
        std::cout << "column " << c << std::endl;
        Row *curRow = matrix[c];
        while (curRow != NULL)
        {
            curRow->DEBUG__printRow();
            curRow = curRow->next;
        }
    }
}

/// checks whether the reduced matrix has at most one row per column
bool Matrix::DEBUG__checkReduced() const
{
    // for each variable i (=column)
    for (arrayindex_t i = 0; i < colCount; ++i)
    {
        if (matrix[i] != NULL)
        {
            assert(matrix[i]->next == NULL);
        }
    }
    return true;
}

/// checks whether the variables in a row a ordered properly
bool Matrix::Row::DEBUG__checkRow() const
{
    // for each variable i
    arrayindex_t leftElem = 0;
    for (arrayindex_t i = 0; i < varCount; ++i)
    {
        assert(variables[i] >= leftElem);
        if (i > 0)
        {
            assert(variables[i] != leftElem);
        }
        leftElem = variables[i];
    }
    return true;
}
// LCOV_EXCL_STOP

/*!
frees memory of current row
/post variables and coefficients are freed
*/
Matrix::Row::~Row()
{
    delete[] variables;
    delete[] coefficients;
}

/*!
creates a new row based on LinearAlgebra.h types
 \param[in] length is the length of the row that gets created
 \param var the array of values for the row
 \param coef the array of coefficients for the row
 \param ref  the number of the current row
 */
Matrix::Row::Row(const arrayindex_t length, const arrayindex_t *var,
                 const int64_t *coef, const arrayindex_t ref) :
    varCount(length), variables(new arrayindex_t[length]),
    coefficients(new int64_t[length]), reference(ref), next(NULL)
{
    // memcpy is used because given and new memory has the same types
    memcpy(variables, var, length * SIZEOF_ARRAYINDEX_T);
    memcpy(coefficients, coef, length * SIZEOF_INT64_T);

    assert(DEBUG__checkRow());
}

/*!
this function eleminates the first variable on the second row of the first variable
\param[in,out] matrix  the Matrix on which this function should work
\param[in] rowToChange  the index of the row which should get changed
*/
void Matrix::Row::apply(Matrix &matrix, arrayindex_t rowToChange)
{
    if (rowToChange == ARRAYINDEX_T_MAX)
    {
        // the variable to be eliminated should be the same
        assert(variables[0] == next->variables[0]);
        assert(this != next);
    }
    else
    {
        // the second row must exist and be singular and start with a lower variable
        assert(matrix.matrix[rowToChange]);
        assert(matrix.matrix[rowToChange]->next == NULL);
        assert(variables[0] > matrix.matrix[rowToChange]->variables[0]);
    }

    const Row *realNext(rowToChange == ARRAYINDEX_T_MAX ? next : matrix.matrix[rowToChange]);
    //DEBUG__printRow();
    //realNext->DEBUG__printRow();
    // determine the index of the variable to eliminate in the second row
    int64_t ggtFactor;
    arrayindex_t useVar(ARRAYINDEX_T_MAX);
    for (arrayindex_t i = 0; i < realNext->varCount; ++i)
        if (variables[0] == realNext->variables[i])
        {
            useVar = i;
            //std::cout << "v0=" << variables[0] << " c0=" << coefficients[0] << " 2v0=" << realNext->variables[i] << " 2c0=" << realNext->coefficients[i] << std::endl;
            ggtFactor = ggt(coefficients[0], realNext->coefficients[i]);
            //std::cout << "ggt=" << ggtFactor << std::endl;
            break;
        }
    //std::cout << "endvar" << std::endl;
    // do nothing if the variable does not exist in this row
    if (useVar == ARRAYINDEX_T_MAX)
    {
        return;
    }
    //std::cout << "ggt=" << ggtFactor << std::endl;
    // calculate corrective factors
    const int64_t firstRowFactor = realNext->coefficients[useVar] / ggtFactor;
    const int64_t secondRowFactor = coefficients[0] / ggtFactor;

    // get some space for the new row (secondRow - firstRow)
    // at most |firstRow| + |secondRow| elements are neccessary
    // one less is also suitable
    arrayindex_t *newVar = new arrayindex_t[(varCount + realNext->varCount)]();
    int64_t *newCoef = new int64_t[(varCount + realNext->varCount)]();
    arrayindex_t newSize = 0;

    // start with the first element, because the first one is not necessarily ruled out
    arrayindex_t firstRow = 0;
    arrayindex_t secondRow = 0;

    // as long as there are some "common" variables left
    while ((firstRow < varCount) && (secondRow < realNext->varCount))
    {
        // at least one element in both rows is left
        if (variables[firstRow] < realNext->variables[secondRow])
        {
            // the one in the first row has the smaller index
            newVar[newSize] = variables[firstRow];
            // new coefficient is (-1) * rowOne * factorOne
            newCoef[newSize] = safeMult(-firstRowFactor, coefficients[firstRow]);
            // goto next element
            firstRow++;
        }
        else if (variables[firstRow] > realNext->variables[secondRow])
        {
            // the one in the second row has the smaller index
            newVar[newSize] = realNext->variables[secondRow];
            // new coefficient is rowTwo * FactorTwo
            newCoef[newSize] = safeMult(secondRowFactor, realNext->coefficients[secondRow]);
            // goto next element
            secondRow++;
        }
        else
        {
            // it's the same index, so calculate new coefficient
            newVar[newSize] = variables[firstRow];
            // new coefficient is (rowTwo * factorTwo) - (rowOne * factorOne)
            newCoef[newSize] = safeMult(secondRowFactor, realNext->coefficients[secondRow]);
            newCoef[newSize] -= safeMult(firstRowFactor, coefficients[firstRow]);
            // new coefficient may be 0
            if (newCoef[newSize] == 0)
            {
                // decrease newSize
                newSize--;
                // assumption: decreasing 0 will lead to maxInt but
                //              upcoming increase will result in 0 again
            }
            // goto next elements
            firstRow++;
            secondRow++;
        }
        // increase newSize
        newSize++;
    }
    // all "common" elements are processed

    while (firstRow < varCount)
    {
        // first row has more elements as second row
        newVar[newSize] = variables[firstRow];
        // new coefficient is (-1) * rowOne * factorOne
        newCoef[newSize] = safeMult(-firstRowFactor, coefficients[firstRow]);

        // goto next elements
        newSize++;
        firstRow++;
    }

    while (secondRow < realNext->varCount)
    {
        // second row has more elements as first row
        newVar[newSize] = realNext->variables[secondRow];
        // new coefficient is rowTwo * FactorTwo
        newCoef[newSize] = safeMult(secondRowFactor, realNext->coefficients[secondRow]);

        // goto next elements
        newSize++;
        secondRow++;
    }

    // new row has been calculated
    // decrease coefficients
    // calculate ggt of new row

    ggtFactor = newCoef[0];
    for (arrayindex_t i = 1; i < newSize; ++i)
    {
        ggtFactor = ggt(ggtFactor, newCoef[i]);
    }
    // use new ggt for new row
    for (arrayindex_t i = 0; i < newSize; ++i)
    {
        newCoef[i] /= ggtFactor;
    }

    // save current reference of second row
    const arrayindex_t curReference = realNext->reference;
    // delete second row
    if (rowToChange == ARRAYINDEX_T_MAX)
    {
        matrix.deleteRow(this);
    }
    else
    {
        delete matrix.matrix[rowToChange];
        --matrix.rowCount;
        matrix.matrix[rowToChange] = NULL;
    }

    // create new row based on new arrays
    if (newSize != 0)
    {
        if (rowToChange == ARRAYINDEX_T_MAX)
        {
            assert(newVar[0] > variables[0]);
        }
        matrix.addRow(newSize, newVar, newCoef, curReference);
    }
    // free memory of the new row (data is already processed)
    delete[] newVar;
    delete[] newCoef;
}
/*!
  frees memory of current matrix
 \brief the Deconstructor of Matrix
 /post all rows in the Matrix and the Matrix itself are freed
 */
Matrix::~Matrix()
{
    for (arrayindex_t c = 0; c < colCount; ++c)
    {
        Row *curRow = matrix[c];
        while (curRow != NULL)
        {
            // save current row
            Row *toDelete = curRow;
            // set next row (successor of current row)
            curRow = curRow->next;
            // delete current row
            delete toDelete;
            --rowCount;
        }
        matrix[c] = NULL;
    }
    // delete array for variables (and their rows)
    delete[] matrix;
}

/*!
 \brief Constructor of Matrix
 creates an new Matrix Object
 \param [in] size the number of variables eg columns of the new Matrix
 */
Matrix::Matrix(const arrayindex_t size) : rowCount(0), colCount(size), significantColCount(0)
{
    matrix = new Row*[size];

    for (arrayindex_t i = 0; i < size; ++i)
    {
        matrix[i] = NULL;
    }
}

/*!
 \brief adds a row to the current matrix
 this functions adds a row to the current matrix and initialises it with the given parameters
 \param [in] length the length of the new row
 \param [in] var a array of values for the row
 \param [in] coef the coefficients for the row
 \param [in] ref the number of the current row
 */
void Matrix::addRow(const arrayindex_t length, const arrayindex_t *var, const int64_t *coef,
                    arrayindex_t ref)
{
    // if new row contains no variables, do nothing
    if (length == 0)
    {
        return;
    }

    // create new row based on given data
    Row *row = new Row(length, var, coef, ref);

    // insert new row at right position
    row->next = matrix[row->variables[0]];
    matrix[row->variables[0]] = row;

    // increase rowCount
    ++rowCount;
}

/*!
 deletes the successor ot the given row in the current matrix
 \param [in] row the predecessor row of the row to get deleted
 \post the successor of row is deleted
 */
void Matrix::deleteRow(Row *row)
{
    // if row or its successor is NULL, do nothing
    if (row == NULL or row->next == NULL)
    {
        return;
    }

    // set successor to successors sucessor
    Row *tmp = row->next;
    row->next = row->next->next;

    // delete successor
    delete tmp;

    // decrease rowCount
    --rowCount;
}


/*!
  reduces the current matrix to triangular form
  \post the current matrix is reduced to triangular form
 */
void Matrix::reduce()
{
    // if there no rows, do nothing
    if (rowCount == 0)
    {
        return;
    }

    // for each variable i (=column)
    for (arrayindex_t i = 0; i < colCount; ++i)
    {
        RT::rep->bar(i, colCount);
        if (matrix[i] != NULL)
        {
            significantColCount++;
        }
        // if there at least two rows with variable i as first variable
        while ((matrix[i] != NULL) && (matrix[i]->next != NULL))
        {
            // get rid of the second row
            matrix[i]->apply(*this);
        }
    }
    RT::rep->bar(1, 1);
    assert(DEBUG__checkReduced());
}

/*!
 reduce the current matrix approaching diagonal form
  \post the current matrix is reduced to diagonal form
 */
void Matrix::diagonalise()
{
    // if there are no rows, do nothing
    if (rowCount == 0)
    {
        return;
    }

    reduce();

    // eliminate entries in upper rows (i) using lower rows (j)
    for (arrayindex_t i = 0; i < colCount; ++i)
        if (matrix[i] != NULL)
        {
            for (arrayindex_t j = i + 1; j < colCount; ++j)
            {
                if (matrix[j] != NULL)
                {
                    matrix[j]->apply(*this, i);
                }
            }
            // make diagonal entry positive (by multiplying row with -1)
            if (matrix[i]->coefficients[0] < 0)
            {
                for (arrayindex_t v = 0; v < matrix[i]->varCount; ++v)
                {
                    matrix[i]->coefficients[v] *= -1;
                }
            }
        }
}

/// Returns true iff a column with given index is significant
bool Matrix::isSignificant(const arrayindex_t column) const
{
    assert(column < colCount);
    return (matrix[column] != NULL);
}

/// Returns row of the first row with given index
Matrix::Row *Matrix::getRow(const arrayindex_t column) const
{
    assert(column < colCount);
    return matrix[column];
}

/// Returns the number of significant (= not empty) columns
arrayindex_t Matrix::getSignificantColCount() const
{
    assert(significantColCount <= colCount);
    return significantColCount;
}
