
#pragma hdrstop

#include "Sparse_Math.h"



// Evaluates of both rows are equal


#include "System.h"

namespace Sparse_Math
{

    Tsparse_matrix::Tsparse_matrix()
        : Row(0),
        col(0),
        len(0)
    {
    }
    TSparse_Complex::TSparse_Complex()
        : Row(0),
        col(0),
        len(0)
    {
    }



    bool Tsparse_matrix::R_equal(PData acols, PData avals, PData bcols, PData bvals)
    {
        bool result = false;
        int idx = 0, rlen = 0;
        result = false;                        // In case they are not equal
        if (acols->size() == bcols->size())   // If they have the same # of Cols
        {
            rlen = 0;                            // First, verify if the cols are the same
            for (int stop = (acols->size() -  1), idx = 0; idx <= stop; idx++)
                if (((*acols)[idx] - (*bcols)[idx] ) != 0)
                    rlen++;
            if (rlen == 0)
                result = true;
        }
        return result;
    }

    // Gets the columns and values at each columns for the row specified

/*

    Tsparse_matrix::Tsparse_matrix()
        : Row(0),
        col(0),
        len(0)
    {
    }
    TSparse_Complex::TSparse_Complex()
        : Row(0),
        col(0),
        len(0)
    {
    }
    */


    void Tsparse_matrix::getrow(int Index, PData cols, PData vals)
    {
        TData rowcols, rowvals;
        int j = 0;
        rowcols.clear();
        rowvals.clear();
        for (int stop = (len - 1), j = 0; j <= stop; j++)
        {
            if (data[j][0] == Index)
            {
                rowcols.push_back(data[j][1]);
                rowvals.push_back(data[j][2]);
            }
        }
        *cols = rowcols;
        *vals = rowvals;
    }


    int Tsparse_matrix::Rank()   // Added 08/16/2018 by DM for calculating the

    {
        int result = 0;

        // Rank of the sparse matrix

        int i = 0, j = 0;
        bool Flag = false;                                   // Row under evaluation
                                         // Reference row

        TData acols, avals, bcols, bvals;
        result = 0;
        for (int stop = (Row - 1), i = 0; i <= stop; i++)
        {
            getrow(i, &acols, &avals);
            if (i > 0)
            {
                j = i - 1;
                Flag = true;
                while (Flag && (j >= 0))
                {
                    getrow(j, &bcols, &bvals);    // sweeps the matrix bottom up
                    Flag = !R_equal(&acols, &avals, &bcols, &bvals);
                    j--;
                }
                if (Flag)
                    result++;
            }
            else
                result++;
        }
        return result;
    }
/*
    Tsparse_matrix::Tsparse_matrix()
        : Row(0),
        col(0),
        len(0)
    {
    }
    TSparse_Complex::TSparse_Complex()
        : Row(0),
        col(0),
        len(0)
    {
    }
*/



    int Tsparse_matrix::NCols()
    {
        int result = 0;
        result = col;
        return result;
    }


    int Tsparse_matrix::NRows()
    {
        int result = 0;
        result = Row;
        return result;
    }


    int Tsparse_matrix::checkifexists(int r, int c)
    {
        int result = 0;
        int i = 0;
        result = -1;                 // Default in case the value doesn't exist
        if (len > 0)
        {
            for (int stop = (len - 1), i = 0; i <= stop; i++)
            {
                if ((data[i][0] == r) && (data[i][1] == c))
                    result = i;              // If the value exists returns the index ( >=0 )
            }
        }
        return result;
    }


    void Tsparse_matrix::sparse_matrix(int r, int c)
    {
        Row = r;    // Initialize row
        col = c;    // Initialize Col
        len = 0;    // Initialize length to 0
        data.clear();
    }

    //Inserts elements into the sparse matrix



    int Tsparse_matrix::Insert(int r, int c, int val)
    {
        int result = 0;
        int lrow = 0  // To store the current lenght of the data matrix
            , lcol = 0;
        result = 1;
        lrow = checkifexists(r, c);
        if (lrow >= 0)
        {
            data[lrow][2] = val;    // Assigns the new value to the existing cell
        }
        else
        {
            // Reshapes the memory space
            lrow = data.size();
            data.resize( lrow + 1 );
            data[lrow].resize(3);
            // Adds the data to the new memory space
            data[data.size() - 1][0] = r;
            data[data.size() - 1][1] = c;
            data[data.size() - 1][2] = val;
            len++;
            if (col < c)
                col = c;
            if (Row < r)
                Row = r;
        }
        return result;
    }
    /*
    Tsparse_matrix::Tsparse_matrix()
        : Row(0),
        col(0),
        len(0)
    {
    }
    TSparse_Complex::TSparse_Complex()
        : Row(0),
        col(0),
        len(0)
    {
    }
    */

    Tsparse_matrix Tsparse_matrix::Add(Tsparse_matrix* b)
    {
        Tsparse_matrix result; // Creates a memory space to store the result
        int addeval = 0, apos = 0, bpos = 0;

        // First checks if the matrices have the same dimensions
        if ((Row != b->Row) || (col != b->col))
        {
            result.sparse_matrix(1, 1);
            result.Insert(0, 0, -1);
        }
        else
        {
            apos = 0;
            bpos = 0;
            result.sparse_matrix(Row, col);
            while ((apos < len) && (bpos < b->len))
            {
                if ((data[apos][0] > b->data[bpos][0]) || ((data[apos][0] == b->data[bpos][0]) && (data[apos][1] > b->data[bpos][1])))
                {
                    result.Insert(b->data[bpos][0], b->data[bpos][1], b->data[bpos][2]);
                    bpos++;
                }
                else
                {
                    if ((data[apos][0] < b->data[bpos][0]) || ((data[apos][0] == b->data[bpos][0]) && (data[apos][1] < b->data[bpos][1])))
                    {
                        result.Insert(data[apos][0], data[apos][1], data[apos][2]);
                        apos++;
                    }
                    else
                    {
                        addeval = data[apos][2] + b->data[bpos][2];
                        if (addeval != 0)
                            result.Insert(data[apos][0], data[apos][1], addeval);
                        apos++;
                        bpos++;
                    }
                }
            }
            // Inserts the remaining elements
            while (apos < (len - 1))
            {
                result.Insert(data[apos][0], data[apos][1], data[apos + 1][2]);
                apos++;
            }
            while (bpos < (b->len - 1))
            {
                result.Insert(b->data[bpos][0], b->data[bpos][1], b->data[bpos + 1][2]);
                bpos++;
            }
        }
        return result;
    }

    // Transposes the sparse matrix



    Tsparse_matrix Tsparse_matrix::Transpose()
    {
        Tsparse_matrix result; // Creates a memory space to store the result
        std::vector < int > count, Index;
        int i = 0, rpos = 0;
                // new matrix with inversed row X col
        result.sparse_matrix(col, Row);
        // same number of elements
        for (int stop = len, i = 1; i <= stop; i++)
            result.Insert(i, 0, 0);
        count.resize(col + 1);
        Index.resize(col + 1);
        // Initialize all to 0
        for (int stop = col, i = 0; i <= stop; i++)
            count[i] = 0;
        for (int stop = (len - 1), i = 0; i <= stop; i++)
            count[data[i][1]]++;
        // to count number of elements having col smaller
        // than particular i
        // as there is no col with value < 1
        Index[0] = 0;
        // initialize rest of the indices
        for (int stop = col, i = 1; i <= stop; i++)
            Index[i] = Index[i - 1] + count[i - 1];
        for (int stop = (len - 1), i = 0; i <= stop; i++)
        {
            // insert a data at rpos and increment its value
            rpos = Index[data[i][1]];
            Index[data[i][1]]++;
            // transpose row=col
            result.data[rpos][0] = data[i][1];

            // transpose col=row
            result.data[rpos][1] = data[i][0];

            // same value
            result.data[rpos][2] = data[i][2];
        }

        // the above method ensures
        // sorting of transpose matrix
        // according to row-col value
        return result;
    }

    // Multiplies another sparse matrix by this matrix

    /*

    Tsparse_matrix::Tsparse_matrix()
        : Row(0),
        col(0),
        len(0)
    {
    }
    TSparse_Complex::TSparse_Complex()
        : Row(0),
        col(0),
        len(0)
    {
    }
    */



    Tsparse_matrix Tsparse_matrix::multiply(Tsparse_matrix* B)
    {
        Tsparse_matrix result; // Creates a memory space to store the result
        int sum = 0, c = 0, tempa = 0, tempb = 0, r = 0, apos = 0, bpos = 0;
        // First checks if the matrices have the right dimensions
        if (col != B->Row)
        {
            result.sparse_matrix(1, 1);
            result.Insert(0, 0, -1);    //Invalid multiplication
        }
        else
        {
            // transpose b to compare row
            // and col values and to add them at the end
            Tsparse_matrix b_ = B->Transpose();
            Tsparse_matrix *b = &b_; // added to minimize changes to the code
            // result matrix of dimension row X b.col
            // however b has been transposed, hence row X b.row
            result.sparse_matrix(Row, b->Row);
            // iterate over all elements of A (this matrix)
            apos = 0;
            while (apos < len)
            {
                r = data[apos][0];
                // iterate over all elements of B
                bpos = 0;
                while (bpos < b->len)
                {
                    // current column of result matrix
                    // data[][0] used as b is transposed
                    c = b->data[bpos][0];

                    // temporary pointers created to add all
                    // multiplied values to obtain current
                    // element of result matrix
                    tempa = apos;
                    tempb = bpos;
                    sum = 0;

                    // iterate over all elements with
                    // same row and col value
                    // to calculate result[r]
                    while ((tempa < len) && (data[tempa][0] == r) && (tempb < b->len) && (b->data[tempb][0] == c))
                    {
                        if (data[tempa][1] < b->data[tempb][1])
                            tempa++;   //skip a
                        else
                        {
                            if (data[tempa][1] > b->data[tempb][1])
                                tempb++;  //skip b
                            else
                            {
                                // same col, so multiply and increment
                                sum = sum + data[tempa][2] * b->data[tempb][2];
                                tempa++;
                                tempb++;
                            }
                        }
                    }
                    // insert sum obtained in result[r]
                    // if its not equal to 0
                    if (sum != 0)
                        result.Insert(r, c, sum);
                    while ((bpos < b->len) && (b->data[bpos][0] == c))
                        bpos++;    // Jump to next column
                }
                while ((apos < len) && (data[apos][0] == r))
                    apos++;    // Jump to next row
            }
        }
        return result;
    }

    // Sorts the content of the matrix by rows, important for multiplications

    void Tsparse_matrix::Sort()
    {
        Tsparse_matrix myTemp;

        myTemp.Reset();

        for (int idx = 0; idx <= Row; idx++)
        {
            for (int i = 0; i < data.size(); i++)
            {
                if (data[i][0] == idx)
                    myTemp.Insert(data[i][0], data[i][1], data[i][2]);
            }
        }
        // moves the new data into the local object
        for (int i = 0; i < myTemp.data.size(); i++)
        {
            data[i][0] = myTemp.data[i][0];
            data[i][1] = myTemp.data[i][1];
            data[i][2] = myTemp.data[i][2];
        }
    }


    // Resets the sparse matrix (makes it empty)



    void Tsparse_matrix::Reset()
    {
        data.clear();
        len = 0;
    }
    // Returns the lenght of the sparse matrix (number of non-zero elements)



    int Tsparse_matrix::NZero()
    {
        int result = 0;
        result = len;
        return result;
    }

    //******************************************************************************
    //*   Complex sparse matrices
    //******************************************************************************

    // Evaluates of both rows are equal

    bool TSparse_Complex::R_equal(PData acols, PData bcols, PComplexArr avals, PComplexArr bvals)
    {
        bool result = false;
        int idx = 0, rlen = 0;
        result = false;                        // In case they are not equal
        if (acols->size() == bcols->size())   // If they have the same # of Cols
        {
            rlen = 0;                            // First, verify if the cols are the same
            for (int stop = (acols->size() - 1), idx = 0; idx <= stop; idx++)
                if (((*acols)[idx] - (*bcols)[idx]) != 0)
                    rlen++;
            if (rlen == 0)
                result = true;
        }
        return result;
    }

    // Returns the value contained at the specific position



    complex TSparse_Complex::getvalue(int Row, int col)
    {
        complex result;
        bool Go_Flag = false;
        int i = 0;
        result = cmplx(0, 0);
        Go_Flag = true;
        i = 0;
        while (Go_Flag)
        {
            if ((CData[i].Row == Row) && (CData[i].col == col))
            {
                result = CData[i].Value;
                Go_Flag = false;
            }
            else
            {
                i++;
                if (i > ( CData.size() - 1 ) )
                    Go_Flag = false;
            }
        }
        return result;
    }

    // Gets the columns and values at each columns for the row specified



    void TSparse_Complex::getrow(int Index, PData cols, PComplexArr vals)
    {
        TData rowcols;
        TComplexArr rowvals;
        int j = 0;
        rowcols.clear();
        rowvals.clear();
        for (int stop = (len - 1), j = 0; j <= stop; j++)
        {
            if (CData[j].Row == Index)
            {
                rowcols.push_back( CData[j].col );
                rowvals.push_back( CData[j].Value );
            }
        }
        *cols = rowcols;
        *vals = rowvals;
    }

    int TSparse_Complex::Rank()   // Added 08/16/2018 by DM for calculating the

    {
        int result = 0;

        // Rank of the sparse matrix

        int i = 0, j = 0;
        bool Flag = false;                                   // Row under evaluation

        TData acols, bcols;                             // Reference row

        TComplexArr avals, bvals;
        result = 0;
        for (int stop = (Row - 1), i = 0; i <= stop; i++)
        {
            getrow(i, &acols, &avals);
            if (i > 0)
            {
                j = i - 1;
                Flag = true;
                while (Flag && (j >= 0))
                {
                    getrow(j, &bcols, &bvals);    // sweeps the matrix bottom up
                    Flag = !R_equal(&acols, &bcols,  &avals,  &bvals);
                    j--;
                }
                if (Flag)
                    result++;
            }
            else
                result++;
        }
        return result;
    }


    int TSparse_Complex::NCols()
    {
        int result = 0;
        result = col;
        return result;
    }


    int TSparse_Complex::NRows()
    {
        int result = 0;
        result = Row;
        return result;
    }


    int TSparse_Complex::checkifexists(int r, int c)
    {
        int result = 0;
        int i = 0;
        result = -1;                 // Default in case the value doesn't exist
        if (len > 0)
        {
            for (int stop = (len - 1), i = 0; i <= stop; i++)
            {
                if ((CData[i].Row == r) && (CData[i].col == c))
                    result = i;              // If the value exists returns the index ( >=0 )
            }
        }
        return result;
    }


    void TSparse_Complex::sparse_matrix_Cmplx(int r, int c)
    {
        Row = r;    // Initialize row
        col = c;    // Initialize Col
        len = 0;    // Initialize length to 0
        CData.clear();
    }

    //Inserts elements into the sparse matrix

    int TSparse_Complex::Insert(int r, int c, complex val)
    {
        int result = 0;
        int lrow = 0  // To store the current lenght of the data matrix
            , lcol = 0;
        result = 1;
        lrow = checkifexists(r, c);
        if (lrow >= 0)
        {
            CData[lrow].Value = val;    // Assigns the new value to the existing cell
        }
        else
        {
            // Reshapes the memory space
            lrow = CData.size();
            CData.resize( lrow + 1 );
            // Adds the data to the new memory space
            CData[CData.size() - 1].Row = r;
            CData[CData.size() - 1].col = c;
            CData[CData.size() - 1].Value = val;
            len++;
            if (col < c)
                col = c;
            if (Row < r)
                Row = r;
        }
        return result;
    }

    // Adds another sparse matrix to this matrix



    TSparse_Complex TSparse_Complex::Add(TSparse_Complex* b)
    {
        TSparse_Complex result; // Creates a memory space to store the result
        complex addeval;
        int apos = 0, bpos = 0;
        // First checks if the matrices have the same dimensions
        if ((Row != b->Row) || (col != b->col))
        {
            result.sparse_matrix_Cmplx(1, 1);
            result.Insert(0, 0, cmplx(-1, 0));
        }
        else
        {
            apos = 0;
            bpos = 0;
            result.sparse_matrix_Cmplx(Row, col);
            while ((apos < len) && (bpos < b->len))
            {
                if ((CData[apos].Row > b->CData[bpos].Row) || ((CData[apos].Row == b->CData[bpos].Row) && (CData[apos].col > b->CData[bpos].col)))
                {
                    result.Insert(b->CData[bpos].Row, b->CData[bpos].col, b->CData[bpos].Value);
                    bpos++;
                }
                else
                {
                    if ((CData[apos].Row < b->CData[bpos].Row) || ((CData[apos].Row == b->CData[bpos].Row) && (CData[apos].col < b->CData[bpos].col)))
                    {
                        result.Insert(CData[apos].Row, CData[apos].col, CData[apos].Value);
                        apos++;
                    }
                    else
                    {
                        addeval = cadd(CData[apos].Value, b->CData[bpos].Value);
                        if ((addeval.re != 0) && (addeval.im != 0))
                            result.Insert(CData[apos].Row, CData[apos].col, addeval);
                        apos++;
                        bpos++;
                    }
                }
            }
            // Inserts the remaining elements
            while (apos < (len - 1))
            {
                result.Insert(CData[apos].Row, CData[apos].col, CData[apos + 1].Value);
                apos++;
            }
            while (bpos < (b->len - 1))
            {
                result.Insert(b->CData[bpos].Row, b->CData[bpos].col, b->CData[bpos + 1].Value);
                bpos++;
            }
        }
        return result;
    }

    // Transposes the sparse matrix

    /*
    Tsparse_matrix::Tsparse_matrix()
        : Row(0),
        col(0),
        len(0)
    {
    }
    TSparse_Complex::TSparse_Complex()
        : Row(0),
        col(0),
        len(0)
    {
    }*/



    TSparse_Complex TSparse_Complex::Transpose()
    {
        TSparse_Complex result; // Creates a memory space to store the result
        std::vector < int > count, Index;
        int i = 0, j = 0, k = 0, rpos = 0;
        
        // new matrix with inversed row X col
        result.sparse_matrix_Cmplx(col, Row);
        // same number of elements
        j = 0;
        k = 0;
        for (int stop = len, i = 1; i <= stop; i++)
        {
            result.Insert(j, k, CZero);
            k++;
            if (k == Row)
            {
                j++;
                k = 0;
            }
        }
        count.resize( col + 1 );
        Index.resize( col + 1 );
        // Initialize all to 0
        for (int stop = col, i = 0; i <= stop; i++)
            count[i] = 0;
        for (int stop = (len - 1), i = 0; i <= stop; i++)
            count[CData[i].col]++;
        // to count number of elements having col smaller
        // than particular i
        // as there is no col with value < 1
        Index[0] = 0;
        // initialize rest of the indices
        for (int stop = col, i = 1; i <= stop; i++)
            Index[i] = Index[i - 1] + count[i - 1];
        for (int stop = (len - 1), i = 0; i <= stop; i++)
        {
            // insert a data at rpos and increment its value
            rpos = Index[CData[i].col];
            Index[CData[i].col]++;
            // transpose row=col
            result.CData[rpos].Row = CData[i].col;

            // transpose col=row
            result.CData[rpos].col = CData[i].Row;

            // same value
            result.CData[rpos].Value = CData[i].Value;
        }

        // the above method ensures
        // sorting of transpose matrix
        // according to row-col value
        return result;
    }

    // Transposes and conjugates the sparse matrix



    TSparse_Complex TSparse_Complex::TransposeConj()
    {
        TSparse_Complex result; // Creates a memory space to store the result
        std::vector < int > count, Index;
        int i = 0, rpos = 0;
        
        // new matrix with inversed row X col
        result.sparse_matrix_Cmplx(col, Row);
        // same number of elements
        for (int stop = len, i = 1; i <= stop; i++)
            result.Insert(i, 0, cmplx(0, 0));
        count.resize( col + 1 );
        Index.resize( col + 1 );
        // Initialize all to 0
        for (int stop = col, i = 0; i <= stop; i++)
            count[i] = 0;
        for (int stop = (len - 1), i = 0; i <= stop; i++)
            count[CData[i].col]++;
        // to count number of elements having col smaller
        // than particular i
        // as there is no col with value < 1
        Index[0] = 0;
        // initialize rest of the indices
        for (int stop = col, i = 1; i <= stop; i++)
            Index[i] = Index[i - 1] + count[i - 1];
        for (int stop = (len - 1), i = 0; i <= stop; i++)
        {
            // insert a data at rpos and increment its value
            rpos = Index[CData[i].col];
            Index[CData[i].col]++;
            // transpose row=col
            result.CData[rpos].Row = CData[i].col;

            // transpose col=row
            result.CData[rpos].col = CData[i].Row;

            // same value
            result.CData[rpos].Value = conjg(CData[i].Value);
        }

        // the above method ensures
        // sorting of transpose matrix
        // according to row-col value
        return result;
    }

    // Multiplies another sparse matrix by this matrix

/*

    Tsparse_matrix::Tsparse_matrix()
        : Row(0),
        col(0),
        len(0)
    {
    }
    TSparse_Complex::TSparse_Complex()
        : Row(0),
        col(0),
        len(0)
    {
    }
    */


    TSparse_Complex TSparse_Complex::multiply(TSparse_Complex* B)
    {
        TSparse_Complex result; // Creates a memory space to store the result
        complex sum;
        int c = 0, tempa = 0, tempb = 0, r = 0, apos = 0, bpos = 0;
        
        // First checks if the matrices have the right dimensions
        if (col != B->Row)
        {
            result.sparse_matrix_Cmplx(1, 1);
            result.Insert(0, 0, cmplx(-1, 0));    //Invalid multiplication
        }
        else
        {
            // transpose b to compare row
            // and col values and to add them at the end
            TSparse_Complex b_ = B->Transpose();
            TSparse_Complex *b = &b_; // added to minimize changes to the code

            // result matrix of dimension row X b.col
            // however b has been transposed, hence row X b.row
            result.sparse_matrix_Cmplx(Row, b->Row);
            // iterate over all elements of A (this matrix)
            apos = 0;
            while (apos < len)
            {
                r = CData[apos].Row;
                // iterate over all elements of B
                bpos = 0;
                while (bpos < b->len)
                {
                    // current column of result matrix
                    // data[][0] used as b is transposed
                    c = b->CData[bpos].Row;

                    // temporary pointers created to add all
                    // multiplied values to obtain current
                    // element of result matrix
                    tempa = apos;
                    tempb = bpos;
                    sum = cmplx(0, 0);

                    // iterate over all elements with
                    // same row and col value
                    // to calculate result[r]
                    while ((tempa < len) && (CData[tempa].Row == r) && (tempb < b->len) && (b->CData[tempb].Row == c))
                    {
                        if (CData[tempa].col < b->CData[tempb].col)
                            tempa++;   //skip a
                        else
                        {
                            if (CData[tempa].col > b->CData[tempb].col)
                                tempb++;  //skip b
                            else
                            {
                                // same col, so multiply and increment
                                sum = cadd(sum, cmul(CData[tempa].Value, b->CData[tempb].Value));
                                tempa++;
                                tempb++;
                            }
                        }
                    }
                    // insert sum obtained in result[r]
                    // if its not equal to 0
                    if ((sum.re != 0) && (sum.im != 0))
                        result.Insert(r, c, sum);
                    while ((bpos < b->len) && (b->CData[bpos].Row == c))
                        bpos++;    // Jump to next column
                }
                while ((apos < len) && (CData[apos].Row == r))
                    apos++;    // Jump to next row
            }
        }
        return result;
    }

    // Sorts the content of the matrix by rows, important for multiplications

    void TSparse_Complex::Sort()
    {
        TSparse_Complex myTemp;

        myTemp.Reset();

        for (int idx = 0; idx <= Row; idx++)
        {
            for (int i = 0; i < CData.size(); i++)
            {
                if (CData[i].Row == idx)
                    myTemp.Insert(CData[i].Row, CData[i].col, CData[i].Value);
            }
        }
        // moves the new data into the local object
        for (int i = 0; i < myTemp.CData.size(); i++)
        {
            CData[i].Row    = myTemp.CData[i].Row;
            CData[i].col    = myTemp.CData[i].col;
            CData[i].Value  = myTemp.CData[i].Value;
        }
    }

    // Resets the sparse matrix (makes it empty)

    void TSparse_Complex::Reset()
    {
        CData.clear();
        len = 0;
    }
    // Returns the lenght of the sparse matrix (number of non-zero elements)



    int TSparse_Complex::NZero()
    {
        int result = 0;
        result = len;
        return result;
    }

} // namespace Sparse_Math







