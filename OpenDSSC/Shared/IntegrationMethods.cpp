#include "IntegrationMethods.h"


/*
* 
* This file contains all the new integration methods considered for the application
* 
* A list of methods implemented (or to be implemented) includes mostly explicit methods and is provided below:
*	- Adams-Bashforth 2nd order
*	- Euler 1st order
*	- Trapezoidal (Adams-Moulton) 2nd order
* 
*/

template <class ModelType>
void IntegrationMethods::ab2order(ModelType *gen, double *delta)
{
	double xold, temp;
	for (int i = 0; i < gen->nstates; i++) {
		
		//	This is done to hold the state value from two time steps ago
		xold = gen->ss[i];						//	This is the state value at the previous time step
		gen->ss[i] = gen->s[i];					//	This is the state value at the current time step

		temp = gen->s[i] - xold / *delta;
		gen->s[i] = gen->s[i] + *delta * (1.5 * gen->ds[i] - 0.5 * temp);		//	This is the state value at the next time step
	}

}
