#pragma once
#ifndef INTEGRATION_METHODS_H
#define INTEGRATION_METHODS_H



using namespace std;

namespace IntegrationMethods
{
	//  This file contains a list of all integration methods used

	/*template <class ModelType>
	void ab2order(ModelType *gen, double *delta);*/
	template <class ModelType>
	void ab2order(ModelType* model, double* delta)
	{
		double xold;
		for (int i = 0; i < model->nstates; i++) {

			//	This is done to hold the state value from two time steps ago
			xold = model->ss[i];															//	This is the state value at the previous time step
			model->ss[i] = model->s[i];														//	Save current state value as old state value
			model->s[i] = 0.5*(model->s[i]+xold) + *delta * (1.5 * model->ds[i]);		//	This is the state value at the next time step
		}

	};

	/*template <class ModelType>
	void euler(ModelType *gen, double *delta);*/
	template <class ModelType>
	void euler(ModelType* model, double* delta)
	{
		double x_old;
		for (int i = 0; i < model->nstates; i++) {
			x_old = model->s[i];
			model->ss[i] = model->s[i];
			model->s[i] = x_old + (*delta) * (model->ds[i]);		//	This is the state value at the next time step
			model->dss[i] = model->ds[i];
		}
	};

	/*template <class ModelType>
	void trapezoidal(ModelType *gen, double *delta);*/
	template <class ModelType>
	void trapezoidal(ModelType* model, double* delta)
	{
		double ds_old, s_old, temp;
		for (int i = 0; i < model->nstates; i++) {

			//	This is done to hold the state value from two time steps ago
			s_old = model->ss[i];
			model->s[i] = s_old + 0.5* (*delta) * (model->ds[i]+model->dss[i]);		//	This is the state value at the next time step
			//model->ss[i] = model->s[i];
		}

	};

}
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace IntegrationMethods;
#endif

#endif // INTEGRATION_METHODS_H
