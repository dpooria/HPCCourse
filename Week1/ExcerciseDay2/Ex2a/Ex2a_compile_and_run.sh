#!/bin/bash

gfortran -ffree-form -c Ex2a.f95
gfortran -ffree-form -c m_Ex2a.f95
gfortran Ex2a.o m_Ex2a.o -o Ex2a.out

./Ex2a.out
