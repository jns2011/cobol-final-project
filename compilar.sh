#!/bin/bash

set -e

echo "🔧 Compilando subprogramas de SUBPROGRAMAS..."
cd subprogramas
cobc -m buscar-cliente.cbl -I../includes
cp BUSCAR-CLIENTE.dll ../altas/
cd ..

echo "🔧 Compilando subprogramas de ALTAS..."
cd altas
cobc -m calculate-cbf.cbl -I./includes -I../includes
cobc -m crear-cbf.cbl -I./includes -I../includes
cobc -m guardar-cliente.cbl -I./includes -I../includes

echo "🚀 Compilando ejecutable ALTAS..."
cobc -x altas.cbl -I./includes -I../includes -o altas.exe

echo "🔧 Compilando subprogramas de CAJERO..."
cd ../cajero
cobc -m crear-transaccion.cbl -I./includes -I../includes

echo "🚀 Compilando ejecutable CAJERO..."
cobc -x cajero.cbl -I./includes -I../includes -o cajero.exe

echo "✅ Compilación completa."